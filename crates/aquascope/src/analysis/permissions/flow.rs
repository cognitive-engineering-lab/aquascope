//! Region flow analysis for lifetime errors.
//!
//! Answers queries of the form, is the flow from region 1  to region 2 valid?
//!
//! ## High-level idea
//!
//! 1. A region R is abstract (φ) if there exists a flow from an abstract source to R.
//!
//! 2. A region R is concrete (ω) if there exists a flow from a local borrow to R.
//!    (in other words, if R conflicts with a body-owned `Place`).
//!
//! NOTE: a region can be both abstract and concrete.
//!
//! The following rules will work on a graph `G`, and a borrow-check error is
//! represented by *borrowfail G*.
//!
//! ### Missing universal constraint
//!
//! A "missing constraint" error occurs IFF:
//!
//! ```text
//! φ_1 -> φ_2     φ_1 ⊈ φ_2
//! ----------------------------
//!       borrowfail G
//! ```
//!
//! An example of this:
//!
//! ```rust,ignore
//! fn ident<'a, 'b, T>(a: &'a T, b: &'b T) -> &'a T {
//!   b
//! }
//! ```
//!
//! Notice that a "flows into" relationship requires a `subset` origin relationship.
//!
//! ### Local outlives universal
//!
//! A "local outlives" error occurs IFF:
//!
//! ```text
//!     ω -> φ
//! ---------------
//!  borrowfail G
//! ```
//!
//! ### Other region related errors
//!
//! Notably, there are other ways in which a region error could occur:
//! - A "hidden type" error occurs if a struct captures a lifetime that does not
//!   appear in the resulting types member constraints (arising from `impl Trait`).
//! - The simplest case, two concrete values involved in a region error.
//!   See examples at: <https://doc.rust-lang.org/book/ch10-03-lifetime-syntax.html>.
//!
//! These types of region errors are not covered by this analysis and may be coming in the future.
//!
//! ## Implementation details
//!
//! The simplified algorithm here represents a subset of the rustc "Region Inference"
//! algorithm, details about it can be found at <https://rustc-dev-guide.rust-lang.org/borrow_check/region_inference.html>.
//!
//! Briefly summarized here, the set of constraints provided by Polonius' `subset_base` facts
//! are turned into a flow graph, where a constraint such as `'a: 'b` get's turned into an
//! edge `'a -> 'b`. This graph forms the basis of flow analysis as outlined previously.
use std::time::Instant;

use itertools::Itertools;
use rustc_borrowck::borrow_set::BorrowData;
use rustc_data_structures::{
  fx::FxHashMap,
  graph::{scc::Sccs, vec_graph::VecGraph, WithSuccessors},
};
use rustc_index::bit_set::{BitMatrix, BitSet, SparseBitMatrix};
use serde::Serialize;
use smallvec::SmallVec;
use ts_rs::TS;

use super::{places_conflict, Origin, PermissionsCtxt};
use crate::mir::utils::{BodyExt, ToRegionVid};

rustc_index::newtype_index! {
  pub struct SccIdx {
    DEBUG_FORMAT = "scc{}"
  }
}

impl polonius_engine::Atom for SccIdx {
  fn index(self) -> usize {
    rustc_index::vec::Idx::index(self)
  }
}

#[derive(Debug, Clone, Serialize, TS)]
#[ts(export)]
pub enum FlowEdgeKind {
  LocalOutlivesUniversal,
  MissingUniversalConstraint,
  // NOTE: this case is not disjoint from `LocalOutlivesUniversal`,
  // but is reported after the other because it happens in more rare cases
  // and I find the former more important to bring attention to.
  LocalInvalidatedAtExit,
  Ok,
}

impl FlowEdgeKind {
  pub fn is_valid_flow(&self) -> bool {
    matches!(self, FlowEdgeKind::Ok)
  }
}

enum AbstractRegionKind {
  // The region belongs to an abstract SCC sink.
  Source(SccIdx),

  // The region can be reached from the given sinks
  // (but is itself not an SCC member).
  Flowed(BitSet<SccIdx>),

  // Not an abstract region.
  None,
}

#[allow(dead_code)]
pub struct RegionFlows {
  /// The flow constraint graph over the `subset_base` relation.
  constraint_graph: Sccs<Origin, SccIdx>,

  /// Full set of known flows per the `known_placeholder_subset` relation.
  known_flows: SparseBitMatrix<SccIdx, SccIdx>,

  /// Components that contain a placeholder region.
  abstract_sources: BitSet<SccIdx>,

  /// Local regions that point to a body-owned value.
  local_sources: BitSet<SccIdx>,

  /// Local regions that could dangle due to an exit invalidation.
  dangling_local_sources: BitSet<SccIdx>,

  /// The set of abstract components that a given component could contain.
  contains_abstract: BitMatrix<SccIdx, SccIdx>,

  /// The set of local components that a given component could contain.
  contains_local: BitMatrix<SccIdx, SccIdx>,
}

impl RegionFlows {
  fn get_abstract_kind(&self, origin: Origin) -> AbstractRegionKind {
    let scc = self.constraint_graph.scc(origin);
    if self.abstract_sources.contains(scc) {
      return AbstractRegionKind::Source(scc);
    }

    if self.contains_abstract.count(scc) > 0 {
      // XXX: do we really need to (or want to) do this?
      let mut sources = BitSet::new_empty(self.constraint_graph.num_sccs());
      for s in self.contains_abstract.iter(scc) {
        sources.insert(s);
      }
      return AbstractRegionKind::Flowed(sources);
    }

    AbstractRegionKind::None
  }

  pub fn scc(&self, origin: Origin) -> SccIdx {
    self.constraint_graph.scc(origin)
  }

  fn is_known_flow(&self, from: SccIdx, to: SccIdx) -> bool {
    self.known_flows.contains(from, to)
  }

  pub fn is_abstract_mem(&self, origin: Origin) -> bool {
    let scc = self.constraint_graph.scc(origin);
    self.contains_abstract.count(scc) > 0
  }

  pub fn is_local_mem(&self, origin: Origin) -> bool {
    let scc = self.constraint_graph.scc(origin);
    self.contains_local.count(scc) > 0
  }

  /// Get the specific kind of flow edge that connects `from` and `to`.
  ///
  /// **ASSUMED**: there must exists an edge between these two origins in the
  /// flow constraint graph, this means that only edges from Polonius' `subset_base`
  /// can be passed as arguments.
  #[allow(clippy::match_same_arms)]
  pub(crate) fn flow_kind(&self, from: Origin, to: Origin) -> FlowEdgeKind {
    let from_contains_concrete = self.is_local_mem(from);
    let from_contains_abstract = self.is_abstract_mem(from);
    let to_contains_concrete = self.is_local_mem(to);
    let to_contains_abstract = self.is_abstract_mem(to);

    log::debug!(
      "Analyzing flow {from:?} -> {to:?} {:?} -> {:?}",
      self.scc(from),
      self.scc(to)
    );
    log::debug!("{from:?} is_local? {from_contains_concrete} is_abstract? {from_contains_abstract}", );
    log::debug!("{to:?} is_local? {to_contains_concrete} is_abstract? {to_contains_abstract}", );

    // The easy case of a potentially concrete region flows to a potentially abstract region.
    if from_contains_concrete && to_contains_abstract {
      log::debug!("early return local outlives universal");
      return FlowEdgeKind::LocalOutlivesUniversal;
    }

    // They're both placeholders, so just check flow constraints:
    match (self.get_abstract_kind(from), self.get_abstract_kind(to)) {
      // Flowing from an abstract source.
      // An abstract source means that the `to`
      (AbstractRegionKind::Source(from), AbstractRegionKind::Source(to)) => {
        if self.is_known_flow(from, to) {
          FlowEdgeKind::Ok
        } else {
          FlowEdgeKind::MissingUniversalConstraint
        }
      }
      // We consider any case where an abstract source flowing into a concrete region is OK.
      (AbstractRegionKind::Source(_), _) => FlowEdgeKind::Ok,

      // Flowing from a concrete region.
      // Local region to abstract sink is disallowed.
      (_, AbstractRegionKind::Source(_)) if from_contains_concrete => {
        FlowEdgeKind::LocalOutlivesUniversal
      }

      // Flowing from a concrete region potentially holding abstract region values.
      (AbstractRegionKind::Flowed(froms), AbstractRegionKind::Source(to))
        if !froms.iter().all(|from| self.is_known_flow(from, to)) =>
      {
        FlowEdgeKind::MissingUniversalConstraint
      }

      // If the region data is flowing from could contain a dangling
      // pointer we report this as a flow violation
      // XXX:(though, I'm not 100% confident here).
      (_, _) => {
        let from = self.constraint_graph.scc(from);
        for local in self.contains_local.iter(from) {
          if self.dangling_local_sources.contains(local) {
            return FlowEdgeKind::LocalInvalidatedAtExit;
          }
        }

        FlowEdgeKind::Ok
      }
    }
  }
}

pub fn compute_flows(ctxt: &mut PermissionsCtxt) {
  let timer = Instant::now();
  let tcx = ctxt.tcx;
  let body = &ctxt.body_with_facts.body;

  macro_rules! flat_nodes {
    ($tups:expr) => {
      $tups
        .iter()
        .flat_map(|&(o1, o2)| SmallVec::<[Origin; 2]>::from_buf([o1, o2]))
        .unique()
    };
  }

  macro_rules! node_count {
    ($tups:expr) => {
      flat_nodes!($tups)
        .minmax_by_key(|v| v.index())
        .into_option()
        .map(|(_, mx)| mx.index() + 1)
        .unwrap_or(0)
    };
  }

  // Compute the constraint graph with all regions.
  let constraints = ctxt
    .polonius_input_facts
    .subset_base
    .iter()
    .map(|&(o1, o2, _)| (o1, o2))
    .collect::<Vec<_>>();

  let vertices = flat_nodes!(constraints).collect::<Vec<_>>();

  // Regions that only occur in the return type are not
  // included in the abstract placeholders set. Example:
  // ```rust,ignore
  // fn mk_string() -> &'a String {
  //   let s = String::from("s");
  //   &s
  // }
  // ```
  // The `'a`, does not appear in the placeholders set.
  let placeholders = ctxt
    .polonius_input_facts
    .placeholder
    .iter()
    .filter_map(|&(p, _)| vertices.contains(&p).then_some(p))
    .chain(body.regions_in_return().map(|rg| rg.to_region_vid()))
    .collect::<Vec<_>>();

  log::debug!("PLACEHOLDERS: {placeholders:#?}");

  let placeholder_edges = ctxt
    .polonius_input_facts
    .known_placeholder_subset
    .iter()
    .filter(|(f, t)| vertices.contains(f) && vertices.contains(t))
    .copied()
    .chain(placeholders.iter().map(|&o| (o, o)))
    .collect::<Vec<_>>();

  log::debug!("Constraints---\n{:#?}", constraints);

  // Graph of constraints that need to be satisfied. This shows
  // us how data flows from one region into another.
  let constraint_graph = VecGraph::new(node_count!(constraints), constraints);

  // Allowed flows between abstract regions.
  let known_flows_graph =
    VecGraph::new(node_count!(placeholder_edges), placeholder_edges);

  let scc_constraints = Sccs::<Origin, SccIdx>::new(&constraint_graph);

  // Compute the total flow facts (expensive).
  let num_sccs = scc_constraints.num_sccs();
  let mut known_flows = SparseBitMatrix::new(num_sccs);

  for &r1 in placeholders.iter() {
    for r2 in known_flows_graph.depth_first_search(r1) {
      let s1 = scc_constraints.scc(r1);
      let s2 = scc_constraints.scc(r2);
      known_flows.insert(s1, s2);
    }
  }

  let mut local_sources = BitSet::new_empty(num_sccs);
  for (_, bd) in ctxt.borrow_set.location_map.iter() {
    if !(bd.borrowed_place.is_indirect()
      || bd.borrowed_place.ty(body, tcx).ty.is_ref())
    {
      let scc = scc_constraints.scc(bd.region);
      local_sources.insert(scc);
    }
  }

  let mut abstract_sources = BitSet::new_empty(num_sccs);
  for origin in placeholders.iter() {
    log::debug!("ABSTRACT REGION {:?}", origin);
    let scc = scc_constraints.scc(*origin);
    abstract_sources.insert(scc);
  }

  log::debug!(
    "Abstract sources---\n{:#?}",
    abstract_sources.iter().collect::<Vec<_>>()
  );

  log::debug!(
    "Local sources---\n{:#?}",
    local_sources.iter().collect::<Vec<_>>()
  );

  // Mapping of region to regions it could contain.
  // row: region
  // col: row-region points to col-region
  let mut contains_abstract = BitMatrix::new(num_sccs, num_sccs);
  let mut abstract_visited = abstract_sources.clone();
  let mut changed = true;
  // Initialize all abstract sources.
  for a in abstract_sources.iter() {
    contains_abstract.insert(a, a);
  }

  while changed {
    changed = false;
    for r1 in contains_abstract.rows() {
      // Mark everything that this region flows to as abstract,
      // every region that `r` could contain the flow to region
      // could also contain.
      for r2 in scc_constraints.depth_first_search(r1) {
        log::debug!("abstract flow {r1:?} -> {r2:?}");
        changed |= abstract_visited.insert(r2);
        changed |= contains_abstract.union_rows(r1, r2);
      }
    }
  }

  let mut contains_local = BitMatrix::new(num_sccs, num_sccs);
  let mut local_visited = local_sources.clone();
  let mut changed = true;
  // Initialize with local sources
  for l in local_sources.iter() {
    contains_local.insert(l, l);
  }

  while changed {
    changed = false;

    for r1 in contains_local.rows() {
      // Mark everything that this region flows to as abstract,
      // every region that `r` could contain the flow to region
      // could also contain.
      for r2 in scc_constraints.depth_first_search(r1) {
        log::debug!("local flow {r1:?} -> {r2:?}");
        changed |= local_visited.insert(r2);
        changed |= contains_local.union_rows(r1, r2);
      }
    }
  }

  let mut dangling_local_sources = BitSet::new_empty(num_sccs);

  for (_, loans) in ctxt.polonius_output.errors.iter() {
    for &loan in loans.iter() {
      let bd = ctxt.loan_to_borrow(loan);
      if check_for_invalidation_at_exit(ctxt, bd) {
        let scc = scc_constraints.scc(bd.region);
        dangling_local_sources.insert(scc);
      }
    }
  }

  log::debug!("=== contains_abstract ===\n{:#?}", {
    contains_abstract
      .rows()
      .map(|r| (r, contains_abstract.iter(r).collect::<Vec<_>>()))
      .collect::<FxHashMap<_, _>>()
  });

  log::debug!("=== contains_local ===\n{:#?}", {
    contains_local
      .rows()
      .map(|r| (r, contains_local.iter(r).collect::<Vec<_>>()))
      .collect::<FxHashMap<_, _>>()
  });

  let region_flows = RegionFlows {
    constraint_graph: scc_constraints,
    known_flows,
    abstract_sources,
    local_sources,
    dangling_local_sources,
    contains_abstract,
    contains_local,
  };

  ctxt.region_flows = Some(region_flows);

  log::info!(
    "region flow analysis for {:?} took: {:?}",
    {
      let owner = tcx.hir().body_owner(ctxt.body_id);
      match tcx.hir().opt_name(owner) {
        Some(name) => name.to_ident_string(),
        None => "<anonymous>".to_owned(),
      }
    },
    timer.elapsed()
  );
}

/// Check if the given borrow is invalidated by an exit point.
///
/// Exit point would refer to a `StorageDead` or `Drop`.
fn check_for_invalidation_at_exit<'tcx>(
  ctxt: &PermissionsCtxt<'_, 'tcx>,
  borrow: &BorrowData<'tcx>,
) -> bool {
  use places_conflict::AccessDepth::{Deep, Shallow};
  use rustc_middle::{
    mir::{PlaceElem, PlaceRef, ProjectionElem},
    ty::TyCtxt,
  };

  let place = borrow.borrowed_place;
  let tcx = ctxt.tcx;
  let body = &ctxt.body_with_facts.body;

  // let root_place = Place::from_local(place.local, tcx);

  struct TyCtxtConsts<'tcx>(TyCtxt<'tcx>);
  impl<'tcx> TyCtxtConsts<'tcx> {
    const DEREF_PROJECTION: &'tcx [PlaceElem<'tcx>; 1] =
      &[ProjectionElem::Deref];
  }

  let mut root_place = PlaceRef {
    local: place.local,
    projection: &[],
  };

  let (might_be_alive, will_be_dropped) =
    if body.local_decls[root_place.local].is_ref_to_thread_local() {
      // Thread-locals might be dropped after the function exits
      // We have to dereference the outer reference because
      // borrows don't conflict behind shared references.
      root_place.projection = TyCtxtConsts::DEREF_PROJECTION;
      (true, true)
    } else {
      (false, ctxt.locals_are_invalidated_at_exit)
    };

  if !will_be_dropped {
    log::debug!(
      "place_is_invalidated_at_exit({:?}) - won't be dropped",
      place
    );
    return false;
  }

  let sd = if might_be_alive { Deep } else { Shallow(None) };

  places_conflict::borrow_conflicts_with_place(
    tcx,
    body,
    place,
    borrow.kind,
    root_place,
    sd,
    places_conflict::PlaceConflictBias::Overlap,
  )
}
