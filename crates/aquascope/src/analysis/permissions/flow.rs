//! Region flow analysis for lifetime errors.
//!
//! Answer queries of the form, can region 1 flow into region 2?
//!
//! 1. A region R is abstract (φ) if there exists a flow from an abstract source to R.
//!
//! 2. A region R is concrete (ω) if there exists a flow from a local borrow to R.
//!    (in other words, if R conflicts with a body-owned `Place`).
//!
//! XXX: a region could be both abstract and concrete.
//!
//! ① A "missing constraint" error occurs IFF:
//!
//! φ_1 -> φ_2     φ_1 ⊈ φ_2
//! ----------------------------
//!       borrowfail G
//!
//! An example of this:
//!
//! ```text
//! fn ident<'a, 'b, T>(a: &'a T, b: &'b T) -> &'a T {
//!   b
//! }
//! ```
//! 'b -> 'a but 'b ⊈ 'a
//!
//! You'll notice that a "flows into" relationship requires
//! a `subset` origin relationship.
//!
//!
//! ② A "local outlives" error occurs IFF:
//!
//!     ω -> φ
//! ---------------
//!  borrowfail G
//!
//! ③  A "hidden type" error occurs IFF:
//!
//! TODO

use itertools::Itertools;
use rustc_borrowck::borrow_set::BorrowData;
use rustc_data_structures::graph::{
  scc::Sccs, vec_graph::VecGraph, WithSuccessors,
};
use rustc_index::bit_set::{BitMatrix, BitSet};
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

/// Given facts from polonius:
///
/// decl placeholder(Origin).
/// decl borrowed_local_place(Origin).
/// decl subset(Origin0, Origin1).
/// decl known_flow(Origin0, Origin1).
///
/// ---
///
/// decl is_local_source(Origin).
///
/// is_local_source(Origin) :-
///   borrowed_local_place(Origin).
///
/// ---
///
/// decl is_abstract_source(Origin).
///
/// is_abstract_source(Origin) :-
///   placeholder(Origin).
///
/// is_abstract_source(Origin) :-
///   subset(Origin0, Origin),
///   subset(Origin, Origin0),
///   is_abstract_source(Origin0).
///
/// ---
///
/// decl abstract_region(Origin).
///
/// abstract_region(Origin) :-
///   is_abstract_source(Origin).
///
/// abstract_region(Origin) :-
///   subset(Origin0, Origin),
///   abstract_region(Origin0).
///
/// ---
///
/// decl local_region(Origin).
///
/// local_region(Origin) :-
///   is_local_source(Origin).
///
/// local_region(Origin) :-
///   subset(Origin0, Origin),
///   local_region(Origin0).
///
/// ---
///
/// decl flow_violation(Origin0, Origin1).
///
/// flow_violation(Origin0, Origin1) :-
///   TODO.

// TODO we might be able to make some of these sparse matrices, I haven't
// gathered too much data on the actual edges yet.
#[allow(dead_code)]
pub struct RegionFlows {
  /// The flow constraint graph over the `subset_base` relation.
  constraint_graph: Sccs<Origin, SccIdx>,

  /// Full set of known flows per the `known_placeholder_subset` relation.
  known_flows: BitMatrix<SccIdx, SccIdx>,

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

  /// Get the specific kind of flow edge that connects `from` and `to`.
  ///
  /// **ASSUMED**: there must exists an edge between these two origins in the
  /// flow constraint graph, this means that only edges from Polonius' `subset_base`
  /// can be passed as arguments.
  #[allow(clippy::match_same_arms)]
  pub(crate) fn flow_kind(&self, from: Origin, to: Origin) -> FlowEdgeKind {
    // They're both placeholders, so just check flow constraints:
    match (self.get_abstract_kind(from), self.get_abstract_kind(to)) {
      // Case 1: flowing from an abstract source.
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

      // Case 2: flowing from a concrete region potentially holding abstract region values.
      (AbstractRegionKind::Flowed(froms), AbstractRegionKind::Source(to))
        if !froms.iter().all(|from| self.is_known_flow(from, to)) =>
      {
        FlowEdgeKind::MissingUniversalConstraint
      }

      // Case 3: flowing from a strictly concrete region.
      // Local region to abstract sink is disallowed.
      (AbstractRegionKind::None, AbstractRegionKind::Source(_)) => {
        FlowEdgeKind::LocalOutlivesUniversal
      }

      // Case 4: if the region data is flowing from could contain a dangling
      // pointer we report this as a flow violation (though, I'm not 100%
      // confident here).
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

  // NOTE: regions that only occur in the return type are not
  // included in the abstract placeholders set. Example:
  // ```rust,no-run
  // fn mk_string() -> &'a String {
  //   let s = String::from("s");
  //   &s
  // }
  // ```
  // The `'a`, included for clarity but would be implicit usually,
  // does not appear in the placeholders set.
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

  // Graph of constraints that need to be satisfied. This shows
  // us how data flows from one region into another.
  let constraint_graph = VecGraph::new(node_count!(constraints), constraints);

  // Allowed flows between abstract regions.
  let known_flows_graph =
    VecGraph::new(node_count!(placeholder_edges), placeholder_edges);

  let scc_constraints = Sccs::<Origin, SccIdx>::new(&constraint_graph);

  // Compute the total flow facts (expensive).
  let num_sccs = scc_constraints.num_sccs();
  let mut known_flows = BitMatrix::new(num_sccs, num_sccs);

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
      if !abstract_visited.contains(r1) {
        continue;
      }

      // Mark everything that this region flows to as abstract,
      // every region that `r` could contain the flow to region
      // could also contain.
      for r2 in scc_constraints.depth_first_search(r1) {
        changed |= abstract_visited.insert(r2);
        changed |= contains_abstract.union_rows(r1, r2);
      }
    }
  }

  let mut contains_local = BitMatrix::new(num_sccs, num_sccs);
  let mut local_visited = local_sources.clone();
  let mut changed = true;

  while changed {
    changed = false;

    for r1 in contains_local.rows() {
      if !local_visited.contains(r1) {
        continue;
      }

      // Mark everything that this region flows to as abstract,
      // every region that `r` could contain the flow to region
      // could also contain.
      for r2 in scc_constraints.depth_first_search(r1) {
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
      (
        // TODO: FIXME: HACK: this isn't always true, but for
        // our context of only evaluating function bodies it
        // may be ok for many situations (not closures though).
        false, true, // self.locals_are_invalidated_at_exit
      )
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
