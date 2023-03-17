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
//! The presented rules work on a graph `G` and a borrow-check error is represented by *borrowfail G*.
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
//!
//! - A "hidden type" error occurs if a struct captures a lifetime that does not
//!   appear in the resulting types member constraints (arising from `impl Trait`).
//!
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
  fx::{FxHashMap as HashMap, FxHashSet as HashSet},
  graph::{
    scc::Sccs, vec_graph::VecGraph, DirectedGraph, WithNumNodes, WithSuccessors,
  },
};
use rustc_index::{
  bit_set::{BitMatrix, HybridBitSet},
  vec::Idx,
};
use serde::Serialize;
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
  /// A local value is flowing into an abstract region.
  LocalOutlivesUniversal,

  /// An abstract region is flowing into another,
  /// but there is no constraint between the two.
  MissingUniversalConstraint,

  /// A local value will be invalidated at an exit point.
  ///
  /// Note, this case is not disjoint from `LocalOutlivesUniversal` but
  /// is slightly more generic. This could be returned if a local value escapes
  /// its scope, not necessarily a function boundary, which would be necessary
  /// for `LocalOutlivesUniversal`. Because it's more generic, this type of
  /// edge kind would be reported with lower priorty.
  LocalInvalidatedAtExit,
  Ok,
}

impl FlowEdgeKind {
  pub fn is_valid_flow(&self) -> bool {
    matches!(self, FlowEdgeKind::Ok)
  }
}
#[allow(dead_code)]
pub struct RegionFlows {
  /// The flow constraint graph over the `subset_base` relation.
  constraint_graph: Sccs<Origin, SccIdx>,

  /// Full set of known flows per the `known_placeholder_subset` relation.
  known_flows: BitMatrix<SccIdx, SccIdx>,

  /// Local regions that could dangle due to an exit invalidation.
  dangling_local_sources: HybridBitSet<SccIdx>,

  /// The set of abstract components that a given component could contain.
  contains_abstract: BitMatrix<SccIdx, SccIdx>,

  /// The set of local components that a given component could contain.
  contains_local: BitMatrix<SccIdx, SccIdx>,
}

impl RegionFlows {
  pub fn scc(&self, origin: Origin) -> SccIdx {
    self.constraint_graph.scc(origin)
  }

  pub fn has_abstract_member(&self, origin: Origin) -> bool {
    self.contains_abstract.count(self.scc(origin)) > 0
  }

  pub fn has_local_member(&self, origin: Origin) -> bool {
    self.contains_local.count(self.scc(origin)) > 0
  }

  /// Get the specific kind of flow edge that connects `from` and `to`.
  #[allow(clippy::match_same_arms)]
  pub(crate) fn flow_kind(&self, from: Origin, to: Origin) -> FlowEdgeKind {
    let scc_from = self.constraint_graph.scc(from);
    let scc_to = self.constraint_graph.scc(to);

    // Data can always flow within the same SCC.
    if scc_from == scc_to {
      return FlowEdgeKind::Ok;
    }

    let from_contains_abstract = self.contains_abstract.count(scc_from) > 0;
    let from_contains_local = self.contains_local.count(scc_from) > 0;

    let to_contains_abstract = self.contains_abstract.count(scc_to) > 0;
    let to_contains_local = self.contains_local.count(scc_to) > 0;

    log::debug!(
      "Analyzing flow {from:?} -> {to:?} {:?} -> {:?}",
      self.scc(from),
      self.scc(to)
    );

    log::debug!("{from:?} is_local? {from_contains_local} is_abstract? {from_contains_abstract}", );
    log::debug!("{to:?} is_local? {to_contains_local} is_abstract? {to_contains_abstract}", );

    // A local value can never flow into a concrete region.
    if from_contains_local && to_contains_abstract {
      log::debug!("early return local outlives universal");
      return FlowEdgeKind::LocalOutlivesUniversal;
    }

    // If both regions contain abstract, we check that all regions in `from`
    // are known to outlive those in `to`. Otherwise, there would be a missing
    // constraint.
    if !self.contains_abstract.iter(scc_from).all(|from| {
      self
        .contains_abstract
        .iter(scc_to)
        .all(|to| self.known_flows.contains(from, to))
    }) {
      return FlowEdgeKind::MissingUniversalConstraint;
    }

    // If `from` is flowing a dangling pointer we would always consider this an error.
    if self
      .contains_local
      .iter(scc_from)
      .any(|local| self.dangling_local_sources.contains(local))
    {
      return FlowEdgeKind::LocalInvalidatedAtExit;
    }

    FlowEdgeKind::Ok
  }
}

// ------------------
// Utilities

fn flatten_tuples<T>(tups: &[(T, T)]) -> impl Iterator<Item = T> + '_
where
  T: Copy + Clone + Eq + std::hash::Hash,
{
  tups.iter().flat_map(|&(o1, o2)| [o1, o2]).unique()
}

fn count_nodes<T: Idx>(tups: &[(T, T)]) -> usize {
  flatten_tuples(tups)
    .minmax_by_key(|v| v.index())
    .into_option()
    .map_or(0, |(_, mx)| mx.index() + 1)
}

/// Compute the transitive flows from a set of given `sources` in `graph`.
fn flow_from_sources<T>(
  mat: &mut BitMatrix<T, T>,
  sources: impl Iterator<Item = T>,
  graph: impl DirectedGraph<Node = T> + WithSuccessors + WithNumNodes,
) where
  T: Idx,
{
  // Initialize all abstract sources.
  for a in sources {
    mat.insert(a, a);
  }

  let mut changed = true;

  while changed {
    changed = false;
    for r1 in mat.rows() {
      for r2 in graph.depth_first_search(r1) {
        log::debug!(
          "flowing {r1:?} -> {r2:?} ({} -> {})",
          r1.index(),
          r2.index()
        );

        changed |= mat.union_rows(r1, r2);
      }
    }
  }
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

// ------------------
// Entry

pub fn compute_flows(ctxt: &mut PermissionsCtxt) {
  let timer = Instant::now();
  let tcx = ctxt.tcx;
  let body = &ctxt.body_with_facts.body;

  // Compute the constraint graph with all regions.
  let constraints = ctxt
    .polonius_input_facts
    .subset_base
    .iter()
    .map(|&(o1, o2, _)| (o1, o2))
    .collect::<Vec<_>>();

  let vertices = flatten_tuples(&constraints).collect::<HashSet<_>>();

  log::debug!("Constraints---\n{:#?}", constraints);

  // Graph of constraints that need to be satisfied. This shows
  // us how data flows from one region into another.
  let constraint_graph = VecGraph::new(count_nodes(&constraints), constraints);

  let scc_constraints = Sccs::<Origin, SccIdx>::new(&constraint_graph);
  let num_sccs = scc_constraints.num_sccs();

  log::debug!("There are a total of {num_sccs} SCCs");

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
    .map(|p| scc_constraints.scc(p))
    .collect::<Vec<_>>();

  log::debug!("placeholders: {placeholders:#?}");

  // We filter the placeholders because the `known_placeholder_subset` contains a
  // top and bottom of the abstract lattice, but we only care about those
  // that actually appear in the body.
  let placeholder_edges = ctxt
    .polonius_input_facts
    .known_placeholder_subset
    .iter()
    .filter(|(f, t)| vertices.contains(f) && vertices.contains(t))
    .map(|&(f, t)| (scc_constraints.scc(f), scc_constraints.scc(t)))
    .chain(placeholders.iter().map(|&o| (o, o)))
    .collect::<Vec<_>>();

  log::debug!("placeholder_edges: {placeholder_edges:#?}");

  // Allowed flows between abstract regions.
  let known_flows_graph = VecGraph::new(num_sccs, placeholder_edges);

  // Compute the flow facts between abstract regions.
  let mut known_flows = BitMatrix::new(num_sccs, num_sccs);
  flow_from_sources(
    &mut known_flows,
    placeholders.iter().copied(),
    &known_flows_graph,
  );

  // Compute local sources:
  // If `Place::is_indirect` returns false, the caller knows
  // that the Place refers to the same region of memory as its base.
  let mut local_sources = HybridBitSet::new_empty(num_sccs);
  for (_, bd) in ctxt.borrow_set.location_map.iter() {
    if !bd.borrowed_place.is_indirect() {
      let scc = scc_constraints.scc(bd.region);
      local_sources.insert(scc);
    }
  }

  log::debug!(
    "Local sources---\n{:#?}",
    local_sources.iter().collect::<Vec<_>>()
  );

  // Mapping of region to regions it could contain.
  // row: region
  // col: row-region points to col-region
  let mut contains_abstract = BitMatrix::new(num_sccs, num_sccs);
  flow_from_sources(
    &mut contains_abstract,
    placeholders.iter().copied(),
    &scc_constraints,
  );

  let mut contains_local = BitMatrix::new(num_sccs, num_sccs);
  flow_from_sources(
    &mut contains_local,
    local_sources.iter(),
    &scc_constraints,
  );

  let mut dangling_local_sources = HybridBitSet::new_empty(num_sccs);

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
      .collect::<HashMap<_, _>>()
  });

  log::debug!("=== contains_local ===\n{:#?}", {
    contains_local
      .rows()
      .map(|r| (r, contains_local.iter(r).collect::<Vec<_>>()))
      .collect::<HashMap<_, _>>()
  });

  let region_flows = RegionFlows {
    constraint_graph: scc_constraints,
    known_flows,
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
