//! Region flow analysis for lifetime errors.
//!
//! Answers queries of the form, is the flow from region 1  to region 2 valid?
//!
//! ## High-level idea
//!
//! We define an *abstract origin* (ϱ) to be the origin parameters of a body. This is
//! the same definition as given by Oxide.
//!
//! We define a *local source* S as the direct values within a body.
//!
//! We define a taint-environment Θ ::= `r` ↦ {ϱ_1, ϱ_2, ..., S_1, S_2, ...} mapping regions `r` to the sources
//! that taint them (both local and abstract).
//!
//! 1. A region `r` is abstract-tainted if there exists a flow from an abstract origin ϱ to `r`.
//!
//! 2. A region `r` is local-tainted if there exists a flow from the borrow of a local source S to `r`.
//!    (in other words, if `r` conflicts with a body-owned `Place`).
//!
//! The presented rules work on a graph `G` and a borrow-check error is represented by *borrowfail G*.
//!
//! ### Missing universal constraint
//!
//! A "missing constraint" error occurs IFF:
//!
//! ```text
//! Θ ⊢ r_1 : { ϱ_1 }
//! Θ ⊢ r_2 : { ϱ_2 }
//! r_1 -> r_2     ϱ_1 ⊈ ϱ_2
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
//! Θ ⊢ r_1 : { S }
//! Θ ⊢ r_2 : { ϱ }
//! r_1 -> r_2
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
  fx::FxHashSet as HashSet,
  graph::{
    scc::Sccs, vec_graph::VecGraph, DirectedGraph, WithNumNodes, WithSuccessors,
  },
  transitive_relation::{TransitiveRelation, TransitiveRelationBuilder},
};
use rustc_index::{bit_set::HybridBitSet, vec::Idx};
use serde::Serialize;
use ts_rs::TS;

use super::{places_conflict, Origin, PermissionsCtxt};
use crate::mir::utils::{BodyExt, ToRegionVid};

rustc_index::newtype_index! {
  #[debug_format = "scc{}"]
  pub struct SccIdx {}
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
// NOTE: all `TransitiveRelation`s are using a points to relationship. This means, that
// if you want to know if the constraint `'a: 'b` ('a outlives 'b) was specified by the
// user you, would use `specified_flows.contains('b, 'a)` phrasing this as
// "`'b` can point to `'a`, but not the other way around".
// This feels a little more natural when using `contains_abstract.contains('b, 'a)`
// which in english specifies that "`'b` could contain data from the abstract region `'a`".
pub struct RegionFlows {
  /// The flow constraint graph over the `subset_base` relation.
  constraint_graph: Sccs<Origin, SccIdx>,

  /// Full set of known flows per the `known_placeholder_subset` relation.
  specified_flows: TransitiveRelation<SccIdx>,

  /// Local regions that could dangle due to an exit invalidation.
  dangling_local_sources: HybridBitSet<SccIdx>,

  /// Regions that are equivalent to placeholders.
  abstract_sources: HybridBitSet<SccIdx>,

  /// The set of abstract components that a given component could contain.
  contains_abstract: TransitiveRelation<SccIdx>,

  /// The set of local components that a given component could contain.
  contains_local: TransitiveRelation<SccIdx>,
}

impl RegionFlows {
  pub fn scc(&self, origin: Origin) -> SccIdx {
    self.constraint_graph.scc(origin)
  }

  /// Returns whether `origin` belongs to an abstract SCC.
  pub fn is_abstract_member(&self, origin: Origin) -> bool {
    self.abstract_sources.contains(self.scc(origin))
  }

  /// Returns whether `origin` is abstract-tainted.
  pub fn has_abstract_member(&self, origin: Origin) -> bool {
    !self
      .contains_abstract
      .reachable_from(self.scc(origin))
      .is_empty()
  }

  pub fn has_local_member(&self, origin: Origin) -> bool {
    !self
      .contains_local
      .reachable_from(self.scc(origin))
      .is_empty()
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

    let from_contains_abstract = self.has_abstract_member(from);
    let from_contains_local = self.has_local_member(from);

    let to_contains_abstract = self.has_abstract_member(to);
    let to_contains_local = self.has_local_member(to);

    log::debug!(
      "Analyzing flow {from:?} -> {to:?} {:?} -> {:?}",
      self.scc(from),
      self.scc(to)
    );

    log::debug!("{from:?} is_local? {from_contains_local} is_abstract? {from_contains_abstract}", );
    log::debug!("{to:?} is_local? {to_contains_local} is_abstract? {to_contains_abstract}", );

    // A local value can never flow into an abstract region.
    if from_contains_local && self.is_abstract_member(to) {
      log::info!("early return LocalOutlivesUniversal: {from:?}->{to:?} ({scc_from:?} -> {scc_to:?})");
      log::info!(
        "FROM abstracts {:#?}",
        self.contains_abstract.reachable_from(scc_from)
      );
      log::info!(
        "TO abstracts {:#?}",
        self.contains_abstract.reachable_from(scc_to)
      );
      return FlowEdgeKind::LocalOutlivesUniversal;
    }

    // If both regions contain abstract, we check that all regions in `from`
    // are known to outlive those in `to`. Otherwise, there is a
    // missing constraint that needs to be specified.
    if !self
      .contains_abstract
      .reachable_from(scc_from)
      .into_iter()
      .all(|from| {
        self
          .contains_abstract
          .reachable_from(scc_to)
          .into_iter()
          // was `'from: 'to` user-specified?
          .all(|to| self.specified_flows.contains(to, from))
      })
    {
      return FlowEdgeKind::MissingUniversalConstraint;
    }

    // If `from` is flowing a dangling pointer we would always consider this an error.
    if self
      .contains_local
      .reachable_from(scc_from)
      .into_iter()
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
///
/// The return closure answers queries of the form "for (v, s) did `s` flow to v?"
fn flow_from_sources<T>(
  sources: impl Iterator<Item = T>,
  graph: impl DirectedGraph<Node = T> + WithSuccessors + WithNumNodes,
) -> TransitiveRelation<T>
where
  T: Idx,
{
  let mut tcb = TransitiveRelationBuilder::default();

  // Compute the transitive closure, then assert that they're the same.
  for s in sources {
    for t in graph.depth_first_search(s) {
      // `t` can point to `s`
      tcb.add(t, s);
    }
  }

  tcb.freeze()
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

  log::debug!("Abstract sources(placeholders):\n{placeholders:#?}");

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

  // Allowed flows between abstract regions specified in the type signature.
  //
  // e.g. `fn foo<'a, 'b: 'a>(...) ...` would cause a `'b: 'a` specified flow in this graph.
  let specified_flows_graph = VecGraph::new(num_sccs, placeholder_edges);

  // Compute the flow facts between abstract regions.
  let specified_flows =
    flow_from_sources(placeholders.iter().copied(), &specified_flows_graph);

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
    "Local sources:\n{:#?}",
    local_sources.iter().collect::<Vec<_>>()
  );

  // Mapping of region to regions it could contain.
  // row: region
  // col: row-region points to col-region
  let contains_abstract =
    flow_from_sources(placeholders.iter().copied(), &scc_constraints);

  let contains_local =
    flow_from_sources(local_sources.iter(), &scc_constraints);

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

  let mut abstract_sources = HybridBitSet::new_empty(num_sccs);
  for scc in placeholders.iter() {
    abstract_sources.insert(*scc);
  }

  log::debug!("Contains abstract:\n{contains_abstract:#?}");

  log::debug!("Contains local:\n{contains_local:#?}");

  let region_flows = RegionFlows {
    constraint_graph: scc_constraints,
    specified_flows,
    dangling_local_sources,
    abstract_sources,
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
