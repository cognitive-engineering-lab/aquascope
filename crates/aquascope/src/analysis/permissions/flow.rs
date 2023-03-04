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

use flowistry::mir::utils::PlaceExt;
use itertools::Itertools;
use rustc_borrowck::borrow_set::BorrowData;
use rustc_data_structures::{
  fx::{FxHashMap as HashMap, FxHashSet as HashSet},
  graph::{scc::Sccs, vec_graph::VecGraph, WithSuccessors},
};
use rustc_middle::mir::Place;
use serde::Serialize;
use smallvec::SmallVec;
use ts_rs::TS;

use super::{places_conflict, Origin, PermissionsCtxt};

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
  Flowed(HashSet<SccIdx>),

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
#[allow(dead_code)]
pub struct RegionFlows {
  /// The flow constraint graph over the `subset_base` relation.
  constraint_graph: Sccs<Origin, SccIdx>,

  /// Full set of known flows per the `known_placeholder_subset` relation.
  known_flows: HashSet<(SccIdx, SccIdx)>,

  /// Components that contain a placeholder region.
  abstract_sources: HashSet<SccIdx>,

  /// Local regions that point to a body-owned value.
  local_sources: HashSet<SccIdx>,

  /// Local regions that could dangle due to an exit invalidation.
  dangling_local_sources: HashSet<SccIdx>,

  /// The set of abstract components that a given component could contain.
  contains_abstract: HashMap<SccIdx, HashSet<SccIdx>>,

  /// The set of local components that a given component could contain.
  contains_local: HashMap<SccIdx, HashSet<SccIdx>>,
}

impl RegionFlows {
  fn get_abstract_kind(&self, origin: Origin) -> AbstractRegionKind {
    let sccid = self.constraint_graph.scc(origin);
    self
      .abstract_sources
      .iter()
      .any(|&id| sccid == id)
      .then_some(AbstractRegionKind::Source(sccid))
      .or_else(|| {
        self
          .contains_abstract
          .get(&sccid)
          .map(|sources| AbstractRegionKind::Flowed(sources.clone()))
      })
      .unwrap_or_else(|| AbstractRegionKind::None)
  }

  fn is_known_flow(&self, from: SccIdx, to: SccIdx) -> bool {
    self.known_flows.contains(&(from, to))
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
        if !froms.iter().all(|&from| self.is_known_flow(from, to)) =>
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
        let empty_hash = HashSet::default();

        let locals = self.contains_local.get(&from).unwrap_or(&empty_hash);
        if locals.intersection(&self.dangling_local_sources).count() == 0 {
          FlowEdgeKind::Ok
        } else {
          FlowEdgeKind::LocalInvalidatedAtExit
        }
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

  log::debug!("SUBSET BASE ---\n{:#?}", {
    let mut v = constraints.clone();
    v.sort();
    v.dedup();
    v
  });

  log::debug!("ERRORS ---\n{:#?}", {
    ctxt
      .polonius_output
      .errors
      .iter()
      .map(|(point, loans)| {
        let loc = ctxt.point_to_location(*point);
        (loc, loans)
      })
      .collect::<Vec<_>>()
  });
  log::debug!("SUBSET ERRORS ---\n{:#?}", {
    &ctxt.polonius_output.subset_errors
  });

  let vertices = flat_nodes!(constraints).collect::<Vec<_>>();
  let placeholders = ctxt
    .polonius_input_facts
    .placeholder
    .iter()
    .filter_map(|&(p, _)| vertices.contains(&p).then_some(p))
    .collect::<Vec<_>>();

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
  let known_flows =
    VecGraph::new(node_count!(placeholder_edges), placeholder_edges);

  let scc_constraints = Sccs::<Origin, SccIdx>::new(&constraint_graph);

  // Compute the total flow facts (expensive).
  let known_flows = placeholders
    .iter()
    .flat_map(|placeholder| {
      log::debug!("flowing with placeholder {placeholder:?}");
      log::debug!("placeholder scc {:?}", scc_constraints.scc(*placeholder));
      known_flows.depth_first_search(*placeholder).map(|r2| {
        let s1 = scc_constraints.scc(*placeholder);
        let s2 = scc_constraints.scc(r2);
        (s1, s2)
      })
    })
    .collect::<HashSet<_>>();

  let local_sources = ctxt
    .borrow_set
    .location_map
    .iter()
    .filter_map(|(_, bd)| {
      (!(bd.borrowed_place.is_indirect()
        || bd.borrowed_place.ty(body, tcx).ty.is_ref()))
      .then_some(scc_constraints.scc(bd.region))
    })
    .collect::<HashSet<_>>();

  let abstract_sources = placeholders
    .iter()
    .map(|origin| scc_constraints.scc(*origin))
    .collect::<HashSet<_>>();

  let contains_abstract = abstract_sources
    .iter()
    .map(|&source| {
      (
        source,
        scc_constraints
          .depth_first_search(source)
          .collect::<Vec<_>>(),
      )
    })
    .fold(
      HashMap::<SccIdx, HashSet<SccIdx>>::default(),
      |mut acc, (from, tos)| {
        for &to in tos.iter() {
          acc.entry(to).or_default().insert(from);
        }
        acc
      },
    );

  let contains_local = local_sources
    .iter()
    .map(|&source| {
      (
        source,
        scc_constraints
          .depth_first_search(source)
          .collect::<Vec<_>>(),
      )
    })
    .fold(
      HashMap::<SccIdx, HashSet<SccIdx>>::default(),
      |mut acc, (from, tos)| {
        for &to in tos.iter() {
          acc.entry(to).or_default().insert(from);
        }
        acc
      },
    );

  let dangling_local_sources = ctxt
    .polonius_output
    .errors
    .iter()
    .filter_map(|(_, loans)| {
      loans.iter().find_map(|&loan| {
        let bd = ctxt.loan_to_borrow(loan);
        check_for_invalidation_at_exit(ctxt, bd)
          .then_some(scc_constraints.scc(bd.region))
      })
    })
    .collect::<HashSet<_>>();

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
  use places_conflict::AccessDepth::Shallow;

  let place = borrow.borrowed_place;
  let tcx = ctxt.tcx;
  let body = &ctxt.body_with_facts.body;

  let root_place = Place::from_local(place.local, tcx);

  // let mut root_place = PlaceRef {
  //   local: place.local,
  //   projection: &[],
  // };

  // let (might_be_alive, will_be_dropped) =
  //   if self.body.local_decls[root_place.local].is_ref_to_thread_local() {
  //     // Thread-locals might be dropped after the function exits
  //     // We have to dereference the outer reference because
  //     // borrows don't conflict behind shared references.
  //     root_place.projection = TyCtxtConsts::DEREF_PROJECTION;
  //     (true, true)
  //   } else {
  //     (false, self.locals_are_invalidated_at_exit)
  //   };

  // if !will_be_dropped {
  //   debug!("place_is_invalidated_at_exit({:?}) - won't be dropped", place);
  //   return;
  // }

  // let sd = if might_be_alive { Deep } else { Shallow(None) };
  let sd = Shallow(None);

  places_conflict::borrow_conflicts_with_place(
    tcx,
    body,
    place,
    borrow.kind,
    root_place.as_ref(),
    sd,
    places_conflict::PlaceConflictBias::Overlap,
  )
}
