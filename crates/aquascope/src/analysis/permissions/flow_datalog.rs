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
//! ```text
//! decl flows(Origin, Origin).
//!
//! flows(Origin, Origin) :-
//!   subset(Origin, Origin, _).
//!
//! decl contains_abstract(Origin, Origin).
//!
//! contains_abstract(Origin, Origin) :-
//!   placeholders(Origin, _).
//!
//! contains_abstract(Origin, OriginA) :-
//!   flow(Origin0, Origin),
//!   contains_absract(Origin0, OriginA).
//!
//! decl contains_local(Origin, Loan).
//!
//! contains_local(Origin, Loan) :-
//!   loan_issued_at(Origin, Loan, _),
//!   !is_indirect_borrow(Loan).
//!
//! contains_local(Origin, Loan) :-
//!   contains_local(Origin0, Loan),
//!   flow(Origin0, Origin).
//! ```
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
//! ```text
//! decl missing_universal_constraint(Origin, Origin).
//!
//! missing_universal_constraint(Origin0, Origin1) :-
//!   contains_abstract(Origin0, OriginA0),
//!   contains_abstract(Origin1, OriginA1),
//!   flow(Origin0, Origin1),
//!   !known_universal_subset(OriginA0, OriginA1).
//! ```
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
//! ```text
//! decl local_outlives_abstract(Origin0, Origin1).
//!
//! local_outlives_abstract(Origin0, Origin1) :-
//!   contains_local(Origin0, _),
//!   contains_abstract(Origin1, _),
//!   flow(Origin0, Origin1).
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

use datafrog::{Iteration, Relation, RelationLeaper, ValueFilter};
use rustc_data_structures::fx::{FxHashMap as HashMap, FxHashSet as HashSet};
use serde::Serialize;
use ts_rs::TS;

use super::{Loan, Origin, PermissionsCtxt};

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
#[allow(dead_code)] // because we aren't reporting exit invalidations anymore
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

#[allow(dead_code)]
pub struct RegionFlows {
  contains_local: HashMap<Origin, Vec<Loan>>,
  contains_abstract: HashMap<Origin, Vec<Origin>>,
  missing_universal_constraint: Relation<(Origin, Origin)>,
  local_outlives_abstract: Relation<(Origin, Origin)>,
}

impl RegionFlows {
  pub fn is_abstract_mem(&self, origin: Origin) -> bool {
    self.contains_abstract.contains_key(&origin)
  }

  pub fn is_local_mem(&self, origin: Origin) -> bool {
    self.contains_local.contains_key(&origin)
  }

  /// Get the specific kind of flow edge that connects `from` and `to`.
  ///
  /// **ASSUMED**: there must exists an edge between these two origins in the
  /// flow constraint graph, this means that only edges from Polonius' `subset_base`
  /// can be passed as arguments.
  #[allow(clippy::match_same_arms)]
  pub(crate) fn flow_kind(&self, from: Origin, to: Origin) -> FlowEdgeKind {
    if self.missing_universal_constraint.contains(&(from, to)) {
      FlowEdgeKind::MissingUniversalConstraint
    } else if self.local_outlives_abstract.contains(&(from, to)) {
      FlowEdgeKind::LocalOutlivesUniversal
    } else {
      FlowEdgeKind::Ok
    }
  }
}

pub fn compute_flows(ctxt: &mut PermissionsCtxt) {
  let timer = Instant::now();
  let tcx = ctxt.tcx;

  // This will cut down on redundant subset rules, as well as join all subset relations
  // regardless of the  point they were active.
  // XXX: we could use a BitMatrix to cut down on runtime and memory usage.
  let mut subset_anywhere = HashMap::<Origin, HashSet<Origin>>::default();

  for (_, subsets) in ctxt.polonius_output.subset.iter() {
    for (origin0, outlives) in subsets.iter() {
      for origin1 in outlives.iter() {
        subset_anywhere
          .entry(*origin0)
          .or_default()
          .insert(*origin1);
      }
    }
  }

  log::debug!("subset_anywhere\n{subset_anywhere:#?}");

  let flows: Relation<(Origin, Origin)> =
    Relation::from_iter(subset_anywhere.iter().flat_map(
      |(origin0, origin1s)| origin1s.iter().map(|origin1| (*origin0, *origin1)),
    ));

  let mut iteration = Iteration::new();
  let contains_abstract =
    iteration.variable::<(Origin, Origin)>("contains_abstract");
  let contains_local = iteration.variable::<(Origin, Loan)>("contains_local");

  // contains_abstract(Origin, Origin) :-
  //   universal_region(Origin, _).
  contains_abstract.extend(
    ctxt
      .polonius_input_facts
      .universal_region
      .iter()
      .map(|&o| (o, o)),
  );

  // contains_local(Origin, Loan) :-
  //   loan_issued_at(Origin, Loan, _),
  //   !is_indirect_borrow(Loan).
  contains_local.extend(
    ctxt.polonius_input_facts.loan_issued_at.iter().filter_map(
      |&(origin, loan, _point)| {
        let borrow = ctxt.loan_to_borrow(loan);
        if borrow.borrowed_place.is_indirect() {
          None
        } else {
          Some((origin, loan))
        }
      },
    ),
  );

  while iteration.changed() {
    // contains_abstract(Origin, OriginA) :-
    //   contains_absract(Origin0, OriginA),
    //   flow(Origin0, Origin).
    contains_abstract.from_join(
      &contains_abstract,
      &flows,
      |&_origin_0, &origin_a, &origin| (origin, origin_a),
    );

    // contains_local(Origin, Loan) :-
    //   contains_local(Origin0, Loan),
    //   flow(Origin0, Origin).
    contains_local.from_join(
      &contains_local,
      &flows,
      |&_origin_0, &loan, &origin| (origin, loan),
    );
  }

  let contains_abstract = contains_abstract.complete();
  let contains_local = contains_local.complete();

  let kps = &ctxt.polonius_input_facts.known_placeholder_subset;
  // FIXME: this should be an iteration variable.
  //
  // missing_universal_constraint(Origin0, Origin1) :-
  //   contains_abstract(Origin0),
  //   contains_abstract(Origin1),
  //   flow(Origin0, Origin1),
  //   !known_universal_subset(Origin0, Origin1).
  let missing_universal_constraint: Relation<(Origin, Origin)> =
    Relation::from_iter(
      contains_abstract
        .iter()
        .flat_map(|t0| contains_abstract.iter().map(|t1| (*t0, *t1)))
        .filter_map(|((o0, oa0), (o1, oa1))| {
          (oa0 != oa1 && !kps.contains(&(oa0, oa1))).then_some((o0, o1))
        }),
    );

  // FIXME: this should be an iteration variable.
  //        first, write it correctly this is terrible.
  //
  // local_outlives_abstract(Origin0, Origin1) :-
  //   contains_local(Origin0, Loan),
  //   contains_abstract(Origin1, _),
  //   flow(Origin0, Origin1),
  //   !flow(Origin1, Origin0).
  let local_outlives_abstract: Relation<(Origin, Origin)> =
    Relation::from_leapjoin(
      &contains_local,
      (
        flows.extend_with(|&(origin, _loan)| origin),
        ValueFilter::from(|&(origin, _loan), &origin1| {
          contains_abstract.iter().any(|&(o, _)| o == origin1)
            // We don't want to report flow violations inter-SCC.
            && !flows.contains(&(origin1, origin))
        }),
      ),
      |&(origin, _loan), &origin1| (origin, origin1),
    );

  log::debug!(
    "missing_universal_constrint:\n{:#?}",
    missing_universal_constraint.iter().collect::<Vec<_>>()
  );

  log::debug!(
    "local_outlives_abstract:\n{:#?}",
    local_outlives_abstract.iter().collect::<Vec<_>>()
  );

  macro_rules! insert_into_map {
    ($relation:expr, $map:expr) => {
      for &(k, v) in $relation.iter() {
        $map.entry(k).or_default().push(v);
      }
    };
  }

  let mut locals = HashMap::<Origin, Vec<Loan>>::default();
  let mut abstracts = HashMap::<Origin, Vec<Origin>>::default();

  insert_into_map!(contains_local, locals);
  insert_into_map!(contains_abstract, abstracts);

  log::debug!("contains_local\n:{:#?}", locals);
  log::debug!("contains_abstract\n:{:#?}", abstracts);

  let region_flows = RegionFlows {
    contains_local: locals,
    contains_abstract: abstracts,
    missing_universal_constraint,
    local_outlives_abstract,
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
