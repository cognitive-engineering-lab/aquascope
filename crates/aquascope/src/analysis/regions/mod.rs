//! Analysis for finding lifetime violations.

use anyhow::Result;
use either::Either;
use flowistry::mir::utils::{
  OperandExt, PlaceExt as FlowistryPlaceExt, SpanExt,
};
use polonius_engine::FactTypes;
use rustc_borrowck::{borrow_set::BorrowData, consumers::RustcFacts};
use rustc_data_structures::fx::FxHashSet as HashSet;
use rustc_hir::HirId;
use rustc_middle::{
  mir::{Body, Location, Place, Rvalue, Statement, StatementKind},
  ty::TyCtxt,
};
use rustc_span::Span;
use serde::Serialize;
use ts_rs::TS;

use crate::{
  analysis::{
    ir_mapper::{GatherDepth, IRMapper},
    permissions::{
      places_conflict, Permissions, PermissionsCtxt, PermissionsData, Point,
    },
    AquascopeAnalysis,
  },
  errors,
  mir::utils::PlaceExt as AquascopePlaceExt,
  Range,
};

// ------------------------------------------------
// Data

#[derive(Debug, Clone, Serialize, TS)]
#[ts(export)]
pub enum RegionViolation {}

pub struct RegionOutlivesViolation {}

// ------------------------------------------------

/* keeping these handy

fn add_ref(v: &mut Vec<&i32>, n: i32) {
    let r = &n;
    v.push(r);
}

fn return_a_string() -> &String {
    let s = String::from("Hello world");
    &s
}

fn make_a_cloner(s_ref: &str) -> impl Fn() -> String {
    move || s_ref.to_string()
}

 */

type Origin = <RustcFacts as FactTypes>::Origin;

fn check_for_invalidation_at_exit<'tcx>(
  ctxt: &PermissionsCtxt<'_, 'tcx>,
  location: Location,
  borrow: &BorrowData<'tcx>,
) -> bool {
  use places_conflict::AccessDepth::*;

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

fn compute_trace_points_for_local_invalidation<'tcx>(
  ctxt: &PermissionsCtxt<'_, 'tcx>,
  location: Location,
  borrow: &BorrowData<'tcx>,
) -> RegionViolation {
  let borrow_location = borrow.reserve_location;
  log::trace!(
    "computing trace for local invalidation ... \n  {location:?} \n  {borrow:?}\n  {borrow_location:?}"
  );
  let bad_origin = borrow.region;
  let points = ctxt.location_to_points(borrow.reserve_location);
  let Some(captured_in_origin) = points.iter().find_map(|point| {
    ctxt
      .polonius_output
      .subset
      .get(point)
      .and_then(|os| os.get(&bad_origin).and_then(|ps| ps.first()))
  }) else {
    unreachable!("could not find capturing origin in borrow data@{points:?} {borrow:?}");
  };

  log::debug!("captured_in_origin: {:#?}", captured_in_origin);

  let mut subset_to_anywhere = HashSet::default();
  subset_to_anywhere.insert(captured_in_origin);
  let mut changed = true;

  while changed {
    changed = false;

    for (_point, os) in ctxt.polonius_output.subset.iter() {
      for (o, osp) in os.iter() {
        if subset_to_anywhere.contains(&o) {
          for o2 in osp.iter() {
            if !subset_to_anywhere.contains(o2) {
              subset_to_anywhere.insert(o2);
              changed = true;
            }
          }
        }
      }
    }
  }

  log::debug!("SUB ANYWHERE:\n{:#?}", subset_to_anywhere);

  panic!("STOP");
}

fn report_region_local_violation(
  ctxt: &PermissionsCtxt,
) -> Option<RegionViolation> {
  use datafrog::{Iteration, Relation, RelationLeaper, ValueFilter};
  // For our small test cases these are produced from Polonius
  // as an ERROR (not subset_error), here's a little example:
  //
  // fn add_ref(v: &mut Vec<&_#0 i32>, n: i32) {
  //
  //     let r = &_#1 n;  // concrete region: _#1
  //     v.push(r);      // _#1 :> _#0
  //
  //     // StorageDead(r);
  //     // ^^^ violation of region _#1
  //
  // } // loans _#0 and _#1 are live here
  //
  // what we really want to say is that _#0 </: _#1 framing this as a SUBSET ERROR.
  //
  // for abstract lifetime ϱ
  // and concrete lifetime r
  // This can be summarized as:
  //
  // r outlives ϱ at I
  // ----------------------
  //     borrowfail G
  //
  // decl local_subset_error(Origin0, Origin1).
  //
  // decl local_subset_error(Origin0, Origin1) :-
  //   !placeholder(Origin0),
  //   placeholder(Origin1),
  //   subset(Origin0, Origin1, _Point).

  for (point, loans) in ctxt.polonius_output.errors.iter() {
    let location = ctxt.point_to_location(*point);
    for l in loans.iter() {
      let borrow = ctxt.loan_to_borrow(*l);
      if check_for_invalidation_at_exit(ctxt, location, borrow) {
        return Some(compute_trace_points_for_local_invalidation(
          ctxt, location, borrow,
        ));
      }
    }
  }

  None
}

type OutlivesViolation = (Origin, Origin);

fn report_region_outlives_violation(
  ctxt: &PermissionsCtxt,
) -> Option<OutlivesViolation> {
  let mut subset_errors = ctxt
    .polonius_output
    .subset_errors
    .iter()
    .flat_map(|(_location, subset_errors)| subset_errors.iter())
    .collect::<Vec<_>>();

  subset_errors.sort();
  subset_errors.dedup();

  // TODO: we want to report the first that happens in the MIR
  // not just the first in the set.
  subset_errors
    .first()
    .map(|(longer, shorter)| (*longer, *shorter))
}

// FIXME: this analysis will not handle closure propagation.
// All errors are eagerly assumed to be final.
fn report_region_violation(ctxt: &PermissionsCtxt) -> Option<RegionViolation> {
  // TODO REMOVE
  // ----------------------------------------------

  log::debug!("STARTING LIFETIME ANALYSIS!!!");

  log::debug!("errors ... {:#?}", ctxt.polonius_output.errors);
  log::debug!(
    "subset errors ... {:#?}",
    ctxt.polonius_output.subset_errors
  );
  log::debug!("move errors ... {:#?}", ctxt.polonius_output.move_errors);

  for (point, loans) in ctxt.polonius_output.errors.iter() {
    let loc = ctxt.point_to_location(*point);
    log::debug!("\nErrors at: {loc:?}");
    for l in loans.iter() {
      let borrow = ctxt.loan_to_borrow(*l);
      log::debug!("  invalidated borrow: {:#?}", borrow);
    }
  }

  for (point, error_subsets) in ctxt.polonius_output.subset_errors.iter() {
    let loc = ctxt.point_to_location(*point);
    log::debug!("\nSubset errors at: {loc:?}");
    for (o1, o2) in error_subsets.iter() {
      log::debug!("  {o1:?}: {o2:?} unproved");
    }
  }

  // ----------------------------------------------

  if let Some((longer_fr, shorter_fr)) = report_region_outlives_violation(ctxt)
  {
    log::debug!("CANNOT PROVE REGION REL: {longer_fr:?}: {shorter_fr:?}");
  }

  if let Some(..) = report_region_local_violation(ctxt) {
    ();
  }

  None
}

#[allow(clippy::module_name_repetitions)]
pub fn compute_region_info<'a, 'tcx: 'a>(
  ctxt: &AquascopeAnalysis<'a, 'tcx>,
) -> Result<Option<RegionViolation>> {
  let permissions = &ctxt.permissions;
  Ok(report_region_violation(permissions))
}
