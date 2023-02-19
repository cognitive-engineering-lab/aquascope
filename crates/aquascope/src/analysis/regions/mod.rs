//! Analysis for finding lifetime violations.

use anyhow::Result;
use either::Either;
use flowistry::mir::utils::{OperandExt, SpanExt};
use polonius_engine::FactTypes;
use rustc_borrowck::consumers::RustcFacts;
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
    permissions::{Permissions, PermissionsCtxt, PermissionsData, Point},
    AquascopeAnalysis,
  },
  errors,
  mir::utils::PlaceExt,
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

  // Will be incredibly expensive for even medium-sized functions.
  let subset: Relation<(Origin, Origin, Point)> =
    Relation::from_iter(ctxt.polonius_output.subset.iter().flat_map(
      |(point, origins)| {
        origins
          .iter()
          .flat_map(|(o1, os)| os.iter().map(|o2| (*o1, *o2, *point)))
      },
    ));

  let placeholder = ctxt
    .polonius_input_facts
    .placeholder
    .iter()
    .map(|&(origin, _)| origin)
    .collect::<HashSet<_>>();

  log::debug!("placeholders {placeholder:#?}");

  let local_subset_error: Relation<(Origin, Origin, Point)> =
    Relation::from_iter(
      subset
        .iter()
        .filter(|&(origin1, origin2, _point)| {
          !placeholder.contains(origin1) && placeholder.contains(origin2)
        })
        .copied(),
    );

  // TODO: Here, we have information about local regions which must outlive
  // placeholder regions, however, we want to know about all regions of
  // the SCC which must outlive the placeholder region. We could also try
  // something closer to what Rustc does, but this may be too much.
  //
  // Example
  // ```text
  // fn add_ref(v: &mut Vec<&i32>, n: i32) {
  //     let r = &n;
  //     v.push(r);
  // }
  // ```
  //
  // MIR
  //
  // ```text
  // fn dump_me(_1: &'_#8r mut std::vec::Vec<&'_#9r i32>, _2: i32) -> () {
  //     debug v => _1;                       // in scope 0 at src/main.rs:4:12: 4:13
  //     debug n => _2;                       // in scope 0 at src/main.rs:4:31: 4:32
  //     let mut _0: ();                      // return place in scope 0 at src/main.rs:4:39: 4:39
  //     let _4: ();                          // in scope 0 at src/main.rs:6:3: 6:12
  //     let mut _5: &'_#11r mut std::vec::Vec<&'_#12r i32>; // in scope 0 at src/main.rs:6:3: 6:12
  //     let mut _6: &'_#13r i32;             // in scope 0 at src/main.rs:6:10: 6:11
  //     ...
  // }
  // ```
  //
  // placeholders are:
  // { '_#0r, '_#2r, '_#1r, '_#3r,  }
  //
  // The local_subset_errors:
  // [
  //     ('_#7r,  '_#2r,),
  //     ('_#8r,  '_#1r,),
  //     ('_#9r,  '_#2r,),
  //     ('_#12r, '_#2r,),
  //     ('_#13r, '_#2r,),
  // ]
  //
  // we just need to make sure that we don't count the temps
  // when looking at subset errors. The one we really care about
  // in this example is (_#4, _#2) and it can be derived from ('_#7r,  '_#2r,),
  // '_#4 <: '_#10 <: '_#6 < '_#7

  let mut local_subset_error = local_subset_error
    .iter()
    .map(|&(f, s, _)| (f, s))
    .collect::<Vec<_>>();
  local_subset_error.sort();
  local_subset_error.dedup();

  log::debug!("LOCAL SUBSET ERRORS:\n{local_subset_error:#?}");

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
