//! Analysis for finding lifetime violations.

mod abstract_outlives;
mod local_outlives;

use abstract_outlives::report_region_outlives_violation;
use anyhow::{anyhow, Result};
use datafrog::{Iteration, Relation, RelationLeaper, ValueFilter};
use either::Either;
use flowistry::mir::utils::{
  OperandExt, PlaceExt as FlowistryPlaceExt, SpanExt,
};
use local_outlives::report_region_local_violation;
use polonius_engine::FactTypes;
use rustc_borrowck::{borrow_set::BorrowData, consumers::RustcFacts};
use rustc_data_structures::fx::{FxHashMap as HashMap, FxHashSet as HashSet};
use rustc_hir::HirId;
use rustc_middle::{
  mir::{Body, Local, Location, Place, Rvalue, Statement, StatementKind},
  ty::{subst::GenericArgKind, BoundVariableKind, Region, Ty, TyCtxt},
};
use rustc_span::Span;
use rustc_type_ir::RegionKind;
use serde::Serialize;
use ts_rs::TS;

use crate::{
  analysis::{
    ir_mapper::{
      diagnostics::{
        IRDiagnosticMapper, RegionLocation, RegionNameHighlight,
        RegionNameSource,
      },
      GatherDepth, IRMapper,
    },
    permissions::{
      places_conflict, Permissions, PermissionsCtxt, PermissionsData, Point,
    },
    smooth_elements, AquascopeAnalysis,
  },
  errors,
  mir::utils::{
    BodyExt as AquascopeBodyExt, PlaceExt as AquascopePlaceExt, ToRegionVid,
    TyExt as AquascopeTyExt,
  },
  Range,
};

type Origin = <RustcFacts as FactTypes>::Origin;

// ------------------------------------------------
// Data

#[derive(Debug, Clone, Serialize, TS)]
#[serde(tag = "type")]
#[ts(export)]
pub enum RegionViolationKind {
  LocalOutlivesUniversal {
    local_binding_range: Range,
    abstract_range: Range,
  },
  OutlivesConstraintMissing {
    longer: Range,
    shorter: Range,
  },
}

#[derive(Debug, Clone, Serialize, TS)]
#[ts(export)]
pub struct RegionViolation {
  kind: RegionViolationKind,
  flows: Vec<RegionFlow>,
  flow_ranges: Vec<Range>,
}

#[derive(Debug, Clone, Serialize, TS)]
#[ts(export)]
// FIXME: we don't currently generate good region names so these
// aren't (currently) suitable for showing in any visualization.
pub struct RegionFlow {
  from: String,
  to: String,
  range: Range,
}

// ------------------------------------------------

#[derive(Debug)]
struct FlowPoint {
  from: Origin,
  to: Origin,
  at: Location,
}

/// Is the given [`Origin`] a placeholder?
fn is_placeholder(ctxt: &PermissionsCtxt, rv: Origin) -> bool {
  ctxt
    .polonius_input_facts
    .placeholder
    .iter()
    .any(|&(r, _)| rv == r)
}

fn find_origin_in_sig<'tcx>(
  body: &Body<'tcx>,
  f: impl Fn(&Region<'tcx>) -> bool,
) -> Option<Region<'tcx>> {
  body
    .regions_in_args()
    .chain(body.regions_in_return())
    .find(|concrete| f(concrete))
}

/// Find the concrete lifetime appearing in the type signature for the given placeholder origin.
fn get_concrete_origin_for_placeholder<'tcx>(
  ctxt: &PermissionsCtxt<'_, 'tcx>,
  placeholder: Origin,
) -> Option<Region<'tcx>> {
  assert!(is_placeholder(ctxt, placeholder));
  let body = &ctxt.body_with_facts.body;
  let subset_base = &ctxt.polonius_input_facts.subset_base;
  let start = ctxt.location_to_point(Location::START);

  find_origin_in_sig(body, |concrete| {
    let vid = concrete.to_region_vid();
    subset_base.contains(&(placeholder, vid, start))
      && subset_base.contains(&(vid, placeholder, start))
  })
}

fn get_location_of_concrete_placeholder(
  body: &Body,
  origin: Origin,
) -> Option<RegionLocation> {
  let contains_it = |ty: Ty| {
    ty.inner_regions()
      .any(|region| region.to_region_vid() == origin)
  };

  body
    .args_iter()
    .find_map(|local| {
      contains_it(body.local_decls[local].ty)
        .then_some(RegionLocation::Arg(local.as_usize()))
    })
    .or_else(|| {
      let ret_ty = body.return_ty();
      contains_it(ret_ty).then_some(RegionLocation::Return)
    })
}

fn range_of_placeholder(
  ctxt: &PermissionsCtxt,
  ir_mapper: &IRDiagnosticMapper,
  span_to_range: impl Fn(Span) -> Range,
  placeholder: Origin,
) -> Option<Range> {
  let body = &ctxt.body_with_facts.body;
  let concrete = get_concrete_origin_for_placeholder(ctxt, placeholder)?;

  let span_anon_region = || -> Option<Span> {
    let location =
      get_location_of_concrete_placeholder(body, concrete.to_region_vid())?;
    let span = match ir_mapper
      .get_fn_sig_region_highlight(concrete.to_region_vid(), location)
    {
      RegionNameHighlight::MatchedHirTy(span)
      | RegionNameHighlight::MatchedAdtAndSegment(span)
      | RegionNameHighlight::CannotMatchHirTy(span, _)
      | RegionNameHighlight::Occluded(span, _) => span,
    };

    Some(span)
  };

  let span = ir_mapper
    .give_name_from_error_region(concrete.to_region_vid())
    .and_then(|region_name| match region_name.source {
      RegionNameSource::NamedFreeRegion(span) => Some(span),
      _ => None,
    })
    .or_else(span_anon_region)?;

  Some(span_to_range(span))
}

/// Check if the given borrow is invalidated by an exit point.
fn check_for_invalidation_at_exit<'tcx>(
  ctxt: &PermissionsCtxt<'_, 'tcx>,
  _location: Location,
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

/// Compute all the Origins to which `from` flows.
fn compute_flow_points<'tcx>(
  ctxt: &PermissionsCtxt<'_, 'tcx>,
  from: Origin,
) -> Vec<FlowPoint> {
  let mut subset_to_anywhere = HashSet::default();
  let mut subset_flows = Vec::default();
  let mut changed = true;

  subset_to_anywhere.insert(from);

  while changed {
    changed = false;

    for &(o1, o2, point) in ctxt.polonius_input_facts.subset_base.iter() {
      // FIXME: is there a need to minimize the point here?
      if subset_to_anywhere.contains(&o1) && !subset_to_anywhere.contains(&o2) {
        changed = true;
        subset_to_anywhere.insert(o2);
        subset_flows.push(FlowPoint {
          from: o1,
          to: o2,
          at: ctxt.point_to_location(point),
        });
      }
    }
  }

  subset_flows
}

// FIXME: there are a few things which this analysis will **not** handle.
// Anything interacting with upvar capture because this has to do with
// how errors are propagated
// All errors are eagerly assumed to be final.
fn report_region_violation(
  ctxt: &PermissionsCtxt,
  ir_mapper: &IRDiagnosticMapper,
  span_to_range: impl Fn(Span) -> Range + std::marker::Copy,
) -> Result<Option<RegionViolation>> {
  // We want to report an outlives constraint first because they
  // strictly happen between abstract regions.
  match report_region_outlives_violation(ctxt, ir_mapper, span_to_range) {
    // If no error is reported, but no value found, this means we should
    // keep looking for lifetime errors (but different forms).
    Result::Ok(None) => {
      report_region_local_violation(ctxt, ir_mapper, span_to_range)
    }
    // Result::Err and Result::Ok(Some(..)) should be reported, in the first
    // case something very wrong happened, and we would expect the latter.
    v => v,
  }
}

#[allow(clippy::module_name_repetitions)]
pub fn compute_region_info<'a, 'tcx: 'a>(
  ctxt: &AquascopeAnalysis<'a, 'tcx>,
) -> Result<Option<RegionViolation>> {
  let permissions = &ctxt.permissions;
  let span_to_range = |span| ctxt.span_to_range(span);
  let diagnostic_mapper = &IRDiagnosticMapper::new(&ctxt.ir_mapper);
  report_region_violation(permissions, diagnostic_mapper, span_to_range)
}
