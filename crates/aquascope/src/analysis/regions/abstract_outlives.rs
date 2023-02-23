//! Report lifetime violation when a constraint is missing between two universal regions.
use anyhow::{anyhow, Result};
use datafrog::{Iteration, Relation, RelationLeaper, ValueFilter};
use either::Either;
use flowistry::mir::utils::{
  OperandExt, PlaceExt as FlowistryPlaceExt, SpanExt,
};
use polonius_engine::FactTypes;
use rustc_borrowck::{borrow_set::BorrowData, consumers::RustcFacts};
use rustc_data_structures::fx::{FxHashMap as HashMap, FxHashSet as HashSet};
use rustc_hir::HirId;
use rustc_middle::{
  mir::{Body, Local, Location, Place, Rvalue, Statement, StatementKind},
  ty::{subst::GenericArgKind, BoundVariableKind, Ty, TyCtxt},
};
use rustc_span::Span;
use rustc_type_ir::RegionKind;
use serde::Serialize;
use ts_rs::TS;

use super::*;
use crate::{
  analysis::{
    ir_mapper::{
      diagnostics::{IRDiagnosticMapper, RegionLocation, RegionNameHighlight},
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

/// Generate a [`RegionViolation`] if there exists a `subset_error`.
///
/// Subset errors are computed as:
/// ```text
/// subset_error(Origin1, Origin2, Point) :-
///   subset(Origin1, Origin2, Point),
///   placeholder_origin(Origin1),
///   placeholder_origin(Origin2),
///   !known_placeholder_subset(Origin1, Origin2).
/// ```
/// This also means that a subset error can *only* appear between two abstract origins.
pub fn report_region_outlives_violation(
  ctxt: &PermissionsCtxt,
  ir_mapper: &IRDiagnosticMapper,
  span_to_range: impl Fn(Span) -> Range + std::marker::Copy,
) -> Result<Option<RegionViolation>> {
  let mut subset_errors = ctxt
    .polonius_output
    .subset_errors
    .iter()
    .flat_map(|(_location, subset_errors)| subset_errors.iter())
    .collect::<Vec<_>>();

  subset_errors.sort();
  subset_errors.dedup();

  // `longer` should outlive `shorter` but this constraint was not provided (or inferred).
  let Some(&&(longer, shorter)) = subset_errors.first() else {
    // This case would not be an error because there weren't any
    // subset errors reported. This should probably be checked before
    // calling the method but *shrug*.
    return Ok(None);
  };

  log::debug!("UNSAT origin constraint: ({longer:?}, {shorter:?})");

  // This should only happen for abstract regions.
  assert!(is_placeholder(ctxt, longer));
  assert!(is_placeholder(ctxt, shorter));

  // Get the concrete free variable corresponding to the placeholder origin.
  let longer_concrete = get_concrete_origin_for_placeholder(ctxt, longer)
    .ok_or(anyhow!(
      "could not find concrete lifetime for placeholder {:?}",
      longer
    ))?;

  // Find the source-level range for each of the lifetime
  // parameters as they appear in the signature.
  let longer =
    range_of_placeholder(ctxt, ir_mapper, span_to_range, longer).ok_or(
      anyhow!("could not find a range for lifetime parameter {:?}", longer),
    )?;

  let shorter = range_of_placeholder(ctxt, ir_mapper, span_to_range, shorter)
    .ok_or(anyhow!(
    "could not find a range for lifetime parameter {:?}",
    shorter
  ))?;

  // Find region flow points from the `longer` placeholder to the `shorter`.
  let subset_flows = compute_flow_points(ctxt, longer_concrete.to_region_vid());

  // Various source mapping jazz

  let flow_ranges = subset_flows
    .iter()
    .map(|flow| span_to_range(ctxt.location_to_span(flow.at)))
    .collect::<Vec<_>>();
  let flow_ranges = smooth_elements(flow_ranges);

  let flows = subset_flows
    .into_iter()
    .map(|FlowPoint { from, to, at }| {
      let from = format!("{from:?}");
      let to = format!("{to:?}");
      let span = ctxt.location_to_span(at);
      let range = span_to_range(span);
      RegionFlow { from, to, range }
    })
    .collect::<Vec<_>>();

  Ok(Some(RegionViolation {
    kind: RegionViolationKind::OutlivesConstraintMissing { longer, shorter },
    flows,
    flow_ranges,
  }))
}
