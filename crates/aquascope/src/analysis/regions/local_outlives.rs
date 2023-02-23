//! Lifetime violation when locals outlive universal regions.
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

/// Try to report a concrete outlives abstract lifetime error.
///
/// For abstract lifetime ϱ and concrete lifetime r:
///
/// r outlives ϱ at I
/// ----------------------
///     borrowfail G
///
/// decl local_subset_error(Origin0, Origin1).
///
/// decl local_subset_error(Origin0, Origin1) :-
///   !placeholder(Origin0),
///   placeholder(Origin1),
///   subset(Origin0, Origin1, _).
pub fn report_region_local_violation(
  ctxt: &PermissionsCtxt,
  ir_mapper: &IRDiagnosticMapper,
  span_to_range: impl Fn(Span) -> Range + std::marker::Copy,
) -> Result<Option<RegionViolation>> {
  let body = &ctxt.body_with_facts.body;

  let mut loc_and_borrow = None;

  // Find the location and borrow that gets invalidated by an exit point (if any).
  'outer: for (point, loans) in ctxt.polonius_output.errors.iter() {
    let location = ctxt.point_to_location(*point);

    // We don't want to reason about locations appearing
    // in a cleanup block. If an error happens in a cleanup,
    // it should also happen along the happy path.
    if body.basic_blocks[location.block].is_empty_unreachable() {
      continue;
    }

    for l in loans.iter() {
      let borrow = ctxt.loan_to_borrow(*l);
      if check_for_invalidation_at_exit(ctxt, location, borrow) {
        loc_and_borrow = Some((location, borrow));
        break 'outer;
      }
    }
  }

  let Some((_, borrow)) = loc_and_borrow else {
    return Ok(None);
  };

  let borrow_location = borrow.reserve_location;
  let bad_origin = borrow.region;
  let points = ctxt.location_to_points(borrow.reserve_location);
  let captured_in_origin = points
    .iter()
    .find_map(|point| {
      ctxt
        .polonius_output
        .subset
        .get(point)
        .and_then(|os| os.get(&bad_origin).and_then(|ps| ps.first()))
    })
    .ok_or(anyhow!(
      "could not find capturing origin in borrow data@{:?} {:?}",
      points,
      borrow
    ))?;

  // Find all of the origins that the local borrow flows into.
  let mut subset_flows = compute_flow_points(ctxt, *captured_in_origin);

  // Manually push the local borrow because the search
  // only starts at the capturing origin.
  subset_flows.push(FlowPoint {
    from: bad_origin,
    to: *captured_in_origin,
    at: borrow_location,
  });

  // The placeholder that we are trying to outlive. In theory there could actually
  // be multiple, but we only are reporting the first one found (for now).
  let placeholder =
    subset_flows.iter().find_map(|FlowPoint { to, .. }| {
      is_placeholder(ctxt, *to).then_some(*to)
    }).ok_or(anyhow!("local outlives abstract region {:?} {:?}. But no placeholder found in flows to set {:#?}", bad_origin, captured_in_origin, subset_flows))?;

  // Convert the information into something more "source-visible".
  let borrowed_local_info = &body.local_decls[borrow.borrowed_place.local];
  let local_binding_range = span_to_range(borrowed_local_info.source_info.span);

  let abstract_range =
    range_of_placeholder(ctxt, ir_mapper, span_to_range, placeholder).ok_or(
      anyhow!(
        "could not find range for region placeholder {:?}",
        placeholder
      ),
    )?;

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
    kind: RegionViolationKind::LocalOutlivesUniversal {
      local_binding_range,
      abstract_range,
    },
    flows,
    flow_ranges,
  }))
}
