//! Analysis for finding expected vs actual permission on path usage.
pub(crate) mod path_visitor;

use anyhow::Result;
use either::Either;
use flowistry::mir::utils::{OperandExt, SpanExt};
use path_visitor::get_path_boundaries;
use rustc_hir::HirId;
use rustc_middle::mir::{Rvalue, Statement, StatementKind};
use rustc_span::Span;
use serde::Serialize;
use ts_rs::TS;

use crate::{
  analysis::{
    ir_mapper::{GatherDepth, IRMapper},
    permissions::{Permissions, PermissionsCtxt, PermissionsData},
    AquascopeAnalysis,
  },
  errors,
  mir::utils::PlaceExt,
  Range,
};

/// A point where the permissions reality are checked against their expectations.
#[derive(Debug, Clone, Serialize, TS)]
#[ts(export)]
pub struct PermissionsBoundary {
  pub location: usize,
  pub expected: Permissions,
  pub actual: PermissionsData,
}

// ----------------------------------
// Permission boundaries on path uses

struct PathBoundary {
  pub hir_id: HirId,
  pub location: Span,
  pub expected: Permissions,
}

impl std::fmt::Debug for PathBoundary {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("PathBoundary")
      .field("location", &self.location)
      .field("hir_id", &self.hir_id)
      .field("expected", &self.expected)
      .finish()
  }
}

fn path_to_perm_boundary(
  path_boundary: PathBoundary,
  ctxt: &PermissionsCtxt,
  ir_mapper: &IRMapper,
  span_to_range: impl Fn(Span) -> Range,
) -> Option<PermissionsBoundary> {
  let body = &ctxt.body_with_facts.body;
  let tcx = ctxt.tcx;
  let hir = tcx.hir();
  let hir_id = path_boundary.hir_id;

  log::debug!(
    "Resolving permissions boundary for {}",
    hir.node_to_string(path_boundary.hir_id)
  );

  // For a given Path, the MIR location may not be immediately associated with it.
  // For example, in a function call `foo( &x );`, the Hir Node::Path `&x` will not
  // have the MIR locations associated with it, the Hir Node::Call `foo( &x )` will,
  // so we traverse upwards in the tree until we find a location associated with it.
  let search_at_hir_id = |hir_id| {
    let mir_locations_opt =
      ir_mapper.get_mir_locations(hir_id, GatherDepth::Nested);

    macro_rules! maybe_in_op {
      ($op:expr, $loc:expr) => {
        $op
          .to_place()
          .and_then(|p| p.is_source_visible(tcx, body).then_some(($loc, p)))
      };
    }

    let mir_locations = mir_locations_opt?
      .values()
      .filter_map(|loc| {
        log::debug!("looking at {loc:?}");
        if let Either::Left(Statement {
          kind: StatementKind::Assign(box (_, rvalue)),
          ..
        }) = body.stmt_at(loc)
        {
          match rvalue {
            Rvalue::Ref(_, _, place) if place.is_source_visible(tcx, body) => {
              Some((loc, *place))
            }
            Rvalue::Use(op) => maybe_in_op!(op, loc),
            Rvalue::BinaryOp(_, box (left_op, right_op)) => {
              maybe_in_op!(left_op, loc).or_else(|| maybe_in_op!(right_op, loc))
            }
            Rvalue::CheckedBinaryOp(_, box (left_op, right_op)) => {
              maybe_in_op!(left_op, loc).or_else(|| maybe_in_op!(right_op, loc))
            }
            Rvalue::CopyForDeref(place)
              if place.is_source_visible(tcx, body) =>
            {
              Some((loc, *place))
            }
            _ => {
              log::warn!("couldn't find in RVALUE {rvalue:?}");
              None
            }
          }
        } else {
          None
        }
      })
      .collect::<Vec<_>>();

    let (loc, place) = mir_locations.first()?;
    log::debug!("Chosen place at location {place:#?} {loc:#?}");
    let point = ctxt.location_to_point(*loc);
    let path = ctxt.place_to_path(place);

    Some((point, path))
  };

  search_at_hir_id(hir_id)
    .or_else(|| {
      hir.parent_iter(hir_id).find_map(|(hir_id, _)| {
        log::debug!("\tsearching upwards in: {hir_id:?}");
        search_at_hir_id(hir_id)
      })
    })
    .map(|(point, path)| {
      let actual = ctxt.permissions_data_at_point(path, point);
      let expected = path_boundary.expected;

      log::debug!("Permissions data: {actual:#?}");

      let span = path_boundary
        .location
        .as_local(body.span)
        .unwrap_or(path_boundary.location);

      // FIXME(gavinleroy): the spans are chosen in the `path_visitor` such that the end
      // of the span is where we want the stack to be placed. I would like to
      // make this a bit more explicit.
      let location = span_to_range(span).char_end;

      PermissionsBoundary {
        location,
        expected,
        actual,
      }
    })
}

pub(crate) fn compute_boundaries(
  ctxt: &PermissionsCtxt,
  ir_mapper: &IRMapper,
  span_to_range: impl Fn(Span) -> Range + std::marker::Copy,
) -> Result<Vec<PermissionsBoundary>> {
  let tcx = ctxt.tcx;

  let path_use_points = get_path_boundaries(tcx, ctxt.body_id, ctxt)?
    .into_iter()
    .filter_map(|pb| path_to_perm_boundary(pb, ctxt, ir_mapper, span_to_range));

  let first_error_span_opt =
    errors::get_span_of_first_error(ctxt.def_id.expect_local())
      .and_then(|s| s.as_local(ctxt.body_with_facts.body.span));

  let boundaries = path_use_points
    .filter(|pb| {
      first_error_span_opt
        .map_or(true, |error_span| (pb.location as u32) <= error_span.hi().0)
    })
    .collect::<Vec<_>>();

  Ok(boundaries)
}

#[allow(clippy::module_name_repetitions)]
pub fn compute_permission_boundaries<'a, 'tcx: 'a>(
  ctxt: &AquascopeAnalysis<'a, 'tcx>,
) -> Result<Vec<PermissionsBoundary>> {
  compute_boundaries(&ctxt.permissions, &ctxt.ir_mapper, |span| {
    ctxt.span_to_range(span)
  })
}
