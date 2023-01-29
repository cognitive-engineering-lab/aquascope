//! Analysis for finding expected vs actual permission on path usage.
mod path_visitor;

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
    ir_mapper::GatherDepth,
    permissions::{Permissions, PermissionsData},
    AquascopeAnalysis, KeyShifter, LoanKey,
  },
  mir::utils::PlaceExt,
};

/// A point where the permissions reality are checked against their expectations.
#[derive(Debug, Clone, Serialize, TS)]
#[ts(export)]
pub struct PermissionsBoundary {
  // instead of giving the range, the backend should supply the exact location. this will
  // be especially usefull when we have permissions on more than just method calls.
  pub location: usize,
  pub expected: Permissions,
  pub actual: PermissionsData,
}

impl KeyShifter for PermissionsBoundary {
  fn shift_keys(self, loan_shift: LoanKey) -> Self {
    PermissionsBoundary {
      actual: self.actual.shift_keys(loan_shift),
      ..self
    }
  }
}

// A previous implementation of the permission boundaries
// computation allowed for multiple stacks to get generated
// per HirId, this isn't the case currently so we could
// cleanup these types a bit.
trait IntoMany {
  type Elem;
  fn into_many(self) -> Box<dyn Iterator<Item = Self::Elem>>;
}

// ----------------------------------
// Permission boundaries on path uses

struct PathBoundary<'a, 'tcx: 'a> {
  pub hir_id: HirId,
  pub location: Span,
  pub expected: Permissions,
  pub analysis_ctxt: &'a AquascopeAnalysis<'a, 'tcx>,
}

impl std::fmt::Debug for PathBoundary<'_, '_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("PathBoundary")
      .field("location", &self.location)
      .field("hir_id", &self.hir_id)
      .field("expected", &self.expected)
      .finish()
  }
}

impl IntoMany for PathBoundary<'_, '_> {
  type Elem = PermissionsBoundary;
  fn into_many(self) -> Box<dyn Iterator<Item = Self::Elem>> {
    let ctxt = &self.analysis_ctxt.permissions;
    let body = &ctxt.body_with_facts.body;
    let tcx = ctxt.tcx;
    let hir = tcx.hir();
    let ir_mapper = &self.analysis_ctxt.ir_mapper;
    let hir_id = self.hir_id;

    log::debug!(
      "Resolving permissions boundary for {}",
      hir.node_to_string(self.hir_id)
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
              Rvalue::Ref(_, _, place)
                if place.is_source_visible(tcx, body) =>
              {
                Some((loc, *place))
              }
              Rvalue::Use(op) => maybe_in_op!(op, loc),
              Rvalue::BinaryOp(_, box (left_op, right_op)) => {
                maybe_in_op!(left_op, loc)
                  .or_else(|| maybe_in_op!(right_op, loc))
              }
              Rvalue::CheckedBinaryOp(_, box (left_op, right_op)) => {
                maybe_in_op!(left_op, loc)
                  .or_else(|| maybe_in_op!(right_op, loc))
              }
              Rvalue::CopyForDeref(place)
                if place.is_source_visible(tcx, body) =>
              {
                Some((loc, *place))
              }
              _ => None,
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

    let i = search_at_hir_id(hir_id)
      .or_else(|| {
        hir.parent_iter(hir_id).find_map(|(hir_id, _)| {
          log::debug!("\tsearching upwards in: {hir_id:?}");
          search_at_hir_id(hir_id)
        })
      })
      .map(|(point, path)| {
        let actual = ctxt.permissions_data_at_point(path, point);
        let expected = self.expected;

        log::debug!("Permissions data: {actual:#?}");

        let span = self.location.as_local(body.span).unwrap_or(self.location);

        // NOTE: all permission stacks are placed directly to the _right_ of the path.
        let location = self.analysis_ctxt.span_to_range(span).char_end;

        PermissionsBoundary {
          location,
          expected,
          actual,
        }
      })
      .into_iter();

    Box::new(i)
  }
}

// ----------------------------------
// Entry

#[allow(clippy::module_name_repetitions)]
pub fn compute_permission_boundaries<'a, 'tcx: 'a>(
  ctxt: &AquascopeAnalysis<'a, 'tcx>,
) -> Vec<PermissionsBoundary> {
  // -----------------------------------
  // TODO cleanup and move mod elsewhere
  // -----------------------------------

  let path_use_points =
    get_path_boundaries(ctxt.permissions.tcx, ctxt.permissions.body_id, ctxt)
      .into_iter()
      .map(IntoMany::into_many);

  path_use_points.flatten().collect::<Vec<_>>()
}
