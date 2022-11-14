//! Core contextual analysis for Aquascope.

pub mod find_bindings;
pub mod find_calls;
mod find_hir_calls;
mod permissions;

use std::cell::RefCell;

pub use find_bindings::find_bindings;
use find_calls::FindCalls;
use find_hir_calls::find_method_call_spans;
use permissions::PermissionsCtxt;
use rustc_borrowck::consumers::{BodyWithBorrowckFacts, RustcFacts};
use rustc_data_structures::fx::{FxHashMap as HashMap, FxHashSet as HashSet};
use rustc_hir::{
  def::Res, def_id::LocalDefId, hir_id::HirId, BindingAnnotation, BodyId, Expr,
  ExprKind, Node, QPath,
};
use rustc_middle::{
  mir::{Location, Mutability, Operand, Place, Rvalue, StatementKind},
  ty::{Ty, TyCtxt},
};
use rustc_mir_dataflow::move_paths::{LookupResult, MoveData};
use rustc_span::Span;
use serde::Serialize;
use ts_rs::TS;

use crate::{
  analysis::permissions::{Loan, Path, Point},
  Range,
};

thread_local! {
  pub static BODY_ID_STACK: RefCell<Vec<BodyId>> =
    RefCell::new(Vec::default());
}

pub fn compute_permissions<'a, 'tcx>(
  tcx: TyCtxt<'tcx>,
  body_id: BodyId,
  body_with_facts: &'a BodyWithBorrowckFacts<'tcx>,
) -> PermissionsCtxt<'a, 'tcx> {
  BODY_ID_STACK.with(|stack| {
    stack.borrow_mut().push(body_id);

    let permissions = permissions::compute(tcx, body_id, body_with_facts);

    // TODO rather than just computing the permissions, we should return a
    // permission context which includes all the information necessary to
    // map things back to the source level.

    permissions::utils::dump_permissions_with_mir(&permissions);

    permissions
  })
}

#[derive(Clone, Copy, PartialEq, Eq, Serialize, TS)]
#[ts(export)]
pub struct Permissions {
  pub read: bool,
  pub write: bool,
  pub drop: bool,
}

///// Debugging traits, just for visualization purposes
impl std::fmt::Debug for Permissions {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    if !self.read && !self.write && !self.drop {
      write!(f, "∅")
    } else {
      if self.read {
        write!(f, "R")?;
      }
      if self.write {
        write!(f, "W")?;
      }
      if self.drop {
        write!(f, "D")?;
      }
      Ok(())
    }
  }
}

// XXX: this is only valid when the Ty is an *expected* type.
// This is because expected types do not rely on the mutability of
// the binding, e.g. `let mut x = ...` and all of the expected information
// is really just in the type.
impl<'tcx> From<Ty<'tcx>> for Permissions {
  fn from(ty: Ty<'tcx>) -> Self {
    let read = true;
    let (write, drop) = match ty.ref_mutability() {
      None => (false, true),
      Some(Mutability::Not) => (false, false),
      Some(Mutability::Mut) => (true, false),
    };
    Self { read, write, drop }
  }
}

#[derive(Debug, Clone, Serialize, TS)]
#[ts(export)]
pub struct PermissionsInfo {
  pub range: Range,
  pub expected: Permissions,
  pub actual: Permissions,
  pub refined_by: Option<RefinementInfo>,
}

#[derive(Debug, Clone, Serialize, TS)]
#[ts(export)]
pub struct RefinementRegion {
  pub loan_location: Range,
  pub start: Range,
  pub end: Range,
}

#[derive(Debug, Clone, Serialize, TS)]
#[ts(export)]
pub struct RefinementInfo {
  pub read: Option<RefinementRegion>,
  pub write: Option<RefinementRegion>,
  pub drop: Option<RefinementRegion>,
}

pub fn pair_permissions_to_calls<'a, 'tcx>(
  ctxt: &PermissionsCtxt<'a, 'tcx>,
  span_to_range: impl Fn(Span) -> Range + std::marker::Copy,
) -> Vec<PermissionsInfo> {
  let locations_to_body_info = ctxt.body_with_facts.body.find_calls();

  let never_write = &ctxt.permissions_output.never_write;
  let never_drop = &ctxt.permissions_output.never_drop;

  let method_spans = find_method_call_spans(ctxt.tcx, ctxt.body_id);

  locations_to_body_info
    .iter()
    .filter_map(|(loc, call_info)| {
      // HACK: if there is no overlap, then just ignore the call.
      method_spans
        .iter()
        .find(|&&(fn_span, _)| call_info.fn_span.overlaps(fn_span))
        .map(|&(fn_span, fn_sig)| {
          let point_call = ctxt.location_to_point(*loc);
          let path = &ctxt.place_to_path(&call_info.receiver_place);

          // HACK: there is a small issue with the path retrived from the call site.
          // A piece of code like the following:
          //
          // ```rust
          // let v = vec![];
          // v.push(0);
          // ```
          //
          // will get desugared into code that looks (roughly) like this:
          //
          // ```rust
          // let v = vec![];
          // let _t = &mut v;
          // Vec::push(move _t, 0);
          // ```
          // What this means, is that using `_t` for the permissions at the call site
          // will show that there is *always* enough permissions. Even though, in the
          // above example `v` is missing Write permissions. And the borrow is illegal.
          //
          // The hack below finds the borrowed / moved path which would represent
          // the receiver `v` in the example.

          let place_0 = ctxt.path_to_place(*path);
          let place_0_local = place_0.local;
          let mut definitions = ctxt
            .polonius_input_facts
            .var_defined_at
            .iter()
            .filter_map(|&(v, p)| (v == place_0_local).then_some(p))
            .collect::<Vec<_>>();
          definitions.sort();
          assert!(definitions.len() == 3);
          // XXX: The brittle assumption here is that there are
          // three points of assignment:
          // 0. StorageLive
          // 1. place = ...
          // 2. StorageDead
          // thus we eagerly take the middle assignment.
          let point_assign = definitions[1];
          let location = ctxt.point_to_location(point_assign);
          let stmt = ctxt.body_with_facts.body.stmt_at(location);

          // XXX: the assumption here is that when assigning to a
          // temporary path for a function call it is either:
          // - a reference that gets borrowed from another place.
          // - a move from another place.
          //
          // However, if the statement at the derived location is
          // a terminator, then the default is to take the original
          // place and point of the method call. This could happen,
          // for example, in a line such as: `Vec::default().push(0)`.
          let (place_1, point) =
            stmt.left().map_or((place_0, point_call), |stmt| {
              let place = match stmt.kind {
                StatementKind::Assign(box (_, Rvalue::Ref(_, _, place))) => {
                  place
                }
                StatementKind::Assign(box (
                  _,
                  Rvalue::Use(Operand::Move(place)),
                )) => place,
                _ => unreachable!(),
              };
              (place, point_assign)
            });

          let path = ctxt.place_to_path(&place_1);
          let actual =
            ctxt.permissions_output.permissions_at_point(path, point);
          let expected = fn_sig.inputs()[0].into();

          let refined_by =
            find_refinements_at_point(ctxt, path, point, span_to_range);

          PermissionsInfo {
            range: span_to_range(call_info.fn_span),
            actual,
            expected,
            refined_by,
          }
        })
    })
    .collect()
}

pub fn find_refinements_at_point<'a, 'tcx>(
  ctxt: &PermissionsCtxt<'a, 'tcx>,
  path: Path,
  point: Point,
  span_to_range: impl Fn(Span) -> Range,
) -> Option<RefinementInfo> {
  let path = &path;
  let point = &point;
  let empty_hash = &HashMap::default();

  let loan_regions = ctxt.loan_regions.as_ref().unwrap();

  let cannot_read = ctxt
    .permissions_output
    .cannot_read
    .get(point)
    .unwrap_or(empty_hash);
  let cannot_write = ctxt
    .permissions_output
    .cannot_write
    .get(point)
    .unwrap_or(empty_hash);
  let cannot_drop = ctxt
    .permissions_output
    .cannot_drop
    .get(point)
    .unwrap_or(empty_hash);

  let find_refinement = |hshr: &HashMap<Path, Loan>| {
    // TODO: the permissions_output only keeps one loan per path, however, there could
    // theoretically be several which are refining. When this is fixed, the analysis
    // here should pick the loan which lasts the longest.
    hshr.get(path).map(|loan| {
      let (p_0, p_e) = loan_regions.get(loan).unwrap();
      // TODO: using `reserve_location` is not exactly accurate because this
      // could be a two-phase borrow. This needs to use the `activation_location`.
      let loan_loc = ctxt.borrow_set[*loan].reserve_location;
      let start_loc = ctxt.point_to_location(*p_0);
      let end_loc = ctxt.point_to_location(*p_e);

      let loan_span = ctxt.body_with_facts.body.source_info(loan_loc).span;
      let start_span = ctxt.body_with_facts.body.source_info(start_loc).span;
      let end_span = ctxt.body_with_facts.body.source_info(end_loc).span;

      let loan_location = span_to_range(loan_span);
      let start = span_to_range(start_span);
      let end = span_to_range(end_span);

      RefinementRegion {
        loan_location,
        start,
        end,
      }
    })
  };

  let read = find_refinement(cannot_read);
  let write = find_refinement(cannot_write);
  let drop = find_refinement(cannot_drop);

  Some(RefinementInfo { read, write, drop })
}
