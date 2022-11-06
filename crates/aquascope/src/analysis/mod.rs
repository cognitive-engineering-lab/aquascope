//! Core contextual analysis for Aquascope.

pub mod find_bindings;
pub mod find_calls;
mod find_hir_calls;
mod permissions;

use std::cell::RefCell;

pub use find_bindings::find_bindings;
use find_calls::FindCalls;
use find_hir_calls::find_method_call_spans;
use rustc_borrowck::consumers::{BodyWithBorrowckFacts, RustcFacts};
use rustc_data_structures::fx::{FxHashMap as HashMap, FxHashSet as HashSet};
use rustc_hir::{
  def::Res, def_id::LocalDefId, hir_id::HirId, BindingAnnotation, BodyId, Expr,
  ExprKind, Node, QPath,
};
use rustc_middle::{mir::Place, ty::TyCtxt};
use rustc_mir_dataflow::move_paths::{LookupResult, MoveData};
use rustc_span::Span;
use serde::Serialize;
use ts_rs::TS;

use crate::Range;

type PermissionsResults = permissions::Output<RustcFacts>;

thread_local! {
  pub static BODY_ID_STACK: RefCell<Vec<BodyId>> =
    RefCell::new(Vec::default());
}

pub fn compute_permissions<'a, 'tcx>(
  tcx: TyCtxt<'tcx>,
  body_id: BodyId,
  body_with_facts: &'a BodyWithBorrowckFacts<'tcx>,
) -> PermissionsResults {
  BODY_ID_STACK.with(|stack| {
    stack.borrow_mut().push(body_id);

    let permissions = permissions::compute(tcx, body_id, body_with_facts);

    // TODO rather than just computing the permissions, we should return a
    // permission context which includes all the information necessary to
    // map things back to the source level.

    permissions
  })
}

#[derive(Debug, Clone, Serialize, TS)]
#[ts(export)]
pub struct PermissionsInfo {
  pub range: Range,
  pub read: bool,
  pub write: bool,
  pub drop: bool,
  pub refined_by: Option<RefinementInfo>,
}

#[derive(Debug, Clone, Serialize, TS)]
#[ts(export)]
pub struct RefinementInfo {}

pub fn pair_permissions_to_calls<'a, 'tcx>(
  tcx: TyCtxt<'tcx>,
  body_id: BodyId,
  body_with_facts: &'a BodyWithBorrowckFacts<'tcx>,
  permissions: &'a PermissionsResults,
  span_to_range: impl Fn(Span) -> Range,
) -> Vec<PermissionsInfo> {
  // FIXME: the MoveData should only be computed once.

  let def_id = tcx.hir().body_owner_def_id(body_id);
  let (_, move_data) =
    MoveData::gather_moves(&body_with_facts.body, tcx, tcx.param_env(def_id))
      .unwrap();
  let move_data = &move_data;

  let place_to_path = |p: Place| match move_data.rev_lookup.find(p.as_ref()) {
    LookupResult::Exact(path) => path,
    LookupResult::Parent(Some(path)) => path,
    r => {
      log::debug!("place to path failed with {:?}", p);
      log::debug!("lookupres {:?}", r);
      todo!()
    }
  };

  let locations_to_body_info = body_with_facts.body.find_calls();

  let never_write = &permissions.never_write;
  let never_drop = &permissions.never_drop;

  let method_spans = find_method_call_spans(tcx);

  locations_to_body_info
    .iter()
    .filter_map(|(loc, call_info)| {
      // HACK: if there is no overlap, then just ignore the call.
      if !method_spans
        .iter()
        .any(|&method_span| call_info.fn_span.overlaps(method_span))
      {
        return None;
      }

      // we need to turn the locations into `LocationIndex`s
      // XXX: assuming the important information we need is at
      // the mid_point
      let point = body_with_facts.location_table.start_index(*loc);
      // get the permissions for the given receivers place
      let path = &place_to_path(call_info.receiver_place);
      let empty = &HashMap::default();
      let cannot_read = permissions.cannot_read.get(&point).unwrap_or(empty);
      let cannot_write = permissions.cannot_write.get(&point).unwrap_or(empty);
      let cannot_drop = permissions.cannot_drop.get(&point).unwrap_or(empty);
      let read = !cannot_read.contains_key(path);
      let write =
        !(never_write.contains(path) || cannot_write.contains_key(path));
      let drop = !(never_drop.contains(path) || cannot_drop.contains_key(path));

      Some(PermissionsInfo {
        range: span_to_range(call_info.fn_span),
        read,
        write,
        drop,
        // TODO
        refined_by: None,
      })
    })
    .collect()
}
