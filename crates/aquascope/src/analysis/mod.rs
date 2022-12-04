//! Core contextual analysis for Aquascope.

#[allow(dead_code)]
pub mod find_bindings;
mod find_hir_calls;
pub mod find_mir_calls;
pub mod permissions;

use std::cell::RefCell;

pub use find_bindings::find_bindings;
use permissions::PermissionsCtxt;
pub use permissions::{
  permission_boundaries::pair_permissions_to_calls,
  permission_stepper::compute_permission_steps,
};
use rustc_borrowck::consumers::BodyWithBorrowckFacts;
use rustc_hir::BodyId;
use rustc_middle::ty::TyCtxt;

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

    if cfg!(debug_assertions) {
      permissions::utils::dump_permissions_with_mir(&permissions);
    }

    permissions
  })
}
