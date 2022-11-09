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
    def::Res, def_id::LocalDefId, hir_id::HirId, BindingAnnotation, BodyId, Expr, ExprKind, Node,
    QPath,
};
use rustc_middle::{
    mir::{Mutability, Place},
    ty::{Ty, TyCtxt},
};
use rustc_mir_dataflow::move_paths::{LookupResult, MoveData};
use rustc_span::Span;
use serde::Serialize;
use ts_rs::TS;

use crate::Range;

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
pub struct RefinementInfo {}

pub fn pair_permissions_to_calls<'a, 'tcx>(
    ctxt: &PermissionsCtxt<'a, 'tcx>,
    span_to_range: impl Fn(Span) -> Range,
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
                    let point = ctxt.location_to_point(*loc);
                    let path = &ctxt.place_to_path(&call_info.receiver_place);

                    log::debug!("Perms for {:?} {:?}", ctxt.path_to_place(*path), path);

                    let actual = ctxt.permissions_output.permissions_at_point(*path, point);
                    let expected = fn_sig.inputs()[0].into();
                    PermissionsInfo {
                        range: span_to_range(call_info.fn_span),
                        actual,
                        expected,
                        // TODO
                        refined_by: None,
                    }
                })
        })
        .collect()
}
