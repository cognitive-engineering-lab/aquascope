//! Core contextual analysis for Aquascope.

mod engine;
pub mod find_bindings;
pub mod find_calls;

use std::cell::RefCell;

pub use find_bindings::find_bindings;
pub use find_calls::find_method_calls;
use polonius_engine::{Algorithm, Output};
use rustc_borrowck::consumers::BodyWithBorrowckFacts;
use rustc_data_structures::fx::FxHashMap as HashMap;
use rustc_hir::{
  def::Res, def_id::LocalDefId, hir_id::HirId, BindingAnnotation, BodyId, Expr,
  ExprKind, Node, QPath,
};
use rustc_middle::ty::{Ty, TyCtxt};
use rustc_span::Span;
use serde::Serialize;
use ts_rs::TS;

use crate::Range;

// TODO what sorts of things are we going to include in the contex?
// What sort of information do we want to know?
pub type ScopeResults<'a, 'tcx> = i32;

thread_local! {
  pub static BODY_ID_STACK: RefCell<Vec<BodyId>> =
    RefCell::new(Vec::default());
}

// TODO promote comments like these into more informative doc comments.
//
// This enumerates the different kinds of analyses that Aquascope
// will perform. During initial development the running experiment
// will regard Lifetimes, however, many of the analysis results
// can probably be cached and used within each other.
pub enum ScopeAnalysisKind {
  Lifetime,
}

// TODO This function should take a BodyId and produce a context with
// information about the body. Preprocessing can be done when the
// resulting context
pub fn compute_context<'a, 'tcx>(
  tcx: TyCtxt<'tcx>,
  body_id: BodyId,
  body_with_facts: &'a BodyWithBorrowckFacts<'tcx>,
) -> ScopeResults<'a, 'tcx> {
  BODY_ID_STACK.with(|stack| {
    stack.borrow_mut().push(body_id);

    log::debug!("Context for: {:?}", body_id);
    log::debug!(
      "{}",
      rustc_hir_pretty::to_string(rustc_hir_pretty::NO_ANN, |s| s
        .print_expr(&tcx.hir().body(body_id).value))
    );

    // The input facts information gives a list of input facts for each Fact.
    //
    // A RustcFact is defined as follows:
    // ```rust
    // impl polonius_engine::FactTypes for RustcFacts {
    //     type Origin = RegionVid;
    //     type Loan = BorrowIndex;
    //     type Point = LocationIndex;
    //     type Variable = Local;
    //     type Path = MovePathIndex;
    // }
    // ```
    // log::debug!("Input Facts: {:?}", body_with_facts.input_facts);
    // log::debug!("Output Facts: {:?}", body_with_facts.output_facts);

    log::debug!(
      "Loans issued in body {:?}",
      body_with_facts.input_facts.loan_issued_at
    );

    log::debug!(
      "Loan invalidated at {:?}",
      body_with_facts.input_facts.loan_invalidated_at
    );

    engine::compute(body_with_facts);

    // HACK this is essentially copying the
    // `LocationTable` logic from `rustc_borrowck::location`.

    // let mut num_points = 0;
    // let bbds: Vec<(usize, &Vec<rustc_middle::mir::Statement<'tcx>>)> =
    //   body_with_facts
    //     .body
    //     .basic_blocks
    //     .iter()
    //     .map(|block_data| {
    //       let v = num_points;
    //       num_points += (block_data.statements.len() + 1) * 2;
    //       (v, &block_data.statements)
    //     })
    //     .collect();

    // log::debug!("Body Statements {:?}", bbds);

    0
  })
}

#[derive(Debug, Clone, Serialize, TS)]
#[ts(export)]
pub struct CallTypes {
  pub expected: TypeInfo,
  pub actual: TypeInfo,
}

#[derive(Debug, Clone, Serialize, TS)]
#[ts(export)]
pub struct TypeInfo {
  pub range: Range,
  pub of_type: TypeState,
}

#[derive(Debug, Clone, Serialize, TS)]
#[ts(export)]
pub enum TypeState {
  Owned { mutably_bound: bool },
  Ref { is_mut: bool },
}

impl TypeState {
  fn from_ty<'tcx>(ty: &Ty<'tcx>, ba: Option<&BindingAnnotation>) -> Self {
    if ty.is_mutable_ptr() {
      TypeState::Ref { is_mut: true }
    } else if ty.is_ref() {
      TypeState::Ref { is_mut: false }
    } else {
      log::debug!("Owned value {:?} {:?}", ty, ba);
      TypeState::Owned {
        mutably_bound: if let Some(BindingAnnotation(
          _,
          rustc_hir::Mutability::Mut,
        )) = ba
        {
          true
        } else {
          false
        },
      }
    }
  }
}

pub fn process_method_calls_in_item<'tcx>(
  tcx: TyCtxt<'tcx>,
  body_id: BodyId,
  parent_item: LocalDefId,
  call_ids: Vec<HirId>,
  program_bindings: &HashMap<HirId, BindingAnnotation>,
  span_to_range: impl Fn(Span) -> Range,
) -> Vec<CallTypes> {
  let hir = tcx.hir();
  let def_id_of_body = hir.body_owner_def_id(body_id);
  let body = hir.body(body_id);

  let tyck_res = tcx.typeck_body(body_id);

  // FIXME using filter_map here is lossy. IF there's some Id that
  // fails (for a number of reasons) then filter_map will just ignore
  // it. However, this could indicate an error somewhere. (or simply
  // naïveté on my part in previous logic).
  call_ids
    .iter()
    .filter_map(|call_node_hir_id| {
      if let Some(Node::Expr(expr)) = hir.find(*call_node_hir_id) {
        match expr.kind {
          ExprKind::MethodCall(_, rcvr, _, call_span) => {
            if let Some((_, def_id)) =
              tyck_res.type_dependent_def(*call_node_hir_id)
            {
              let rcvr_t = tyck_res.expr_ty(rcvr);
              // HACK FIXME
              // See: https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/ty/sty/struct.Binder.html#method.skip_binder
              let fn_sig = tcx.fn_sig(def_id).skip_binder();
              let expected_rcvr_t = fn_sig.inputs_and_output[0];
              let rcvr_range = span_to_range(rcvr.span);
              // The range of the method Segment.
              let method_range = span_to_range(call_span);

              log::trace!("Found receiver type: {rcvr_t:?}");
              log::trace!("Found function signature: {fn_sig:?}");

              log::trace!("Getting bindings for receiver: {:?}", rcvr.hir_id);

              let rcvr_bound =
                if let ExprKind::Path(QPath::Resolved(_, path)) = rcvr.kind {
                  match path.res {
                    Res::Local(id) => program_bindings.get(&id),
                    _ => None,
                  }
                } else {
                  None
                };

              log::debug!("RCVR BOUND: {:?}", rcvr);
              log::debug!("ALL BINDINGS: {:?}", program_bindings);
              log::debug!("Rcvr_bound: {:?}", rcvr_bound);

              let actual_tys = TypeState::from_ty(&rcvr_t, rcvr_bound);
              // XXX Is there a case when we'd want to know a
              // potential binding for the expected type?
              let expected_tys = TypeState::from_ty(&expected_rcvr_t, None);

              let call_type = CallTypes {
                actual: TypeInfo {
                  range: rcvr_range,
                  of_type: actual_tys,
                },
                expected: TypeInfo {
                  range: method_range,
                  of_type: expected_tys,
                },
              };

              Some(call_type)
            } else {
              panic!("No dependent def found for method call HirId.");
            }
          }
          _ => unreachable!(),
        }
      } else {
        unreachable!();
      }
    })
    .collect()
}
