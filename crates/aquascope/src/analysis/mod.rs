//! Core contextual analysis for Aquascope.

use std::cell::RefCell;

use polonius_engine::{Algorithm, Output};
use rustc_borrowck::consumers::BodyWithBorrowckFacts;
use rustc_data_structures::fx::FxHashMap as HashMap;
use rustc_hir::{
  def_id::LocalDefId, hir_id::HirId, BindingAnnotation, BodyId, Expr, ExprKind,
  Node,
};
use rustc_hir_analysis::{
  self,
  check::{FnCtxt, Inherited, InheritedBuilder},
};
use rustc_middle::ty::{Ty, TyCtxt};
use rustc_span::Span;

use crate::source_map::Range;

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
    log::debug!("Input Facts: {:?}", body_with_facts.input_facts);

    log::debug!("Output Facts: {:?}", body_with_facts.output_facts);

    let naive_output =
      Output::compute(&body_with_facts.input_facts, Algorithm::Naive, true);

    if !naive_output.errors.is_empty() {
      log::debug!("THERE IS A BORROWCK ERROR!");
      todo!();
    }

    log::debug!("No borrowck errors found.");

    log::debug!("Extracting information for: {:?}", body_id.hir_id.local_id);

    let input_facts = &body_with_facts.input_facts;
    let local_point: u32 = body_id.hir_id.local_id.into();
    let Some((path, point)) = input_facts
      .path_assigned_at_base
      .iter()
      .filter(|(_, pnt)| pnt.as_u32() == local_point)
      .next() else { todo!() };

    0
  })
}

#[derive(Debug, Clone)]
pub struct CallTypes {
  pub expected: TyInfo,
  pub actual: TyInfo,
}

#[derive(Debug, Clone)]
pub struct TyInfo {
  pub range: Range,
  pub of_type: TypeState,
}

#[derive(Debug, Clone)]
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
      match ba {
        // No binding annotations suggest that this is an expected type
        // of the method signature. If the type is something like `foo(mut self, ...)`
        // we ignore the `mut` modifier because this is only relevant for the
        // bindings after. Hence the hardcoded `false` below.
        None => TypeState::Owned {
          mutably_bound: false,
        },
        Some(ba) => {
          todo!()
        }
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

              // FIXME there's a mismatch somewhere with the binding annotations
              // retrieved and this lookup always returns None.
              let rcvr_bound = program_bindings.get(&rcvr.hir_id);
              let actual_tys = TypeState::from_ty(&rcvr_t, rcvr_bound);
              // XXX Is there a case when we'd want to know a
              // potential binding for the expected type?
              let expected_tys = TypeState::from_ty(&expected_rcvr_t, None);

              let call_type = CallTypes {
                actual: TyInfo {
                  range: rcvr_range,
                  of_type: actual_tys,
                },
                expected: TyInfo {
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
