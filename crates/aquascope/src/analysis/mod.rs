//! Core contextual analysis for Aquascope.

use std::cell::RefCell;

use polonius_engine::{Algorithm, Output};
use rustc_borrowck::consumers::BodyWithBorrowckFacts;
use rustc_hir::{
  def_id::LocalDefId, hir_id::HirId, BodyId, Expr, ExprKind, Node,
};
use rustc_hir_analysis::{
  self,
  check::{FnCtxt, Inherited, InheritedBuilder},
};
use rustc_middle::ty::{Ty, TyCtxt};
use rustc_span::Span;

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

struct BodyMethodCallAnalysis<'tcx> {
  body_id: BodyId,
  // FIXME if a failure occurs it will happen at the level of
  // individual `HirId`s. We need a good way of indicating what happened
  // though, I imagine, if the `HirId`s are actually `ExprKind::MethodCall`s
  // they shouldn't fail.
  // TODO make this return type better. (Actual, Expected)
  call_exprs: Vec<(TyInfo<'tcx>, TyInfo<'tcx>)>,
}

struct TyInfo<'tcx> {
  span: Span,
  raw_ty: Ty<'tcx>,
  is_mut: bool,
  is_ref: bool,
}

// TODO what we want back is a vector of results
// - Ok({ Rcvr: (Span, Ty), FnSig: (Span, FnSig) })
// - Err(
//   - Not a method call
//   - No type dependent sig
// )
pub fn process_method_calls_in_item<'tcx>(
  tcx: TyCtxt<'tcx>,
  body_id: BodyId,
  parent_item: LocalDefId,
  call_ids: Vec<HirId>,
) -> Result<BodyMethodCallAnalysis, String>
// FIXME Results should be unified for d
{
  let hir = tcx.hir();
  let def_id_of_body = hir.body_owner_def_id(body_id);
  let body = hir.body(body_id);

  // NOTE `typeck_body` does all of the heavy lifting in typechecking.
  // However, I can't figure out how to do a `lookup_method` from the POV
  // of the typechecked body. We can easily get the receiver type with
  // `expr_ty` but I don't see a way (with the public interface) to
  // get the signature of the resolved method.
  let tyck_res = tcx.typeck_body(body_id);

  call_ids.iter().for_each(|call_node_hir_id| {
    if let Some(Node::Expr(expr)) = hir.find(*call_node_hir_id) {
      match expr.kind {
        ExprKind::MethodCall(segment, rcvr, args, _) => {
          if let Some((def_kind, def_id)) =
            tyck_res.type_dependent_def(*call_node_hir_id)
          {
            // TODO How can we get the binding mutability of the receiver?
            // Tricky, is if this is a chained method call, the receiver
            // could be currently unbound (at the source level) and can still
            // be coerced into a `&mut T`.
            log::debug!("Rcvr Ty {:?}", tyck_res.expr_ty(rcvr));
            log::debug!("Fn Sig {:?}", tcx.fn_sig(def_id));
          } else {
            log::debug!("No dependent def found");
          }
        }
        _ => unreachable!(),
      }
    }
  });

  todo!();
}
