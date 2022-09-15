//! Core contextual analysis for Aquascope.

use std::cell::RefCell;

use polonius_engine::{Algorithm, Output};
use rustc_borrowck::consumers::BodyWithBorrowckFacts;
use rustc_hir::BodyId;
use rustc_middle::ty::TyCtxt;

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
