//! Datalog analysis for Aquascope

use std::time::Instant;

use datafrog::Relation;
use polonius_engine::FactTypes;
use rustc_borrowck::consumers::BodyWithBorrowckFacts;
use rustc_data_structures::fx::FxHashMap as HashMap;

// TODO read this post before doing anything with datafrog
// https://github.com/frankmcsherry/blog/blob/master/posts/2018-05-19.md

pub struct RefinementContext<T: FactTypes> {
  pub path_moved_at_base: Relation<(T::Path, T::Point)>,
  pub cfg_edge: Relation<(T::Point, T::Point)>,
}

pub struct Ouput<T: FactTypes> {
  pub loan_live_at: HashMap<T::Point, Vec<T::Loan>>,
  pub refined_by: HashMap<T::Point, Vec<(T::Path, T::Loan)>>,
}

pub(crate) fn compute<'a, 'tcx>(
  body_with_facts: &'a BodyWithBorrowckFacts<'tcx>,
) {
  let timer = Instant::now();

  log::debug!("computing ...");

  let facts = &body_with_facts.input_facts;

  log::debug!("FACTS {:?}", facts);

  /* XXX: current proposal for refinement information.
   *
   * .decl bot_type_at(Q:path, P:point)
   *
   * bot_type_at(Q:path, P2:point) :-
   *     path_moved_at_base(Q:path, P1:point),
   *     cfg_edge(P1:point, P2:point).
   *
   *
   * .decl  refined_by(V:variable, L:loan, P:point)
   *
   * refined_by(Q:path, L:loan, P:point) :-
   *     loan_created_from(L:loan, Q:path),
   *     loan_live_at(L:loan, P:point).
   *
   */

  // TODO

  log::info!("analysis duration: {}", timer.elapsed());
}
