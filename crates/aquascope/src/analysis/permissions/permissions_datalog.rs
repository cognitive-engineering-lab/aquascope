//! Datalog analysis for Aquascope

use std::time::Instant;

use datafrog::{Iteration, Relation, RelationLeaper, ValueFilter};
use flowistry::mir::utils::PlaceExt;
use polonius_engine::{Algorithm, FactTypes, Output as PEOutput};
use rustc_borrowck::{borrow_set::BorrowSet, consumers::BodyWithBorrowckFacts};
use rustc_data_structures::fx::{FxHashMap as HashMap, FxHashSet as HashSet};
use rustc_hir::{BodyId, Mutability};
use rustc_index::vec::IndexVec;
use rustc_middle::{
  mir::{Place, ProjectionElem},
  ty::TyCtxt,
};
use rustc_mir_dataflow::move_paths::MoveData;

use super::{
  context::PermissionsCtxt,
  places_conflict::{AccessDepth, PlaceConflictBias},
  AquascopeFacts, Loan, Path, Point,
};

// FIXME the HashMap should map to multiple loans, because at a
// given point a path could be refined my multiple loans even
// if we only care about a single (more recent).
pub struct Output<T: FactTypes> {
  pub cannot_read: HashMap<T::Point, HashMap<T::Path, T::Loan>>,
  pub cannot_write: HashMap<T::Point, HashMap<T::Path, T::Loan>>,
  pub cannot_drop: HashMap<T::Point, HashMap<T::Path, T::Loan>>,
  pub never_write: HashSet<T::Path>,
  pub never_drop: HashSet<T::Path>,
}

/*
 ****
 *
 * .decl never_write(Path)
 *
 * never_write(Path) :-
 *    is_direct(Path),
 *    declared_readonly(Path).
 *
 * never_write(Path) :-
 *    !is_direct(Path),
 *    prefix_of(Prefix, Path),
 *    is_immut_ref(Prefix).
 *
 ****
 *
 * .decl never_drop(Path)
 *
 * never_drop(Path) :-
 *    !is_direct(Path).
 *
 ****
 *
 * .decl cannot_read(Path:path, Point:point)
 *
 * cannot_read(Path, Loan, Point) :-
 *    loan_conflicts_with(Loan, Path),
 *    loan_live_at(Loan, Point),
 *    loan_mutable(Loan).
 *
 ****
 *
 * .decl cannot_write(Path:path, Point:point)
 *
 * cannot_write(Path, Loan, Point) :-
 *    loan_conflicts_with(Loan, Path),
 *    loan_live_at(Loan, Point).
 *
 ****
 *
 * .decl cannot_drop(Path, Loan, Point)
 *
 * cannot_drop(Path, Loan, Point)
 *    loan_conflicts_with(Loan, Path),
 *    loan_live_at(Loan, Point).
 *
 */

impl Default for Output<AquascopeFacts> {
  fn default() -> Self {
    Self {
      cannot_read: HashMap::default(),
      cannot_write: HashMap::default(),
      cannot_drop: HashMap::default(),
      never_write: HashSet::default(),
      never_drop: HashSet::default(),
    }
  }
}

impl Output<AquascopeFacts> {
  pub fn populate_ctxt<'a, 'tcx>(ctxt: &mut PermissionsCtxt<'a, 'tcx>) {
    // auxilary help and facts

    let def_id = ctxt.tcx.hir().body_owner_def_id(ctxt.body_id);
    let body = &ctxt.body_with_facts.body;
    let tcx = ctxt.tcx;
    let places: Vec<Place<'tcx>> = body
      .local_decls
      .indices()
      .flat_map(|local| {
        Place::from_local(local, tcx).interior_paths(
          tcx,
          body,
          def_id.to_def_id(),
        )
      })
      .collect();

    let paths: Vec<Path> =
      places.iter().map(|place| ctxt.new_path(*place)).collect();

    let loan_to_borrow = |l: Loan| &ctxt.borrow_set[l];

    let is_never_write = |path: Path| {
      let place = &ctxt.path_to_place(path);
      (!place.is_indirect() && ctxt.is_declared_readonly(place)) || {
        // Iff there exists an immutable prefix it is also `never_write`
        place
          .iter_projections()
          .filter_map(|(prefix, elem)| {
            matches!(elem, ProjectionElem::Deref).then_some(prefix)
          })
          .any(|prefix| {
            let ty = prefix.ty(&body.local_decls, tcx).ty;
            match ty.ref_mutability() {
              Some(mutability) => mutability == Mutability::Not,
              None => false, // TODO: unimplemented!(), // raw pointers, not handling this now
            }
          })
      }
    };

    let is_never_drop = |path: Path| ctxt.path_to_place(path).is_indirect();

    // .decl loan_conflicts_with(Loan, Path)
    let loan_conflicts_with: Relation<(Loan, Path)> = Relation::from_iter(
      ctxt.polonius_input_facts.loan_issued_at.iter().flat_map(
        |(_origin, loan, _point)| {
          let borrow = loan_to_borrow(*loan);
          places.iter().filter_map(|place| {
            super::places_conflict::borrow_conflicts_with_place(
              tcx,
              body,
              borrow.borrowed_place,
              borrow.kind,
              place.as_ref(),
              AccessDepth::Deep,
              PlaceConflictBias::Overlap,
            )
            .then_some((*loan, ctxt.place_to_path(place)))
          })
        },
      ),
    );

    let loan_live_at: Relation<(Loan, Point)> = Relation::from_iter(
      ctxt
        .polonius_output
        .loan_live_at
        .iter()
        .flat_map(|(point, values)| values.iter().map(|loan| (*loan, *point))),
    );

    let cannot_read: Relation<(Path, Loan, Point)> = Relation::from_leapjoin(
      &loan_conflicts_with,
      (
        loan_live_at.extend_with(|&(loan, _path)| loan),
        ValueFilter::from(|&(loan, _path), _point| ctxt.is_mutable_loan(loan)),
      ),
      |&(loan, path), &point| (path, loan, point),
    );

    let cannot_write: Relation<(Path, Loan, Point)> = Relation::from_join(
      &loan_conflicts_with,
      &loan_live_at,
      |&loan, &path, &point| (path, loan, point),
    );

    let cannot_drop: Relation<(Path, Loan, Point)> = Relation::from_join(
      &loan_conflicts_with,
      &loan_live_at,
      |&loan, &path, &point| (path, loan, point),
    );

    // log::debug!(
    //   "LOAN_CONFLICTS_WITH {:?}",
    //   loan_conflicts_with.iter().collect::<Vec<_>>()
    // );
    // log::debug!("LOAN_LIVE_AT {:?}", loan_live_at.iter().collect::<Vec<_>>());
    // log::debug!("CANNOT_READ {:?}", cannot_read.iter().collect::<Vec<_>>());
    // log::debug!("CANNOT_WRITE {:?}", cannot_write.iter().collect::<Vec<_>>());
    // log::debug!("CANNOT_DROP {:?}", cannot_drop.iter().collect::<Vec<_>>());

    // We don't need to use these relations in the rules so write
    // them directly to the output.

    let never_write = paths
      .iter()
      .filter_map(|path| is_never_write(*path).then_some(*path))
      .collect();

    let never_drop = paths
      .iter()
      .filter_map(|path| is_never_drop(*path).then_some(*path))
      .collect();

    ctxt.permissions_output.never_write = never_write;
    ctxt.permissions_output.never_drop = never_drop;

    for &(path, loan, point) in cannot_read.iter() {
      ctxt
        .permissions_output
        .cannot_read
        .entry(point)
        .or_default()
        .insert(path, loan);
    }

    for &(path, loan, point) in cannot_write.iter() {
      ctxt
        .permissions_output
        .cannot_write
        .entry(point)
        .or_default()
        .insert(path, loan);
    }

    for &(path, loan, point) in cannot_drop.iter() {
      ctxt
        .permissions_output
        .cannot_drop
        .entry(point)
        .or_default()
        .insert(path, loan);
    }

    log::debug!(
      "#cannot_read {} #cannot_write {} #cannot_drop {}",
      cannot_read.len(),
      cannot_write.len(),
      cannot_drop.len()
    );
  }
}

// ----------
// Main entry

pub fn compute<'a, 'tcx>(
  tcx: TyCtxt<'tcx>,
  body_id: BodyId,
  body_with_facts: &'a BodyWithBorrowckFacts<'tcx>,
) -> PermissionsCtxt<'a, 'tcx> {
  let timer = Instant::now();
  let def_id = tcx.hir().body_owner_def_id(body_id);
  let body = &body_with_facts.body;

  // for debugging pruposes only
  let owner = tcx.hir().body_owner(body_id);
  let Some(name) = tcx.hir().opt_name(owner) else { unreachable!() };
  log::debug!("computing body permissions {:?}", name.as_str());

  let polonius_input_facts = &body_with_facts.input_facts;
  let polonius_output =
    PEOutput::compute(polonius_input_facts, Algorithm::Naive, true);

  let locals_are_invalidated_at_exit =
    tcx.hir().body_owner_kind(def_id).is_fn_or_closure();
  let (_, move_data) =
    MoveData::gather_moves(body, tcx, tcx.param_env(def_id)).unwrap();
  let borrow_set =
    BorrowSet::build(tcx, body, locals_are_invalidated_at_exit, &move_data);

  let mut ctxt = PermissionsCtxt {
    tcx,
    permissions_output: Output::default(),
    polonius_input_facts,
    polonius_output,
    body_id,
    def_id: def_id.to_def_id(),
    body_with_facts,
    borrow_set,
    move_data,
    place_data: IndexVec::new(),
    rev_lookup: HashMap::default(),
  };

  Output::populate_ctxt(&mut ctxt);

  log::info!("permissions analysis duration: {:?}", timer.elapsed());

  ctxt
}
