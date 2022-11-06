//! Datalog analysis for Aquascope

use std::{
  collections::{BTreeMap, BTreeSet},
  time::Instant,
};

use datafrog::{ExtendWith, Iteration, Relation, RelationLeaper};
use flowistry::mir::utils::PlaceExt;
use polonius_engine::{Algorithm, AllFacts, FactTypes, Output as PEOutput};
use rustc_borrowck::{
  borrow_set::{BorrowData, BorrowSet},
  consumers::{BodyWithBorrowckFacts, RustcFacts},
};
use rustc_data_structures::fx::{FxHashMap as HashMap, FxHashSet as HashSet};
use rustc_hir::{BodyId, Mutability};
use rustc_middle::{
  mir::{BorrowKind, Place, ProjectionElem},
  ty::TyCtxt,
};
use rustc_mir_dataflow::move_paths::{LookupResult, MoveData};

use super::places_conflict::{AccessDepth, PlaceConflictBias};

// XXX: would it be more space efficient to use BTreeMap?
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
 * // Base Case
 * cannot_read(Path, Loan, Point) :-
 *    loan_issued_at(_, Loan, Point),
 *    loan_conflicts_with(Loan, Path),
 *    loan_mutable(Loan).
 *
 * // Recursive Step
 * cannot_read(Path, Loan, Point1) :-
 *    cannot_read(Path, Loan, Point0)
 *    loan_live_at(Loan, Point1),
 *    cfg_edge(Point0, Point1),
 *
 ****
 *
 * .decl cannot_write(Path:path, Point:point)
 *
 * // Base Case
 * cannot_write(Path, Loan, Point) :-
 *    loan_issued_at(_, Loan, Point),
 *    loan_conflicts_with(Loan, Path),
 *
 * // Recursive Step
 * cannot_write(Path, Loan, Point1) :-
 *    cannot_write(Path, Loan, Point0),
 *    loan_live_at(Loan, Point1),
 *    cfg_edge(Point0, Point1).
 *
 ****
 *
 * .decl cannot_drop(Path, Loan, Point)
 *
 * // Base Case
 * cannot_drop(Path, Loan, Point)
 *    loan_issued_at(_, Loan, Point),
 *    loan_conflicts_with(Loan, Path)
 *
 * // Recursive Step
 * cannot_drop(Path, Loan, Point1)
 *    cannot_drop(Path, Loan, Point0),
 *    loan_live_at(Loan, Point),
 *    cfg_edge(Point0, Point1).
 *
 */

impl Default for Output<RustcFacts> {
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

impl Output<RustcFacts> {
  pub fn compute<'tcx>(
    tcx: TyCtxt<'tcx>,
    body_id: BodyId,
    body_with_facts: &BodyWithBorrowckFacts<'tcx>,
    borrow_set: &BorrowSet<'tcx>,
    move_data: &MoveData,
    input_facts: &AllFacts<RustcFacts>,
    pe_output: &PEOutput<RustcFacts>,
  ) -> Self {
    let mut output = Output::default();

    type Path = <RustcFacts as FactTypes>::Path;
    type Point = <RustcFacts as FactTypes>::Point;
    type Loan = <RustcFacts as FactTypes>::Loan;

    // auxilary help and facts

    let loan_to_borrow = |l: Loan| &borrow_set[l];

    let place_to_path = |p: Place| match move_data.rev_lookup.find(p.as_ref()) {
      LookupResult::Exact(path) => path,
      LookupResult::Parent(Some(path)) => path,
      r => {
        log::debug!("place to path failed with {:?}", p);
        log::debug!("lookupres {:?}", r);
        todo!()
      }
    };

    let is_mutable_borrow = |brw: &BorrowData<'tcx>| {
      matches!(brw.kind, BorrowKind::Mut {
        allow_two_phase_borrow: _,
      })
    };

    let is_loan_mutable = |loan| is_mutable_borrow(loan_to_borrow(loan));

    let def_id = tcx.hir().body_owner_def_id(body_id);
    let body = &body_with_facts.body;
    let places = body
      .local_decls
      .indices()
      .flat_map(|local| {
        Place::from_local(local, tcx).interior_paths(
          tcx,
          body,
          def_id.to_def_id(),
        )
      })
      .collect::<Vec<Place>>();

    let is_declared_readonly = |place: Place| {
      body.local_decls[place.local].mutability != Mutability::Mut
    };

    let is_never_write = |place: Place<'tcx>| {
      (!place.is_indirect() && is_declared_readonly(place)) || {
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

    let is_never_drop = |place: Place| place.is_indirect();

    // .decl loan_conflicts_with(Loan, Path)
    let loan_conflicts_with =
      Relation::from_iter(input_facts.loan_issued_at.iter().flat_map(
        |&(_origin, loan, _point)| {
          let borrow = loan_to_borrow(loan);
          places.iter().filter_map(move |place| {
            super::places_conflict::borrow_conflicts_with_place(
              tcx,
              body,
              borrow.borrowed_place,
              borrow.kind,
              place.as_ref(),
              AccessDepth::Deep,
              PlaceConflictBias::Overlap,
            )
            .then_some(((loan, place_to_path(*place)), ()))
          })
        },
      ));

    let cfg_edge: Relation<(Point, Point)> =
      input_facts.cfg_edge.clone().into();

    let loan_live_at: Relation<(Loan, Point)> =
      Relation::from_iter(pe_output.loan_live_at.iter().flat_map(
        |(&point, values)| values.iter().map(move |&loan| (loan, point)),
      ));

    // We don't need to use these relations in the rules so write
    // them directly to the output.

    output.never_write = places
      .iter()
      .filter_map(|&place| {
        is_never_write(place).then_some(place_to_path(place))
      })
      .collect();

    output.never_drop = places
      .iter()
      .filter_map(|&place| is_never_drop(place).then_some(place_to_path(place)))
      .collect();

    // variables

    let mut iteration = Iteration::new();

    let cannot_read = iteration.variable::<(Path, Loan, Point)>("cannot_read");
    let cannot_write =
      iteration.variable::<(Path, Loan, Point)>("cannot_write");
    let cannot_drop = iteration.variable::<(Path, Loan, Point)>("cannot_drop");

    // variable base cases

    cannot_read.extend(input_facts.loan_issued_at.iter().flat_map(
      |&(_, loan, point)| {
        loan_conflicts_with
          .iter()
          .filter_map(move |&((ln, path), _)| {
            (loan == ln && is_loan_mutable(loan)).then_some((path, loan, point))
          })
      },
    ));

    cannot_write.extend(input_facts.loan_issued_at.iter().flat_map(
      |&(_, loan, point)| {
        loan_conflicts_with
          .iter()
          .filter_map(move |&((ln, path), _)| {
            (loan == ln).then_some((path, loan, point))
          })
      },
    ));

    cannot_drop.extend(input_facts.loan_issued_at.iter().flat_map(
      |&(_, loan, point)| {
        loan_conflicts_with
          .iter()
          .filter_map(move |&((ln, path), _)| {
            (loan == ln).then_some((path, loan, point))
          })
      },
    ));

    // apply iteration rules

    let mut iterations = 0;

    while iteration.changed() {
      iterations += 1;

      cannot_read.from_leapjoin(
        &cannot_read,
        (
          cfg_edge.extend_with(|&(_path, _loan, point1)| point1),
          loan_live_at.extend_with(|&(_path, loan, _point1)| loan),
        ),
        |&(path, loan, _point1), &point2| (path, loan, point2),
      );

      cannot_write.from_leapjoin(
        &cannot_write,
        (
          cfg_edge.extend_with(|&(_path, _loan, point1)| point1),
          loan_live_at.extend_with(|&(_path, loan, _point1)| loan),
        ),
        |&(path, loan, _point1), &point2| (path, loan, point2),
      );

      cannot_drop.from_leapjoin(
        &cannot_drop,
        (
          cfg_edge.extend_with(|&(_path, _loan, point1)| point1),
          loan_live_at.extend_with(|&(_path, loan, _point1)| loan),
        ),
        |&(path, loan, _point1), &point2| (path, loan, point2),
      );
    }

    let cannot_read = cannot_read.complete();
    let cannot_write = cannot_write.complete();
    let cannot_drop = cannot_drop.complete();

    for &(path, loan, point) in cannot_read.iter() {
      output
        .cannot_read
        .entry(point)
        .or_default()
        .insert(path, loan);
    }

    for &(path, loan, point) in cannot_write.iter() {
      output
        .cannot_write
        .entry(point)
        .or_default()
        .insert(path, loan);
    }

    for &(path, loan, point) in cannot_drop.iter() {
      output
        .cannot_drop
        .entry(point)
        .or_default()
        .insert(path, loan);
    }

    log::debug!(
      "#iterations {} #cannot_read {} #cannot_write {} #cannot_drop {}",
      iterations,
      cannot_read.len(),
      cannot_write.len(),
      cannot_drop.len()
    );

    output
  }
}

// ----------
// Main entry

pub fn compute<'a, 'tcx>(
  tcx: TyCtxt<'tcx>,
  body_id: BodyId,
  body_with_facts: &'a BodyWithBorrowckFacts<'tcx>,
) -> Output<RustcFacts> {
  let timer = Instant::now();

  let def_id = tcx.hir().body_owner_def_id(body_id);
  let owner = tcx.hir().body_owner(body_id);
  let Some(name) = tcx.hir().opt_name(owner) else { panic!() };

  let body = &body_with_facts.body;

  log::debug!("computing body permissions ...");

  let input_facts = &body_with_facts.input_facts;

  let polonius_output = &PEOutput::compute(input_facts, Algorithm::Naive, true);

  let locals_are_invalidated_at_exit =
    tcx.hir().body_owner_kind(def_id).is_fn_or_closure();
  let (_, move_data) =
    MoveData::gather_moves(body, tcx, tcx.param_env(def_id)).unwrap();
  let move_data = &move_data;
  let borrow_set =
    &BorrowSet::build(tcx, body, locals_are_invalidated_at_exit, move_data);

  let output = Output::compute(
    tcx,
    body_id,
    body_with_facts,
    borrow_set,
    move_data,
    input_facts,
    polonius_output,
  );

  log::info!("permissions analysis duration: {:?}", timer.elapsed());

  output
}
