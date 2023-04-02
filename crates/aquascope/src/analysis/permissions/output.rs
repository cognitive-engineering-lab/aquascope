//! Core datalog analysis for Aquascope.
//!
//! The permissions analysis directly translates output facts
//! from [polonius](https://github.com/rust-lang/polonius) into the
//! core set of Aquascope [`Output`] facts.
//!
//! For information about working with these facts see [`PermissionsCtxt`].

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
  flow,
  places_conflict::{self, AccessDepth, PlaceConflictBias},
  AquascopeFacts, Loan, Move, Path, Point, ENABLE_FLOW_DEFAULT,
  ENABLE_FLOW_PERMISSIONS,
};
use crate::mir::utils::BodyExt;

/// Aquascope permissions facts output.
#[derive(Debug)]
pub struct Output<T>
where
  T: FactTypes + std::fmt::Debug,
{
  /// Paths which are *declared* as immutable.
  ///
  /// ```text
  /// .decl never_write(Path)
  ///
  /// never_write(Path) :-
  ///    is_direct(Path),
  ///    declared_readonly(Path).
  ///
  /// never_write(Path) :-
  ///    !is_direct(Path),
  ///    prefix_of(Prefix, Path),
  ///    is_immut_ref(Prefix).
  /// ```
  ///
  pub(crate) never_write: HashSet<T::Path>,

  /// A [`Path`] whose read permissions are refined at [`Point`] due to an active [`Loan`].
  ///
  /// ```text
  /// .decl cannot_read(Path:path, Point:point)
  ///
  /// cannot_read(Path, Loan, Point) :-
  ///    path_maybe_uninitialized_on_entry(Path, Point).
  ///
  /// cannot_read(Path, Loan, Point) :-
  ///    loan_conflicts_with(Loan, Path),
  ///    loan_live_at(Loan, Point),
  ///    loan_mutable(Loan).
  /// ```
  ///
  pub(crate) loan_read_refined: HashMap<T::Point, HashMap<T::Path, T::Loan>>,

  /// A [`Path`] whose write permissions are refined at [`Point`] due to an active [`Loan`].
  ///
  /// ```text
  /// .decl cannot_write(Path:path, Point:point)
  ///
  /// cannot_write(Path, Loan, Point) :-
  ///    path_maybe_uninitialized_on_entry(Path, Point).
  ///
  /// cannot_write(Path, Loan, Point) :-
  ///    loan_conflicts_with(Loan, Path),
  ///    loan_live_at(Loan, Point).
  /// ```
  ///
  pub(crate) loan_write_refined: HashMap<T::Point, HashMap<T::Path, T::Loan>>,

  /// A [`Path`] whose drop permissions are refined at [`Point`] due to an active [`Loan`].
  ///
  /// ```text
  /// .decl cannot_drop(Path, Loan, Point)
  ///
  /// cannot_drop(Path, Loan, Point) :-
  ///    path_maybe_uninitialized_on_entry(Path, Point).
  ///
  /// cannot_read(PathParent, Loan, Point) :-
  ///    ancestor_path(PathParent, PathChild),
  ///    path_maybe_uninitialized_on_entry(PathChild, Point).
  ///
  /// cannot_drop(Path, Loan, Point) :-
  ///    loan_conflicts_with(Loan, Path),
  ///    loan_live_at(Loan, Point).
  /// ```
  ///
  pub(crate) loan_drop_refined: HashMap<T::Point, HashMap<T::Path, T::Loan>>,

  /// A [`Path`] that may be uninitialized on [`Point`] entry.
  ///
  /// Uninitialized can mean one of three things:
  /// - The path was never initialized.
  /// - The path has been moved.
  /// - The path is partially moved (for ADTs).
  ///
  /// ```text
  /// .decl path_maybe_uninitialized_on_entry(Point, Path)
  ///
  /// path_maybe_uninitialized_on_entry(Point1, Path) :-
  ///    path_maybe_uninitialized_on_exit(Point0, Path)
  ///    cfg_edge(Point0, Point1)
  ///
  /// path_maybe_uninitialized_on_entry(Point, PathParent) :-
  ///    child_path(PathChild, PathParent),
  ///    path_maybe_uninitialized_on_entry(PathChild, Point).
  /// ```
  ///
  pub(crate) path_maybe_uninitialized_on_entry:
    HashMap<T::Point, HashSet<T::Path>>,

  /// A [`Path`] that is moved on [`Point`] entry.
  ///
  /// move_refined(Path, Move, Point) :-
  ///   move_live_at(Move, Point),
  ///   move_conflicts_with(Move, Path).
  pub(crate) move_refined: HashMap<T::Point, HashMap<T::Path, Move>>,

  pub(crate) move_live_at: HashMap<T::Point, Vec<Move>>,
}

impl Default for Output<AquascopeFacts> {
  fn default() -> Self {
    Output {
      never_write: HashSet::default(),
      loan_read_refined: HashMap::default(),
      loan_write_refined: HashMap::default(),
      loan_drop_refined: HashMap::default(),
      path_maybe_uninitialized_on_entry: HashMap::default(),
      move_refined: HashMap::default(),
      move_live_at: HashMap::default(),
    }
  }
}

/// Populate the [`Output`] facts in the current [`PermissionsCtxt`].
// TODO(gavinleroy) lots of data is kept around below for clarity,
// but this could definitely be optimized.
#[allow(clippy::similar_names)]
pub fn derive_permission_facts(ctxt: &mut PermissionsCtxt) {
  let def_id = ctxt.tcx.hir().body_owner_def_id(ctxt.body_id);
  let body = &ctxt.body_with_facts.body;
  let tcx = ctxt.tcx;

  // We consider all place that are either:
  // 1. Internal to a local declaration.
  // 2. A path considered moveable by rustc.
  let places = body
    .all_places(tcx, def_id.to_def_id())
    .chain(ctxt.move_data.move_paths.iter().map(|v| v.place))
    .collect::<Vec<_>>();

  // Normalize all places and get the associated AquascopeFacts::Point,
  // any MIR place that is not initialized here could cause a panic later
  // in the pipeline if a transformation (path -> [point|moveable_path,...])
  // happens.
  let paths = places
    .iter()
    .map(|place| ctxt.new_path(*place))
    .collect::<Vec<_>>();

  let cfg_edge: Relation<(Point, Point)> = Relation::from_iter(
    ctxt
      .polonius_input_facts
      .cfg_edge
      .iter()
      .map(|&(p1, p2)| (p1, p2)),
  );

  let path_maybe_uninitialized_on_exit: Relation<(Point, Path)> =
    Relation::from_iter(
      ctxt
        .polonius_output
        .path_maybe_uninitialized_on_exit
        .iter()
        .flat_map(|(point, paths)| {
          paths.iter().map(|path| {
            let path = ctxt.moveable_path_to_path(*path);
            (*point, path)
          })
        }),
    );

  // move_conflics_with(Move, Path) :-
  //   moved_out(Move, Path)
  //
  // move_conflicts_with(Move, PathChild) :-
  //   move_conflicts_with(Move, PathParent),
  //   interior_path(PathParent, PathChild)
  //
  // move_conflicts_with(Move, PathParent) :-
  //   move_conflicts_with(Move, PathChild),
  //   ancestor_path(PathParent, PathChild)
  let ictxt = &*ctxt;
  let move_conflicts_with: Relation<(Move, Path)> =
    Relation::from_iter(ictxt.move_data.moves.iter_enumerated().flat_map(
      |(move_idx, move_out)| {
        let path = ctxt.moveable_path_to_path(move_out.path);
        let place = ctxt.path_to_place(path);
        let move_path = &ictxt.move_data.move_paths[move_out.path];
        // Moving a path moves all of its interior paths.
        place
          .interior_paths(tcx, body, def_id.to_def_id())
          .into_iter()
          .map(move |place| {
            let path = ictxt.place_to_path(&place);
            (move_idx, path)
          })
          // Moving a path makes its parent *moveable* paths partially initialized.
          .chain(move_path.parents(&ictxt.move_data.move_paths).map(
            move |(_, move_parent)| {
              let path = ictxt.place_to_path(&move_parent.place);
              (move_idx, path)
            },
          ))
      },
    ));

  // See: https://github.com/rust-lang/polonius/blob/master/polonius-engine/src/facts.rs#L58
  //
  // Stores (Child, Parent) relationships
  let child_path: Relation<(Path, Path)> =
    Relation::from_iter(ctxt.polonius_input_facts.child_path.iter().map(
      |&(child, parent)| {
        (
          ctxt.moveable_path_to_path(child),
          ctxt.moveable_path_to_path(parent),
        )
      },
    ));

  // We only need iteration for crawling across child paths
  // Paths that are partially moved can not have R/O permissions,
  // thus, if a child path is uninitialized (moved or non-initialized),
  // then the parent must also be uninitialized.

  let mut iteration = Iteration::new();

  let path_maybe_uninitialized_on_entry =
    iteration.variable::<(Path, Point)>("path_maybe_uninitialized_on_entry");
  let move_live_at = iteration.variable::<(Move, Point)>("move_live_at");

  // move_live_at(Move, Point) :-
  //   move_out(Move, Point).
  move_live_at.extend(ctxt.move_data.moves.iter_enumerated().map(
    |(move_idx, &move_out)| {
      let point = ctxt.location_to_point(move_out.source);
      (move_idx, point)
    },
  ));

  // path_maybe_uninitialized_on_entry(Point1, Path) :-
  //    path_maybe_uninitialized_on_exit(Point0, Path)
  //    cfg_edge(Point0, Point1)
  path_maybe_uninitialized_on_entry.insert(Relation::from_join(
    &path_maybe_uninitialized_on_exit,
    &cfg_edge,
    |&_point1, &path, &point2| (path, point2),
  ));

  // FIXME: a lot of these do repeat work. You could rely on datafrog more.
  while iteration.changed() {
    // TODO: write datalog rule
    move_live_at.from_leapjoin(
      &move_live_at,
      (
        cfg_edge.extend_with(|&(_path, point1)| point1),
        ValueFilter::from(|&(movep, _point1), &point2| {
          let mpath = ctxt.move_to_moveable_path(movep);
          !ctxt.polonius_input_facts.path_assigned_at_base.iter().any(
            |&(assigned_to, p)| {
              p == point2 && {
                let pp = ctxt.moveable_path_to_path(assigned_to);
                let place1 = ctxt.path_to_place(pp);
                let pp = ctxt.moveable_path_to_path(mpath);
                let place2 = ctxt.path_to_place(pp);
                places_conflict::places_conflict(
                  tcx,
                  body,
                  place1,
                  place2,
                  PlaceConflictBias::Overlap,
                )
              }
            },
          )
        }),
      ),
      |&(path, _point1), &point2| (path, point2),
    );

    // path_maybe_uninitialized_on_entry(PathParent, Point) :-
    //    ancestor_path(PathParent, PathChild),
    //    path_maybe_uninitialized_on_entry(PathChild, Point).
    path_maybe_uninitialized_on_entry.from_join(
      &path_maybe_uninitialized_on_entry,
      &child_path,
      |&_child, &point, &parent| (parent, point),
    );
  }

  let path_maybe_uninitialized_on_entry =
    path_maybe_uninitialized_on_entry.complete();
  let move_live_at = move_live_at.complete();

  // We need to shift the move liveness by one in the MIR.
  let move_live_at: Relation<(Move, Point)> = Relation::from_leapjoin(
    &move_live_at,
    cfg_edge.extend_with(|&(_movep, point1)| point1),
    |&(movep, _point1), &point2| (movep, point2),
  );

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
          log::debug!(
            "checking to see if prefix {:?}: {:?} is mutable",
            prefix,
            ty
          );

          if let Some(mutability) = ty.ref_mutability() {
            return mutability == Mutability::Not;
          }

          // If the type is not a ref, it could be an array or box. In these
          // cases we defer back to the mutability of the binding.
          let local_place = Place::from_local(prefix.local, tcx);
          ctxt.is_declared_readonly(&local_place)
        })
    }
  };

  // .decl loan_conflicts_with(Loan, Path)
  let loan_conflicts_with: Relation<(Loan, Path)> = Relation::from_iter(
    ctxt.polonius_input_facts.loan_issued_at.iter().flat_map(
      |(_origin, loan, _point)| {
        let borrow = ctxt.loan_to_borrow(*loan);
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

  let loan_read_refined: Relation<(Path, Loan, Point)> =
    Relation::from_leapjoin(
      &loan_conflicts_with,
      (
        loan_live_at.extend_with(|&(loan, _path)| loan),
        ValueFilter::from(|&(loan, _path), _point| ctxt.is_mutable_loan(loan)),
      ),
      |&(loan, path), &point| (path, loan, point),
    );

  let loan_write_refined: Relation<(Path, Loan, Point)> = Relation::from_join(
    &loan_conflicts_with,
    &loan_live_at,
    |&loan, &path, &point| (path, loan, point),
  );

  let loan_drop_refined: Relation<(Path, Loan, Point)> = Relation::from_join(
    &loan_conflicts_with,
    &loan_live_at,
    |&loan, &path, &point| (path, loan, point),
  );

  let move_refined: Relation<(Path, Move, Point)> = Relation::from_join(
    &move_conflicts_with,
    &move_live_at,
    |&movep, &path, &point| (path, movep, point),
  );

  let never_write = paths
    .iter()
    .filter_map(|path| is_never_write(*path).then_some(*path))
    .collect::<HashSet<_>>();

  ctxt.permissions_output.never_write = never_write;

  for &(path, point) in path_maybe_uninitialized_on_entry.iter() {
    ctxt
      .permissions_output
      .path_maybe_uninitialized_on_entry
      .entry(point)
      .or_default()
      .insert(path);
  }

  for &(movep, point) in move_live_at.iter() {
    ctxt
      .permissions_output
      .move_live_at
      .entry(point)
      .or_default()
      .push(movep);
  }

  macro_rules! insert_facts {
    ($input:expr, $field:expr) => {
      for &(path, loan, point) in $input.iter() {
        $field.entry(point).or_default().insert(path, loan);
      }
    };
  }

  insert_facts!(loan_read_refined, ctxt.permissions_output.loan_read_refined);
  insert_facts!(
    loan_write_refined,
    ctxt.permissions_output.loan_write_refined
  );
  insert_facts!(loan_drop_refined, ctxt.permissions_output.loan_drop_refined);
  insert_facts!(move_refined, ctxt.permissions_output.move_refined);

  log::debug!(
    "#loan_R_refined {} #loan_W_refined {} #loan_D_refined {} #move_refined{}",
    loan_read_refined.len(),
    loan_write_refined.len(),
    loan_drop_refined.len(),
    move_refined.len()
  );
}

// ----------
// Main entry

/// Compute the [`PermissionsCtxt`] for a given body.
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
  let name = match tcx.hir().opt_name(owner) {
    Some(name) => name.to_ident_string(),
    None => "<anonymous>".to_owned(),
  };
  log::debug!("computing body permissions {:?}", name);

  let polonius_input_facts = &body_with_facts.input_facts;
  let polonius_output =
    PEOutput::compute(polonius_input_facts, Algorithm::Naive, true);

  let locals_are_invalidated_at_exit =
    tcx.hir().body_owner_kind(def_id).is_fn_or_closure();
  let move_data = match MoveData::gather_moves(body, tcx, tcx.param_env(def_id))
  {
    Ok((_, move_data)) => move_data,
    Err((move_data, _illegal_moves)) => {
      log::debug!("illegal moves found {_illegal_moves:?}");
      move_data
    }
  };
  let borrow_set =
    BorrowSet::build(tcx, body, locals_are_invalidated_at_exit, &move_data);
  let def_id = def_id.to_def_id();
  let param_env = tcx.param_env_reveal_all_normalized(def_id);

  // This should always be true for the current analysis of aquascope
  let locals_are_invalidated_at_exit = def_id.as_local().map_or(false, |did| {
    tcx.hir().body_owner_kind(did).is_fn_or_closure()
  });

  let mut ctxt = PermissionsCtxt {
    tcx,
    permissions_output: Output::default(),
    polonius_input_facts,
    polonius_output,
    body_id,
    def_id,
    body_with_facts,
    borrow_set,
    move_data,
    locals_are_invalidated_at_exit,
    param_env,
    loan_regions: None,
    place_data: IndexVec::new(),
    rev_lookup: HashMap::default(),
    region_flows: None,
  };

  derive_permission_facts(&mut ctxt);

  ctxt.construct_loan_regions();

  log::info!(
    "permissions analysis for {:?} took: {:?}",
    name,
    timer.elapsed()
  );

  if ENABLE_FLOW_PERMISSIONS
    .copied()
    .unwrap_or(ENABLE_FLOW_DEFAULT)
  {
    flow::compute_flows(&mut ctxt);
  }

  ctxt
}
