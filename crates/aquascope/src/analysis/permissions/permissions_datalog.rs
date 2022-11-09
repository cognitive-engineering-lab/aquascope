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
    AquascopeFacts, Loan, Path, Permissions, Point,
};

// FIXME the HashMap should map to multiple loans, because at a
// given point a path could be refined my multiple loans even
// if we only care about a single (more recent).
pub struct Output<T: FactTypes> {
    pub cannot_read: HashMap<T::Point, HashMap<T::Path, T::Loan>>,
    pub cannot_write: HashMap<T::Point, HashMap<T::Path, T::Loan>>,
    pub cannot_drop: HashMap<T::Point, HashMap<T::Path, T::Loan>>,
    pub path_moved_at: HashMap<T::Point, HashSet<T::Path>>,
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
 * XXX: new to handle moves. If this indeed is what we want,
 * it would be better to compute `loan_live_at` ouselves and avoid
 * running polonius altogether.
 *
 * .decl path_moved_at(Path, Point)
 *
 * path_moved_at(Path, Point) :-
 *    path_moved_at_base(Path, Point)
 *
 * path_moved_at(Path, Point1) :-
 *    path_moved_at(Path, Point0),
 *    cfg_edge(Point0, Point1),
 *    !path_assigned_at_base(Path, Point1).
 *
 ****
 *
 * .decl cannot_read(Path:path, Point:point)
 *
 * cannot_read(Path, Loan, Point) :-
 *    path_moved_at(Path, Point);
 *    loan_conflicts_with(Loan, Path),
 *    loan_live_at(Loan, Point),
 *    loan_mutable(Loan).
 *
 ****
 *
 * .decl cannot_write(Path:path, Point:point)
 *
 * cannot_write(Path, Loan, Point) :-
 *    path_moved_at(Path, Point);
 *    loan_conflicts_with(Loan, Path),
 *    loan_live_at(Loan, Point).
 *
 ****
 *
 * .decl cannot_drop(Path, Loan, Point)
 *
 * cannot_drop(Path, Loan, Point)
 *    path_moved_at(Path, Point);
 *    loan_conflicts_with(Loan, Path),
 *    loan_live_at(Loan, Point).
 *
 */

impl Default for Output<AquascopeFacts> {
    fn default() -> Self {
        Output {
            cannot_read: HashMap::default(),
            cannot_write: HashMap::default(),
            cannot_drop: HashMap::default(),
            path_moved_at: HashMap::default(),
            never_write: HashSet::default(),
            never_drop: HashSet::default(),
        }
    }
}

impl Output<AquascopeFacts> {
    // XXX: it's really inefficient to call this per point
    pub fn permissions_at_point(&self, path: Path, point: Point) -> Permissions {
        let path = &path;
        let point = &point;
        let empty_hash = &HashMap::default();
        let empty_set = &HashSet::default();

        // Get point-specific information
        let path_moved_at = self.path_moved_at.get(point).unwrap_or(empty_set);
        let cannot_read = self.cannot_read.get(point).unwrap_or(empty_hash);
        let cannot_write = self.cannot_write.get(point).unwrap_or(empty_hash);
        let cannot_drop = self.cannot_drop.get(point).unwrap_or(empty_hash);

        let read = !(cannot_read.contains_key(path) || path_moved_at.contains(path));
        let write = !(self.never_write.contains(path)
            || cannot_write.contains_key(path)
            || path_moved_at.contains(path));
        let drop = !(self.never_drop.contains(path)
            || cannot_drop.contains_key(path)
            || path_moved_at.contains(path));

        Permissions { read, write, drop }
    }

    pub fn populate_ctxt<'a, 'tcx>(ctxt: &mut PermissionsCtxt<'a, 'tcx>) {
        // auxilary help and facts

        let def_id = ctxt.tcx.hir().body_owner_def_id(ctxt.body_id);
        let body = &ctxt.body_with_facts.body;
        let tcx = ctxt.tcx;
        let places: Vec<Place<'tcx>> = body
            .local_decls
            .indices()
            .flat_map(|local| {
                Place::from_local(local, tcx).interior_paths(tcx, body, def_id.to_def_id())
            })
            .collect();

        let paths: Vec<Path> = places.iter().map(|place| ctxt.new_path(*place)).collect();

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
                            // TODO: raw pointers, assume that they are always mutable
                            None => false,
                        }
                    })
            }
        };

        let is_never_drop = |path: Path| ctxt.path_to_place(path).is_indirect();

        // .decl loan_conflicts_with(Loan, Path)
        let loan_conflicts_with: Relation<(Loan, Path)> =
            Relation::from_iter(ctxt.polonius_input_facts.loan_issued_at.iter().flat_map(
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
            ));

        let loan_live_at: Relation<(Loan, Point)> = Relation::from_iter(
            ctxt.polonius_output
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

        // XXX: new dynamic things to handle (basic) moves

        let mut iteration = Iteration::new();

        let cfg_edge: Relation<(Point, Point)> = Relation::from_iter(
            ctxt.polonius_input_facts
                .cfg_edge
                .iter()
                .map(|&(p1, p2)| (p1, p2)),
        );

        let path_assigned_at_base = Relation::from_iter(
            ctxt.polonius_input_facts
                .path_assigned_at_base
                .iter()
                .map(|&(mp, point)| (ctxt.moveable_path_to_path(mp), point)),
        );

        let path_moved_at = iteration.variable::<(Path, Point)>("path_moved_at");

        // path_moved_at(Path, Point) :-
        //    path_moved_at_base(Path, Point)
        path_moved_at.extend(
            ctxt.polonius_input_facts
                .path_moved_at_base
                .iter()
                .map(|&(mp, point)| (ctxt.moveable_path_to_path(mp), point)),
        );

        let mut iterations = 0;

        while iteration.changed() {
            iterations += 1;

            // path_moved_at(Path, Point1) :-
            //    path_moved_at(Path, Point0),
            //    cfg_edge(Point0, Point1),
            //    !path_assigned_at_base(Path, Point1).
            path_moved_at.from_leapjoin(
                &path_moved_at,
                (
                    cfg_edge.extend_with(|&(_path, point1)| point1),
                    path_assigned_at_base.extend_anti(|&(path, _point1)| path),
                ),
                |&(path, point1), &point2| (path, point2),
            );
        }

        log::debug!("finished in {} iterations", iterations);

        let path_moved_at = path_moved_at.complete();

        for &(path, point) in path_moved_at.iter() {
            log::debug!("Path {:?} moved at {:?}", path, point);
            ctxt.permissions_output
                .path_moved_at
                .entry(point)
                .or_default()
                .insert(path);
        }

        for &(path, loan, point) in cannot_read.iter() {
            ctxt.permissions_output
                .cannot_read
                .entry(point)
                .or_default()
                .insert(path, loan);
        }

        for &(path, loan, point) in cannot_write.iter() {
            ctxt.permissions_output
                .cannot_write
                .entry(point)
                .or_default()
                .insert(path, loan);
        }

        for &(path, loan, point) in cannot_drop.iter() {
            ctxt.permissions_output
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
    let polonius_output = PEOutput::compute(polonius_input_facts, Algorithm::Naive, true);

    let locals_are_invalidated_at_exit = tcx.hir().body_owner_kind(def_id).is_fn_or_closure();
    let (_, move_data) = MoveData::gather_moves(body, tcx, tcx.param_env(def_id)).unwrap();
    let borrow_set = BorrowSet::build(tcx, body, locals_are_invalidated_at_exit, &move_data);

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
