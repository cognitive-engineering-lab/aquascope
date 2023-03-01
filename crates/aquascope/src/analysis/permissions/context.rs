use flowistry::mir::utils::{PlaceExt, SpanExt};
use polonius_engine::{AllFacts, FactTypes, Output as PEOutput};
use rustc_borrowck::{
  borrow_set::{BorrowData, BorrowSet},
  consumers::{BodyWithBorrowckFacts, RichLocation, RustcFacts},
};
use rustc_data_structures::fx::{FxHashMap as HashMap, FxHashSet as HashSet};
use rustc_hir::{def_id::DefId, BodyId, Mutability};
use rustc_index::vec::IndexVec;
use rustc_infer::infer::TyCtxtInferExt;
use rustc_middle::{
  mir::{BorrowKind, Local, Location, Place},
  ty::{self, ParamEnv, Ty, TyCtxt},
};
use rustc_mir_dataflow::move_paths::MoveData;
use rustc_span::Span;
use rustc_trait_selection::infer::InferCtxtExt;
use smallvec::{smallvec, SmallVec};

use crate::{
  analysis::permissions::{
    flow::RegionFlows, AquascopeFacts, Loan, LoanKey, Move, MoveKey, Output,
    Path, Permissions, PermissionsData, PermissionsDomain, Point, Variable,
  },
  mir::utils::BodyExt,
};

/// A path as defined in rustc.
type MoveablePath = <RustcFacts as FactTypes>::Path;

/// The central data structure for working with permissions.
///
/// It holds the derived permissions information
/// and anything necessary to analyze a Rust Body.
pub struct PermissionsCtxt<'a, 'tcx> {
  pub tcx: TyCtxt<'tcx>,
  pub polonius_input_facts: &'a AllFacts<RustcFacts>,
  pub polonius_output: PEOutput<RustcFacts>,
  pub permissions_output: Output<AquascopeFacts>,
  pub body_id: BodyId,
  pub def_id: DefId,
  pub body_with_facts: &'a BodyWithBorrowckFacts<'tcx>,
  pub borrow_set: BorrowSet<'tcx>,
  pub move_data: MoveData<'tcx>,
  pub loan_regions: Option<HashMap<Loan, (Point, Point)>>,
  pub(crate) param_env: ParamEnv<'tcx>,
  pub(crate) place_data: IndexVec<Path, Place<'tcx>>,
  pub(crate) rev_lookup: HashMap<Local, Vec<Path>>,
  pub(crate) region_flows: Option<RegionFlows>,
}

impl<'a, 'tcx> PermissionsCtxt<'a, 'tcx> {
  pub fn new_path(&mut self, place: Place<'tcx>) -> Path {
    let place = place.normalize(self.tcx, self.def_id);
    let new_path = self.place_data.push(place);
    let local = place.local;
    self.rev_lookup.entry(local).or_default().push(new_path);
    new_path
  }

  pub fn region_flows(&self) -> &RegionFlows {
    let Some(ref rf) = self.region_flows else {
      unreachable!("attempted to get region flows before they are computed.");
    };

    rf
  }

  // Conversion helpers

  /// Convert and normalize a rustc [`Place`] into a [`Path`] understood by Aquascope.
  pub fn place_to_path(&self, p: &Place<'tcx>) -> Path {
    let p = p.normalize(self.tcx, self.def_id);
    *self
      .rev_lookup
      .get(&p.local)
      .unwrap()
      .iter()
      .find(|path| self.path_to_place(**path) == p)
      .unwrap_or_else(|| {
        panic!(
          "Could not find path for place {p:?}\n Acceptable places are: {:#?}",
          self.place_data.iter().collect::<Vec<_>>()
        )
      })
  }

  pub fn path_to_place(&self, p: Path) -> Place<'tcx> {
    self.place_data[p]
  }

  pub fn location_to_point(&self, l: Location) -> Point {
    self.body_with_facts.location_table.start_index(l)
  }

  pub fn location_to_points(&self, l: Location) -> SmallVec<[Point; 2]> {
    smallvec![
      self.body_with_facts.location_table.start_index(l),
      self.body_with_facts.location_table.mid_index(l)
    ]
  }

  pub fn point_to_location(&self, p: Point) -> Location {
    match self.body_with_facts.location_table.to_location(p) {
      RichLocation::Start(l) | RichLocation::Mid(l) => l,
    }
  }

  pub fn path_to_moveable_path(&self, p: Path) -> MoveablePath {
    let place = self.path_to_place(p);
    self.move_data.rev_lookup.find_local(place.local)
  }

  pub fn moveable_path_to_path(&self, mp: MoveablePath) -> Path {
    self.place_to_path(&self.move_data.move_paths[mp].place)
  }

  pub fn path_to_variable(&self, path: Path) -> Variable {
    self.path_to_place(path).local
  }

  pub fn location_to_span(&self, l: Location) -> Span {
    let span = self.body_with_facts.body.source_info(l).span;
    span
      .as_local(self.body_with_facts.body.span)
      .unwrap_or(span)
  }

  // XXX: experimental predicates for Moves

  pub fn move_to_moveable_path(&self, mv: Move) -> MoveablePath {
    self.move_data.moves[mv].path
  }

  pub fn move_to_path(&self, mv: Move) -> Path {
    let mpath = self.move_to_moveable_path(mv);
    self.moveable_path_to_path(mpath)
  }

  pub fn loan_to_borrow(&self, l: Loan) -> &BorrowData<'tcx> {
    &self.borrow_set[l]
  }

  // Predicates

  pub fn is_location_operational(&self, loc: Location) -> bool {
    use either::Either::{Left, Right};
    use rustc_middle::mir::{Statement, StatementKind as SK, Terminator};

    let bb = loc.block;
    let body = &self.body_with_facts.body;
    let bbs = &body.basic_blocks;

    !bbs[bb].is_cleanup
      && !bbs[bb].is_empty_unreachable()
      && matches!(
        body.stmt_at(loc),
        Right(Terminator { .. })
          | Left(Statement {
            kind: SK::Assign(..),
            ..
          })
      )
  }

  pub fn is_point_operational(&self, point: Point) -> bool {
    let loc = self.point_to_location(point);
    self.is_location_operational(loc)
  }

  pub fn is_mutable_borrow(&self, brw: &BorrowData<'tcx>) -> bool {
    matches!(brw.kind, BorrowKind::Mut {
      allow_two_phase_borrow: _,
    })
  }

  pub fn is_mutable_loan(&self, loan: Loan) -> bool {
    self.is_mutable_borrow(&self.borrow_set[loan])
  }

  pub fn is_declared_readonly(&self, place: &Place<'tcx>) -> bool {
    self.body_with_facts.body.local_decls[place.local].mutability
      != Mutability::Mut
  }

  pub fn is_path_live_at(&self, path: Path, point: Point) -> bool {
    let var = self.path_to_variable(path);
    self
      .polonius_output
      .var_live_on_entry
      .get(&point)
      .map_or(false, |live_vars| live_vars.contains(&var))
  }

  pub fn is_path_copyable(&self, path: Path) -> bool {
    use rustc_hir::lang_items::LangItem;
    let body = &self.body_with_facts.body;
    let place = self.path_to_place(path);
    let ty = place.ty(&body.local_decls, self.tcx).ty;
    let copy_def_id = self.tcx.require_lang_item(LangItem::Copy, None);
    self.implements_trait(ty, copy_def_id)
  }

  pub fn is_path_write_enabled(&self, path: Path) -> bool {
    !self.permissions_output.never_write.contains(&path)
  }

  /// Does a Path's type allow it to be dropped?
  /// NOTE: the utility is the negation of the datalog rule.
  ///
  /// .decl never_drop(Path)
  ///
  /// never_drop(Path) :-
  ///    !is_direct(Path),
  ///    has_adt_prefix_with_dtor(Path).
  pub fn is_path_drop_enabled(&self, path: Path) -> bool {
    let place = self.path_to_place(path);
    let is_indirect = place.is_indirect();
    // Rust maintains invariant that all `Drop`
    // ADT's remain fully-initialized so that user-defined destructor
    // can safely read from all of the ADT's fields. If a prefix of this
    // path is such an ADT then it cannot be moved.
    //
    // See: https://github.com/rust-lang/rust/blob/master/compiler/rustc_mir_dataflow/src/move_paths/mod.rs#L359-L363
    let has_adt_prefix_with_dtor = || {
      place.projection.iter().enumerate().any(|(i, _)| {
        let proj_base = &place.projection[.. i];
        let body = &self.body_with_facts.body;
        let tcx = self.tcx;
        let place_ty = Place::ty_from(place.local, proj_base, body, tcx).ty;
        matches!(place_ty.kind(),
          ty::Adt(adt, _) if adt.has_dtor(tcx) && !adt.is_box())
      })
    };

    !is_indirect && !has_adt_prefix_with_dtor()
  }

  // Permission utilities

  fn implements_trait(&self, ty: Ty<'tcx>, trait_def_id: DefId) -> bool {
    use rustc_infer::traits::EvaluationResult;

    let infcx = self.tcx.infer_ctxt().build();
    let ty = self.tcx.erase_regions(ty);
    let result =
      infcx.type_implements_trait(trait_def_id, [ty], self.param_env);
    matches!(
      result,
      EvaluationResult::EvaluatedToOk
        | EvaluationResult::EvaluatedToOkModuloRegions
    )
  }

  pub fn max_permissions(&self, path: Path) -> Permissions {
    let write = self.is_path_write_enabled(path);
    let drop = self.is_path_drop_enabled(path);
    Permissions {
      write,
      // There is no way in the type system to have a non-readable type.
      read: true,
      drop,
    }
  }

  pub fn permissions_for_const_ty(&self, ty: Ty<'tcx>) -> PermissionsData {
    use rustc_hir::lang_items::LangItem;
    let copy_def_id = self.tcx.require_lang_item(LangItem::Copy, None);
    let type_copyable = self.implements_trait(ty, copy_def_id);

    PermissionsData {
      is_live: true,
      type_droppable: true,
      type_writeable: true,
      type_copyable,
      path_moved: None,
      path_uninitialized: false,
      loan_read_refined: None,
      loan_write_refined: None,
      loan_drop_refined: None,
      permissions: Permissions {
        read: true,
        write: true,
        drop: true,
      },
    }
  }

  #[allow(clippy::if_not_else)]
  // TODO: we can extend the PermissionsData type to incldue the move
  // regions, this way, we can show the move region for a given path
  // when it is missing. (exactly how we do the loans)
  pub fn permissions_data_at_point(
    &self,
    path: Path,
    point: Point,
  ) -> PermissionsData {
    let empty_hash_move = &HashMap::default();
    let empty_hash_loan = &HashMap::default();
    let empty_set = &HashSet::default();
    let is_live = self.is_path_live_at(path, point);
    let pms = &self.permissions_output;
    let path = &path;
    let point = &point;

    // Get point-specific information
    let path_uninitialized = pms
      .path_maybe_uninitialized_on_entry
      .get(point)
      .unwrap_or(empty_set);
    let path_moved = pms.move_refined.get(point).unwrap_or(empty_hash_move);
    let loan_read_refined =
      pms.loan_read_refined.get(point).unwrap_or(empty_hash_loan);
    let loan_write_refined =
      pms.loan_write_refined.get(point).unwrap_or(empty_hash_loan);
    let loan_drop_refined =
      pms.loan_drop_refined.get(point).unwrap_or(empty_hash_loan);

    let type_writeable = self.is_path_write_enabled(*path);
    let type_droppable = self.is_path_drop_enabled(*path);
    let type_copyable = self.is_path_copyable(*path);
    let path_uninitialized = path_uninitialized.contains(path);

    let path_moved: Option<MoveKey> =
      path_moved.get(path).map(Into::<MoveKey>::into);
    let loan_read_refined: Option<LoanKey> =
      loan_read_refined.get(path).map(Into::<LoanKey>::into);
    let loan_write_refined: Option<LoanKey> =
      loan_write_refined.get(path).map(Into::<LoanKey>::into);
    let loan_drop_refined: Option<LoanKey> =
      loan_drop_refined.get(path).map(Into::<LoanKey>::into);

    let mem_uninit = path_moved.is_some() || path_uninitialized;

    let permissions = if !is_live {
      Permissions::bottom()
    } else {
      // A path is readable IFF:
      // - it is not moved.
      // - there doesn't exist a read-refining loan at this point.
      let read = !mem_uninit && loan_read_refined.is_none();

      // A path is writeable IFF:
      // - the path's declared type allows for mutability.
      // - the path is readable (you can't write if you can't read)
      //   this implies that the path isn't moved.
      // - there doesn't exist a write-refining loan at this point.
      let write = type_writeable && read && loan_write_refined.is_none();

      // A path is droppable if it is doppable or copyable.
      //
      // * A path can be copied (implies drop) IFF:
      //   - the declared type is copyable.
      //   - the value can be read.
      //
      // * A path can be dropped (without copy) IFF:
      //   - the path's declared type is droppable.
      //   - it isn't moved.
      //   - no drop-refining loan exists at this point.
      let drop = (type_copyable && read)
        || (type_droppable && !mem_uninit && loan_drop_refined.is_none());

      Permissions { read, write, drop }
    };

    PermissionsData {
      type_droppable,
      type_writeable,
      type_copyable,
      is_live,
      path_uninitialized,
      path_moved,
      loan_read_refined,
      loan_write_refined,
      loan_drop_refined,
      permissions,
    }
  }

  pub fn domain_places(&self) -> HashSet<Place<'tcx>> {
    let def_id = self.def_id;
    let tcx = self.tcx;
    let body = &self.body_with_facts.body;
    body.all_places(tcx, def_id).collect::<HashSet<_>>()
  }

  pub fn permissions_domain_at_point(
    &self,
    point: Point,
  ) -> PermissionsDomain<'tcx> {
    // Use the base to get the full domain (e.g. possible places)
    let places = self.domain_places();
    places
      .into_iter()
      .fold(HashMap::default(), |mut acc, place| {
        let path = self.place_to_path(&place);
        let perms = self.permissions_data_at_point(path, point);
        acc.insert(place, perms);
        acc
      })
      .into()
  }

  /// Compute the beginning and end of the live loan region
  /// using source code location for comparison.
  ///
  /// NOTE: this method will soon be deprecated and should not be used.
  pub(crate) fn construct_loan_regions(&mut self) {
    trait PointExt {
      fn from_point(p: Point) -> Self;
      fn expand(&mut self, p: Point, ctxt: &PermissionsCtxt);
    }

    impl PointExt for (Point, Point) {
      fn from_point(p: Point) -> Self {
        (p, p)
      }

      fn expand(&mut self, p: Point, ctxt: &PermissionsCtxt) {
        let curr_earliest = ctxt.point_to_location(self.0);
        let curr_latest = ctxt.point_to_location(self.1);
        let candidate = ctxt.point_to_location(p);
        let curr_earliest = ctxt.location_to_span(curr_earliest);
        let curr_latest = ctxt.location_to_span(curr_latest);
        let candidate = ctxt.location_to_span(candidate);

        if candidate.lo() < curr_earliest.lo() {
          self.0 = p;
        }

        if curr_latest.hi() < candidate.hi() {
          self.1 = p;
        }
      }
    }

    // disallow reconstrucution of loan regions.
    if self.loan_regions.is_some() {
      return;
    }

    let mut hash: HashMap<Loan, (Point, Point)> = HashMap::default();

    self
      .polonius_output
      .loan_live_at
      .iter()
      .for_each(|(point, loans)| {
        loans.iter().for_each(|loan| {
          hash
            .entry(*loan)
            .or_insert_with(|| PointExt::from_point(*point))
            .expand(*point, self)
        })
      });

    self.loan_regions = Some(hash);
  }
}
