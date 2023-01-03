use flowistry::mir::utils::PlaceExt;
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
  mir::{BorrowKind, Local, Location, Place, TerminatorKind},
  ty::{ParamEnv, Ty, TyCtxt},
};
use rustc_mir_dataflow::move_paths::MoveData;
use rustc_span::Span;
use rustc_trait_selection::infer::InferCtxtExt;

use crate::analysis::permissions::{
  AquascopeFacts, Loan, LoanKey, Output, Path, Permissions, PermissionsData,
  PermissionsDomain, Point, Variable,
};

type MoveablePath = <RustcFacts as FactTypes>::Path;

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
}

impl<'a, 'tcx> PermissionsCtxt<'a, 'tcx> {
  pub fn new_path(&mut self, place: Place<'tcx>) -> Path {
    let place = place.normalize(self.tcx, self.def_id);
    let new_path = self.place_data.push(place);
    let local = place.local;
    self.rev_lookup.entry(local).or_default().push(new_path);
    new_path
  }

  // Conversion helpers

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

  pub fn point_to_location(&self, p: Point) -> Location {
    match self.body_with_facts.location_table.to_location(p) {
      RichLocation::Start(l) => l,
      RichLocation::Mid(l) => l,
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
    self.body_with_facts.body.source_info(l).span
  }

  // Predicates

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

  pub fn is_path_drop_enabled(&self, path: Path) -> bool {
    // TODO: experimental, I(gavin) want to get rid of the 'is_never_drop'
    // relation in the permissions engine, because it uses memory that
    // doesn't need to be stored.
    !self.path_to_place(path).is_indirect()
    // !self.permissions_output.never_drop.contains(&path)
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
      path_moved: false,
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

  pub fn permissions_data_at_point(
    &self,
    path: Path,
    point: Point,
  ) -> PermissionsData {
    let empty_hash = &HashMap::default();
    let empty_set = &HashSet::default();
    let is_live = self.is_path_live_at(path, point);
    let pms = &self.permissions_output;
    let path = &path;
    let point = &point;

    // Get point-specific information
    let path_moved_at = pms
      .path_maybe_uninitialized_on_entry
      .get(point)
      .unwrap_or(empty_set);
    let cannot_read = pms.cannot_read.get(point).unwrap_or(empty_hash);
    let cannot_write = pms.cannot_write.get(point).unwrap_or(empty_hash);
    let cannot_drop = pms.cannot_drop.get(point).unwrap_or(empty_hash);

    let type_writeable = self.is_path_write_enabled(*path);
    let type_droppable = self.is_path_drop_enabled(*path);
    let type_copyable = self.is_path_copyable(*path);

    let path_moved = path_moved_at.contains(path);

    let loan_read_refined: Option<LoanKey> =
      cannot_read.get(path).map(Into::<LoanKey>::into);
    let loan_write_refined: Option<LoanKey> =
      cannot_write.get(path).map(Into::<LoanKey>::into);
    let loan_drop_refined: Option<LoanKey> =
      cannot_drop.get(path).map(Into::<LoanKey>::into);

    let permissions = if !is_live {
      Permissions::bottom()
    } else {
      Permissions {
        read: !path_moved && loan_read_refined.is_none(),
        write: type_writeable && !path_moved && loan_write_refined.is_none(),
        drop: type_copyable
          || (type_droppable && !path_moved && loan_drop_refined.is_none()),
      }
    };

    PermissionsData {
      is_live,
      type_droppable,
      type_writeable,
      type_copyable,
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
    body
      .local_decls
      .indices()
      .flat_map(|local| {
        Place::from_local(local, tcx).interior_paths(tcx, body, def_id)
      })
      .collect::<HashSet<_>>()
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

  // HACK: this method should be called with extreme caution, please prefer `permissions_domain_at_point`.
  //
  // This method currently exists to provide a quick fix for the following problem. At a given MIR Location,
  // a statement could cause some permissions change. For example:
  //
  // ```
  // let s = String::from("hi!");
  // ```
  //
  // The HIR node for this Local will have a few MIR locations associated with it, simplified these are:
  //
  // ```
  // StorageLive(s);
  // FakeRead(ForLet(None), s);
  // ```
  //
  // When finding the permission steps throughout a program, we need to talk about the before, and after
  // effects of a node. For the above Local assignment, we want to reason about the before and after state,
  // which includes any trailing affect the assignment might have.
  //
  // TODO: a much better way to handle this is to include a notion of Before / After for our permissions
  // analysis. This would give us a more principled way to look at it rather than manipulating the
  // location.
  pub fn permissions_domain_after_point_effect(
    &self,
    point: Point,
  ) -> Option<PermissionsDomain<'tcx>> {
    let location = self.point_to_location(point);
    let body = &self.body_with_facts.body;
    let block_data = &body.basic_blocks[location.block];
    if block_data.statements.len() == location.statement_index {
      let terminator = block_data.terminator();
      match terminator.kind {
        TerminatorKind::Goto { target } => {
          let loc = Location {
            block: target,
            statement_index: 0,
          };
          let point = self.location_to_point(loc);
          Some(self.permissions_domain_at_point(point))
        }
        _ => None,
      }
    } else {
      let next_location = location.successor_within_block();
      let point = self.location_to_point(next_location);
      Some(self.permissions_domain_at_point(point))
    }
  }

  /// Compute the beginning and end of the live loan region
  /// using source code location for comparison. We want to get
  /// rid of this method as it is more of a HACK.
  pub fn construct_loan_regions(&mut self) {
    if self.loan_regions.is_some() {
      // disallow reconstrucution of loan regions.
      return;
    }

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
