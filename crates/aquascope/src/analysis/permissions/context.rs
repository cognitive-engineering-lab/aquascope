use flowistry::mir::utils::PlaceExt;
use polonius_engine::{AllFacts, FactTypes, Output as PEOutput};
use rustc_borrowck::{
  borrow_set::{BorrowData, BorrowSet},
  consumers::{BodyWithBorrowckFacts, RichLocation, RustcFacts},
};
use rustc_data_structures::fx::{FxHashMap as HashMap, FxHashSet as HashSet};
use rustc_hir::{def_id::DefId, BodyId, Mutability};
use rustc_index::vec::IndexVec;
use rustc_middle::{
  mir::{BorrowKind, Local, Location, Place},
  ty::TyCtxt,
};
use rustc_mir_dataflow::move_paths::MoveData;
use rustc_span::Span;

use super::{AquascopeFacts, Loan, Output, Path, Permissions, Point, Variable};

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
  pub(crate) place_data: IndexVec<Path, Place<'tcx>>,
  pub(crate) rev_lookup: HashMap<Local, Vec<Path>>,
}

impl<'a, 'tcx> PermissionsCtxt<'a, 'tcx> {
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

  pub fn new_path(&mut self, place: Place<'tcx>) -> Path {
    let place = place.normalize(self.tcx, self.def_id);
    let new_path = self.place_data.push(place);
    let local = place.local;
    self.rev_lookup.entry(local).or_default().push(new_path);
    new_path
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

  pub fn is_mutable_borrow(&self, brw: &BorrowData<'tcx>) -> bool {
    matches!(brw.kind, BorrowKind::Mut {
      allow_two_phase_borrow: _,
    })
  }

  pub fn path_to_moveable_path(&self, p: Path) -> MoveablePath {
    let place = self.path_to_place(p);
    self.move_data.rev_lookup.find_local(place.local)
  }

  pub fn moveable_path_to_path(&self, mp: MoveablePath) -> Path {
    self.place_to_path(&self.move_data.move_paths[mp].place)
  }

  pub fn is_mutable_loan(&self, loan: Loan) -> bool {
    self.is_mutable_borrow(&self.borrow_set[loan])
  }

  pub fn is_declared_readonly(&self, place: &Place<'tcx>) -> bool {
    self.body_with_facts.body.local_decls[place.local].mutability
      != Mutability::Mut
  }

  pub fn path_to_variable(&self, path: Path) -> Variable {
    self.path_to_place(path).local
  }

  pub fn is_path_live_at(&self, path: Path, point: Point) -> bool {
    let var = self.path_to_variable(path);
    self
      .polonius_output
      .var_live_on_entry
      .get(&point)
      .map_or(false, |live_vars| live_vars.contains(&var))
  }

  pub fn location_to_span(&self, l: Location) -> Span {
    self.body_with_facts.body.source_info(l).span
  }

  pub fn max_permissions(&self, path: Path) -> Permissions {
    let write = !self.permissions_output.never_write.contains(&path);
    let drop = !self.permissions_output.never_drop.contains(&path);
    Permissions {
      write,
      // There is no way in the type system to have a non-readable type.
      read: true,
      drop,
    }
  }

  pub fn permissions_at_point(&self, path: Path, point: Point) -> Permissions {
    let path = path;
    let point = point;
    let empty_hash = &HashMap::default();
    let empty_set = &HashSet::default();

    // If the underlying variable is not live at a given point we should
    // report it as not having permissions.
    if !self.is_path_live_at(path, point) {
      return Permissions::bottom();
    }

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

    let read =
      !(cannot_read.contains_key(path) || path_moved_at.contains(path));
    let write = !(pms.never_write.contains(path)
      || cannot_write.contains_key(path)
      || path_moved_at.contains(path));
    let drop = !(pms.never_drop.contains(path)
      || cannot_drop.contains_key(path)
      || path_moved_at.contains(path));

    Permissions { read, write, drop }
  }

  pub fn initial_body_permissions(&self) -> HashMap<Place<'tcx>, Permissions> {
    let def_id = self.def_id;
    let tcx = self.tcx;
    let body = &self.body_with_facts.body;
    body
      .local_decls
      .indices()
      .flat_map(|local| {
        Place::from_local(local, tcx).interior_paths(tcx, body, def_id)
      })
      .fold(HashMap::default(), |mut acc, place| {
        // let p = self.place_to_path(&place);
        // acc.insert(place, self.max_permissions(p));

        // XXX(gavinleroy) EXPERIMENT: start the places off with *no permissions*
        // instead of using their max permissions. This in theory should not change anything.
        acc.insert(place, Permissions::bottom());
        acc
      })
  }

  /// Compute the beginning and end of the live loan region
  /// using source code location for comparison. We want to get
  /// rid of this method as it is more of a HACK.
  pub fn construct_loan_regions(&mut self) {
    if self.loan_regions.is_some() {
      // XXX: disallow reconstrucution of loan regions.
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
