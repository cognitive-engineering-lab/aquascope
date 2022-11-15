use polonius_engine::{AllFacts, FactTypes, Output as PEOutput};
use rustc_borrowck::{
  borrow_set::{BorrowData, BorrowSet},
  consumers::{BodyWithBorrowckFacts, RichLocation, RustcFacts},
};
use rustc_data_structures::fx::FxHashMap as HashMap;
use rustc_hir::{def_id::DefId, BodyId, Mutability};
use rustc_index::vec::IndexVec;
use rustc_middle::{
  mir::{BorrowKind, Local, Location, Place},
  ty::TyCtxt,
};
use rustc_mir_dataflow::move_paths::MoveData;

use super::{AquascopeFacts, Loan, Output, Path, Point};

type MoveablePath = <RustcFacts as FactTypes>::Path;

pub struct PermissionsCtxt<'a, 'tcx> {
  pub tcx: TyCtxt<'tcx>,
  pub polonius_input_facts: &'a AllFacts<RustcFacts>,
  pub polonius_output: Option<PEOutput<RustcFacts>>,
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
    // log::debug!("looking for place {p:?}");
    *self
      .rev_lookup
      .get(&p.local)
      .unwrap()
      .iter()
      .find(|path| self.path_to_place(**path) == *p)
      .unwrap()
  }

  pub fn new_path(&mut self, place: Place<'tcx>) -> Path {
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

  pub fn construct_loan_regions(&mut self) {
    if self.loan_regions.is_some() {
      // XXX: disallow reconstrucution of loan regions.
      return;
    }

    trait PointExt {
      fn from_point(p: Point) -> Self;
      fn expand(&mut self, p: Point);
    }

    impl PointExt for (Point, Point) {
      fn from_point(p: Point) -> Self {
        (p, p)
      }

      fn expand(&mut self, p: Point) {
        if p < self.0 {
          self.0 = p;
        } else if p > self.1 {
          self.1 = p
        }
      }
    }

    let mut hash: HashMap<Loan, (Point, Point)> = HashMap::default();

    self
      .polonius_output
      .as_ref()
      .unwrap()
      .loan_live_at
      .iter()
      .for_each(|(point, loans)| {
        loans.iter().for_each(|loan| {
          hash
            .entry(*loan)
            .or_insert_with(|| PointExt::from_point(*point))
            .expand(*point)
        })
      });

    self.loan_regions = Some(hash);
  }
}
