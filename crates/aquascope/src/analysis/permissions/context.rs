use std::collections::BTreeMap;

use polonius_engine::{AllFacts, FactTypes, Output as PEOutput};
use rustc_borrowck::{
  borrow_set::{BorrowData, BorrowSet},
  consumers::{BodyWithBorrowckFacts, RustcFacts},
};
use rustc_data_structures::fx::FxHashMap as HashMap;
use rustc_hir::{
  def_id::{DefId, LocalDefId},
  BodyId, Mutability,
};
use rustc_index::vec::IndexVec;
use rustc_middle::{
  mir::{BorrowKind, Local, Location, Place},
  ty::TyCtxt,
};
use rustc_mir_dataflow::move_paths::{LookupResult, MoveData};

use super::{AquascopeFacts, Loan, Output, Path, Point};

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
  pub(crate) place_data: IndexVec<Path, Place<'tcx>>,
  pub(crate) rev_lookup: HashMap<Local, Vec<Path>>,
}

impl<'a, 'tcx> PermissionsCtxt<'a, 'tcx> {
  pub fn place_to_path(&self, p: &Place<'tcx>) -> Path {
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
    let place = self.place_data[p];
    log::debug!("Path {:?} to Place {:?}", p, place);
    place
  }

  pub fn location_to_point(&self, l: Location) -> Point {
    self.body_with_facts.location_table.start_index(l)
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
}
