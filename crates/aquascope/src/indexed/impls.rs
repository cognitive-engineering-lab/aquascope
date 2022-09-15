use std::rc::Rc;

use rustc_data_structures::fx::FxHashSet as HashSet;
use rustc_index::vec::IndexVec;
use rustc_middle::mir::{BasicBlock, Body, Local, Location, Place};

use super::{
  DefaultDomain, IndexSet, IndexedDomain, IndexedValue, OwnedSet, ToIndex,
};
use crate::{
  mir::utils::{BodyExt, PlaceExt},
  to_index_impl,
};

rustc_index::newtype_index! {
  pub struct LocationIndex {
      DEBUG_FORMAT = "l{}"
  }
}

to_index_impl!(Location);

impl IndexedValue for Location {
  type Index = LocationIndex;
  type Domain = LocationDomain;
}

pub type LocationSet<S = OwnedSet<Location>> = IndexSet<Location, S>;
pub struct LocationDomain {
  domain: DefaultDomain<LocationIndex, Location>,
  arg_block: BasicBlock,
  real_locations: usize,
}

pub fn arg_location<'tcx>(
  place: Place<'tcx>,
  body: &Body<'tcx>,
) -> Option<Location> {
  place.is_arg(body).then(|| {
    let arg_block = BasicBlock::from_usize(body.basic_blocks().len());
    Location {
      block: arg_block,
      statement_index: place.local.as_usize(),
    }
  })
}

impl LocationDomain {
  pub fn new(body: &Body) -> Rc<Self> {
    let mut locations = body.all_locations().collect::<Vec<_>>();

    let arg_block = BasicBlock::from_usize(body.basic_blocks().len());

    let real_locations = locations.len();

    let arg_locations = (0 .. body.arg_count).map(|i| Location {
      block: arg_block,
      statement_index: i + 1,
    });
    locations.extend(arg_locations);

    let domain = DefaultDomain::new(locations);

    Rc::new(LocationDomain {
      domain,
      arg_block,
      real_locations,
    })
  }

  pub fn num_real_locations(&self) -> usize {
    self.real_locations
  }

  pub fn arg_to_location(&self, local: Local) -> LocationIndex {
    let location = Location {
      block: self.arg_block,
      statement_index: local.as_usize(),
    };
    self.index(&location)
  }

  pub fn location_to_local(
    &self,
    location: impl ToIndex<Location>,
  ) -> Option<Local> {
    let location = self.value(location.to_index(self));
    (location.block == self.arg_block)
      .then(|| Local::from_usize(location.statement_index))
  }
}

impl IndexedDomain for LocationDomain {
  type Index = LocationIndex;
  type Value = Location;

  fn value(&self, index: Self::Index) -> &Self::Value {
    self.domain.value(index)
  }

  fn index(&self, value: &Self::Value) -> Self::Index {
    self.domain.index(value)
  }

  fn contains(&self, value: &Self::Value) -> bool {
    self.domain.contains(value)
  }

  fn as_vec(&self) -> &IndexVec<Self::Index, Self::Value> {
    self.domain.as_vec()
  }
}

pub type PlaceSet<'tcx> = HashSet<Place<'tcx>>;
