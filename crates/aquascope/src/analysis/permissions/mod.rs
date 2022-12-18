#![allow(dead_code)]

mod context;
mod graphviz;
pub mod permission_boundaries;
pub mod permission_stepper;
mod permissions_datalog;
mod places_conflict;
pub mod utils;

use std::{
  collections::hash_map::Entry,
  ops::{Deref, DerefMut},
};

pub use context::PermissionsCtxt;
pub use permissions_datalog::{compute, Output};
use polonius_engine::FactTypes;
use rustc_borrowck::consumers::RustcFacts;
use rustc_data_structures::fx::FxHashMap as HashMap;
use rustc_middle::{
  mir::{Mutability, Place},
  ty::Ty,
};
use serde::Serialize;
use ts_rs::TS;

use crate::Range;

#[derive(Copy, Clone, Debug)]
pub struct AquascopeFacts;

impl polonius_engine::FactTypes for AquascopeFacts {
  type Origin = <RustcFacts as FactTypes>::Origin;
  type Loan = <RustcFacts as FactTypes>::Loan;
  type Point = <RustcFacts as FactTypes>::Point;
  type Variable = <RustcFacts as FactTypes>::Variable;
  type Path = PathIndex;
}

rustc_index::newtype_index! {
  pub struct PathIndex {
    DEBUG_FORMAT = "path{}"
  }
}

impl polonius_engine::Atom for PathIndex {
  fn index(self) -> usize {
    rustc_index::vec::Idx::index(self)
  }
}

// ------------------------------------------------
// Permission Boundaries

pub type Path = <AquascopeFacts as FactTypes>::Path;
pub type Point = <AquascopeFacts as FactTypes>::Point;
pub type Loan = <AquascopeFacts as FactTypes>::Loan;
pub type Variable = <AquascopeFacts as FactTypes>::Variable;

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, TS)]
#[ts(export)]
pub struct Permissions {
  pub read: bool,
  pub write: bool,
  pub drop: bool,
}

// In contrast to Permissions, the PermissionsData stores all relevant
// information about what factors into the permissions. Things like
// declared type information, loan refinements, move refinements, etc.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct PermissionsData {
  // Type declaration information
  pub type_droppable: bool,
  pub type_writeable: bool,

  // Liveness information
  pub is_live: bool,

  // Initialization information
  pub path_moved: bool,

  // Refinement information
  pub loan_read_refined: bool,
  pub loan_write_refined: bool,
  pub loan_drop_refined: bool,

  // Even though these are derivable from the
  // above information, we'll keep it explicitly so
  // others don't have to compute permissions.
  pub permissions: Permissions,
}

/// A point where the permissions reality are checked against their expectations.
/// Currently, the only boundary supported is method calls on a receiver, however,
/// these boundaries could be drawn in any program location.
#[derive(Debug, Clone, Serialize, PartialEq, TS)]
#[ts(export)]
pub struct PermissionsBoundary {
  // instead of giving the range, the backend should supply the exact location. this will
  // be especially usefull when we have permissions on more than just method calls.
  pub location: usize,
  pub expected: Permissions,
  pub actual: Permissions,
  pub was_copied: bool,
  pub explanations: MissingPermsInfo,
}

#[derive(Debug, Clone, Serialize, PartialEq, TS)]
#[ts(export)]
pub struct MissingPermsInfo {
  #[serde(skip_serializing_if = "Option::is_none")]
  pub read: Option<MissingPermReason>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub write: Option<MissingPermReason>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub drop: Option<MissingPermReason>,
}

#[derive(Debug, Clone, Serialize, PartialEq, TS)]
#[serde(tag = "type")]
#[ts(export)]
pub enum MissingPermReason {
  // TODO store information to visually build the explanation
  InsufficientType,
  Refined(RefinementRegion),
}

#[derive(Debug, Clone, Serialize, PartialEq, TS)]
#[serde(tag = "type")]
#[ts(export)]
pub enum Refiner {
  Loan(Range),
  Move(Range),
}

#[derive(Debug, Clone, Serialize, PartialEq, TS)]
#[ts(export)]
pub struct RefinementRegion {
  pub refiner_point: Refiner,
  pub refined_ranges: Vec<Range>,
  // NOTE: the start and end only cary meaning in a linear scope.
  pub start: Range,
  pub end: Range,
}

// ------------------------------------------------
// Permission Stepper

#[derive(Clone, Debug, Serialize, TS)]
#[ts(export)]
pub struct PermissionsStateStep {
  pub location: Range,
  pub state: Vec<(String, PermissionsDataDiff)>,
}

#[derive(Clone, Debug, Serialize, TS)]
#[serde(tag = "type")]
#[ts(export)]
pub enum BoolStep {
  High,
  Low,
  /// value indicates whether or not the permission is held.
  None {
    value: bool,
  },
}

// -----------------
// Permission Domain

#[derive(Clone, PartialEq, Eq, Default, Debug)]
/// A representation of the permissions *forall* places in the body under analysis.
pub struct PermissionsDomain<'tcx>(HashMap<Place<'tcx>, PermissionsData>);

pub trait Difference {
  type Diff;

  fn diff(&self, rhs: Self) -> Self::Diff;
}

#[derive(Clone, Debug, Serialize, TS)]
#[ts(export)]
pub struct PermissionsDataDiff {
  pub is_live: BoolStep,
  pub type_droppable: BoolStep,
  pub type_writeable: BoolStep,
  pub loan_read_refined: BoolStep,
  pub loan_write_refined: BoolStep,
  pub loan_drop_refined: BoolStep,
  pub path_moved: BoolStep,
  pub permissions: PermissionsDiff,
}

impl PermissionsDataDiff {
  fn is_empty(&self) -> bool {
    // self.is_live.is_empty()
    //   && self.type_droppable.is_empty()
    //   && self.type_writeable.is_empty()
    //   && self.loan_read_refined.is_empty()
    //   && self.loan_write_refined.is_empty()
    //   && self.loan_drop_refined.is_empty()
    //   && self.path_moved.is_empty()
    //   &&
    self.permissions.is_empty()
  }
}

// A handy macro for making difference types with only BoolStep fields
// TODO(gavinleroy): a diff type should be automatically generated if all the fields
// in a macro can ge diffed, but I'll save that for later. Below is mostly a syntactic
// macro to simplify things for the time being.
// FIXME: no longer sufficient *rewrite*. Shouldn't need to pass the name $diff
// and fields should be able to have a specified type, if not provided then
// the default BoolStep can be taken.
macro_rules! make_diff {
  ($base:ident => $diff:ident { $($i:ident),* }) => {
    #[derive(Clone, Debug, Serialize, TS)]
    #[ts(export)]
    pub struct $diff {
      $( pub $i: BoolStep, )*
    }

    impl $diff {
      fn is_empty(&self) -> bool {
        $( self.$i.is_empty() && )* true
      }

      fn split(&self) -> ($base, $base) {
        (
          $base {
            $(
              $i: self.$i.split().0,
            )*
          },
          $base {
            $(
              $i: self.$i.split().1,
            )*
          }
        )
      }
    }
  }
}

make_diff!(Permissions => PermissionsDiff {
   read, write, drop
});

// ---------------------
// Impls for above types

// Again these should all be generated, very pattern oriented.

impl Difference for bool {
  type Diff = BoolStep;
  fn diff(&self, rhs: bool) -> Self::Diff {
    if *self && !rhs {
      BoolStep::Low
    } else if !*self && rhs {
      BoolStep::High
    } else {
      BoolStep::None { value: *self }
    }
  }
}

impl Difference for Permissions {
  type Diff = PermissionsDiff;

  fn diff(&self, rhs: Permissions) -> Self::Diff {
    PermissionsDiff {
      read: self.read.diff(rhs.read),
      write: self.write.diff(rhs.write),
      drop: self.drop.diff(rhs.drop),
    }
  }
}

impl Difference for PermissionsData {
  type Diff = PermissionsDataDiff;

  fn diff(&self, rhs: PermissionsData) -> Self::Diff {
    PermissionsDataDiff {
      is_live: self.is_live.diff(rhs.is_live),
      type_droppable: self.type_droppable.diff(rhs.type_droppable),
      type_writeable: self.type_writeable.diff(rhs.type_writeable),
      loan_read_refined: self.loan_read_refined.diff(rhs.loan_read_refined),
      loan_write_refined: self.loan_write_refined.diff(rhs.loan_write_refined),
      loan_drop_refined: self.loan_drop_refined.diff(rhs.loan_drop_refined),
      path_moved: self.path_moved.diff(rhs.path_moved),
      permissions: self.permissions.diff(rhs.permissions),
    }
  }
}

impl Permissions {
  // No "Top" value exists for permissions as this is on a per-place basis.
  // That is, the top value depends on a places type declaration.
  fn bottom() -> Permissions {
    Permissions {
      read: false,
      write: false,
      drop: false,
    }
  }
}

impl std::fmt::Debug for Permissions {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    if !self.read && !self.write && !self.drop {
      write!(f, "∅")
    } else {
      if self.read {
        write!(f, "R")?;
      }
      if self.write {
        write!(f, "W")?;
      }
      if self.drop {
        write!(f, "D")?;
      }
      Ok(())
    }
  }
}

// XXX: this is only valid when the Ty is an *expected* type.
// This is because expected types do not rely on the mutability of
// the binding, e.g. `let mut x = ...` and all of the expected information
// is really just in the type.
impl<'tcx> From<Ty<'tcx>> for Permissions {
  fn from(ty: Ty<'tcx>) -> Self {
    let read = true;
    let (write, drop) = match ty.ref_mutability() {
      None => (false, true),
      Some(Mutability::Not) => (false, false),
      Some(Mutability::Mut) => (true, false),
    };
    Self { read, write, drop }
  }
}

impl BoolStep {
  fn is_empty(&self) -> bool {
    matches!(self, Self::None { .. })
  }

  fn split(&self) -> (bool, bool) {
    match self {
      Self::High => (false, true),
      Self::Low => (true, false),
      Self::None { value } => (*value, *value),
    }
  }
}

impl<'tcx> From<HashMap<Place<'tcx>, PermissionsData>>
  for PermissionsDomain<'tcx>
{
  fn from(m: HashMap<Place<'tcx>, PermissionsData>) -> Self {
    PermissionsDomain(m)
  }
}

impl<'tcx> Deref for PermissionsDomain<'tcx> {
  type Target = HashMap<Place<'tcx>, PermissionsData>;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl DerefMut for PermissionsDomain<'_> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

impl<'tcx> Difference for &PermissionsDomain<'tcx> {
  type Diff = HashMap<Place<'tcx>, PermissionsDataDiff>;
  fn diff(&self, rhs: &PermissionsDomain<'tcx>) -> Self::Diff {
    self
      .iter()
      .fold(HashMap::default(), |mut acc, (place, p1)| {
        let p2 = rhs.get(place).unwrap();
        let diff = p1.diff(*p2);

        match acc.entry(*place) {
          Entry::Occupied(_) => {
            panic!("Permissions step already in output for {place:?}");
          }
          Entry::Vacant(entry) => {
            entry.insert(diff);
          }
        }

        acc
      })
  }
}
