#![allow(dead_code)]

mod context;
pub mod permission_boundaries;
pub mod permission_stepper;
mod permissions_datalog;
mod places_conflict;
pub mod utils;

pub use context::PermissionsCtxt;
pub use permissions_datalog::{compute, Output};
use polonius_engine::FactTypes;
use rustc_borrowck::consumers::RustcFacts;
use rustc_middle::{mir::Mutability, ty::Ty};
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

impl Permissions {
  fn step(&self, new_state: &Permissions) -> PermsDiff {
    PermsDiff {
      read: PermDiff::new(self.read, new_state.read),
      write: PermDiff::new(self.write, new_state.write),
      drop: PermDiff::new(self.drop, new_state.drop),
    }
  }

  // No "Top" value exists for permissions as this is on a per-place basis.
  // That is, the top value depends on a places type declaration.
  fn bottom() -> Self {
    Permissions {
      read: false,
      write: false,
      drop: false,
    }
  }
}

///// Debugging traits, just for visualization purposes
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
  pub state: Vec<(String, PermsDiff)>,
}

#[derive(Clone, Debug, Serialize, TS)]
#[serde(tag = "type")]
#[ts(export)]
pub enum PermDiff {
  Add,
  Sub,
  /// value indicates whether or not the permission is held.
  None {
    value: bool,
  },
}

impl PermDiff {
  fn new(old_state: bool, new_state: bool) -> Self {
    if old_state && !new_state {
      Self::Sub
    } else if !old_state && new_state {
      Self::Add
    } else {
      Self::None { value: old_state }
    }
  }

  fn is_none(&self) -> bool {
    matches!(self, Self::None { .. })
  }
}

#[derive(Clone, Debug, Serialize, TS)]
#[ts(export)]
pub struct PermsDiff {
  pub read: PermDiff,
  pub write: PermDiff,
  pub drop: PermDiff,
}

impl PermsDiff {
  /// An empty value indicates that no permission was changed.
  fn is_empty(&self) -> bool {
    self.read.is_none() && self.write.is_none() && self.drop.is_none()
  }
}
