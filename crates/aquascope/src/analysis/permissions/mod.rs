mod context;
pub(crate) mod flow;
// pub(crate) mod flow_datalog;
#[allow(clippy::all, clippy::pedantic)]
mod graphviz;
mod output;
#[allow(clippy::all, clippy::pedantic)]
pub(crate) mod places_conflict;
pub mod utils;

use std::ops::{Deref, DerefMut};

pub use context::PermissionsCtxt;
use fluid_let::fluid_let;
pub use output::{compute, Output};
use polonius_engine::FactTypes;
use rustc_borrowck::consumers::RustcFacts;
use rustc_data_structures::fx::FxHashMap;
use rustc_middle::mir::Place;
use serde::Serialize;
use ts_rs::TS;

use crate::{
  analysis::{LoanKey, MoveKey},
  Range,
};

fluid_let!(pub static ENABLE_FLOW_PERMISSIONS: bool);
pub const ENABLE_FLOW_DEFAULT: bool = false;

/// Permission facts in Aquascope, similar to [`RustcFacts`].
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
// General Information

pub type Origin = <AquascopeFacts as FactTypes>::Origin;
pub type Path = <AquascopeFacts as FactTypes>::Path;
pub type Point = <AquascopeFacts as FactTypes>::Point;
pub type Loan = <AquascopeFacts as FactTypes>::Loan;
pub type Variable = <AquascopeFacts as FactTypes>::Variable;
pub type Move = rustc_mir_dataflow::move_paths::MoveOutIndex;

// ------------------------------------------------
// Permission Boundaries

/// Read, Write, and Own permissions.
///
/// NOTE: previously, the term *drop* was used instead of *own*
/// and this terminology remains within the source and internal documentation.
#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, TS)]
#[ts(export)]
pub struct Permissions {
  pub read: bool,
  pub write: bool,
  pub drop: bool,
}

/// Permissions and first-order provenance for permission refinement.
///
/// In contrast to [`Permissions`], the `PermissionsData` stores all relevant
/// information about what factors into the permissions. Things like
/// declared type information, loan refinements, move refinements, etc.
/// `PermissionsData` corresponds to a single [`Place`].
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, Serialize, TS)]
#[ts(export)]
pub struct PermissionsData {
  /// Was the type declared as droppable (i.e. an owned value)?
  pub type_droppable: bool,

  /// Was the type declared as writeable (i.e. is it `mut`)?
  pub type_writeable: bool,

  /// Is the type copyable (i.e. does it implement the `Copy` trait)?
  pub type_copyable: bool,

  /// Is the [`Place`] currently live?
  pub is_live: bool,

  /// Is this place uninitialized?
  pub path_uninitialized: bool,

  /// Is the [`Place`] currently uninitialzed due to a move?
  #[serde(skip_serializing_if = "Option::is_none")]
  pub path_moved: Option<MoveKey>,

  #[serde(skip_serializing_if = "Option::is_none")]
  /// Is a live loan removing `read` permissions?
  pub loan_read_refined: Option<LoanKey>,

  #[serde(skip_serializing_if = "Option::is_none")]
  /// Is a live loan removing `write` permissions?
  pub loan_write_refined: Option<LoanKey>,

  #[serde(skip_serializing_if = "Option::is_none")]
  /// Is a live loan removing `drop` permissions?
  pub loan_drop_refined: Option<LoanKey>,

  /// Computed permissions given the above information.
  pub permissions: Permissions,
}

/// A permissions refiner. [`Loan`]s and moves can refine permissions.
#[derive(Debug, Clone, Serialize, PartialEq, TS)]
#[ts(export)]
pub enum Refiner {
  Loan(LoanKey),
  Move(MoveKey),
}

/// The live source-level range of a refinement.
#[derive(Debug, Clone, Serialize, PartialEq, TS)]
#[ts(export)]
pub struct RefinementRegion {
  pub refiner_point: Refiner,
  pub refined_ranges: Vec<Range>,
}

/// Permissions data *forall* places in the body under analysis.
#[derive(Clone, PartialEq, Eq, Default, Debug)]
pub struct PermissionsDomain<'tcx>(FxHashMap<Place<'tcx>, PermissionsData>);

// ------------------------------------------------

impl Permissions {
  // No "Top" value exists for permissions as this is on a per-place basis.
  // That is, the top value depends on a places type declaration.
  pub fn bottom() -> Self {
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

// impl<'tcx> From<Ty<'tcx>> for Permissions {
//   fn from(ty: Ty<'tcx>) -> Self
// }

impl<'tcx> From<FxHashMap<Place<'tcx>, PermissionsData>>
  for PermissionsDomain<'tcx>
{
  fn from(m: FxHashMap<Place<'tcx>, PermissionsData>) -> Self {
    PermissionsDomain(m)
  }
}

impl<'tcx> Deref for PermissionsDomain<'tcx> {
  type Target = FxHashMap<Place<'tcx>, PermissionsData>;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl DerefMut for PermissionsDomain<'_> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}
