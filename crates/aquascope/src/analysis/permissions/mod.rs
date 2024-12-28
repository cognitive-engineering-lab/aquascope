//! Aquascope permissions analysis.

mod context;
pub(crate) mod flow;
// pub(crate) mod flow_datalog;
mod output;

pub mod utils;

use std::ops::{Deref, DerefMut};

pub use context::PermissionsCtxt;
use fluid_let::fluid_let;
pub use output::{compute, Output};
use polonius_engine::FactTypes;
use rustc_borrowck::consumers::RustcFacts;
use rustc_data_structures::fx::FxHashMap;
use rustc_middle::mir::Place;
use rustc_utils::source_map::range::CharRange;
use serde::Serialize;
use ts_rs::TS;

use crate::analysis::{LoanKey, MoveKey};

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
  #[derive(PartialOrd, Ord)]
  #[debug_format = "path{}"]
  pub struct PathIndex {}
}

impl polonius_engine::Atom for PathIndex {
  fn index(self) -> usize {
    rustc_index::Idx::index(self)
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

/// Read, Write, and Own permissions for a single [`Place`].
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

  /// Is the [`Place`] currently uninitialized due to a move?
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
}

impl PermissionsData {
  /// If the place is not live, then the permissions are always the bottom type.
  /// Otherwise, see `permissions_ignore_liveness`. (You may want to ignore liveness
  /// when you know a variable is being used, and should therefore be live.)
  pub fn permissions(&self) -> Permissions {
    if !self.is_live {
      Permissions::bottom()
    } else {
      self.permissions_ignore_liveness()
    }
  }

  /// A path is readable IFF:
  /// - it is not moved.
  /// - there doesn't exist a read-refining loan at this point.
  ///
  /// A path is writeable IFF:
  /// - the path's declared type allows for mutability.
  /// - the path is readable (you can't write if you can't read)
  ///   this implies that the path isn't moved.
  /// - there doesn't exist a write-refining loan at this point.
  ///
  /// A path is droppable(without copy) IFF:
  /// - the path's declared type is droppable.
  /// - it isn't moved.
  /// - no drop-refining loan exists at this point.
  pub fn permissions_ignore_liveness(&self) -> Permissions {
    let mem_uninit = self.path_moved.is_some() || self.path_uninitialized;
    let read = !mem_uninit && self.loan_read_refined.is_none();
    let write =
      self.type_writeable && read && self.loan_write_refined.is_none();
    let drop = self.type_droppable && read && self.loan_drop_refined.is_none();
    Permissions { read, write, drop }
  }
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
  pub refined_ranges: Vec<CharRange>,
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
