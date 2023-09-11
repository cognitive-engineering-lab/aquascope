//! Analysis for the “Missing-at” relations.

mod hir_steps;
#[allow(clippy::similar_names)]
mod segmented_mir;
mod table_builder;

use std::collections::hash_map::Entry;

use anyhow::{bail, Result};
use fluid_let::fluid_let;
use rustc_data_structures::{self, fx::FxHashMap as HashMap};
use rustc_hir::intravisit::Visitor as HirVisitor;
use rustc_middle::mir::{Location, Place};
use rustc_span::Span;
use rustc_utils::source_map::range::CharRange;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::analysis::{
  permissions::{
    Permissions, PermissionsCtxt, PermissionsData, PermissionsDomain,
  },
  AquascopeAnalysis, LoanKey, MoveKey,
};

fluid_let!(pub static INCLUDE_MODE: PermIncludeMode);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Deserialize, Serialize, Hash)]
pub enum PermIncludeMode {
  Changes,
  All,
}

impl std::str::FromStr for PermIncludeMode {
  type Err = String;
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "Changes" => Ok(Self::Changes),
      "All" => Ok(Self::All),
      _ => Err(format!("Could not parse: {s}")),
    }
  }
}

pub trait Difference {
  type Diff;

  fn diff(&self, rhs: Self) -> Self::Diff;
}

/// Table representation of the permissions difference between two locations.
#[derive(Clone, Debug, Serialize, TS)]
#[ts(export)]
pub struct PermissionsStepTable {
  pub from: CharRange,
  pub to: CharRange,
  pub state: Vec<(String, PermissionsDataDiff)>,
}

/// A collection of [`PermissionsStepTable`] which are to be shown at the same location.
#[derive(Clone, Debug, Serialize, TS)]
#[ts(export)]
pub struct PermissionsLineDisplay {
  pub location: CharRange,
  pub state: Vec<PermissionsStepTable>,
}

pub trait Stepable:
  Copy
  + Clone
  + std::fmt::Debug
  + std::cmp::PartialEq
  + std::cmp::Eq
  + std::hash::Hash
  + Serialize
  + TS
{
}

impl<A> Stepable for A where
  A: Copy
    + Clone
    + std::fmt::Debug
    + std::cmp::PartialEq
    + std::cmp::Eq
    + std::hash::Hash
    + Serialize
    + TS
{
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Serialize, TS)]
#[serde(tag = "type")]
#[ts(export)]
pub enum ValueStep<A: Stepable> {
  High {
    value: A,
  },
  Low,
  None {
    #[serde(skip_serializing_if = "Option::is_none")]
    value: Option<A>,
  },
}

impl<A: Stepable> ValueStep<A> {
  // TODO: this is a loose surface-level notion of symmetry.
  fn is_symmetric_diff(&self, rhs: &Self) -> bool {
    matches!(
      (self, rhs),
      (ValueStep::High { .. }, ValueStep::Low { .. })
        | (ValueStep::Low { .. }, ValueStep::High { .. })
        | (ValueStep::None { .. }, ValueStep::None { .. })
    )
  }
}

impl<A: Stepable> std::fmt::Debug for ValueStep<A> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ValueStep::High { .. } => write!(f, "↑"),
      ValueStep::Low => write!(f, "↓"),
      ValueStep::None { value } => write!(f, "―<{value:?}>―"),
    }
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
    #[derive(Copy, Clone, PartialEq, Eq, Hash, Serialize, TS)]
    #[ts(export)]
    pub struct $diff {
      $( pub $i: ValueStep<bool>, )*
    }

    impl $diff {
      fn is_empty(&self) -> bool {
        $( self.$i.is_empty() && )* true
      }
    }
  }
}

make_diff!(Permissions => PermissionsDiff {
   read, write, drop
});

impl std::fmt::Debug for PermissionsDiff {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{:?}R   {:?}W   {:?}D", self.read, self.write, self.drop)
  }
}

#[derive(Copy, Clone, Serialize, TS, PartialEq, Eq, Hash)]
#[ts(export)]
pub struct PermissionsDataDiff {
  pub is_live: ValueStep<bool>,
  pub type_droppable: ValueStep<bool>,
  pub type_writeable: ValueStep<bool>,
  pub path_moved: ValueStep<MoveKey>,
  pub path_uninitialized: ValueStep<bool>,
  pub loan_read_refined: ValueStep<LoanKey>,
  pub loan_write_refined: ValueStep<LoanKey>,
  pub loan_drop_refined: ValueStep<LoanKey>,
  pub permissions: PermissionsDiff,
}

impl std::fmt::Debug for PermissionsDataDiff {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    writeln!(f, "\nPermissions Data Step")?;
    writeln!(f, "  Permissions:          {:?}", self.permissions)?;
    writeln!(f, "    is_live:            {:?}", self.is_live)?;
    writeln!(f, "    type_droppable:     {:?}", self.type_droppable)?;
    writeln!(f, "    type_writeable:     {:?}", self.type_writeable)?;
    writeln!(f, "    path_moved:         {:?}", self.path_moved)?;
    writeln!(f, "    loan_write_refined: {:?}", self.loan_write_refined)?;
    writeln!(f, "    loan_drop_refined:  {:?}", self.loan_drop_refined)?;
    Ok(())
  }
}

impl PermissionsDataDiff {
  fn is_empty(&self) -> bool {
    self.permissions.is_empty()
  }

  fn is_symmetric_diff(&self, rhs: &PermissionsDataDiff) -> bool {
    let p1 = &self.permissions;
    let p2 = &rhs.permissions;

    p1.read.is_symmetric_diff(&p2.read)
      && p1.write.is_symmetric_diff(&p2.write)
      && p1.drop.is_symmetric_diff(&p2.drop)
  }
}

impl Difference for bool {
  type Diff = ValueStep<bool>;
  fn diff(&self, rhs: bool) -> Self::Diff {
    if *self && !rhs {
      ValueStep::Low
    } else if !*self && rhs {
      ValueStep::High { value: true }
    } else {
      ValueStep::None { value: Some(*self) }
    }
  }
}

impl<T: Stepable> ValueStep<T> {
  fn is_empty(&self) -> bool {
    matches!(self, Self::None { .. })
  }
}

impl<A: Stepable> Difference for Option<A> {
  type Diff = ValueStep<A>;

  fn diff(&self, rhs: Option<A>) -> Self::Diff {
    match (self, rhs) {
      (None, None) => ValueStep::None { value: None },
      (Some(_), None) => ValueStep::Low,
      (None, Some(value)) => ValueStep::High { value },
      (Some(v0), Some(v1)) => {
        if *v0 != v1 {
          log::warn!(
            "Option diff Some does not contain same value {v0:?} -> {v1:?}"
          );
        }
        ValueStep::None { value: Some(v1) }
      }
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
      path_uninitialized: self.path_uninitialized.diff(rhs.path_uninitialized),
      permissions: self.permissions.diff(rhs.permissions),
    }
  }
}

impl<'tcx> Difference for &PermissionsDomain<'tcx> {
  type Diff = HashMap<Place<'tcx>, PermissionsDataDiff>;
  fn diff(&self, rhs: &PermissionsDomain<'tcx>) -> Self::Diff {
    let mut diffs = HashMap::default();

    for (place, p1) in self.iter() {
      let p2 = rhs.get(place).unwrap();
      let diff = p1.diff(*p2);

      match diffs.entry(*place) {
        Entry::Occupied(_) => {
          panic!("Permissions step already in output for {place:?}");
        }
        Entry::Vacant(entry) => {
          entry.insert(diff);
        }
      }
    }

    diffs
  }
}

/// Represents a segment of the MIR control-flow graph.
///
/// A `MirSegment` corresponds directly to locations where a permissions step
/// will be made. However, a segment is also control-flow specific.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct MirSegment {
  pub from: Location,
  pub to: Location,
}

impl std::fmt::Debug for MirSegment {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "MirSegment({:?} -> {:?})", self.from, self.to)
  }
}

impl MirSegment {
  pub fn new(l1: Location, l2: Location) -> Self {
    MirSegment { from: l1, to: l2 }
  }

  /// A _rough_ approximation of the source span of the step.
  pub fn span(&self, ctxt: &PermissionsCtxt) -> Span {
    let lo = ctxt.location_to_span(self.from);
    let hi = ctxt.location_to_span(self.to);
    lo.with_hi(hi.hi())
  }

  pub fn into_diff<'tcx>(
    self,
    ctxt: &PermissionsCtxt<'_, 'tcx>,
  ) -> HashMap<Place<'tcx>, PermissionsDataDiff> {
    let p0 = ctxt.location_to_point(self.from);
    let p1 = ctxt.location_to_point(self.to);
    let before = &ctxt.permissions_domain_at_point(p0);
    let after = &ctxt.permissions_domain_at_point(p1);
    before.diff(after)
  }
}

// ----------
// Main entry

pub fn compute_permission_steps<'a, 'tcx>(
  analysis: &AquascopeAnalysis<'a, 'tcx>,
) -> Result<Vec<PermissionsLineDisplay>>
where
  'tcx: 'a,
{
  let mode = INCLUDE_MODE.copied().unwrap_or(PermIncludeMode::Changes);
  let ctxt = &analysis.permissions;
  let ir_mapper = &analysis.ir_mapper;
  let body = &ctxt.body_with_facts.body;
  let mut hir_visitor =
    hir_steps::HirStepPoints::make(&ctxt.tcx, body, ctxt.body_id, ir_mapper)?;

  hir_visitor.visit_nested_body(ctxt.body_id);

  if let Some(msg) = hir_visitor.get_unsupported_feature() {
    bail!(msg);
  }

  if let Some(fatal_error) = hir_visitor.get_internal_error() {
    bail!(fatal_error);
  }

  hir_visitor.finalize(analysis, mode)
}
