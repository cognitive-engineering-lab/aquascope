pub(crate) mod find_steps;
mod segment_tree;

use std::collections::hash_map::Entry;

use anyhow::Result;
use fluid_let::fluid_let;
use rustc_data_structures::fx::FxHashMap as HashMap;
use rustc_middle::mir::Place;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::{
  analysis::{
    permissions::{Permissions, PermissionsData, PermissionsDomain},
    AquascopeAnalysis, LoanKey,
  },
  Range,
};

fluid_let!(pub static INCLUDE_MODE: PermIncludeMode);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Deserialize, Serialize, Hash)]
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
  pub from: Range,
  pub to: Range,
  pub state: Vec<(String, PermissionsDataDiff)>,
}

/// A collection of [`PermissionsStepTable`] which are to be shown at the same location.
#[derive(Clone, Debug, Serialize, TS)]
#[ts(export)]
pub struct PermissionsLineDisplay {
  pub location: Range,
  pub state: Vec<PermissionsStepTable>,
}

#[derive(Clone, PartialEq, Eq, Serialize, TS)]
#[serde(tag = "type")]
#[ts(export)]
pub enum ValueStep<A>
where
  A: Clone
    + std::fmt::Debug
    + std::cmp::PartialEq
    + std::cmp::Eq
    + Serialize
    + TS,
{
  High {
    value: A,
  },
  Low,
  None {
    #[serde(skip_serializing_if = "Option::is_none")]
    value: Option<A>,
  },
}

impl<A> std::fmt::Debug for ValueStep<A>
where
  A: Clone
    + std::fmt::Debug
    + std::cmp::PartialEq
    + std::cmp::Eq
    + Serialize
    + TS,
{
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
    #[derive(Clone, PartialEq, Eq, Serialize, TS)]
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

#[derive(Clone, Serialize, TS, PartialEq, Eq)]
#[ts(export)]
pub struct PermissionsDataDiff {
  pub is_live: ValueStep<bool>,
  pub type_droppable: ValueStep<bool>,
  pub type_writeable: ValueStep<bool>,
  pub path_moved: ValueStep<bool>,
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

impl<T> ValueStep<T>
where
  T: Clone + std::fmt::Debug + std::cmp::PartialEq + Eq + Serialize + TS,
{
  fn is_empty(&self) -> bool {
    matches!(self, Self::None { .. })
  }
}

impl<A> Difference for Option<A>
where
  A: Clone + PartialEq + Eq + std::fmt::Debug + Serialize + TS,
{
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
      permissions: self.permissions.diff(rhs.permissions),
    }
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

pub fn compute_permission_steps<'a, 'tcx>(
  ctxt: &AquascopeAnalysis<'a, 'tcx>,
) -> Result<Vec<PermissionsLineDisplay>>
where
  'tcx: 'a,
{
  let mode = INCLUDE_MODE.copied().unwrap_or(PermIncludeMode::Changes);
  let permissions_ctxt = &ctxt.permissions;
  let span_to_range = |span| ctxt.span_to_range(span);
  find_steps::compute_permission_steps(
    permissions_ctxt,
    &ctxt.ir_mapper,
    mode,
    span_to_range,
  )
}
