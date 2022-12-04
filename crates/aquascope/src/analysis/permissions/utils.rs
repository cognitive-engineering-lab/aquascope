use std::{
  collections::hash_map::Entry,
  fmt::Debug,
  ops::{Deref, DerefMut},
};

use flowistry::mir::utils::{dump_results, PlaceExt};
use rustc_data_structures::fx::FxHashMap as HashMap;
use rustc_middle::mir::{Location, Place};
use rustc_mir_dataflow::{
  fmt::DebugWithContext, Analysis, AnalysisDomain, JoinSemiLattice, Results,
};
use rustc_span::def_id::LocalDefId;

use super::{context::PermissionsCtxt, Permissions};

pub(crate) fn dump_permissions_with_mir(ctxt: &PermissionsCtxt) {
  // XXX: Unfortunately, the only way I know how to do this is to do a MIR
  // dataflow analysis and simply take the information from the context.
  // This mean there will only be a single pass but :shrug:

  let def_id = ctxt.tcx.hir().body_owner_def_id(ctxt.body_id);

  // Temporary hack: only run the analysis on a specific function
  let owner = ctxt.tcx.hir().body_owner(ctxt.body_id);
  let Some(name) = ctxt.tcx.hir().opt_name(owner) else { return };
  if name.as_str() != "dump_me" {
    return;
  }

  let results = flow_mir_permissions(ctxt);

  log::debug!("Dumping results for {:?}", name.as_str());

  if let Err(e) = dump_results(
    &ctxt.body_with_facts.body,
    &results,
    def_id.to_def_id(),
    ctxt.tcx,
  ) {
    log::warn!("{:?}", e);
  }
}

pub(crate) fn flow_mir_permissions<'a, 'tcx>(
  ctxt: &'a PermissionsCtxt<'a, 'tcx>,
) -> Results<'tcx, PAnalysis<'a, 'tcx>> {
  let analysis = PAnalysis { ctxt };
  analysis
    .into_engine(ctxt.tcx, &ctxt.body_with_facts.body)
    .iterate_to_fixpoint()
}

// --------------------------------------------------
// Domain

impl Permissions {
  pub fn none() -> Self {
    Permissions {
      read: false,
      write: false,
      drop: false,
    }
  }

  pub fn all() -> Self {
    Permissions {
      read: true,
      write: true,
      drop: true,
    }
  }
}

impl JoinSemiLattice for Permissions {
  fn join(&mut self, other: &Self) -> bool {
    let orig = *self;
    self.read &= other.read;
    self.write &= other.write;
    self.drop &= other.drop;
    orig != *self
  }
}

#[derive(Clone, PartialEq, Eq, Default, Debug)]
pub(crate) struct PDomain<'tcx>(HashMap<Place<'tcx>, Permissions>);

impl JoinSemiLattice for PDomain<'_> {
  fn join(&mut self, other: &Self) -> bool {
    let mut changed = false;
    for (place, perms) in other.0.iter() {
      match self.0.entry(*place) {
        Entry::Occupied(mut entry) => {
          changed |= entry.get_mut().join(perms);
        }
        Entry::Vacant(entry) => {
          entry.insert(*perms);
          changed = true;
        }
      }
    }
    changed
  }
}

impl<'tcx> From<HashMap<Place<'tcx>, Permissions>> for PDomain<'tcx> {
  fn from(m: HashMap<Place<'tcx>, Permissions>) -> Self {
    PDomain(m)
  }
}

impl<'tcx> Deref for PDomain<'tcx> {
  type Target = HashMap<Place<'tcx>, Permissions>;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl DerefMut for PDomain<'_> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

impl<C> DebugWithContext<C> for Permissions {
  fn fmt_diff_with(
    &self,
    old: &Self,
    _ctxt: &C,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    let mut fmt_field = |old_field: bool, self_field: bool, letter: char| {
      if old_field == self_field {
        if self_field {
          write!(f, "{letter}")
        } else {
          Ok(())
        }
      } else if !old_field && self_field {
        write!(f, "<font color=\"darkgreen\">+{letter}</font>")
      } else {
        write!(f, "<font color=\"red\">-{letter}</font>")
      }
    };

    fmt_field(old.read, self.read, 'R')?;
    fmt_field(old.write, self.write, 'W')?;
    fmt_field(old.drop, self.drop, 'D')?;

    Ok(())
  }
}

impl<C> DebugWithContext<C> for PDomain<'_> {
  fn fmt_with(
    &self,
    _ctxt: &C,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    write!(
      f,
      r#"<table border="0" cellborder="1" cellspacing="0" cellpadding="2">"#
    )?;
    for (place, perms) in self.iter() {
      write!(
        f,
        r#"<tr><td align="left">{place:?}</td><td align="left">{perms:?}</td></tr>"#
      )?;
    }
    write!(f, "</table>")?;

    Ok(())
  }

  fn fmt_diff_with(
    &self,
    old: &Self,
    ctxt: &C,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    if old == self {
      return Ok(());
    }

    write!(
      f,
      r#"<table border="0" cellborder="1" cellspacing="0" cellpadding="2" sides="rb">"#
    )?;
    for (place, perms) in self.0.iter() {
      match old.0.get(place) {
        Some(old_perms) => {
          if perms != old_perms {
            write!(
              f,
              r#"<tr><td align="left">{place:?}</td><td align="left">"#
            )?;
            perms.fmt_diff_with(old_perms, ctxt, f)?;
            write!(f, "</td></tr>")?;
          }
        }
        None => {
          write!(
            f,
            r#"<tr><td align="left"><font color="darkgreen">{place:?}</font></td><td align="left">{perms:?}</td></tr>"#
          )?;
        }
      }
    }
    write!(f, "</table>")?;
    Ok(())
  }
}

// --------------------------------------------------
// Analysis

pub(crate) struct PAnalysis<'a, 'tcx> {
  ctxt: &'a PermissionsCtxt<'a, 'tcx>,
}

impl<'a, 'tcx> PAnalysis<'a, 'tcx> {
  pub fn max_permissions(&self, place: Place<'tcx>) -> Permissions {
    let path = self.ctxt.place_to_path(&place);
    self.ctxt.max_permissions(path)
  }

  fn check_location(&self, state: &mut PDomain<'tcx>, location: Location) {
    let point = self.ctxt.location_to_point(location);
    for (place, perms) in state.iter_mut() {
      // Reset permissions to their max
      *perms = self.max_permissions(*place);
      let path = self.ctxt.place_to_path(place);
      let perm = self.ctxt.permissions_at_point(path, point);
      perms.join(&perm);
    }
  }
}

impl<'tcx> AnalysisDomain<'tcx> for PAnalysis<'_, 'tcx> {
  type Domain = PDomain<'tcx>;
  const NAME: &'static str = "PermissionsAnalysisDatalog";

  fn bottom_value(
    &self,
    _body: &rustc_middle::mir::Body<'tcx>,
  ) -> Self::Domain {
    self.ctxt.initial_body_permissions().into()
  }

  fn initialize_start_block(
    &self,
    _body: &rustc_middle::mir::Body<'tcx>,
    _state: &mut Self::Domain,
  ) {
  }
}

impl<'tcx> Analysis<'tcx> for PAnalysis<'_, 'tcx> {
  fn apply_statement_effect(
    &self,
    state: &mut Self::Domain,
    _statement: &rustc_middle::mir::Statement<'tcx>,
    location: rustc_middle::mir::Location,
  ) {
    self.check_location(state, location);
  }

  fn apply_terminator_effect(
    &self,
    state: &mut Self::Domain,
    _terminator: &rustc_middle::mir::Terminator<'tcx>,
    location: rustc_middle::mir::Location,
  ) {
    self.check_location(state, location);
  }

  fn apply_call_return_effect(
    &self,
    _state: &mut Self::Domain,
    _block: rustc_middle::mir::BasicBlock,
    _return_places: rustc_mir_dataflow::CallReturnPlaces<'_, 'tcx>,
  ) {
  }
}
