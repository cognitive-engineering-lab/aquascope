//! Various *debugging* utilities for permissions.

use std::{collections::hash_map::Entry, io::Write, path::Path};

use anyhow::{bail, Result};
use rustc_data_structures::fx::FxHashMap as HashMap;
use rustc_graphviz as dot;
use rustc_hir::def_id::DefId;
use rustc_middle::{
  mir::{Body, Location},
  ty::TyCtxt,
};
use rustc_mir_dataflow::{
  fmt::DebugWithContext, Analysis, AnalysisDomain, JoinSemiLattice, Results,
};

use super::{
  context::PermissionsCtxt, graphviz, Permissions, PermissionsData,
  PermissionsDomain,
};

pub(crate) fn dump_permissions_with_mir(ctxt: &PermissionsCtxt) {
  // XXX: Unfortunately, the only way I know how to do this is to do a MIR
  // dataflow analysis and simply take the information from the context.
  // This mean there will only be a single pass but :shrug:

  let def_id = ctxt.tcx.hir().body_owner_def_id(ctxt.body_id);

  // Only print the analysis on a specific function
  let owner = ctxt.tcx.hir().body_owner(ctxt.body_id);
  let Some(name) = ctxt.tcx.hir().opt_name(owner) else { return };
  if name.as_str() != "dump_me" {
    return;
  }

  let analysis = PAnalysis { ctxt };
  let results = analysis
    .into_engine(ctxt.tcx, &ctxt.body_with_facts.body)
    .iterate_to_fixpoint();

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

pub(crate) fn dump_mir_debug(ctxt: &PermissionsCtxt) {
  let tcx = ctxt.tcx;
  let body = &ctxt.body_with_facts.body;
  let _basic_blocks = body.basic_blocks.indices();

  let mut stderr = std::io::stderr();
  rustc_middle::mir::pretty::write_mir_fn(
    tcx,
    body,
    &mut |_, _| Ok(()),
    &mut stderr,
  )
  .unwrap();
}

fn write_dot(path: &Path, buf: Vec<u8>) -> Result<()> {
  use std::process::{Command, Stdio};

  // let mut file = File::create(path)?;
  // file.write_all(&buf)?;
  // file.sync_all()?;

  let mut p = Command::new("dot")
    .args(["-Tpdf", "-o", &path.display().to_string()])
    .stdin(Stdio::piped())
    .spawn()?;

  p.stdin.as_mut().unwrap().write_all(&buf)?;
  let status = p.wait()?;

  if !status.success() {
    bail!("dot for {} failed", path.display())
  };

  Ok(())
}

fn dump_results<'tcx, A>(
  body: &Body<'tcx>,
  results: &Results<'tcx, A>,
  _def_id: DefId,
  _tcx: TyCtxt<'tcx>,
) -> Result<()>
where
  A: Analysis<'tcx>,
  A::Domain: DebugWithContext<A>,
{
  let graphviz =
    graphviz::Formatter::new(body, results, graphviz::OutputStyle::AfterOnly);
  let mut buf = Vec::new();
  dot::render(&graphviz, &mut buf)?;

  let output_dir = Path::new("target");
  let fname = "results";
  let output_path = output_dir.join(format!("{fname}.pdf"));

  write_dot(&output_path, buf)
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

impl JoinSemiLattice for PermissionsDomain<'_> {
  fn join(&mut self, other: &Self) -> bool {
    let mut changed = false;
    for (place, perms) in other.0.iter() {
      match self.0.entry(*place) {
        Entry::Occupied(mut entry) => {
          changed |= entry.get_mut().permissions.join(&perms.permissions);
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
        write!(f, r#"<font color="darkgreen">+{letter}</font>"#)
      } else {
        write!(f, r#"<font color="red">-{letter}</font>"#)
      }
    };

    fmt_field(old.read, self.read, 'R')?;
    fmt_field(old.write, self.write, 'W')?;
    fmt_field(old.drop, self.drop, 'D')?;

    Ok(())
  }
}

impl<C> DebugWithContext<C> for PermissionsDomain<'_> {
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
      let perms = perms.permissions;
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
    let no_perm_changes = self.0.iter().all(|(place, permsd)| {
      let perms = permsd.permissions;
      old
        .0
        .get(place)
        .map_or(true, |permd| permd.permissions == perms)
    });

    if old == self || no_perm_changes {
      return Ok(());
    }

    write!(
      f,
      r#"<table border="0" cellborder="1" cellspacing="0" cellpadding="2" sides="rb">"#
    )?;
    for (place, perms) in self.0.iter() {
      let perms = perms.permissions;
      match old.0.get(place) {
        Some(old_perms) => {
          let old_perms = old_perms.permissions;
          if perms != old_perms {
            write!(
              f,
              r#"<tr><td align="left">{place:?}</td><td align="left">"#
            )?;
            perms.fmt_diff_with(&old_perms, ctxt, f)?;
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
  fn check_location(
    &self,
    state: &mut PermissionsDomain<'tcx>,
    location: Location,
  ) {
    let point = self.ctxt.location_to_point(location);
    let dmn = self.ctxt.permissions_domain_at_point(point);
    for (place, perms) in state.iter_mut() {
      let new_perms = dmn.get(place).unwrap();
      *perms = *new_perms;
    }
  }
}

impl<'tcx> AnalysisDomain<'tcx> for PAnalysis<'_, 'tcx> {
  type Domain = PermissionsDomain<'tcx>;
  const NAME: &'static str = "PermissionsAnalysisDatalog";

  fn bottom_value(
    &self,
    _body: &rustc_middle::mir::Body<'tcx>,
  ) -> Self::Domain {
    self
      .ctxt
      .domain_places()
      .into_iter()
      .map(|place| {
        let path = self.ctxt.place_to_path(&place);
        let mp = self.ctxt.max_permissions(path);
        // NOTE: I'm currently just ignoring the permissions data
        // for this utility just so we can see the permissions changes.
        (place, PermissionsData {
          is_live: false,
          type_droppable: false,
          type_writeable: false,
          type_copyable: false,
          path_moved: None,
          path_uninitialized: false,
          loan_read_refined: None,
          loan_write_refined: None,
          loan_drop_refined: None,
          permissions: mp,
        })
      })
      .collect::<HashMap<_, _>>()
      .into()
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
