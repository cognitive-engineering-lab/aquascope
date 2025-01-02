//! Various *debugging* utilities for permissions.

use std::collections::hash_map::Entry;

use rustc_middle::mir::pretty::PrettyPrintMirOptions;
use rustc_mir_dataflow::{fmt::DebugWithContext, JoinSemiLattice};

use super::{context::PermissionsCtxt, Permissions, PermissionsDomain};

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
    PrettyPrintMirOptions {
      include_extra_comments: false,
    },
  )
  .unwrap();

  log::debug!("{:?}", ctxt.polonius_output);
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
          changed |= entry.get_mut().permissions().join(&perms.permissions());
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
      let perms = perms.permissions();
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
      let perms = permsd.permissions();
      old
        .0
        .get(place)
        .is_none_or(|permd| permd.permissions() == perms)
    });

    if old == self || no_perm_changes {
      return Ok(());
    }

    write!(
      f,
      r#"<table border="0" cellborder="1" cellspacing="0" cellpadding="2" sides="rb">"#
    )?;
    for (place, perms) in self.0.iter() {
      let perms = perms.permissions();
      match old.0.get(place) {
        Some(old_perms) => {
          let old_perms = old_perms.permissions();
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
