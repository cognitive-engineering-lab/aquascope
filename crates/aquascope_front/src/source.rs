use anyhow::Result;
use aquascope::analysis::compute_permissions;
use flowistry::{
  mir::borrowck_facts::{self, get_body_with_borrowck_facts},
  source_map::find_bodies,
};
use serde::{Deserialize, Serialize};

use crate::plugin::AquascopeResult;

// NOTE REMOVE: `source` is no longer used and has become
// a sort of "playground" for when I'm testing things.

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SourceOutput {
  pub filename: String,
}

struct Callbacks {
  filename: String,
  output: Option<SourceOutput>,
}

impl rustc_driver::Callbacks for Callbacks {
  fn config(&mut self, config: &mut rustc_interface::Config) {
    config.override_queries = Some(borrowck_facts::override_queries);
  }

  fn after_parsing<'tcx>(
    &mut self,
    compiler: &rustc_interface::interface::Compiler,
    queries: &'tcx rustc_interface::Queries<'tcx>,
  ) -> rustc_driver::Compilation {
    queries.global_ctxt().unwrap().take().enter(|tcx| {
      let hir = tcx.hir();

      find_bodies(tcx).into_iter().for_each(|(_, body_id)| {
        let local_def_id = hir.body_owner_def_id(body_id);

        let body_w_facts = get_body_with_borrowck_facts(tcx, local_def_id);

        compute_permissions(tcx, body_id, body_w_facts);
      });
    });

    rustc_driver::Compilation::Stop
  }
}

pub fn source(
  args: &[String],
  filename: String,
) -> AquascopeResult<SourceOutput> {
  log::debug!("Running source for {filename}");
  log::debug!("with args {args:?}");

  let mut callbacks = Callbacks {
    filename,
    output: None,
  };
  crate::plugin::run_with_callbacks(args, &mut callbacks)?;
  Ok(callbacks.output.unwrap())
}
