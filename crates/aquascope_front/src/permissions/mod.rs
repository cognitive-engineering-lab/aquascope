use anyhow::Result;
use aquascope::{
  analysis::{self, PermissionsInfo},
  Range,
};
use flowistry::{
  mir::{
    borrowck_facts::{self, get_body_with_borrowck_facts},
    utils::SpanExt,
  },
  source_map::{self, find_bodies},
};
use rustc_hir::BodyId;
use rustc_middle::ty::TyCtxt;
use serde::Serialize;
use ts_rs::TS;

use crate::plugin::AquascopeResult;

#[derive(Debug, Clone, Serialize, TS)]
#[ts(export)]
pub struct PermissionsOutput(Vec<PermissionsInfo>);

struct Callbacks {
  filename: String,
  output: Vec<Vec<PermissionsInfo>>,
}

impl rustc_driver::Callbacks for Callbacks {
  fn config(&mut self, config: &mut rustc_interface::Config) {
    config.override_queries = Some(borrowck_facts::override_queries);
  }

  fn after_parsing<'tcx>(
    &mut self,
    _compiler: &rustc_interface::interface::Compiler,
    queries: &'tcx rustc_interface::Queries<'tcx>,
  ) -> rustc_driver::Compilation {
    queries.global_ctxt().unwrap().take().enter(|tcx| {
      find_bodies(tcx).into_iter().for_each(|(_, body_id)| {
        let def_id = tcx.hir().body_owner_def_id(body_id);
        let body_with_facts = get_body_with_borrowck_facts(tcx, def_id);
        let permissions_results =
          &analysis::compute_permissions(tcx, body_id, body_with_facts);

        let source_map = tcx.sess.source_map();
        // let source_file = source_map::Range {
        //   byte_start: 0,
        //   byte_end: 0,
        //   char_start: 0,
        //   char_end: 0,
        //   filename: self.filename.clone(),
        // }
        // .source_file(source_map)
        // .unwrap();

        let call_infos = analysis::pair_permissions_to_calls(
          tcx,
          body_id,
          body_with_facts,
          permissions_results,
          |span| {
            source_map::Range::from_span(span, source_map)
              .ok()
              .unwrap_or_default()
              .into()
          },
        );

        self.output.push(call_infos);
      });
    });

    rustc_driver::Compilation::Stop
  }
}

pub fn permissions(
  args: &[String],
  filename: String,
) -> AquascopeResult<PermissionsOutput> {
  let mut callbacks = Callbacks {
    filename,
    output: Vec::default(),
  };
  crate::plugin::run_with_callbacks(args, &mut callbacks)?;
  Ok(PermissionsOutput(
    callbacks.output.into_iter().flatten().collect(),
  ))
}
