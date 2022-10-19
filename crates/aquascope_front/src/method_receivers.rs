use aquascope::{
  analysis::{self, CallTypes},
  mir::borrowck_facts::get_body_with_borrowck_facts,
  source_map::{find_bindings, find_bodies, find_method_calls, Range},
};
use serde::Serialize;
use ts_rs::TS;

use crate::plugin::AquascopeResult;

#[derive(Serialize, TS)]
#[ts(export)]
pub struct ReceiverTypesOutput {
  call_info: Vec<CallTypes>,
}

struct Callbacks {
  filename: String,
  output: Option<ReceiverTypesOutput>,
}

impl rustc_driver::Callbacks for Callbacks {
  fn after_parsing<'tcx>(
    &mut self,
    compiler: &rustc_interface::interface::Compiler,
    queries: &'tcx rustc_interface::Queries<'tcx>,
  ) -> rustc_driver::Compilation {
    queries.global_ctxt().unwrap().take().enter(|tcx| {
      let hir_call_ids = find_method_calls(tcx);
      let body_ids = find_bodies(tcx);
      let bindings = find_bindings(tcx);
      let hir = tcx.hir();

      log::debug!("Found program bindings: {:?}", bindings);

      let source_map = compiler.session().source_map();
      let source_file = Range {
        byte_start: 0,
        byte_end: 0,
        char_start: 0,
        char_end: 0,
        filename: self.filename.clone(),
      }
      .source_file(source_map)
      .unwrap();

      let call_info: Vec<CallTypes> = hir_call_ids
        .iter()
        .flat_map(|(parent, calls)| {
          let body_id = hir.body_owned_by(*parent);

          analysis::process_method_calls_in_item(
            tcx,
            body_id,
            *parent,
            calls.to_vec(),
            &bindings,
            |span| Range::from_span(span, source_map).ok().unwrap_or_default(),
          )

          // For the given parts in the method call.
          // We want the DefId in order to get the type of it.
          // That is, for the first argument to the Call (the receiver)
          // we want to know where that's defined.
          // For the function itself, we want to know where it's defined.
        })
        .collect();

      self.output = Some(ReceiverTypesOutput { call_info });
    });

    rustc_driver::Compilation::Stop
  }
}

pub fn method_calls(
  args: &[String],
  filename: String,
) -> AquascopeResult<ReceiverTypesOutput> {
  let mut callbacks = Callbacks {
    filename,
    output: None,
  };
  crate::plugin::run_with_callbacks(args, &mut callbacks)?;
  Ok(callbacks.output.unwrap())
}
