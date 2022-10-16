use aquascope::{
  analysis,
  mir::borrowck_facts::get_body_with_borrowck_facts,
  source_map::{find_bodies, find_method_calls, Range},
};
use serde::Serialize;

use crate::plugin::AquascopeResult;

#[derive(Serialize)]
pub struct ReceivierTypesOutput {}

struct Callbacks {
  filename: String,
  output: Option<ReceivierTypesOutput>,
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

      let hir = tcx.hir();

      log::debug!("CALLS {hir_call_ids:?}");
      log::debug!("BODYS {body_ids:?}");

      hir_call_ids.iter().for_each(|(parent, calls)| {
        let body_id = hir.body_owned_by(*parent);

        // Every MethodCall should have a corresponding Node in the Hir Map.

        analysis::process_method_calls_in_item(
          tcx,
          body_id,
          *parent,
          calls.to_vec(),
        );

        // For the given parts in the method call.
        // We want the DefId in order to get the type of it.
        // That is, for the first argument to the Call (the receiver)
        // we want to know where that's defined.
        // For the function itself, we want to know where it's defined.
      });
      todo!()
    })
  }
}

pub fn method_calls(
  args: &[String],
  filename: String,
) -> AquascopeResult<ReceivierTypesOutput> {
  let mut callbacks = Callbacks {
    filename,
    output: None,
  };
  crate::plugin::run_with_callbacks(args, &mut callbacks)?;
  Ok(callbacks.output.unwrap())
}
