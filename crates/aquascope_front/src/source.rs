use aquascope::{
  analysis::compute_context,
  mir::borrowck_facts::{self, get_body_with_borrowck_facts},
  source_map::find_bodies,
};
use serde::{Deserialize, Serialize};

use crate::{
  plugin::AquascopeResult,
  style::{self, Style},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SourceOutput {
  pub filename: String,
  pub styles: Vec<Style>,
  pub enriched_toks: Vec<Vec<(String, u8)>>,
}

struct Callbacks {
  filename: String,
  output: Option<SourceOutput>,
}

impl rustc_driver::Callbacks for Callbacks {
  fn config(&mut self, config: &mut rustc_interface::Config) {
    config.override_queries = Some(borrowck_facts::override_queries);
  }

  // fn after_parsing<'tcx>(
  //   &mut self,
  //   compiler: &rustc_interface::interface::Compiler,
  //   queries: &'tcx rustc_interface::Queries<'tcx>,
  // ) -> rustc_driver::Compilation {
  //   log::debug!("Highlighting code in {}", self.filename);

  //   let (styled_source_toks, styles) =
  //     style::stylize_source(self.filename.clone(), Vec::default());

  //   self.output = Some(SourceOutput {
  //     filename: self.filename.clone(),
  //     styles,
  //     enriched_toks: styled_source_toks,
  //   });

  //   rustc_driver::Compilation::Stop
  // }

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

        compute_context(tcx, body_id, body_w_facts);
      });

      // let spans = find_bodies(tcx).into_iter().map(|(span, _)| span);

      // log::debug!("All spans {spans:?}");
      // let source_map = compiler.session().source_map();
      // let source_file = Range {
      //   byte_start: 0,
      //   byte_end: 0,
      //   char_start: 0,
      //   char_end: 0,
      //   filename: self.filename.clone(),
      // }
      // .source_file(source_map)
      // .unwrap();

      // let spans = spans
      //   .into_iter()
      //   .filter(|span| {
      //     source_map.lookup_source_file(span.lo()).name_hash
      //       == source_file.name_hash
      //   })
      //   .filter_map(|span| Range::from_span(span, source_map).ok())
      //   .collect::<Vec<_>>();
      // self.output = Some(SpansOutput { spans });
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
