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
  // HACK in theory, the source fontifier should be able to handle any source
  // and running the parsing first is a waste of time.
  fn after_parsing<'tcx>(
    &mut self,
    compiler: &rustc_interface::interface::Compiler,
    queries: &'tcx rustc_interface::Queries<'tcx>,
  ) -> rustc_driver::Compilation {
    log::debug!("Highlighting code in {}", self.filename);

    let (styled_source_toks, styles) =
      style::stylize_source(self.filename.clone(), Vec::default());

    self.output = Some(SourceOutput {
      filename: self.filename.clone(),
      styles,
      enriched_toks: styled_source_toks,
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
