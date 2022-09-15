use serde::Serialize;
use syntect::highlighting::Style;

use crate::{plugin::AquascopeResult, style};

#[derive(Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct SourceOutput {
  filename: String,
  enriched_toks: Vec<Vec<(Style, String)>>,
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

    let stylized_source_toks =
      style::stylize_source(self.filename.clone(), Vec::default());

    self.output = Some(SourceOutput {
      filename: self.filename.clone(),
      enriched_toks: stylized_source_toks,
    });

    rustc_driver::Compilation::Stop
  }
}

pub fn source(
  args: &[String],
  filename: String,
) -> AquascopeResult<SourceOutput> {
  let mut callbacks = Callbacks {
    filename,
    output: None,
  };
  crate::plugin::run_with_callbacks(args, &mut callbacks)?;
  Ok(callbacks.output.unwrap())
}
