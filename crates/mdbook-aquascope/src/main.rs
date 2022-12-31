use anyhow::{bail, Result};
use mdbook_preprocessor_utils::{
  mdbook::preprocess::PreprocessorContext, Asset, SimplePreprocessor,
};
use regex::Regex;
use std::{fmt::Write, fs, path::Path, process::Command};
use tempfile::tempdir;

mdbook_preprocessor_utils::asset_generator!("../../../frontend/packages/aquascope-embed/dist/");

const FRONTEND_ASSETS: [Asset; 2] = [make_asset!("lib.js"), make_asset!("lib.css")];

struct AquascopePreprocessor {
  regex: Regex,
}

impl AquascopePreprocessor {
  fn process_code(&self, operation: &str, code: &str) -> Result<String> {
    let tempdir = tempdir()?;
    let root = tempdir.path();
    let status = Command::new("cargo")
      .args(["new", "--bin", "example"])
      .current_dir(root)
      .status()?;
    if !status.success() {
      bail!("Cargo failed");
    }

    fs::write(root.join("example/src/main.rs"), code)?;

    let output = Command::new("cargo")
      .args(["aquascope", "interpret"])
      .current_dir(root.join("example"))
      .output()?;
    if !output.status.success() {
      bail!("Aquascope failed: {}", String::from_utf8(output.stderr)?)
    }

    let response = String::from_utf8(output.stdout)?;

    let mut html = String::from(r#"<pre class="aquascope""#);

    let mut add_data = |k: &str, v: &str| {
      write!(
        html,
        " data-{}=\"{}\" ",
        k,
        html_escape::encode_double_quoted_attribute(v)
      )
    };

    add_data("operation", operation)?;
    add_data("response", &response)?;

    write!(html, ">{code}</pre>")?;

    Ok(html)
  }
}

impl SimplePreprocessor for AquascopePreprocessor {
  fn name() -> &'static str {
    "aquascope"
  }

  fn build(_ctx: &PreprocessorContext) -> Result<Self> {
    let regex = Regex::new("```aquascope,?(.*)\n((?s).*)\n```")?;
    Ok(AquascopePreprocessor { regex })
  }

  fn replacements(
    &self,
    _chapter_dir: &Path,
    content: &str,
  ) -> Result<Vec<(std::ops::Range<usize>, String)>> {
    self
      .regex
      .captures_iter(content)
      .map(|captures| {
        let range = captures.get(0).unwrap().range();
        let operation = captures.get(1).unwrap().as_str();
        let code = captures.get(2).unwrap().as_str();
        let html = self.process_code(operation, code)?;
        Ok((range, html))
      })
      .collect()
  }

  fn linked_assets(&self) -> Vec<Asset> {
    FRONTEND_ASSETS.to_vec()
  }

  fn all_assets(&self) -> Vec<Asset> {
    self.linked_assets()
  }
}

fn main() {
  mdbook_preprocessor_utils::main::<AquascopePreprocessor>()
}
