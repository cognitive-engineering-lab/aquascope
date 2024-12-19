//! Parser for Aquascope code blocks within Markdown.

use std::{
  collections::HashMap,
  fs,
  path::PathBuf,
  process::{Command, Stdio},
  sync::RwLock,
  time::Duration,
};

use anyhow::{bail, Result};
use aquascope_workspace_utils::{miri_sysroot, run_and_get_output, rustc};
use mdbook_preprocessor_utils::HtmlElementBuilder;
use rayon::prelude::*;
use tempfile::tempdir;
use wait_timeout::ChildExt;

use crate::{block::AquascopeBlock, cache::Cache};

pub struct AquascopePreprocessor {
  miri_sysroot: PathBuf,
  target_libdir: PathBuf,
  cache: RwLock<Cache<AquascopeBlock, String>>,
}

impl AquascopePreprocessor {
  pub fn new() -> Result<Self> {
    let miri_sysroot = miri_sysroot()?;
    let rustc = rustc()?;
    let output = run_and_get_output(
      Command::new(rustc).args(["--print", "target-libdir"]),
    )?;
    let target_libdir = PathBuf::from(output);

    let cache = RwLock::new(Cache::load()?);
    Ok(AquascopePreprocessor {
      miri_sysroot,
      target_libdir,
      cache,
    })
  }

  /// Runs cargo-aquascope on code from a given Aquascope block.
  fn run_aquascope(&self, block: &AquascopeBlock) -> Result<String> {
    // TODO: this code shares a lot of structure w/ aquascope_serve.
    // Can we unify them?
    let tempdir = tempdir()?;
    let root = tempdir.path();
    let status = Command::new("cargo")
      .args(["new", "--bin", "example"])
      .current_dir(root)
      .stdout(Stdio::null())
      .stderr(Stdio::null())
      .status()?;
    if !status.success() {
      bail!("Cargo failed");
    }

    fs::write(root.join("example/src/main.rs"), &block.code)?;

    let mut responses = HashMap::new();
    for operation in &block.operations {
      let mut cmd = Command::new("cargo");
      cmd
        .arg("aquascope")
        .env("SYSROOT", &self.miri_sysroot)
        .env("MIRI_SYSROOT", &self.miri_sysroot)
        .env("DYLD_LIBRARY_PATH", &self.target_libdir)
        .env("LD_LIBRARY_PATH", &self.target_libdir)
        .env("RUST_BACKTRACE", "1")
        .current_dir(root.join("example"));

      let should_fail = block.config.iter().any(|(k, _)| k == "shouldFail");
      if should_fail {
        cmd.arg("--should-fail");
      }

      cmd.arg(operation);

      let show_flows = block.config.iter().any(|(k, _)| k == "showFlows");
      if show_flows {
        cmd.arg("--show-flows");
      }

      let mut child =
        cmd.stdout(Stdio::piped()).stderr(Stdio::piped()).spawn()?;
      if child.wait_timeout(Duration::from_secs(10))?.is_none() {
        child.kill()?;
        bail!("Aquascope timed out on program:\n{}", block.code)
      };

      let output = child.wait_with_output()?;

      if !output.status.success() {
        let error = String::from_utf8(output.stderr)?;
        bail!(
          "Aquascope failed for program:\n{}\nwith error:\n{error}",
          block.code
        )
      }

      let response = String::from_utf8(output.stdout)?;
      let response_json: serde_json::Value = serde_json::from_str(&response)?;
      let is_err = match (response_json.as_object(), response_json.as_array()) {
        (Some(obj), _) => obj.get("Err").is_some(),
        (_, Some(arr)) => arr.iter().any(|obj| obj.get("Err").is_some()),
        _ => false,
      };
      if is_err {
        let stderr = String::from_utf8(output.stderr)?;
        bail!(
          "Aquascope failed for program:\n{}\nwith error:\n{stderr}",
          block.code,
        )
      }

      if let Some("BuildError") =
        response_json.get("type").and_then(|ty| ty.as_str())
      {
        bail!("Aquascope failed for program:\n{}", block.code)
      }

      responses.insert(operation, response_json);
    }

    Ok(serde_json::to_string(&responses)?)
  }

  /// Get the HTML output for an Aquascope block
  fn process_code(&self, block: AquascopeBlock) -> Result<String> {
    let cached_response = {
      let cache = self.cache.read().unwrap();
      cache.get(&block).cloned()
    };
    let response_str = match cached_response {
      Some(response) => response,
      None => {
        let response = self.run_aquascope(&block)?;
        self
          .cache
          .write()
          .unwrap()
          .set(block.clone(), response.clone());
        response
      }
    };
    let response: serde_json::Value =
      serde_json::from_str(response_str.trim_end())?;

    let mut html = HtmlElementBuilder::new();
    html
      .attr("class", "aquascope-embed")
      .data("code", &block.code)?
      .data("annotations", &block.annotations)?
      .data("operations", &block.operations)?
      .data("responses", response)?;

    let config = block
      .config
      .iter()
      .map(|(k, v)| (k, v))
      .collect::<HashMap<_, _>>();
    html.data("config", &config)?;

    // TODO: make this configurable
    html.data("no-interact", true)?;

    // TODO: add a code path to enable this from config
    // add_data("show-bug-reporter", true)?;

    Ok(html.finish())
  }

  pub fn replacements(
    &self,
    content: &str,
  ) -> Result<Vec<(std::ops::Range<usize>, String)>> {
    let to_process = AquascopeBlock::parse_all(content);
    to_process
      .into_par_iter()
      .map(|(range, block)| {
        let html = self.process_code(block)?;
        Ok((range, html))
      })
      .chain(crate::permissions::parse_perms(content).par_bridge())
      .collect()
  }

  pub fn save_cache(&mut self) {
    self.cache.write().unwrap().save().unwrap();
  }
}
