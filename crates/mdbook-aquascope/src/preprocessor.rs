//! Parser for Aquascope code blocks within Markdown.

use std::{
  collections::HashMap,
  fmt::Write,
  fs,
  path::PathBuf,
  process::{Command, Stdio},
  sync::RwLock,
  time::Duration,
};

use anyhow::{bail, Result};
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
    let run_and_get_output = |cmd: &mut Command| -> Result<String> {
      let output = cmd.output()?;
      if !output.status.success() {
        bail!("Command failed");
      }
      let stdout = String::from_utf8(output.stdout)?;
      Ok(stdout.trim_end().to_string())
    };

    let miri_sysroot = aquascope_workspace_utils::miri_sysroot()?;

    let output = run_and_get_output(Command::new("rustup").args([
      "which",
      "--toolchain",
      &aquascope_workspace_utils::toolchain()?,
      "rustc",
    ]))?;
    let rustc = PathBuf::from(output);

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
        .env("DYLD_LIBRARY_PATH", &self.target_libdir)
        .env("RUST_BACKTRACE", "1")
        .current_dir(root.join("example"));

      let should_fail = block.config.iter().any(|(k, _)| k == "shouldFail");
      if should_fail {
        cmd.arg("--should-fail");
      }

      cmd.arg(operation);

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
      if let Some(err) = response_json.get("Err") {
        let stderr = String::from_utf8(output.stderr)?;
        bail!(
          "Aquascope failed for program:\n{}\nwith error: {}\n{stderr}",
          block.code,
          err.as_str().unwrap()
        )
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
    let response = match cached_response {
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

    let mut html = String::from(r#"<div class="aquascope-embed""#);

    let mut add_data = |k: &str, v: &str| {
      write!(
        html,
        " data-{}=\"{}\" ",
        k,
        html_escape::encode_double_quoted_attribute(v)
      )
    };

    add_data("code", &serde_json::to_string(&block.code)?)?;
    add_data("annotations", &serde_json::to_string(&block.annotations)?)?;
    add_data("operations", &serde_json::to_string(&block.operations)?)?;
    add_data("responses", response.trim_end())?;
    let config = block
      .config
      .iter()
      .map(|(k, v)| (k, v))
      .collect::<HashMap<_, _>>();
    add_data("config", &serde_json::to_string(&config)?)?;

    // TODO: make this configurable
    add_data("no-interact", "true")?;

    write!(html, "></div>")?;

    Ok(html)
  }

  pub fn replacements(
    &self,
    content: &str,
  ) -> Result<Vec<(std::ops::Range<usize>, String)>> {
    let to_process = AquascopeBlock::parse_all(content);
    let replacements = to_process
      .into_par_iter()
      .map(|(range, block)| {
        let html = self.process_code(block)?;
        Ok((range, html))
      })
      .collect::<Result<Vec<_>>>()?;

    Ok(replacements)
  }

  pub fn save_cache(&mut self) {
    self.cache.write().unwrap().save().unwrap();
  }
}
