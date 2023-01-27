#![feature(iter_intersperse)]

use std::{
  collections::HashMap,
  fmt::Write,
  fs,
  path::{Path, PathBuf},
  process::{Command, Stdio},
  sync::RwLock,
};

use anyhow::{bail, Result};
use mdbook_aquascope::cache::Cache;
use mdbook_preprocessor_utils::{
  mdbook::preprocess::PreprocessorContext, Asset, SimplePreprocessor,
};
use nom::{
  bytes::complete::{tag, take_until},
  character::complete::{anychar, char, none_of},
  multi::many0,
  sequence::{preceded, separated_pair, tuple},
  IResult,
};
use nom_locate::LocatedSpan;
use rayon::prelude::*;
use tempfile::tempdir;

mdbook_preprocessor_utils::asset_generator!(
  "../../../frontend/packages/aquascope-embed/dist/"
);

const FRONTEND_ASSETS: [Asset; 2] =
  [make_asset!("lib.js"), make_asset!("lib.css")];

struct AquascopePreprocessor {
  miri_sysroot: PathBuf,
  target_libdir: PathBuf,
  cache: RwLock<Cache<(String, String), String>>,
}

impl AquascopePreprocessor {
  fn run_aquascope(&self, code: &str, operation: &str) -> Result<String> {
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

    fs::write(root.join("example/src/main.rs"), code)?;

    let output = Command::new("cargo")
      .args(["aquascope", operation])
      .env("SYSROOT", &self.miri_sysroot)
      .env("DYLD_LIBRARY_PATH", &self.target_libdir)
      .env("RUST_BACKTRACE", "1")
      .current_dir(root.join("example"))
      .output()?;
    if !output.status.success() {
      let error = String::from_utf8(output.stderr)?;
      bail!("Aquascope failed for program:\n{code}\nwith error:\n{error}")
    }

    let response = String::from_utf8(output.stdout)?;
    let response_json: serde_json::Value = serde_json::from_str(&response)?;
    if let Some(err) = response_json.get("Err") {
      let stderr = String::from_utf8(output.stderr)?;
      bail!(
        "Aquascope failed for program:\n{code}\nwith error: {}\n{stderr}",
        err.as_str().unwrap()
      )
    }

    Ok(response)
  }

  fn process_code(
    &self,
    AquascopeBlock {
      operation,
      config,
      code,
    }: AquascopeBlock,
  ) -> Result<String> {
    let (cleaned, annot) =
      mdbook_aquascope::annotations::parse_annotations(&code)?;

    let key = (cleaned.clone(), operation.clone());
    let cached_response = {
      let cache = self.cache.read().unwrap();
      cache.get(&key).cloned()
    };
    let response = match cached_response {
      Some(response) => response,
      None => {
        let response = self.run_aquascope(&cleaned, &operation)?;
        self.cache.write().unwrap().set(key, response.clone());
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

    add_data("code", &serde_json::to_string(&cleaned)?)?;
    add_data("annotations", &serde_json::to_string(&annot)?)?;
    add_data("operation", &operation)?;
    add_data("response", response.trim_end())?;
    let config = config
      .iter()
      .map(|(k, v)| (k, v))
      .collect::<HashMap<_, _>>();
    add_data("config", &serde_json::to_string(&config)?)?;

    // TODO: make this configurable?
    add_data("no-interact", "true")?;

    write!(html, "></div>")?;

    Ok(html)
  }
}

struct AquascopeBlock {
  operation: String,
  config: Vec<(String, String)>,
  code: String,
}

fn parse_aquascope_block(
  i: LocatedSpan<&str>,
) -> IResult<LocatedSpan<&str>, AquascopeBlock> {
  fn parse_sym(i: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, String> {
    let (i, v) = many0(none_of(",=\n"))(i)?;
    Ok((i, v.into_iter().collect::<String>()))
  }

  let mut parser = tuple((
    tag("```aquascope"),
    preceded(char(','), parse_sym),
    many0(preceded(
      char(','),
      separated_pair(parse_sym, char('='), parse_sym),
    )),
    take_until("```"),
    tag("```"),
  ));
  let (i, (_, operation, config, code, _)) = parser(i)?;
  let code = code.fragment().trim().to_string();
  Ok((i, AquascopeBlock {
    operation,
    config,
    code,
  }))
}

impl SimplePreprocessor for AquascopePreprocessor {
  fn name() -> &'static str {
    "aquascope"
  }

  fn build(_ctx: &PreprocessorContext) -> Result<Self> {
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

  fn replacements(
    &self,
    _chapter_dir: &Path,
    content: &str,
  ) -> Result<Vec<(std::ops::Range<usize>, String)>> {
    let mut content = LocatedSpan::new(content);
    let mut to_process = Vec::new();
    loop {
      if let Ok((next, block)) = parse_aquascope_block(content) {
        let range = content.location_offset() .. next.location_offset();
        to_process.push((range, block));
        content = next;
      } else {
        match anychar::<_, nom::error::Error<LocatedSpan<&str>>>(content) {
          Ok((next, _)) => {
            content = next;
          }
          Err(_) => break,
        }
      }
    }

    let replacements = to_process
      .into_par_iter()
      .map(|(range, block)| {
        let html = self.process_code(block)?;
        Ok((range, html))
      })
      .collect::<Result<Vec<_>>>()?;

    Ok(replacements)
  }

  fn linked_assets(&self) -> Vec<Asset> {
    FRONTEND_ASSETS.to_vec()
  }

  fn all_assets(&self) -> Vec<Asset> {
    self.linked_assets()
  }

  fn finish(self) {
    self.cache.write().unwrap().save().unwrap();
  }
}

fn main() {
  mdbook_preprocessor_utils::main::<AquascopePreprocessor>()
}

#[cfg(test)]
mod test {
  use std::fs;

  use anyhow::Result;
  use mdbook_aquascope::cache::CACHE_PATH;
  use mdbook_preprocessor_utils::testing::MdbookTestHarness;

  use super::AquascopePreprocessor;

  #[test]
  fn cache_test() -> Result<()> {
    let harness = MdbookTestHarness::new()?;
    let mk_contents = |x: &str| {
      format!(
        r#"
```aquascope,interpreter
fn main() {{
  let x = {x};
}}
```
    "#
      )
    };
    let chapter_path = harness.root().join("src/chapter_1.md");
    fs::write(&chapter_path, mk_contents("0"))?;

    let _book =
      harness.compile::<AquascopePreprocessor>(serde_json::json!({}))?;

    // After running the first compile, a cache should exist
    let cache_path = harness.root().join(CACHE_PATH);
    let cache_contents = fs::read(&cache_path)?;

    // After a second compile, the cache should be unchanged
    let _book =
      harness.compile::<AquascopePreprocessor>(serde_json::json!({}))?;
    assert_eq!(fs::read(&cache_path)?, cache_contents);

    // After modifying the chapter and recompiling, the cache should be changed
    fs::write(&chapter_path, mk_contents("1"))?;
    let _book =
      harness.compile::<AquascopePreprocessor>(serde_json::json!({}))?;
    assert_ne!(fs::read(&cache_path)?, cache_contents);

    Ok(())
  }
}
