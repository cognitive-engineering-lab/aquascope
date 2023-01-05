use std::{
  collections::HashMap,
  fmt::Write,
  fs,
  path::{Path, PathBuf},
  process::{Command, Stdio},
};

use anyhow::{bail, Result};
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
}

fn strip_markers(code: &str) -> String {
  code.replace("`[", "").replace("]`", "")
}

impl AquascopePreprocessor {
  fn process_code(
    &self,
    AquascopeBlock {
      operation,
      config,
      code,
    }: AquascopeBlock,
  ) -> Result<String> {
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

    let cleaned = strip_markers(&code);
    fs::write(root.join("example/src/main.rs"), &cleaned)?;

    let output = Command::new("cargo")
      .args(["aquascope", &operation])
      .env("SYSROOT", &self.miri_sysroot)
      .env("DYLD_LIBRARY_PATH", &self.target_libdir)
      .env("RUST_BACKTRACE", "1")
      .current_dir(root.join("example"))
      .output()?;
    if !output.status.success() {
      let error = String::from_utf8(output.stderr)?;
      bail!("Aquascope failed for program:\n{cleaned}\nwith error:\n{error}")
    }

    let response = String::from_utf8(output.stdout)?;
    // let logs = String::from_utf8(output.stderr)?;
    // eprintln!("{logs}");

    let mut html = String::from(r#"<div class="aquascope-embed""#);

    let mut add_data = |k: &str, v: &str| {
      write!(
        html,
        " data-{}=\"{}\" ",
        k,
        html_escape::encode_double_quoted_attribute(v)
      )
    };

    add_data("code", &serde_json::to_string(&code)?)?;
    add_data("operation", &operation)?;
    add_data("response", response.trim_end())?;
    let config = config
      .iter()
      .map(|(k, v)| (k, v))
      .collect::<HashMap<_, _>>();
    add_data("config", &serde_json::to_string(&config)?)?;

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

    Ok(AquascopePreprocessor {
      miri_sysroot,
      target_libdir,
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
}

fn main() {
  mdbook_preprocessor_utils::main::<AquascopePreprocessor>()
}
