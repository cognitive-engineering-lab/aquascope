use anyhow::{bail, Result};
use mdbook_preprocessor_utils::{
  mdbook::preprocess::PreprocessorContext, Asset, SimplePreprocessor,
};
use nom::{
  bytes::complete::{tag, take_until},
  character::complete::{anychar, char, none_of},
  multi::many0,
  sequence::preceded,
  IResult,
};
use nom_locate::LocatedSpan;
use std::{
  fmt::Write,
  fs,
  path::{Path, PathBuf},
  process::Command,
};
use tempfile::tempdir;

mdbook_preprocessor_utils::asset_generator!("../../../frontend/packages/aquascope-embed/dist/");

const FRONTEND_ASSETS: [Asset; 2] = [make_asset!("lib.js"), make_asset!("lib.css")];

struct AquascopePreprocessor {
  miri_sysroot: PathBuf,
  target_libdir: PathBuf,
}

fn strip_markers(code: &str) -> String {
  code.replace("`[", "").replace("]`", "")
}

impl AquascopePreprocessor {
  fn process_code(&self, header: &[String], code: &str) -> Result<String> {
    let tempdir = tempdir()?;
    let root = tempdir.path();
    let status = Command::new("cargo")
      .args(["new", "--bin", "example"])
      .current_dir(root)
      .status()?;
    if !status.success() {
      bail!("Cargo failed");
    }

    let cleaned = strip_markers(code);
    fs::write(root.join("example/src/main.rs"), cleaned)?;

    let output = Command::new("cargo")
      .args(["aquascope", "interpret"])
      .env("SYSROOT", &self.miri_sysroot)
      .env("DYLD_LIBRARY_PATH", &self.target_libdir)
      .current_dir(root.join("example"))
      .output()?;
    if !output.status.success() {
      bail!("Aquascope failed: {}", String::from_utf8(output.stderr)?)
    }

    let response = String::from_utf8(output.stdout)?;
    let logs = String::from_utf8(output.stderr)?;
    eprintln!("{logs}");

    let mut html = String::from(r#"<div class="aquascope-embed""#);

    let mut add_data = |k: &str, v: &str| {
      write!(
        html,
        " data-{}=\"{}\" ",
        k,
        html_escape::encode_double_quoted_attribute(v)
      )
    };

    add_data("code", &serde_json::to_string(code)?)?;
    add_data("operation", &header[0])?;
    add_data("response", response.trim_end())?;

    write!(html, "></div>")?;

    Ok(html)
  }
}

fn parse_aquascope_block(
  i: LocatedSpan<&str>,
) -> IResult<LocatedSpan<&str>, (Vec<String>, String)> {
  let (i, _) = tag("```aquascope")(i)?;
  let (i, header) = many0(preceded(char(','), many0(none_of(",\n"))))(i)?;
  let (i, code) = take_until("```")(i)?;
  let (i, _) = tag("```")(i)?;
  let header = header
    .into_iter()
    .map(|v| v.into_iter().collect::<String>())
    .collect::<Vec<_>>();
  let code = code.fragment().trim().to_string();
  Ok((i, (header, code)))
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

    // TODO: read this from rust-toolchain.toml
    const TOOLCHAIN: &str = "nightly-2022-12-07";
    let output = run_and_get_output(Command::new("cargo").args([
      &format!("+{TOOLCHAIN}"),
      "miri",
      "setup",
      "--print-sysroot",
    ]))?;
    let miri_sysroot = PathBuf::from(output);

    let output = run_and_get_output(Command::new("rustup").args([
      "which",
      "--toolchain",
      TOOLCHAIN,
      "rustc",
    ]))?;
    let rustc = PathBuf::from(output);

    let output = run_and_get_output(Command::new(&rustc).args(["--print", "target-libdir"]))?;
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
    let mut replacements = Vec::new();
    let mut content = LocatedSpan::new(content);
    loop {
      if let Ok((next, (header, code))) = parse_aquascope_block(content) {
        let html = self.process_code(&header, &code)?;
        let range = content.location_offset()..next.location_offset();
        replacements.push((range, html));
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
