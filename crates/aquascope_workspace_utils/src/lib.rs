use std::{path::PathBuf, process::Command};

use anyhow::{bail, Context, Result};

const TOOLCHAIN_TOML: &str = include_str!("../rust-toolchain.toml");

pub fn run_and_get_output(cmd: &mut Command) -> Result<String> {
  let output = cmd.output()?;
  if !output.status.success() {
    bail!(
      "Command failed with stderr:\n{}",
      String::from_utf8(output.stderr).unwrap()
    );
  }
  let stdout = String::from_utf8(output.stdout)?;
  Ok(stdout.trim_end().to_string())
}

pub fn rustc() -> Result<PathBuf> {
  if let Ok(rustc) = std::env::var("RUSTC_PATH") {
    return Ok(PathBuf::from(rustc));
  }

  if let Ok(toolchain) = toolchain() {
    let output = run_and_get_output(Command::new("rustup").args([
      "which",
      "--toolchain",
      &toolchain,
      "rustc",
    ]))?;
    Ok(PathBuf::from(output))
  } else {
    let output = run_and_get_output(Command::new("which").arg("rustc"))?;
    Ok(PathBuf::from(output))
  }
}

pub fn toolchain() -> Result<String> {
  let config: toml::Value = toml::from_str(TOOLCHAIN_TOML)?;
  Ok(
    config
      .get("toolchain")
      .context("Missing toolchain key")?
      .get("channel")
      .context("Missing channel key")?
      .as_str()
      .unwrap()
      .to_string(),
  )
}

pub fn miri_sysroot() -> Result<PathBuf> {
  if let Ok(sysroot) = std::env::var("MIRI_SYSROOT") {
    return Ok(sysroot.into());
  }

  let mut cmd = Command::new("cargo");
  if let Ok(toolchain) = toolchain() {
    cmd.arg(format!("+{}", toolchain));
  }

  let output = cmd.args(["miri", "setup", "--print-sysroot"]).output()?;
  if !output.status.success() {
    bail!("Command failed");
  }
  let stdout = String::from_utf8(output.stdout)?;
  Ok(PathBuf::from(stdout.trim_end()))
}
