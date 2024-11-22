use std::{path::PathBuf, process::Command};

use anyhow::{bail, Context, Result};

const TOOLCHAIN_TOML: &str = include_str!("../rust-toolchain.toml");

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
