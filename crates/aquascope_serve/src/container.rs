use std::{
  fs, io, os::unix::fs::PermissionsExt, path::PathBuf, str, time::Duration,
};

use aquascope_workspace_utils::miri_sysroot;
use snafu::prelude::*;
use tempfile::{tempdir, TempDir};
use tokio::{self, process::Command, time};

use crate::{ServerResponse, SingleFileRequest};

#[derive(Debug, Snafu)]
pub enum Error {
  #[snafu(display("Unable to create temporary local directory {}", source))]
  UnableToCreateTempDir { source: io::Error },
  #[snafu(display("Unable to create output directory: {}", source))]
  UnableToWriteFile { source: io::Error },
  #[snafu(display("Unable to execute local command {}", source))]
  UnableToExecCommand { source: io::Error },
  #[snafu(display("Unable to create source file: {}", source))]
  UnableToCreateSourceFile { source: io::Error },
  #[snafu(display("Command execution took longer than {} ms", timeout.as_millis()))]
  CommandExecTimedOut {
    source: time::error::Elapsed,
    timeout: Duration,
  },
  #[snafu(display("Miscellaneous error: {}", source))]
  Misc { source: anyhow::Error },
}

impl From<anyhow::Error> for Error {
  fn from(e: anyhow::Error) -> Self {
    Error::Misc { source: e }
  }
}

pub type Result<T, E = Error> = ::std::result::Result<T, E>;

const DEFAULT_IMAGE: &str = "aquascope";
const DEFAULT_PROJECT_PATH: &str = "aquascope_tmp_proj";

const COMMAND_TIMEOUT: Duration = Duration::from_secs(20);

pub struct Container {
  project_dir: Option<String>,
  workspace: TempDir,
}

impl Container {
  pub async fn new() -> Result<Self> {
    let td = tempdir().context(UnableToCreateTempDirSnafu)?;
    let mut this = Container {
      workspace: td,
      project_dir: None,
    };

    this.cargo_new().await?;

    Ok(this)
  }

  #[allow(dead_code)]
  pub async fn get_pid(&self, process: &str) -> Result<i64> {
    let mut cmd = Command::new("pidof");
    cmd.arg(process);
    let (response, _) = self.exec_output(&mut cmd).await?;
    Ok(
      response
        .split(' ')
        .next()
        .unwrap()
        .parse::<i64>()
        .unwrap_or_else(|_| {
          panic!(
            "Invalid response in get_pid:
    {response}"
          )
        }),
    )
  }

  pub async fn exec_output(
    &self,
    cmd: &mut Command,
  ) -> Result<(String, String)> {
    let timeout = COMMAND_TIMEOUT;
    let cmd = cmd.current_dir(self.cwd());
    let timed_out = match time::timeout(timeout, cmd.output()).await {
      Ok(Ok(output)) => Ok(output),
      // failure
      Ok(e @ Err(_)) => {
        return e.map(|_| unreachable!()).context(UnableToExecCommandSnafu)
      }
      // timeout
      Err(e) => Err(e),
    };

    let output = timed_out.context(CommandExecTimedOutSnafu { timeout })?;

    let stdout = String::from_utf8(output.stdout).unwrap();
    let stderr = String::from_utf8(output.stderr).unwrap();
    log::info!("{}", stderr);

    Ok((stdout, stderr))
  }

  async fn cargo_new(&mut self) -> Result<()> {
    if self.project_dir.is_some() {
      log::warn!("Attempt to create a second project directory ignored");
      return Ok(());
    }

    let pwd = DEFAULT_PROJECT_PATH;

    let mut cmd = Command::new("cargo");
    cmd.args(["new", "--bin", pwd, "--quiet"]);

    let (output_s, stderr) = self.exec_output(&mut cmd).await?;

    if !stderr.trim().is_empty() {
      log::error!("{}", stderr);
      panic!("`cargo new` failed {stderr}");
    }

    log::debug!("Cargo output {}", output_s);

    self.project_dir = Some(pwd.to_owned());

    Ok(()) // TODO what happens when we can't create the new directory?
  }

  fn cwd(&self) -> PathBuf {
    let base = self.workspace.path();
    match self.project_dir.as_ref() {
      None => base.to_path_buf(),
      Some(p) => base.join(p),
    }
  }

  fn main_abs_path(&self) -> String {
    let mut d = self.cwd();
    d.push("src/main.rs");

    d.to_str().unwrap().to_owned()
  }

  async fn write_source_code(&self, code: &str) -> Result<()> {
    let main = self.main_abs_path();
    fs::write(main, code).context(UnableToWriteFileSnafu)
  }

  pub async fn permissions(
    &self,
    req: &SingleFileRequest,
  ) -> Result<ServerResponse> {
    self.write_source_code(&req.code).await?;

    let mut cmd = self.permissions_command()?;

    let (stdout, stderr) = self.exec_output(&mut cmd).await?;

    Ok(ServerResponse {
      // XXX: we'll assume that if there was anything on `stdout`
      // then there's something successful to report. This does not
      // mean that `stderr` was empty and all things there shouldn't
      // go unreported.
      success: !stdout.trim().is_empty(),
      stdout,
      stderr,
    })
  }

  pub async fn interpreter(
    &self,
    req: &SingleFileRequest,
  ) -> Result<ServerResponse> {
    self.write_source_code(&req.code).await?;

    let mut cmd = self.interpreter_command(req)?;

    let (stdout, stderr) = self.exec_output(&mut cmd).await?;

    Ok(ServerResponse {
      // XXX: we'll assume that if there was anything on `stdout`
      // then there's something successful to report. This does not
      // mean that `stderr` was empty and all things there shouldn't
      // go unreported.
      success: !stdout.trim().is_empty(),
      stdout,
      stderr,
    })
  }

  fn permissions_command(&self) -> Result<Command> {
    let cwd = self.cwd();
    let mut cmd = Command::new("cargo");
    cmd
      .args(["--quiet", "aquascope", "permissions"])
      .env("RUST_LOG", "trace")
      .env("RUST_BACKTRACE", "1")
      .env("MIRI_SYSROOT", miri_sysroot()?)
      .current_dir(cwd);

    Ok(cmd)
  }

  fn interpreter_command(&self, req: &SingleFileRequest) -> Result<Command> {
    let cwd = self.cwd();
    let mut cmd = Command::new("cargo");
    cmd
      .args(["--quiet", "aquascope"])
      .current_dir(cwd)
      .env("RUST_LOG", "debug")
      .env("RUST_BACKTRACE", "1")
      .env("MIRI_SYSROOT", miri_sysroot()?);

    if let Some(config) = req.config.as_ref().and_then(|cfg| cfg.as_object()) {
      if config.contains_key("shouldFail") {
        cmd.arg("--should-fail");
      }
    }

    cmd.arg("interpreter");

    Ok(cmd)
  }

  pub async fn cleanup(self) -> Result<()> {
    Ok(())
  }
}

#[allow(dead_code)]
fn full_permissions() -> std::fs::Permissions {
  PermissionsExt::from_mode(0o777)
}

#[cfg(test)]
mod test {
  use super::*;

  #[tokio::test]
  async fn container_test_new_project() -> Result<()> {
    let code = "fn main() { return 0; }";
    let mut container = Container::new().await?;
    container.cargo_new().await.expect("cargo new");
    assert!(container.project_dir.is_some());
    container.write_source_code(code).await?;
    let main_rs = container.main_abs_path();
    let mut cmd = Command::new("cat");
    cmd.arg(&main_rs);

    let (output, _) = container.exec_output(&mut cmd).await?;
    container.cleanup().await?;
    assert_eq!(output, code);

    Ok(())
  }
}
