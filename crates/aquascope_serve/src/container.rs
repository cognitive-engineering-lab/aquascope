use snafu::prelude::*;

use std::{io, os::unix::fs::PermissionsExt, path::PathBuf, str, time::Duration};
use tokio::{self, process::Command, time};

use crate::{ServerResponse, SingleFileRequest};

#[cfg(not(feature = "no-docker"))]
use {
    bollard::{
        container::{
            Config, CreateContainerOptions, LogOutput, RemoveContainerOptions,
            StartContainerOptions, UploadToContainerOptions,
        },
        exec::{CreateExecOptions, StartExecResults},
        models::{ContainerCreateResponse, HostConfig},
        Docker,
    },
    futures::StreamExt,
    serde::Serialize,
    std::{env, iter, path::Path, sync::Arc},
};

#[cfg(feature = "no-docker")]
use {
    std::fs,
    tempfile::{tempdir, TempDir},
};

#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("Bollard operation failed {}", source))]
    Bollard { source: bollard::errors::Error },
    #[snafu(display("Unable to create temporary local directory {}", source))]
    UnableToCreateTempDir { source: io::Error },
    #[snafu(display("Unable to create output directory: {}", source))]
    UnableToWriteFile { source: io::Error },
    #[snafu(display("Unable to execute local command {}", source))]
    UnableToExecCommand { source: io::Error },
    #[snafu(display("Unable to create source file: {}", source))]
    UnableToCreateSourceFile { source: io::Error },
    #[snafu(display("Unable to launch the docker container: {}", source))]
    UnableToStartDocker { source: io::Error },

    #[snafu(display("Command execution took longer than {} ms", timeout.as_millis()))]
    CommandExecTimedOut {
        source: time::error::Elapsed,
        timeout: Duration,
    },
}

pub type Result<T, E = Error> = ::std::result::Result<T, E>;

const DEFAULT_IMAGE: &str = "aquascope";
const DEFAULT_PROJECT_PATH: &str = "aquascope_tmp_proj";

const COMMAND_TIMEOUT: Duration = Duration::from_secs(20);
// Memory limit in bytes.
const DOCKER_MEM_LIMIT: i64 = 512_000_000;
// Swap limit in bytes (when MEM_LIMIT == SWAP_LIMIT no swap is allowed).
const DOCKER_SWAP_LIMIT: i64 = 512_000_000;
const DOCKER_PID_LIMIT: i64 = 32;
// Default docker `--cpu-quota` is 100000 microseconds
// `--cpus` is a better flag to use but Bollard doesn't provide this.
const DOCKER_CPU_PERIOD: i64 = 100_000;
// Cap a container to use 50% of one available CPU.
const DOCKER_CPU_QUOTA: i64 = DOCKER_CPU_PERIOD / 2;

#[cfg(not(feature = "no-docker"))]
pub struct Container {
    id: String,
    killed: bool,
    project_dir: Option<String>,
    docker: Arc<Docker>,
}

#[cfg(feature = "no-docker")]
pub struct Container {
    project_dir: Option<String>,
    workspace: TempDir,
}

impl Container {
    #[cfg(feature = "no-docker")]
    pub async fn new() -> Result<Self> {
        let td = tempdir().context(UnableToCreateTempDirSnafu)?;
        let mut this = Container {
            workspace: td,
            project_dir: None,
        };

        this.cargo_new().await?;

        Ok(this)
    }

    #[cfg(not(feature = "no-docker"))]
    pub async fn new() -> Result<Self> {
        let d = Docker::connect_with_local_defaults()?.with_timeout(COMMAND_TIMEOUT);
        let docker = Arc::new(d);
        docker.ping().await?;
        let image = DEFAULT_IMAGE;

        Self::with_docker(docker, image).await
    }

    #[cfg(not(feature = "no-docker"))]
    pub(crate) async fn with_docker(docker: Arc<Docker>, image: &str) -> Result<Self> {
        // Launch the container with
        let options: Option<CreateContainerOptions<String>> = None;
        let cwd = env::current_dir().unwrap();
        let mount_path = cwd.join("mount");
        let config = Config {
            tty: Some(true), // to keep the container alive
            image: Some(image),
            host_config: Some(HostConfig {
                cpu_period: Some(DOCKER_CPU_PERIOD),
                cpu_quota: Some(DOCKER_CPU_QUOTA),
                memory: Some(DOCKER_MEM_LIMIT),
                memory_swap: Some(DOCKER_SWAP_LIMIT),
                pids_limit: Some(DOCKER_PID_LIMIT),
                binds: Some(vec![format!("{}:/mnt", mount_path.display())]),
                ..Default::default()
            }),
            ..Default::default()
        };
        let ContainerCreateResponse { id, .. } = docker.create_container(options, config).await?;
        log::info!("Created container with id: {id}");

        let options: Option<StartContainerOptions<String>> = None;
        docker.start_container(&id, options).await?;
        log::info!("Container started: {id}");

        let mut this = Container {
            id,
            killed: false,
            docker: docker.clone(),
            project_dir: None,
        };

        this.cargo_new().await?;

        Ok(this)
    }

    #[cfg(not(feature = "no-docker"))]
    pub async fn exec(
        &self,
        options: CreateExecOptions<impl Into<String> + Serialize>,
    ) -> Result<StartExecResults, Error> {
        let exec = self.docker.create_exec(&self.id, options).await?;
        self.docker
            .start_exec(&exec.id, None)
            .await
            .context(BollardSnafu)
    }

    #[allow(dead_code)]
    pub async fn get_pid(&self, process: &str) -> Result<i64> {
        let mut cmd = Command::new("pidof");
        cmd.arg(process);
        let (response, _) = self.exec_output(&mut cmd).await?;
        Ok(response
            .split(' ')
            .next()
            .unwrap()
            .parse::<i64>()
            .unwrap_or_else(|_| {
                panic!(
                    "Invalid response in get_pid:
    {response}"
                )
            }))
    }

    #[cfg(feature = "no-docker")]
    pub async fn exec_output(&self, cmd: &mut Command) -> Result<(String, String)> {
        let timeout = COMMAND_TIMEOUT;
        let cmd = cmd.current_dir(self.cwd());
        let timed_out = match time::timeout(timeout, cmd.output()).await {
            Ok(Ok(output)) => Ok(output),
            // failure
            Ok(e @ Err(_)) => return e.map(|_| unreachable!()).context(UnableToExecCommandSnafu),
            // timeout
            Err(e) => Err(e),
        };

        let output = timed_out.context(CommandExecTimedOutSnafu { timeout })?;

        // TODO: FIXME
        let stdout = String::from_utf8(output.stdout).unwrap();
        let stderr = String::from_utf8(output.stderr).unwrap();

        // use info here because the "no-docker" feature is used primarily
        // as a debugging target.
        log::info!("{}", stderr);

        Ok((stdout, stderr))
    }

    // Returns (Stdout, Stderr)
    #[cfg(not(feature = "no-docker"))]
    pub async fn exec_output(&self, cmd: &mut Command) -> Result<(String, String)> {
        // Convert back to std::process::Command for access to get_XXX functions.
        let cmd = cmd.as_std();
        let args = iter::once(cmd.get_program())
            .chain(cmd.get_args())
            .map(|s| s.to_string_lossy().to_string())
            .collect::<Vec<_>>();
        let env = cmd
            .get_envs()
            .map(|(k, v)| {
                let ks = k.to_string_lossy();
                match v {
                    Some(v) => format!("{ks}={}", v.to_string_lossy()),
                    None => format!("{ks}="),
                }
            })
            .collect::<Vec<_>>();
        let working_dir = cmd
            .get_current_dir()
            .map(|p| p.to_string_lossy().to_string());
        let exec = self
            .exec(CreateExecOptions {
                attach_stdout: Some(true),
                attach_stderr: Some(true),
                working_dir,
                env: Some(env),
                cmd: Some(args),
                ..Default::default()
            })
            .await?;

        macro_rules! get_variant_as_string {
            ($id:path, $e:expr) => {
                if let $id { message } = $e {
                    String::from_utf8_lossy(message.as_ref()).to_string()
                } else {
                    unreachable!()
                }
            };
        }

        if let StartExecResults::Attached { output, .. } = exec {
            let timeout = COMMAND_TIMEOUT;
            let lines = time::timeout(
                timeout,
                output
                    .filter_map(|log| async move {
                        match log {
                            Ok(LogOutput::StdOut { message }) => {
                                Some(LogOutput::StdOut { message })
                            }

                            Ok(LogOutput::StdErr { message }) => {
                                Some(LogOutput::StdErr { message })
                            }
                            _ => None,
                        }
                    })
                    .collect::<Vec<_>>(),
            )
            .await
            .context(CommandExecTimedOutSnafu { timeout })?;

            let (stdout, stderr): (Vec<_>, Vec<_>) = lines
                .iter()
                .partition(|e| matches!(e, LogOutput::StdOut { .. }));

            let stdout = stdout
                .iter()
                .map(|stdout| get_variant_as_string!(LogOutput::StdOut, stdout))
                .collect::<Vec<_>>();

            let stderr = stderr
                .iter()
                .map(|stderr| get_variant_as_string!(LogOutput::StdErr, stderr))
                .collect::<Vec<_>>();

            Ok((
                stdout.join("\n").trim_end().to_owned(),
                stderr.join("\n").trim_end().to_owned(),
            ))
        } else {
            unreachable!()
        }
    }

    // XXX: in theory this should probably be a default part of the
    // container. It would be great if we could also allow people
    // to have a full project open in the editor but I don't see this
    // for the near future.
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

    #[cfg(not(feature = "no-docker"))]
    fn cwd(&self) -> PathBuf {
        let prj_dir = self.project_dir.as_ref().unwrap();
        Path::new("/app").join(prj_dir)
    }

    #[cfg(feature = "no-docker")]
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

    #[cfg(feature = "no-docker")]
    async fn write_source_code(&self, code: &str) -> Result<()> {
        let main = self.main_abs_path();
        fs::write(main, code).context(UnableToWriteFileSnafu)
    }

    // FIXME: major HACK there should be a much simpler way to copy
    // a temp file to the docker container. Something similar to
    // `docker cp` would be great :)
    #[cfg(not(feature = "no-docker"))]
    async fn write_source_code(&self, code: &str) -> Result<()> {
        let mut header = tar::Header::new_gnu();
        header.set_size(code.len() as u64);
        header.set_cksum();
        let data = code.as_bytes();
        let mut ar = tar::Builder::new(Vec::new());
        // XXX: the method main_abs_path isn't used here because the header needs to
        // specify a *relative* path.
        let path_to_main = Path::new(self.project_dir.as_ref().unwrap()).join("src/main.rs");
        ar.append_data(&mut header, &path_to_main, data).unwrap();

        let options = Some(UploadToContainerOptions {
            // NOTE the default path in the Dockerfile is `/app`
            // if this changes the below path also needs to change.
            // bollard will not allow empty paths.
            path: "/app",
            ..Default::default()
        });
        // upload the tar to the container
        let buf = ar.into_inner().context(UnableToCreateSourceFileSnafu)?;

        self.docker
            .upload_to_container(&self.id, options, buf.into())
            .await
            .context(BollardSnafu)
    }

    pub async fn permissions(&self, req: &SingleFileRequest) -> Result<ServerResponse> {
        self.write_source_code(&req.code).await?;

        let mut cmd = self.permissions_command();

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

    pub async fn interpreter(&self, req: &SingleFileRequest) -> Result<ServerResponse> {
        self.write_source_code(&req.code).await?;

        let mut cmd = self.interpreter_command(req);

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

    // -------------
    // Commands

    fn permissions_command(&self) -> Command {
        let cwd = self.cwd();

        let mut cmd = Command::new("cargo");
        cmd.args(["--quiet", "aquascope", "permissions"])
            .current_dir(cwd);

        if cfg!(feature = "no-docker") {
            let _ = cmd.env("RUST_LOG", "debug").env("RUST_BACKTRACE", "1");
        }

        cmd
    }

    fn interpreter_command(&self, req: &SingleFileRequest) -> Command {
        let cwd = self.cwd();

        let mut cmd = Command::new("cargo");
        cmd.args(["--quiet", "aquascope"]).current_dir(cwd);

        if let Some(config) = req.config.as_ref().and_then(|cfg| cfg.as_object()) {
            if config.contains_key("shouldFail") {
                cmd.arg("--should-fail");
            }
        }

        cmd.arg("interpreter");

        if cfg!(feature = "no-docker") {
            let _ = cmd.env("RUST_LOG", "debug").env("RUST_BACKTRACE", "1");
        }

        cmd
    }

    #[cfg(feature = "no-docker")]
    pub async fn cleanup(self) -> Result<()> {
        Ok(())
    }

    #[cfg(not(feature = "no-docker"))]
    pub async fn cleanup(mut self) -> Result<()> {
        self.killed = true;
        let options: Option<RemoveContainerOptions> = Some(RemoveContainerOptions {
            force: true,
            ..Default::default()
        });

        log::info!("Removing container {}", self.id);

        self.docker
            .remove_container(&self.id, options)
            .await
            .context(BollardSnafu)
    }
}

#[cfg(not(feature = "no-docker"))]
impl Drop for Container {
    fn drop(&mut self) {
        if !self.killed {
            log::warn!(
                "Dropping container {} without calling Container::cleanup.",
                self.id
            );
        }
    }
}

#[allow(dead_code)]
fn full_permissions() -> std::fs::Permissions {
    PermissionsExt::from_mode(0o777)
}

impl From<bollard::errors::Error> for Error {
    fn from(item: bollard::errors::Error) -> Self {
        Error::Bollard { source: item }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    // HACK taken from the rust-playground:
    // https://github.com/integer32llc/rust-playground/blob/master/ui/src/sandbox.rs#L1065-L1075
    //
    // Limits testing to one function at a time. Ensuring that containers
    // have their full allotment when my local machine has fewer resources
    // than the server.
    fn one_test_at_a_time() -> impl Drop {
        use lazy_static::lazy_static;
        use std::sync::Mutex;

        lazy_static! {
            static ref DOCKER_SINGLETON: Mutex<()> = Default::default();
        }

        // We can't poison the empty tuple
        DOCKER_SINGLETON.lock().unwrap_or_else(|e| e.into_inner())
    }

    #[tokio::test]
    #[cfg(not(feature = "no-docker"))]
    async fn container_test() -> Result<()> {
        let _s = one_test_at_a_time();
        let docker = Arc::new(Docker::connect_with_local_defaults()?);
        docker.ping().await?;
        let container = Container::with_docker(docker, DEFAULT_IMAGE).await?;
        let mut cmd = Command::new("echo");
        cmd.arg("hey");
        let (output, _) = container.exec_output(&mut cmd).await?;
        container.cleanup().await?;
        assert_eq!(output, "hey");

        Ok(())
    }

    #[tokio::test]
    async fn container_test_new_project() -> Result<()> {
        let _s = one_test_at_a_time();
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

    #[tokio::test]
    #[cfg(not(feature = "no-docker"))]
    async fn container_has_timeout() -> Result<()> {
        let _s = one_test_at_a_time();
        let code = r#"
            fn main() {
                loop {}
            }
        "#
        .to_string();
        let container = Container::new().await?;
        let req = SingleFileRequest { code, config: None };
        let res = container.interpreter(&req).await?;

        container.cleanup().await?;

        assert!(!res.success);
        assert!(res.stderr.contains("SIGKILL"));

        Ok(())
    }

    #[tokio::test]
    #[cfg(not(feature = "no-docker"))]
    async fn container_has_memory_limit() -> Result<()> {
        let _s = one_test_at_a_time();
        let code = r#"
            fn main() {
                let gigabyte = 1024 * 1024 * 1024;
                let mut big = vec![0u8; 1 * gigabyte];
                for i in &mut big { *i += 1; }
            }
        "#
        .to_string();
        let container = Container::new().await?;
        let req = SingleFileRequest { code, config: None };
        let res = container.interpreter(&req).await?;

        container.cleanup().await?;

        assert!(!res.success);
        assert!(res.stderr.contains("SIGKILL"));

        Ok(())
    }
}
