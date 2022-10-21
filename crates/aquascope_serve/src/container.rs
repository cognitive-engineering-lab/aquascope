use bollard::{
    container::{
        Config, CreateContainerOptions, LogOutput, RemoveContainerOptions, StartContainerOptions,
        UploadToContainerOptions,
    },
    exec::{CreateExecOptions, StartExecResults},
    models::{ContainerCreateResponse, HostConfig},
    Docker,
};
use flate2::{write::GzEncoder, Compression};
use futures::StreamExt;
use serde::Serialize;
use snafu::prelude::*;
use std::process::Command;
use std::{env, io, iter, os::unix::fs::PermissionsExt, path::PathBuf, sync::Arc};
use tempfile::{self, TempDir};
use tokio::{
    fs, // process::Command,
    time,
};

const DEFAULT_IMAGE: &str = "aquascope";
const DEFAULT_PROJECT_PATH: &str = "aquascope_tmp_proj";

#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("Bollard operation failed {}", source))]
    Bollard { source: bollard::errors::Error },

    // Taken directly from play.rust-lang.org
    #[snafu(display("Unable to create temporary directory: {}", source))]
    UnableToCreateTempDir { source: io::Error },
    #[snafu(display("Unable to create output directory: {}", source))]
    UnableToCreateOutputDir { source: io::Error },
    #[snafu(display("Unable to set permissions for output directory: {}", source))]
    UnableToSetOutputPermissions { source: io::Error },
    #[snafu(display("Unable to create source file: {}", source))]
    UnableToCreateSourceFile { source: io::Error },
    #[snafu(display("Unable to set permissions for source file: {}", source))]
    UnableToSetSourcePermissions { source: io::Error },
    #[snafu(display("Unable to launch the docker container: {}", source))]
    UnableToStartDocker { source: io::Error },
}

pub type Result<T, E = Error> = ::std::result::Result<T, E>;

pub struct Container {
    // my_dir: TempDir,
    // input_file: PathBuf,
    // output_dir: PathBuf,
    id: String,
    killed: bool,
    project_dir: Option<String>,
    docker: Arc<Docker>,
}

impl Container {
    pub async fn new() -> Result<Self> {
        // NOTE this will create a new docker /every time/ a requeust is filed.
        // I still haven't tested how fast this will be, so let's find out!
        let d = Docker::connect_with_local_defaults()?;
        let docker = Arc::new(d);
        docker.ping().await?;
        let image = DEFAULT_IMAGE;

        Self::with_docker(docker, image).await
    }

    pub(crate) async fn with_docker(docker: Arc<Docker>, image: &str) -> Result<Self> {
        // Launch the container with
        let options: Option<CreateContainerOptions<String>> = None;
        let cwd = env::current_dir().unwrap();
        let mount_path = cwd.join("mount");
        let config = Config {
            tty: Some(true), // to keep the container alive
            image: Some(image),
            host_config: Some(HostConfig {
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

        // create temporary files in a scratch buffer.
        // let my_dir = tempfile::Builder::new()
        //     .prefix("aquascope")
        //     .tempdir()
        //     .context(UnableToCreateTempDirSnafu)?;
        // let input_file = my_dir.path().join("input.rs");
        // let output_dir = my_dir.path().join("output");
        // fs::create_dir(&output_dir)
        //     .await
        //     .context(UnableToCreateOutputDirSnafu)?;
        // fs::set_permissions(&output_dir, full_permissions())
        //     .await
        //     .context(UnableToSetOutputPermissionsSnafu)?;

        Ok(Container {
            // my_dir,
            // input_file,
            // output_dir,
            id,
            killed: false,
            docker: docker.clone(),
            project_dir: None,
        })
    }

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

    pub async fn get_pid(&self, process: &str) -> Result<i64, Error> {
        let mut cmd = Command::new("pidof");
        cmd.arg(process);
        let response = self.exec_output(&cmd).await?;
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

    pub async fn exec_output(&self, cmd: &Command) -> Result<String, Error> {
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
                working_dir,
                env: Some(env),
                cmd: Some(args),
                ..Default::default()
            })
            .await?;

        if let StartExecResults::Attached { output, .. } = exec {
            let lines = output
                .filter_map(|log| async move {
                    match log {
                        Ok(LogOutput::StdOut { message }) => {
                            Some(String::from_utf8_lossy(message.as_ref()).to_string())
                        }
                        _ => None,
                    }
                })
                .collect::<Vec<_>>()
                .await;
            Ok(lines.join("\n").trim_end().to_owned())
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
        cmd.args(["new", "--bin", pwd]);
        let output_s = self.exec_output(&cmd).await?;

        log::debug!("Cargo output {}", output_s);

        self.project_dir = Some(pwd.to_owned());

        Ok(()) // TODO what happens when we can't create the new directory?
    }

    fn main_abs_path(&self) -> String {
        let mut prj_root = self.project_dir.as_ref().unwrap().clone();
        prj_root.push_str("/src/main.rs");

        log::debug!("Absolute path to main.rs: {}", prj_root);

        prj_root
    }

    // FIXME: major HACK there should be a much simpler way to copy
    // a temp file to the docker container. Something similar to
    // `docker cp` would be great :)
    async fn write_source_code(&self, code: &str) -> Result<()> {
        let mut header = tar::Header::new_gnu();
        header.set_size(code.len() as u64);
        header.set_cksum();

        let data = code.as_bytes();
        let mut ar = tar::Builder::new(Vec::new());
        let path_to_main = self.main_abs_path();
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

    pub async fn receiver_types(
        &self,
        req: &ReceiverTypesRequest,
    ) -> Result<ReceiverTypesResponse> {
        self.write_source_code(&req.code).await?;
        let cmd = self.receiver_types_command("src/main.rs");

        let output = self.exec_output(&cmd).await?;

        Ok(ReceiverTypesResponse {
            success: true,
            stdout: output,
            stderr: String::default(),
        })
    }

    fn receiver_types_command(&self, filename: &str) -> Command {
        let mut cmd = Command::new("cargo");
        cmd.args(["aquascope", "vis-method-calls", filename]);
        cmd
    }

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

fn full_permissions() -> std::fs::Permissions {
    PermissionsExt::from_mode(0o777)
}

impl From<bollard::errors::Error> for Error {
    fn from(item: bollard::errors::Error) -> Self {
        Error::Bollard { source: item }
    }
}

#[derive(Debug, Clone)]
pub struct ReceiverTypesRequest {
    pub code: String,
}

#[derive(Debug, Clone)]
pub struct ReceiverTypesResponse {
    pub success: bool,
    pub stdout: String,
    pub stderr: String,
}

#[tokio::test]
async fn container_test() -> Result<()> {
    let docker = Arc::new(Docker::connect_with_local_defaults()?);
    docker.ping().await?;
    let container = Container::with_docker(docker, DEFAULT_IMAGE).await?;
    let mut cmd = Command::new("echo");
    cmd.arg("hey");
    let output = container.exec_output(&cmd).await?;
    assert_eq!(output, "hey");
    container.cleanup().await?;

    Ok(())
}

#[tokio::test]
async fn container_test_new_project() -> Result<()> {
    let docker = Arc::new(Docker::connect_with_local_defaults()?);
    docker.ping().await?;
    let mut container = Container::with_docker(docker, DEFAULT_IMAGE).await?;
    container.cargo_new().await?;
    assert!(container.project_dir.is_some());
    let code = "fn main() { return 0; }";
    container.write_source_code(code).await?;

    let main_rs = container.main_abs_path();
    let mut cmd = Command::new("cat");
    cmd.arg(&main_rs);

    let output = container.exec_output(&cmd).await?;
    assert_eq!(output, code);
    container.cleanup().await?;

    Ok(())
}
