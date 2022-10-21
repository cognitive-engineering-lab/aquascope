use std::{future::Future, path::PathBuf, pin::Pin, process::Command, sync::Arc};

use bollard::{
    container::LogOutput,
    exec::{CreateExecOptions, StartExecResults},
};
use futures::io;
use futures_util::StreamExt;
use tokio::{
    io::{AsyncWrite, AsyncWriteExt},
    task::JoinHandle,
};

use crate::container::Container;

pub struct Shell {
    input: Pin<Box<dyn AsyncWrite + Send>>,
    pid: i64,
    container: Arc<Container>,
    _output_handle: JoinHandle<()>,
}

// TODO: why is this needed? why is it not implemented by default?
unsafe impl Sync for Shell {}

impl Shell {
    pub async fn new<Fut>(
        container: &Arc<Container>,
        mut handle_output: impl (FnMut(LogOutput) -> Fut) + Send + 'static,
    ) -> Result<Self>
    where
        Fut: Future + Send + 'static,
    {
        let exec = container
            .exec(CreateExecOptions {
                attach_stdout: Some(true),
                attach_stderr: Some(true),
                attach_stdin: Some(true),
                cmd: Some(vec!["bash"]),
                ..Default::default()
            })
            .await?;

        if let StartExecResults::Attached { input, mut output } = exec {
            let pid = container.get_pid("bash").await?;

            let _output_handle = tokio::spawn(async move {
                while let Some(Ok(msg)) = output.next().await {
                    handle_output(msg).await;
                }
            });

            let shell = Self {
                input,
                pid,
                container: Arc::clone(container),
                _output_handle,
            };

            Ok(shell)
        } else {
            unreachable!()
        }
    }

    pub async fn cwd(&self) -> Result<PathBuf, bollard::errors::Error> {
        let mut cmd = Command::new("pwdx");
        cmd.arg(self.pid.to_string());
        let output = self.container.exec_output(&cmd).await?;
        let cwd = output.split(": ").skip(1).collect::<String>();
        Ok(PathBuf::from(cwd))
    }

    pub async fn run(&mut self, command: impl AsRef<str>) -> io::Result<()> {
        let command = command.as_ref();

        let mut parts = command.split(' ').collect::<Vec<_>>();
        if parts[0] == "cargo" {
            parts.push("--color=always");
        }

        let input = format!("{}\n", parts.join(" "));
        self.input.write_all(input.as_bytes()).await?;
        Ok(())
    }
}

// #[cfg(test)]
// mod test {
//     use std::{sync::Arc, time::Duration};

//     use anyhow::anyhow;
//     use bollard::Docker;
//     use tokio::sync::Mutex;

//     use super::*;

//     #[tokio::test]
//     async fn shell_test() -> Result<()> {
//         env_logger::init();
//         let docker = Arc::new(Docker::connect_with_local_defaults()?);
//         docker.ping().await?;

//         let container = Arc::new(Container::new(&docker, "rust").await?);
//         let stdout = Arc::new(Mutex::new(Vec::new()));
//         let stdout_ref = Arc::clone(&stdout);
//         let mut shell = Shell::new(&container, move |log| {
//             let stdout_ref = Arc::clone(&stdout_ref);
//             async move {
//                 stdout_ref.lock().await.push(format!("{}", log));
//             }
//         })
//         .await?;
//         shell.run("cd /tmp && echo hey").await?;
//         assert_eq!(shell.cwd().await?, PathBuf::from("/tmp/"));

//         // TODO: this might be flaky one day
//         tokio::time::sleep(Duration::from_millis(1000)).await;
//         assert_eq!(*stdout.lock().await, vec!["hey\n".to_owned()]);

//         drop(shell);
//         Arc::try_unwrap(container)
//             .map_err(|_| anyhow!("Hanging container reference"))?
//             .cleanup()
//             .await?;
//         Ok(())
//     }
// }
