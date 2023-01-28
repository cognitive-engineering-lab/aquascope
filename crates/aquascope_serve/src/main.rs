#![allow(dead_code)]
use serde::{Deserialize, Serialize};
use serde_json::Value;
use snafu::prelude::*;
use std::{env, net::SocketAddr};

const DEFAULT_ADDRESS: &str = "127.0.0.1";
const DEFAULT_PORT: u16 = 8008;

mod container;
mod server;

fn main() {
    // Default logging is error only, info is useful for a server.
    let env_logger_config = env_logger::Env::default().default_filter_or("info");
    env_logger::Builder::from_env(env_logger_config).init();

    // TODO setup the environment

    let cfg = Config::from_env();
    server::serve(cfg);
}

struct Config {
    address: String,
    port: u16,
    no_docker: bool,
}

#[derive(Debug, Clone, Serialize)]
struct ErrorJson {
    error: String,
}

impl Config {
    fn from_env() -> Self {
        let address =
            env::var("AQUASCOPE_SERVER_ADDRESS").unwrap_or_else(|_| DEFAULT_ADDRESS.to_owned());
        let port = env::var("AQUASCOPE_SERVER_PORT")
            .ok()
            .and_then(|p| p.parse().ok())
            .unwrap_or(DEFAULT_PORT);
        let no_docker = env::var("AQUASCOPE_NO_DOCKER").is_ok();
        Config {
            address,
            port,
            no_docker,
        }
    }

    fn socket_address(&self) -> SocketAddr {
        let a = self.address.parse().expect("Invalid address");
        SocketAddr::new(a, self.port)
    }
}

#[derive(Debug, Clone, Deserialize)]
struct SingleFileRequest {
    code: String,
    config: Value,
}

#[derive(Debug, Clone, Serialize)]
struct ServerResponse {
    success: bool,
    stdout: String,
    stderr: String,
}

// NOTE this layer of indirection "might" be helpful in the future
// if anything needs to be validated or converted between types, but
// currently it is overkill.
// FIXME probably simpify this when you have a ground zero version running.

impl TryFrom<SingleFileRequest> for container::SingleFileRequest {
    type Error = Error;
    fn try_from(this: SingleFileRequest) -> Result<Self> {
        Ok(container::SingleFileRequest {
            code: this.code,
            config: this.config,
        })
    }
}

impl From<container::ServerResponse> for ServerResponse {
    fn from(this: container::ServerResponse) -> Self {
        ServerResponse {
            success: this.success,
            stdout: this.stdout,
            stderr: this.stderr,
        }
    }
}

#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("Creating the container failed {source}"))]
    ContainerCreation { source: container::Error },
    #[snafu(display("Visualizing receiver types failed {source}"))]
    ReceiverTypes { source: container::Error },
    #[snafu(display("Generating permission flow steps fail {source}"))]
    PermissionSteps { source: container::Error },
    #[snafu(display("Running interpreter failed {source}"))]
    Interpreter { source: container::Error },
    #[snafu(display("An Unknown error occurred: {msg}"))]
    Unknown { msg: String },
}

pub type Result<T, E = Error> = ::std::result::Result<T, E>;

impl axum::response::IntoResponse for Error {
    fn into_response(self) -> axum::response::Response {
        let body = format!("{self}");
        (axum::http::StatusCode::INTERNAL_SERVER_ERROR, body).into_response()
    }
}
