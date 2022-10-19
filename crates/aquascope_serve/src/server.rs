use crate::{
    container::{self, Container},
    Config, ContainerCreationSnafu, Error, ErrorJson, ReceiverTypesRequest, Result, SourceRequest,
};
use aquascope_front::{
    method_receivers::ReceiverTypesOutput,
    plugin::{AquascopeError, AquascopeResult},
    source::SourceOutput,
};
use async_trait::async_trait;
use axum::{
    extract::{self, Extension, Path, TypedHeader},
    //     handler::Handler,
    //     headers::{authorization::Bearer, Authorization, CacheControl, ETag, IfNoneMatch},
    http::{header, uri::PathAndQuery, HeaderValue, Method, Request, StatusCode, Uri},
    //     middleware,
    response::IntoResponse,
    routing::{get, get_service, post, MethodRouter},
    Router,
};
use futures::future::BoxFuture;
use snafu::{prelude::*, IntoError};
use std::process::Command;
use tower_http::cors::{self, CorsLayer};

#[tokio::main]
pub(crate) async fn serve(cfg: Config) {
    let mut app = Router::new()
        .route(
            "/",
            get(|| async {
                log::info!("Received Message");
                "HELLO!"
            }),
        )
        .route("/example-source", post(source))
        .route("/receiver-types", post(receiver_types));

    app = app.layer({
        CorsLayer::new()
            .allow_origin(cors::Any)
            .allow_headers([header::CONTENT_TYPE])
            .allow_methods([Method::GET, Method::POST])
            .allow_credentials(false)
    });

    log::info!("Serving requests on {}:{}", cfg.address, cfg.port);

    axum::Server::bind(&cfg.socket_address())
        .serve(app.into_make_service())
        .await
        .unwrap();
}

#[axum_macros::debug_handler]
async fn source(Json(req): Json<SourceRequest>) -> Result<Json<AquascopeResult<SourceOutput>>> {
    log::info!("Processing source for {}", req.filename);

    let output = Command::new("cargo")
        .current_dir("/Users/gavinleroy/dev/prj/aquascope/files/hello")
        .args(["aquascope", "source", &req.filename])
        .output()
        .expect("failed to execute process");

    match std::str::from_utf8(&output.stdout) {
        Ok(s) => {
            log::debug!("Processed string {}", s);
            serde_json::from_str(s)
                .map(Json)
                .map_err(|e| Error::Unknown { msg: e.to_string() })
        }
        // FIXME this would be an internal error and they
        // should be converted into responses anyways. Perhaps
        // some sort of Internal Error flag that could be sent
        // to users (even though it shouldn't happen).
        Err(e) => {
            log::debug!("Couldn't parse Json");
            panic!("Invalid utf8 sequence {e}");
        }
    }
}

async fn receiver_types(
    Json(req): Json<ReceiverTypesRequest>,
) -> Result<Json<AquascopeResult<ReceiverTypesOutput>>> {
    todo!()
}

async fn with_container<F, Req, Resp, KReq, KResp, Ctx>(req: Req, f: F, ctx: Ctx) -> Result<Resp>
where
    for<'req> F: FnOnce(Container, &'req KReq) -> BoxFuture<'req, container::Result<KResp>>,
    Resp: From<KResp>,
    KReq: TryFrom<Req, Error = Error>,
    Ctx: IntoError<Error, Source = container::Error>,
{
    let container = Container::new().await.context(ContainerCreationSnafu)?;
    let request = req.try_into()?;
    f(container, request).await.map(Into::into).context(ctx)
}

/// This type only exists so that we can recover from the `axum::Json`
/// error and format it using our expected JSON error object.
struct Json<T>(T);

#[async_trait]
impl<T, B> extract::FromRequest<B> for Json<T>
where
    T: serde::de::DeserializeOwned,
    B: axum::body::HttpBody + Send,
    B::Data: Send,
    B::Error: Into<axum::BoxError>,
{
    type Rejection = axum::response::Response;

    async fn from_request(req: &mut extract::RequestParts<B>) -> Result<Self, Self::Rejection> {
        match axum::Json::<T>::from_request(req).await {
            Ok(v) => Ok(Self(v.0)),
            Err(e) => {
                let error = format!("Unable to deserialize request: {e}");
                Err(axum::Json(ErrorJson { error }).into_response())
            }
        }
    }
}

impl<T> IntoResponse for Json<T>
where
    T: serde::Serialize,
{
    fn into_response(self) -> axum::response::Response {
        axum::Json(self.0).into_response()
    }
}
