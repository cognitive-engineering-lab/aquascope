use crate::{
    container::{self, Container},
    Config, ContainerCreationSnafu, Error, ErrorJson, InterpreterSnafu, PermissionsSnafu, Result,
    ServerResponse, SingleFileRequest,
};
use async_trait::async_trait;
use axum::{
    extract,
    handler::Handler,
    http::{header, Method},
    response::IntoResponse,
    routing::{get, post},
    Router,
};
use futures::{future::BoxFuture, FutureExt};
use snafu::{prelude::*, IntoError};
use tower_http::cors::{self, CorsLayer};

#[tokio::main]
pub(crate) async fn serve(cfg: Config) {
    let mut app = Router::new()
        .fallback(fallback.into_service())
        .route(
            "/hi",
            get(|| async {
                log::info!("Received Message");
                "HELLO!"
            }),
        )
        .route("/permissions", post(permissions))
        .route("/interpreter", post(interpreter));

    app = app.layer({
        CorsLayer::new()
            .allow_origin(cors::Any)
            .allow_headers([header::CONTENT_TYPE])
            .allow_methods([Method::GET, Method::POST])
            .allow_credentials(false)
    });

    if cfg.no_docker {
        log::warn!("Requests will be processed outside of docker");
    }

    log::info!("Serving requests on {}:{}", cfg.address, cfg.port);

    axum::Server::bind(&cfg.socket_address())
        .serve(app.into_make_service())
        .await
        .unwrap();
}

macro_rules! make_single_file_endpoint {
    ($name:ident, $ectx:path) => {
        async fn $name(Json(req): Json<SingleFileRequest>) -> Result<Json<ServerResponse>> {
            log::trace!("Received requeset for {}", stringify!($name));

            let json = with_container(
                req,
                |knt, req| {
                    async move {
                        let v = knt.$name(req).await;
                        if let Err(e) = knt.cleanup().await {
                            log::warn!("Error cleaning up container: {:?}", e);
                        }
                        v
                    }
                    .boxed()
                },
                $ectx,
            )
            .await
            .map(Json);

            log::debug!("returning JSON {:?}", json);

            json
        }
    };
}

make_single_file_endpoint!(permissions, PermissionsSnafu);
make_single_file_endpoint!(interpreter, InterpreterSnafu);

async fn with_container<F, Req, Resp, Ctx>(req: Req, f: F, ctx: Ctx) -> Result<Resp, Error>
where
    for<'req> F: FnOnce(Container, &'req Req) -> BoxFuture<'req, container::Result<Resp>>,
    Ctx: IntoError<Error, Source = container::Error>,
{
    let container = Container::new().await.context(ContainerCreationSnafu)?;
    f(container, &req).await.map(Into::into).context(ctx)
}

/// Axum handler for any request that fails to match the router routes.
/// This implementation returns HTTP status code Not Found (404).
pub async fn fallback(uri: axum::http::Uri) -> impl axum::response::IntoResponse {
    (axum::http::StatusCode::NOT_FOUND, format!("No route {uri}"))
}

/// This type only exists so that we can recover from the `axum::Json`
/// error and format it using our expected JSON error object.
#[derive(Debug)]
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
    T: serde::Serialize + std::fmt::Debug,
{
    fn into_response(self) -> axum::response::Response {
        log::trace!("Turning Json<T> into response {:?}", self.0);
        axum::Json(self.0).into_response()
    }
}
