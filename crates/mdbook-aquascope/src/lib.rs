#![allow(clippy::comparison_to_empty)]

mod annotations;
mod block;
mod cache;
mod permissions;
mod preprocessor;

pub use cache::CACHE_PATH;
pub use preprocessor::AquascopePreprocessor;
