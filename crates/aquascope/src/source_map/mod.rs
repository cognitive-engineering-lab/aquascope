//! Mapping MIR instructions to source code.

mod find_bindings;
mod find_bodies;
mod find_calls;
mod hir_span;
mod mir_span;
mod range;
mod span_tree;
mod spanner;

pub use find_bindings::find_bindings;
pub use find_bodies::{find_bodies, find_enclosing_bodies};
pub use find_calls::find_method_calls;
pub use hir_span::EnclosingHirSpans;
pub use range::{FunctionIdentifier, GraphemeIndices, Range, ToSpan};
pub use span_tree::SpanTree;
pub use spanner::Spanner;
