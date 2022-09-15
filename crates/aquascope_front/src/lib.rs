#![feature(rustc_private, unboxed_closures, box_patterns, trait_alias)]
// #![allow(
//   clippy::single_match,
//   clippy::needless_lifetimes,
//   clippy::needless_return,
//   clippy::len_zero,
//   clippy::let_and_return
// )]

extern crate either;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_hir;
extern crate rustc_hir_pretty;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_macros;
extern crate rustc_middle;
extern crate rustc_mir_dataflow;
extern crate rustc_serialize;
extern crate rustc_span;

// #[cfg(feature = "decompose")]
// mod decompose;
// mod focus;
mod plugin;
mod source;
mod spans;
mod style;

pub use plugin::AquascopePlugin;
