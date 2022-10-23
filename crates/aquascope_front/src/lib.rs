#![feature(
  rustc_private,
  unboxed_closures,
  box_patterns,
  trait_alias,
  trace_macros
)]
// #![allow(
//   clippy::single_match,
//   clippy::needless_lifetimes,
//   clippy::needless_return,
//   clippy::len_zero,
//   clippy::let_and_return
// )]

extern crate either;
extern crate proc_macro;
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

#[macro_use]
extern crate eager;

// #[cfg(feature = "decompose")]
// mod decompose;
// mod focus;
pub mod method_receivers;
pub mod plugin;
pub mod source;
mod spans;
mod style;

pub use plugin::AquascopePlugin;

// const INTERFACE_TYPES_DIR: &str = "../../frontend/interface/";

// #[proc_macro]
// pub fn ts_interface_file(
//   item: proc_macro::TokenStream,
// ) -> proc_macro::TokenStream {
//   format!(
//     "#[ts(export, export_to = \"{}{}\")]",
//     INTERFACE_TYPES_DIR,
//     item.to_string()
//   )
//   .parse()
//   .unwrap()
// }
