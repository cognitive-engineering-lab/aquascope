#![feature(
  rustc_private,
  box_patterns,
  associated_type_defaults,
  min_specialization,
  type_alias_impl_trait,
  trait_alias,
  let_chains,
  unboxed_closures,
  once_cell,
  exact_size_is_empty,
  iter_intersperse,
  is_some_and,
  hash_drain_filter,
  drain_filter,
  type_changing_struct_update
)]

#[macro_use]
extern crate rustc_middle;

extern crate datafrog;
extern crate either;
extern crate polonius_engine;
extern crate rustc_abi;
extern crate rustc_apfloat;
extern crate rustc_borrowck;
extern crate rustc_const_eval;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_error_messages;
extern crate rustc_errors;
extern crate rustc_graphviz;
extern crate rustc_hir;
extern crate rustc_hir_pretty;
extern crate rustc_index;
extern crate rustc_infer;
extern crate rustc_interface;
extern crate rustc_macros;
extern crate rustc_mir_dataflow;
extern crate rustc_mir_transform;
extern crate rustc_serialize;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;
extern crate rustc_trait_selection;
extern crate rustc_type_ir;
extern crate smallvec;

pub mod analysis;
pub mod errors;
#[allow(clippy::all)]
pub mod interpreter;
pub mod mir;
#[cfg(feature = "testing")]
pub mod test_utils;

use serde::{Deserialize, Serialize};
use ts_rs::TS;

// re-export Range from Flowistry with TS.
#[derive(
  Serialize, Deserialize, Debug, Clone, Hash, PartialEq, Eq, Default, TS,
)]
#[ts(export)]
pub struct Range {
  pub char_start: usize,
  pub char_end: usize,
  pub filename: usize,
}

impl From<flowistry::source_map::CharRange> for Range {
  fn from(i: flowistry::source_map::CharRange) -> Self {
    Range {
      char_start: i.start.0,
      char_end: i.end.0,
      filename: i.filename.as_usize(),
    }
  }
}
