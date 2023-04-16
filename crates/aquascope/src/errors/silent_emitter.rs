//! Silent diagnostics emitter.
//!
//! See:
//! https://doc.rust-lang.org/nightly/nightly-rustc/rustfmt_nightly/parse/session/struct.SilentEmitter.html#impl-Translate-for-SilentEmitter

use rustc_data_structures::sync::Lrc;
use rustc_errors::{emitter::Emitter, translation::Translate, Diagnostic};
use rustc_span::source_map::SourceMap;

/// Emitter which discards every error.
pub(crate) struct SilentEmitter;

impl Translate for SilentEmitter {
  fn fluent_bundle(&self) -> Option<&Lrc<rustc_errors::FluentBundle>> {
    None
  }

  fn fallback_fluent_bundle(&self) -> &rustc_errors::FluentBundle {
    panic!("silent emitter attempted to translate a diagnostic");
  }
}

impl Emitter for SilentEmitter {
  fn source_map(&self) -> Option<&Lrc<SourceMap>> {
    None
  }

  fn emit_diagnostic(&mut self, _db: &Diagnostic) {}
}
