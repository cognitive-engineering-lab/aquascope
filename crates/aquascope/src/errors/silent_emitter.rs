//! Silent diagnostics emitter.
//!
//! See:
//! https://doc.rust-lang.org/nightly/nightly-rustc/rustfmt_nightly/parse/session/struct.SilentEmitter.html#impl-Translate-for-SilentEmitter

use rustc_errors::{
  emitter::Emitter, translation::Translate, DiagInner, FluentBundle,
};
use rustc_span::source_map::SourceMap;

/// Emitter which discards every error.
pub(crate) struct SilentEmitter;

impl Translate for SilentEmitter {
  fn fluent_bundle(&self) -> Option<&FluentBundle> {
    None
  }

  fn fallback_fluent_bundle(&self) -> &FluentBundle {
    panic!("silent emitter attempted to translate a diagnostic");
  }
}

impl Emitter for SilentEmitter {
  fn source_map(&self) -> Option<&SourceMap> {
    None
  }

  fn emit_diagnostic(&mut self, _db: DiagInner) {}
}
