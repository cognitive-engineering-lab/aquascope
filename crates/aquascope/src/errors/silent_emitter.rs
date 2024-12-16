//! Silent diagnostics emitter.
//!
//! See:
//! https://doc.rust-lang.org/nightly/nightly-rustc/rustfmt_nightly/parse/session/struct.SilentEmitter.html#impl-Translate-for-SilentEmitter

use rustc_driver::DEFAULT_LOCALE_RESOURCES;
use rustc_errors::{
  emitter::Emitter, translation::Translate, DiagInner, FluentBundle,
  LazyFallbackBundle,
};
use rustc_span::source_map::SourceMap;

/// Emitter which discards every error.
pub(crate) struct SilentEmitter(LazyFallbackBundle);

impl SilentEmitter {
  pub fn new() -> Self {
    Self(rustc_errors::fallback_fluent_bundle(
      DEFAULT_LOCALE_RESOURCES.to_vec(),
      false,
    ))
  }
}

impl Translate for SilentEmitter {
  fn fluent_bundle(&self) -> Option<&FluentBundle> {
    None
  }

  fn fallback_fluent_bundle(&self) -> &FluentBundle {
    &self.0
  }
}

impl Emitter for SilentEmitter {
  fn source_map(&self) -> Option<&SourceMap> {
    None
  }

  fn emit_diagnostic(
    &mut self,
    _db: DiagInner,
    _registry: &rustc_errors::registry::Registry,
  ) {
  }
}
