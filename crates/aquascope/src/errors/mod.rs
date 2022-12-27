use std::cell::RefCell;

use rustc_errors::{Diagnostic, TRACK_DIAGNOSTICS};
use rustc_hir::def_id::LocalDefId;
use rustc_span::Span;

thread_local! {
    static BODY_DIAGNOSTICS: RefCell<Vec<DiagnosticInfo>> = RefCell::new(Vec::default());
    static CURRENT_BODY: RefCell<Option<LocalDefId>> = RefCell::new(None);
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
struct DiagnosticInfo {
  primary_span: Span,
  is_error: bool,
}

fn track_diagnostic(d: &Diagnostic) {
  BODY_DIAGNOSTICS.with(|diagnostics| {
    let mut diagnostics = diagnostics.borrow_mut();
    let d = DiagnosticInfo {
      primary_span: d.sort_span,
      is_error: d.is_error(),
    };
    diagnostics.push(d);
  })
}

// ------------------------------------------------
// Interface methods for fetching registered errors

/// This should be called before analysing a new crate.
pub fn initialize_error_tracking() {
  log::debug!("Track diagnostics updated");
  TRACK_DIAGNOSTICS.swap(&(track_diagnostic as fn(&_)));
}

/// Initialize the error tracking for a given routine. It's recommended
/// to call this on start of every new analysis. In Aquascope, this would
/// be per-body analyzed.
pub fn track_body_diagnostics(def_id: LocalDefId) {
  // Update the current LocalDefId
  CURRENT_BODY.with(|id| {
    let mut id = id.borrow_mut();
    let old_value = id.replace(def_id);
    log::debug!("Replacing tracked body id {old_value:?} with {def_id:?}");
  });
  BODY_DIAGNOSTICS.with(|diagnostics| {
    let mut diagnostics = diagnostics.borrow_mut();
    // FIXME(gavinleroy): we should really be caching the diagnostics by
    // LocalDefId, meaning that we don't have to clean them after
    // each analysis. This also has the added benefic of caching
    // in case we ever reuse processes for server queries.
    diagnostics.clear();
  });
}

pub fn get_span_of_first_error(def_id: LocalDefId) -> Option<Span> {
  // A security check that the body expected by the caller is
  // in sync with that of the error diagnostics.
  CURRENT_BODY.with(|id| {
    assert_eq!(def_id, id.borrow().unwrap());
  });

  BODY_DIAGNOSTICS.with(|diagnostics| {
    let diagnostics = diagnostics.borrow();

    log::debug!("Diagnostics {:?}", diagnostics);

    diagnostics
      .iter()
      .filter_map(|d| d.is_error.then_some(d.primary_span))
      .min_by_key(|s| s.lo())
  })
}
