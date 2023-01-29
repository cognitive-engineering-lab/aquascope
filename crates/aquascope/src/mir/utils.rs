//! A smattering of utilities not yet (or that won't ever be) upstreamed to Flowistry.

use rustc_middle::{
  mir::{Body, Place},
  ty::TyCtxt,
};

// ------------------------
// Places

pub trait PlaceExt {
  fn is_source_visible(&self, tcx: TyCtxt, body: &Body) -> bool;
}

impl PlaceExt for Place<'_> {
  fn is_source_visible(&self, tcx: TyCtxt, body: &Body) -> bool {
    let local = self.local;
    let local_info = &body.local_decls[local];

    let is_loc = local_info.is_user_variable();
    let from_desugaring = local_info.from_compiler_desugaring();
    let source_info = local_info.source_info;
    // The assumption is that decls whose source_scope should be collapsed
    // (i.e. with that of the outermost expansion site) are coming from a
    // HIR -> MIR expansion OR are being expanded from some macro not
    // actually visible in the source scope.
    let should_collapse = tcx.should_collapse_debuginfo(source_info.span);

    is_loc && !should_collapse && !from_desugaring
  }
}
