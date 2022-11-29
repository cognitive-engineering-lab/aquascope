use anyhow::Result;
use aquascope::analysis::{self, PermissionsBoundary};
use flowistry::{
  mir::borrowck_facts::get_body_with_borrowck_facts, source_map,
};
use itertools::Itertools;
use rustc_hir::BodyId;
use rustc_middle::ty::TyCtxt;
use serde::Serialize;
use ts_rs::TS;

#[derive(Debug, Clone, Serialize, TS)]
#[ts(export)]
pub struct PermissionsOutput(Vec<PermissionsBoundary>);

impl super::plugin::Join for PermissionsOutput {
  fn join(self, other: Self) -> Self {
    PermissionsOutput(
      self
        .0
        .join(other.0)
        .into_iter()
        .unique_by(|pi| pi.range.clone())
        .collect::<Vec<_>>(),
    )
  }
}

pub fn permissions(tcx: TyCtxt, body_id: BodyId) -> Result<PermissionsOutput> {
  let def_id = tcx.hir().body_owner_def_id(body_id);
  let body_with_facts = get_body_with_borrowck_facts(tcx, def_id);
  let permissions_ctxt =
    &analysis::compute_permissions(tcx, body_id, body_with_facts);
  let source_map = tcx.sess.source_map();
  let call_infos =
    analysis::pair_permissions_to_calls(permissions_ctxt, |span| {
      source_map::Range::from_span(span, source_map)
        .ok()
        .unwrap_or_default()
        .into()
    });

  Ok(PermissionsOutput(call_infos))
}
