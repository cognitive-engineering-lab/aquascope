use anyhow::Result;
use aquascope::analysis::{
  self, boundaries::PermissionsBoundary, stepper::PermissionsLineDisplay,
  AnalysisOutput,
};
use rustc_hir::BodyId;
use rustc_middle::ty::TyCtxt;

pub fn permission_boundaries(
  tcx: TyCtxt,
  body_id: BodyId,
) -> Result<AnalysisOutput<PermissionsBoundary>> {
  Ok(analysis::AquascopeAnalysis::run(
    tcx,
    body_id,
    analysis::compute_permission_boundaries,
  ))
}

pub fn permission_diffs(
  tcx: TyCtxt,
  body_id: BodyId,
) -> Result<AnalysisOutput<PermissionsLineDisplay>> {
  Ok(analysis::AquascopeAnalysis::run(
    tcx,
    body_id,
    analysis::compute_permission_steps,
  ))
}
