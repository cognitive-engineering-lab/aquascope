use anyhow::Result;
use aquascope::analysis::{
  self, boundaries::PermissionsBoundary, stepper::PermissionsStateStep,
  AnalysisOutput, BodyAnalysisJoin,
};
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
pub struct PermissionsBoundaryOutput(Vec<PermissionsBoundary>);

impl BodyAnalysisJoin for PermissionsBoundaryOutput {
  fn analysis_join(self, other: Self) -> Self {
    Self(
      self
        .0
        .analysis_join(other.0)
        .into_iter()
        .unique_by(|pi| pi.location)
        .collect::<Vec<_>>(),
    )
  }
}

macro_rules! gen_permission_ctxt {
  ($tcx:expr, $id:expr) => {
    &analysis::compute_permissions(
      $tcx,
      $id,
      // body_with_facts
      get_body_with_borrowck_facts(
        $tcx,
        // def_id
        $tcx.hir().body_owner_def_id($id),
      ),
    )
  };
}

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

#[derive(Debug, Clone, Serialize, TS)]
#[ts(export)]
pub struct PermissionsDiffOutput(Vec<PermissionsStateStep>);

impl BodyAnalysisJoin for PermissionsDiffOutput {
  fn analysis_join(self, other: Self) -> Self {
    Self(self.0.analysis_join(other.0))
  }
}

pub fn permission_diffs(
  tcx: TyCtxt,
  body_id: BodyId,
) -> Result<PermissionsDiffOutput> {
  let permissions_ctxt = gen_permission_ctxt!(tcx, body_id);
  let source_map = tcx.sess.source_map();

  let steps = analysis::compute_permission_steps(permissions_ctxt, |span| {
    source_map::Range::from_span(span, source_map)
      .ok()
      .unwrap_or_else(|| {
        log::error!("The span {span:?} could not be turned into a valid Range");
        source_map::Range::default()
      })
      .into()
  });

  Ok(PermissionsDiffOutput(steps))
}
