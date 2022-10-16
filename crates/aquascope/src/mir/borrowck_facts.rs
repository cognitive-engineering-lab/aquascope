//! Polonius integration

use rustc_borrowck::consumers::BodyWithBorrowckFacts;
use rustc_hir::def_id::LocalDefId;
use rustc_middle::{
  mir::MirPass,
  ty::{
    self,
    query::{self, query_values::mir_borrowck},
    TyCtxt,
  },
};

use crate::{cached::Cache, mir::utils::SimplifyMir};

pub fn override_queries(
  _session: &rustc_session::Session,
  local: &mut query::Providers,
  _external: &mut query::ExternProviders,
) {
  local.mir_borrowck = mir_borrowck;
}

// Since mir_borrowck does not have access to any other state, we need to use a
// thread-local for storing the obtained MIR bodies.
//
// NOTE: We are using 'static lifetime here, which is in general unsound.
// Unfortunately, that is the only lifetime allowed here. Our use is safe
// because we cast it back to `'tcx` before its usage.
//
// NOTE this technique originates from:
// https://github.com/rust-lang/rust/blob/485ced56b8753ec86936903f2a8c95e9be8996a1/src/test/run-make-fulldeps/obtain-borrowck/driver.rs
thread_local! {
  static MIR_BODIES: Cache<LocalDefId, BodyWithBorrowckFacts<'static>> = Cache::default();
}

pub fn get_body_with_borrowck_facts<'tcx>(
  tcx: TyCtxt<'tcx>,
  def_id: LocalDefId,
) -> &'tcx BodyWithBorrowckFacts<'tcx> {
  let def_path = tcx.def_path(def_id.to_def_id());
  log::debug!("Got bodies for: {}", def_path.to_string_no_crate_verbose());

  let _ = tcx.mir_borrowck(def_id);

  MIR_BODIES.with(|cache| {
    let body = cache.get(def_id, |_| unreachable!());
    let safe_body = unsafe {
      std::mem::transmute::<
        &BodyWithBorrowckFacts<'static>,
        &'tcx BodyWithBorrowckFacts<'tcx>,
      >(body)
    };
    assert!(!body.input_facts.cfg_edge.is_empty());
    safe_body
  })
}

pub fn mir_borrowck<'tcx>(
  tcx: TyCtxt<'tcx>,
  def_id: LocalDefId,
) -> mir_borrowck<'tcx> {
  log::trace!("custom mir_borrowck");
  let mut body_with_facts =
    rustc_borrowck::consumers::get_body_with_borrowck_facts(
      tcx,
      ty::WithOptConstParam::unknown(def_id),
    );

  let body = &mut body_with_facts.body;
  SimplifyMir.run_pass(tcx, body);

  // SAFETY: The reader casts the 'static lifetime to 'tcx before using it.
  let body_with_facts = unsafe { std::mem::transmute(body_with_facts) };

  MIR_BODIES.with(|cache| {
    let _ = cache.get(def_id, |_| body_with_facts);
  });
  let mut providers = query::Providers::default();
  rustc_borrowck::provide(&mut providers);
  let original_mir_borrowck = providers.mir_borrowck;
  original_mir_borrowck(tcx, def_id)
}
