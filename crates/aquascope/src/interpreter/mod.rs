//! Rust runtime visualizer using Miri

use anyhow::Result;
use either::Either;
use flowistry::mir::utils::SpanExt;
use rustc_data_structures::vec_map::VecMap;
use rustc_hir::def_id::LocalDefId;
use rustc_middle::{
  mir::BorrowCheckResult,
  ty::{self, TyCtxt},
};

mod mapper;
mod miri_utils;
mod mvalue;
mod step;

pub use mvalue::MValue;
use rustc_session::Session;
use smallvec::SmallVec;
pub use step::MTrace;

use crate::{interpreter::mapper::Mapper, Range};

pub(crate) fn interpret(tcx: TyCtxt) -> Result<MTrace<Range>> {
  let mut evaluator = step::VisEvaluator::new(tcx).unwrap();
  let mir_steps = evaluator.eval()?;

  // eprintln!("{mir_steps:#?}");

  let mapper = Mapper::new(&evaluator.ecx);
  let hir_steps =
    mapper::group_steps(mir_steps, |loc| mapper.abstract_loc(loc));

  // for step in &hir_steps {
  //   let (_, hir_body_loc) = step.stack.frames.last().unwrap().location;
  //   eprintln!(
  //     "{:?}",
  //     match hir_body_loc {
  //       Either::Left(node_id) => tcx.hir().node_to_string(node_id),
  //       Either::Right(span) => tcx.sess.source_map().span_to_snippet(span).unwrap(),
  //     }
  //   );
  // }
  // eprintln!("{hir_steps:#?}");

  let src_steps = mapper::group_steps(hir_steps, |(owner_id, hir_body_loc)| {
    let hir = tcx.hir();
    let outer_span = hir.span_with_body(owner_id);
    let span = match hir_body_loc {
      Either::Left(node_id) => hir.span(node_id).as_local(outer_span)?,
      Either::Right(span) => span.as_local(outer_span)?,
    };
    let range =
      flowistry::source_map::CharRange::from_span(span, tcx.sess.source_map())
        .unwrap();
    Some(Range::from(range))
  });

  Ok(src_steps)
}

#[derive(Default)]
pub struct InterpretCallbacks {
  pub result: Option<Result<MTrace<Range>>>,
}

// We disable `mir_borrowck` to allow programs with Rust-caught UB to execute
// rather than being rejected out of hand.
fn fake_mir_borrowck(
  tcx: TyCtxt<'_>,
  _id: LocalDefId,
) -> &'_ BorrowCheckResult<'_> {
  tcx.arena.alloc(BorrowCheckResult {
    concrete_opaque_types: VecMap::new(),
    closure_requirements: None,
    used_mut_upvars: SmallVec::new(),
    tainted_by_errors: None,
  })
}

// See `fake_mir_borrowck`
pub fn override_queries(
  _session: &Session,
  providers: &mut ty::query::Providers,
  _extern_providers: &mut ty::query::ExternProviders,
) {
  providers.mir_borrowck = fake_mir_borrowck;
}

impl rustc_driver::Callbacks for InterpretCallbacks {
  // See `fake_mir_borrowck`
  fn config(&mut self, config: &mut rustc_interface::interface::Config) {
    config.override_queries = Some(override_queries);
  }

  fn after_parsing<'tcx>(
    &mut self,
    _compiler: &rustc_interface::interface::Compiler,
    queries: &'tcx rustc_interface::Queries<'tcx>,
  ) -> rustc_driver::Compilation {
    queries.global_ctxt().unwrap().take().enter(|tcx| {
      self.result = Some(interpret(tcx));
    });
    rustc_driver::Compilation::Stop
  }
}
