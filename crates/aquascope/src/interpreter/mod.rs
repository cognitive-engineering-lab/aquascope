//! Rust runtime visualizer using Miri

use anyhow::Result;
use either::Either;
use rustc_data_structures::fx::FxIndexMap;
use rustc_hir::def_id::LocalDefId;
use rustc_middle::{mir::Body, ty::TyCtxt, util::Providers};
use rustc_utils::{SpanExt, source_map::range::CharRange};

mod mapper;
mod miri_utils;
mod mvalue;
mod step;

pub use mvalue::MValue;
use rustc_session::Session;
pub use step::MTrace;

use crate::interpreter::mapper::Mapper;

pub(crate) fn interpret(tcx: TyCtxt) -> Result<MTrace<CharRange>> {
  let mut evaluator = step::VisEvaluator::new(tcx).unwrap();
  let mir_steps = evaluator.eval()?;

  if log::log_enabled!(log::Level::Trace) {
    for step in &mir_steps.steps {
      let (inst, mir_body_loc) = step.stack.frames.last().unwrap().location;
      log::trace!("{}", match mir_body_loc {
        Either::Left(loc) => {
          let body = evaluator.ecx.load_mir(inst.def, None).unwrap();
          format!("{:?}", body.stmt_at(loc))
        }
        Either::Right(span) =>
          tcx.sess.source_map().span_to_snippet(span).unwrap(),
      })
    }
  }

  let mapper = Mapper::new(&evaluator.ecx);
  let hir_steps =
    mapper::group_steps(mir_steps, |loc| mapper.abstract_loc(loc));

  if log::log_enabled!(log::Level::Trace) {
    for step in &hir_steps.steps {
      let (_, hir_body_loc) = step.stack.frames.last().unwrap().location;
      log::trace!("{:?}", match hir_body_loc {
        Either::Left(node_id) => tcx.hir_id_to_string(node_id),
        Either::Right(span) =>
          tcx.sess.source_map().span_to_snippet(span).unwrap(),
      });
    }
  }

  let src_steps = mapper::group_steps(hir_steps, |(owner_id, hir_body_loc)| {
    let outer_span = tcx.hir_span_with_body(owner_id);
    let span = match hir_body_loc {
      Either::Left(node_id) => tcx.hir_span(node_id).as_local(outer_span)?,
      Either::Right(span) => span.as_local(outer_span)?,
    };
    let range = CharRange::from_span(span, tcx.sess.source_map()).unwrap();
    Some(range)
  });

  Ok(src_steps)
}

pub struct InterpretCallbacks {
  should_fail: bool,
  pub result: Option<Result<MTrace<CharRange>>>,
}

impl InterpretCallbacks {
  pub fn new(should_fail: bool) -> Self {
    InterpretCallbacks {
      should_fail,
      result: None,
    }
  }
}
// We disable `mir_borrowck` to allow programs with Rust-caught UB to execute
// rather than being rejected out of hand.
fn fake_mir_borrowck(
  tcx: TyCtxt<'_>,
  _id: LocalDefId,
) -> rustc_middle::queries::mir_borrowck::ProvidedValue<'_> {
  Ok(tcx.arena.alloc(FxIndexMap::default()))
}

// Some optimizations like drop elaboration depend on MoveData, and will raise an error
// if the MoveData is empty. Thankfully we can reset and ignore that error via
// `Handler::reset_err_count` which we do by overriding optimized_mir.
fn fake_optimized_mir(tcx: TyCtxt<'_>, did: LocalDefId) -> &'_ Body<'_> {
  let mut providers = Providers::default();
  rustc_mir_transform::provide(&mut providers);
  let body = (providers.queries.optimized_mir)(tcx, did);
  tcx.sess.dcx().reset_err_count();
  body
}

// See `fake_mir_borrowck`
pub fn override_queries(
  _session: &Session,
  providers: &mut rustc_middle::util::Providers,
) {
  providers.queries.mir_borrowck = fake_mir_borrowck;
  providers.queries.optimized_mir = fake_optimized_mir;
}

impl rustc_driver::Callbacks for InterpretCallbacks {
  // See `fake_mir_borrowck`
  fn config(&mut self, config: &mut rustc_interface::interface::Config) {
    if self.should_fail {
      config.override_queries = Some(override_queries);
    }
  }

  fn after_analysis(
    &mut self,
    _compiler: &rustc_interface::interface::Compiler,
    tcx: TyCtxt<'_>,
  ) -> rustc_driver::Compilation {
    self.result = Some(interpret(tcx));
    rustc_driver::Compilation::Stop
  }
}
