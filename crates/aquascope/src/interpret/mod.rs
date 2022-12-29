use flowistry::mir::utils::SpanExt;

use rustc_middle::ty::TyCtxt;

use anyhow::Result;

mod eval;
mod mapping;
mod mvalue;

pub use mvalue::MValue;

use crate::Range;

use self::eval::MStep;

fn interpret(tcx: TyCtxt) -> Result<Vec<MStep<Range>>> {
  let mapper = mapping::Mapper::build(tcx);
  let mut evaluator = eval::VisEvaluator::new(tcx).unwrap();
  let mir_steps = evaluator.eval().map_err(|e| anyhow::format_err!("{}", e))?;
  let hir_steps = mapping::group_steps(mir_steps, |loc| mapper.abstract_loc(loc));
  let src_steps = mapping::group_steps(hir_steps, |(owner_id, node_id)| {
    let hir = tcx.hir();
    let outer_span = hir.span_with_body(owner_id);
    let span = hir.span(node_id).as_local(outer_span)?;
    let range = flowistry::source_map::Range::from_span(span, tcx.sess.source_map()).unwrap();
    Some(Range::from(range))
  });

  Ok(src_steps)
}

#[derive(Default)]
pub struct InterpretCallbacks {
  pub result: Option<Result<Vec<MStep<Range>>>>,
}

impl rustc_driver::Callbacks for InterpretCallbacks {
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
