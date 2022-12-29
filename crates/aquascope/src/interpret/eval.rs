use std::cell::RefCell;

use anyhow::{anyhow, Context, Result};
use either::Either;
use flowistry::mir::utils::PlaceExt;
use miri::{
  Immediate, InterpCx, InterpResult, LocalValue, Machine, MiriConfig, MiriMachine, OpTy, Operand,
};

use rustc_hir::def_id::LocalDefId;
use rustc_middle::{
  mir::{ClearCrossCrate, LocalInfo, Location, Place, RETURN_PLACE},
  ty::TyCtxt,
};
use rustc_session::CtfeBacktrace;
use rustc_span::Span;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use super::mvalue::{MLocation, MValue};

#[derive(Serialize, Deserialize, Debug, TS)]
#[ts(export)]
pub struct MFrame<L> {
  pub name: String,
  pub location: L,
  pub locals: Vec<(String, MValue)>,
}

pub(crate) type MirLoc = (LocalDefId, Either<Location, Span>);

#[derive(Serialize, Deserialize, Debug, TS)]
#[ts(export)]
pub struct MStack<L> {
  pub frames: Vec<MFrame<L>>,
}

#[derive(Serialize, Deserialize, Debug, TS)]
#[ts(export)]
pub struct MHeap {
  pub locations: Vec<(MLocation, MValue)>,
}

#[derive(Serialize, Deserialize, Debug, TS)]
#[ts(export)]
pub struct MStep<L> {
  pub stack: MStack<L>,
  pub heap: MHeap,
}

pub struct VisEvaluator<'mir, 'tcx> {
  pub(super) tcx: TyCtxt<'tcx>,
  pub(super) ecx: InterpCx<'mir, 'tcx, MiriMachine<'mir, 'tcx>>,
  pub(super) heap_allocs: RefCell<Vec<miri::MPlaceTy<'tcx, miri::Provenance>>>,
}

impl<'mir, 'tcx> VisEvaluator<'mir, 'tcx> {
  pub fn new(tcx: TyCtxt<'tcx>) -> Result<Self> {
    let (main_id, entry_fn_type) = tcx
      .entry_fn(())
      .context("no main or start function found")?;
    let ecx = miri::create_ecx(
      tcx,
      main_id,
      entry_fn_type,
      &MiriConfig {
        mute_stdout_stderr: true,
        // have to make sure miri doesn't complain about us poking around memory
        validate: false,
        borrow_tracker: None,
        ..Default::default()
      },
    )
    .map_err(|e| anyhow!("{e}"))?;

    *tcx.sess.ctfe_backtrace.borrow_mut() = CtfeBacktrace::Capture;

    Ok(VisEvaluator {
      tcx,
      ecx,
      heap_allocs: RefCell::default(),
    })
  }

  fn build_frame(
    &self,
    frame: &miri::Frame<'mir, 'tcx, miri::Provenance, miri::FrameExtra<'tcx>>,
  ) -> InterpResult<'tcx, MFrame<MirLoc>> {
    let source_map = self.tcx.sess.source_map();
    let body = &frame.body;

    let def_id = frame.instance.def_id();

    let name = self
      .tcx
      .def_path_debug_str(def_id)
      // Strip crate name prefix from debug str
      .split("::")
      .skip(1)
      .intersperse("::")
      .collect::<String>();

    let current_loc = (def_id.expect_local(), frame.current_loc());

    let mut locals = frame
      .locals
      .iter_enumerated()
      .filter_map(|(local, state)| {
        let decl = &body.local_decls[local];
        let name = if local == RETURN_PLACE {
          if decl.ty.is_unit() {
            return None;
          }

          "(return)".into()
        } else {
          if !matches!(
            decl.local_info,
            Some(box LocalInfo::User(ClearCrossCrate::Set(_)))
          ) {
            return None;
          }

          Place::from_local(local, self.tcx)
            .to_string(self.tcx, body)
            .unwrap()
        };

        let LocalValue::Live(op) = state.value else { return None };
        let value = match op {
          Operand::Immediate(Immediate::Uninit) => Ok(MValue::Unallocated),
          _ => (|| {
            let op_ty = self.ecx.local_to_op(frame, local, state.layout.get())?;
            let value = self.read(&op_ty)?;
            Ok(value)
          })(),
        };
        Some(value.map(move |value| (name, value)))
      })
      .collect::<InterpResult<'tcx, Vec<_>>>()?;
    locals.sort_by(|(k1, _), (k2, _)| k1.cmp(k2));

    Ok(MFrame {
      name,
      locals,
      location: current_loc,
    })
  }

  fn build_stack(&self) -> InterpResult<'tcx, MStack<MirLoc>> {
    let frames = Machine::stack(&self.ecx)
      .iter()
      .filter(|frame| frame.instance.def_id().is_local())
      .map(|frame| self.build_frame(frame))
      .collect::<InterpResult<'_, _>>()?;
    Ok(MStack { frames })
  }

  fn build_heap(&self) -> InterpResult<'tcx, MHeap> {
    let locations = self
      .heap_allocs
      .borrow()
      .iter()
      .enumerate()
      .map(|(i, mplace)| Ok((i, self.read(&OpTy::from(*mplace))?)))
      .collect::<InterpResult<'_, Vec<_>>>()?;

    Ok(MHeap { locations })
  }

  fn build_step(&self) -> InterpResult<'tcx, Option<MStep<MirLoc>>> {
    self.heap_allocs.borrow_mut().clear();
    let stack = self.build_stack()?;
    if stack.frames.is_empty() {
      return Ok(None);
    }

    let heap = self.build_heap()?;

    return Ok(Some(MStep { stack, heap }));
  }

  fn step(&mut self) -> InterpResult<'tcx, Option<MStep<MirLoc>>> {
    loop {
      if !self.ecx.step()? {
        return Ok(None);
      }

      let Some(step) = self.build_step()? else { continue };
      return Ok(Some(step));
    }
  }

  pub fn eval(&mut self) -> InterpResult<'tcx, Vec<MStep<MirLoc>>> {
    let mut steps = Vec::new();
    while let Some(pair) = self.step()? {
      steps.push(pair);
    }

    Ok(steps)
  }
}
