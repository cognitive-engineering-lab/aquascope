use std::{cell::RefCell, cmp::Ordering, collections::HashMap};

use anyhow::{anyhow, Context, Result};
use either::Either;
use flowistry::mir::utils::PlaceExt;
use miri::{
  Immediate, InterpCx, InterpResult, LocalValue, Machine, MiriConfig, MiriMachine, Operand,
};
use rustc_hir::def_id::DefId;

use rustc_middle::{
  mir::{Body, ClearCrossCrate, LocalInfo, Location, Place, RETURN_PLACE},
  ty::{InstanceDef, TyCtxt},
};
use rustc_session::CtfeBacktrace;
use rustc_span::Span;
use rustc_target::abi::Size;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::Range;

use super::mvalue::{MLocation, MValue};

pub(crate) type MLocals = Vec<(String, MValue)>;

#[derive(Serialize, Deserialize, Debug, TS)]
#[ts(export)]
pub struct MFrame<L> {
  pub name: String,
  pub body_span: Range,
  pub location: L,
  pub locals: MLocals,
}

pub(crate) type MirLoc<'tcx> = (InstanceDef<'tcx>, Either<Location, Span>);

#[derive(Serialize, Deserialize, Debug, TS)]
#[ts(export)]
pub struct MStack<L> {
  pub frames: Vec<MFrame<L>>,
}

#[derive(Serialize, Deserialize, Debug, TS, Default)]
#[ts(export)]
pub struct MHeap {
  pub locations: Vec<MValue>,
}

#[derive(Serialize, Deserialize, Debug, TS)]
#[ts(export)]
pub struct MStep<L> {
  pub stack: MStack<L>,
  pub heap: MHeap,
}

pub(crate) type Addr = Size;

#[derive(Default)]
pub(crate) struct MemoryMap {
  pub(crate) heap: MHeap,
  pub(crate) place_to_loc: HashMap<Addr, MLocation>,
  pub(crate) stack_slots: HashMap<Addr, (usize, String)>,
}

pub struct VisEvaluator<'mir, 'tcx> {
  pub(super) tcx: TyCtxt<'tcx>,
  pub(super) ecx: InterpCx<'mir, 'tcx, MiriMachine<'mir, 'tcx>>,
  pub(super) memory_map: RefCell<MemoryMap>,
}

enum BodySpanType {
  Header,
  Whole,
}

fn body_span(tcx: TyCtxt, def_id: DefId, body_span_type: BodySpanType) -> Span {
  let hir = tcx.hir();
  let fn_node = hir.body_owner(hir.body_owned_by(def_id.expect_local()));
  match body_span_type {
    BodySpanType::Header => hir.span(fn_node),
    BodySpanType::Whole => hir.span_with_body(fn_node),
  }
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
      memory_map: RefCell::default(),
    })
  }

  fn build_locals(
    &self,
    frame: &miri::Frame<'mir, 'tcx, miri::Provenance, miri::FrameExtra<'tcx>>,
    index: usize,
    body: &Body<'tcx>,
  ) -> InterpResult<'tcx, MLocals> {
    frame
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
        match op {
          Operand::Immediate(Immediate::Uninit) => return None,
          Operand::Indirect(mplace) => {
            let mut memory_map = self.memory_map.borrow_mut();
            memory_map
              .stack_slots
              .insert(mplace.ptr.addr(), (index, name.clone()));
          }
          _ => {}
        };
        let value = (|| {
          let op_ty = self.ecx.local_to_op(frame, local, state.layout.get())?;
          let value = self.read(&op_ty)?;
          Ok(value)
        })();
        Some(value.map(move |value| (name, value)))
      })
      .collect()
  }

  fn build_frame(
    &self,
    frame: &miri::Frame<'mir, 'tcx, miri::Provenance, miri::FrameExtra<'tcx>>,
    index: usize,
    loc_override: Option<MirLoc<'tcx>>,
  ) -> InterpResult<'tcx, MFrame<MirLoc<'tcx>>> {
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

    let body_span = Range::from(
      flowistry::source_map::Range::from_span(
        body_span(self.tcx, frame.instance.def_id(), BodySpanType::Whole),
        self.tcx.sess.source_map(),
      )
      .unwrap(),
    );

    let current_loc = loc_override.unwrap_or((frame.instance.def, frame.current_loc()));

    let locals = self.build_locals(frame, index, body)?;

    Ok(MFrame {
      name,
      body_span,
      locals,
      location: current_loc,
    })
  }

  fn build_stack(&self, current_loc: MirLoc<'tcx>) -> InterpResult<'tcx, MStack<MirLoc<'tcx>>> {
    let frames = self
      .local_frames()
      .enumerate()
      .map(|(index, (is_last, frame))| {
        self.build_frame(frame, index, is_last.then_some(current_loc))
      })
      .collect::<InterpResult<'_, _>>()?;
    Ok(MStack { frames })
  }

  fn build_heap(&self) -> MHeap {
    self.memory_map.replace(Default::default()).heap
  }

  fn build_step(
    &self,
    current_loc: MirLoc<'tcx>,
  ) -> InterpResult<'tcx, Option<MStep<MirLoc<'tcx>>>> {
    let stack = self.build_stack(current_loc)?;
    if stack.frames.is_empty() {
      return Ok(None);
    }

    let heap = self.build_heap();
    Ok(Some(MStep { stack, heap }))
  }

  fn local_frames(
    &self,
  ) -> impl Iterator<
    Item = (
      bool,
      &miri::Frame<'mir, 'tcx, miri::Provenance, miri::FrameExtra<'tcx>>,
    ),
  > {
    let stack = Machine::stack(&self.ecx);
    let n = stack.len();
    stack.iter().enumerate().filter_map(move |(i, frame)| {
      frame
        .instance
        .def_id()
        .is_local()
        .then_some((i == n - 1, frame))
    })
  }

  fn step(&mut self) -> InterpResult<'tcx, (Option<MStep<MirLoc<'tcx>>>, bool)> {
    loop {
      let local_frames = self.local_frames().collect::<Vec<_>>();
      let current_loc = local_frames
        .last()
        .map(|(_, frame)| (frame.instance.def, frame.current_loc()));
      let caller_frame_loc = (local_frames.len() >= 2).then(|| {
        let (_, frame) = &local_frames[local_frames.len() - 2];
        frame.current_loc()
      });
      let n_frames = local_frames.len();

      let more_work = self.ecx.step()?;

      if let Some(mut current_loc) = current_loc {
        let local_frames = self.local_frames().collect::<Vec<_>>();
        let frame_change = local_frames.len().cmp(&n_frames);
        match frame_change {
          Ordering::Greater => {
            let (_, frame) = local_frames.last().unwrap();
            let span = body_span(self.tcx, frame.instance.def_id(), BodySpanType::Header);
            current_loc = (frame.instance.def, Either::Right(span));
          }
          Ordering::Less => {
            if let Some(caller_frame_loc) = caller_frame_loc {
              let (_, frame) = local_frames.last().unwrap();
              current_loc = (frame.instance.def, caller_frame_loc);
            }
          }
          _ => {}
        }

        if let Some(step) = self.build_step(current_loc)? {
          return Ok((Some(step), more_work));
        }
      }

      if !more_work {
        return Ok((None, more_work));
      }
    }
  }

  pub fn eval(&mut self) -> InterpResult<'tcx, Vec<MStep<MirLoc<'tcx>>>> {
    let mut steps = Vec::new();
    loop {
      let (step, more_work) = self.step()?;
      if let Some(step) = step {
        steps.push(step);
      }
      if !more_work {
        break;
      }
    }

    Ok(steps)
  }
}
