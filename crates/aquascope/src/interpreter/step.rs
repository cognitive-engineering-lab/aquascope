//! Stepping through a Rust program

use std::{cell::RefCell, cmp::Ordering, collections::HashMap};

use anyhow::{anyhow, bail, Context, Result};
use either::Either;
use flowistry::mir::utils::PlaceExt;
use miri::{
  AllocId, AllocMap, AllocRange, Frame, Immediate, InterpCx, InterpError,
  InterpErrorInfo, InterpResult, LocalState, LocalValue, Machine, MiriConfig,
  MiriMachine, OpTy, Operand, UndefinedBehaviorInfo,
};
use rustc_abi::Size;
use rustc_hir::def_id::DefId;
use rustc_middle::{
  mir::{ClearCrossCrate, Local, LocalInfo, Location, Place, RETURN_PLACE},
  ty::{layout::TyAndLayout, InstanceDef, TyCtxt},
};
use rustc_session::CtfeBacktrace;
use rustc_span::Span;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use super::mvalue::{MMemorySegment, MValue};
use crate::Range;

#[derive(Serialize, Deserialize, Debug, TS)]
#[ts(export)]
pub struct MFrame<L> {
  pub name: String,
  pub body_span: Range,
  pub location: L,
  pub locals: Vec<(String, MValue)>,
}

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

#[derive(Serialize, Deserialize, Debug, TS)]
#[serde(tag = "type", content = "value")]
#[ts(export)]
pub enum MUndefinedBehavior {
  PointerUseAfterFree { alloc_id: usize },
  Other(String),
}

#[derive(Serialize, Deserialize, Debug, TS)]
#[serde(tag = "type", content = "value")]
#[ts(export)]
pub enum MResult {
  Success,
  Error(MUndefinedBehavior),
}

#[derive(Serialize, Deserialize, Debug, TS)]
#[ts(export)]
pub struct MTrace<L> {
  pub steps: Vec<MStep<L>>,
  pub result: MResult,
}

pub(crate) type MirLoc<'tcx> = (InstanceDef<'tcx>, Either<Location, Span>);

#[derive(Default)]
pub(crate) struct MemoryMap<'tcx> {
  pub(crate) heap: MHeap,
  pub(crate) place_to_loc:
    HashMap<AllocId, (MMemorySegment, TyAndLayout<'tcx>)>,
  pub(crate) stack_slots: HashMap<AllocId, (usize, String, TyAndLayout<'tcx>)>,
  pub(crate) alloc_id_remapping: HashMap<AllocId, usize>,
}

pub struct VisEvaluator<'mir, 'tcx> {
  pub(super) ecx: InterpCx<'mir, 'tcx, MiriMachine<'mir, 'tcx>>,
  pub(super) memory_map: RefCell<MemoryMap<'tcx>>,
}

enum BodySpanType {
  Header,
  Whole,
}

/// Returns the span of a body, either just the header or the entire item
fn body_span(tcx: TyCtxt, def_id: DefId, body_span_type: BodySpanType) -> Span {
  let hir = tcx.hir();
  let fn_node = hir.body_owner(hir.body_owned_by(def_id.expect_local()));
  match body_span_type {
    BodySpanType::Header => hir.span(fn_node),
    BodySpanType::Whole => hir.span_with_body(fn_node),
  }
}

type FrameLocals<'tcx> = Vec<(String, OpTy<'tcx, miri::Provenance>)>;

impl<'mir, 'tcx> VisEvaluator<'mir, 'tcx> {
  pub fn new(tcx: TyCtxt<'tcx>) -> Result<Self> {
    let (main_id, entry_fn_type) = tcx
      .entry_fn(())
      .context("no main or start function found")?;
    let ecx = miri::create_ecx(tcx, main_id, entry_fn_type, &MiriConfig {
      mute_stdout_stderr: true,
      // have to make sure miri doesn't complain about us poking around memory
      validate: false,
      borrow_tracker: None,
      ..Default::default()
    })
    .map_err(|e| anyhow!("{e}"))?;

    // Ensures we get nice backtraces from miri evaluation errors
    *tcx.sess.ctfe_backtrace.borrow_mut() = CtfeBacktrace::Capture;

    Ok(VisEvaluator {
      ecx,
      memory_map: RefCell::default(),
    })
  }

  pub(super) fn remap_alloc_id(&self, alloc_id: AllocId) -> usize {
    let mut memory_map = self.memory_map.borrow_mut();
    let n = memory_map.alloc_id_remapping.len();
    *memory_map.alloc_id_remapping.entry(alloc_id).or_insert(n)
  }

  pub(super) fn fn_name(&self, def_id: DefId) -> String {
    let full_name = self.ecx.tcx.def_path_debug_str(def_id);
    // Strip crate name prefix from debug str
    full_name
      .split("::")
      .skip(1)
      .intersperse("::")
      .collect::<String>()
  }

  fn build_frame(
    &self,
    frame: &miri::Frame<'mir, 'tcx, miri::Provenance, miri::FrameExtra<'tcx>>,
    index: usize,
    loc_override: Option<MirLoc<'tcx>>,
    locals: FrameLocals<'tcx>,
  ) -> InterpResult<'tcx, MFrame<MirLoc<'tcx>>> {
    log::trace!("Building frame {index}");

    let def_id = frame.instance.def_id();
    let name = self.fn_name(def_id);

    let tcx = *self.ecx.tcx;
    let body_span = Range::from(
      flowistry::source_map::CharRange::from_span(
        body_span(tcx, frame.instance.def_id(), BodySpanType::Whole),
        tcx.sess.source_map(),
      )
      .unwrap(),
    );

    let current_loc =
      loc_override.unwrap_or((frame.instance.def, frame.current_loc()));

    let locals = locals
      .into_iter()
      .map(|(name, op_ty)| {
        log::trace!("Reading local {name:?}");
        let value = self.read(&op_ty)?;
        Ok((name, value))
      })
      .collect::<InterpResult<'_, Vec<_>>>()?;

    Ok(MFrame {
      name,
      body_span,
      locals,
      location: current_loc,
    })
  }

  fn test_local(
    &self,
    frame: &Frame<'mir, 'tcx, miri::Provenance, miri::FrameExtra<'tcx>>,
    frame_index: usize,
    local: Local,
    state: &LocalState<'tcx, miri::Provenance>,
  ) -> InterpResult<'tcx, Option<(String, OpTy<'tcx, miri::Provenance>)>> {
    let decl = &frame.body.local_decls[local];
    let name = if local == RETURN_PLACE {
      // Don't include unit return types in locals
      if decl.ty.is_unit() {
        return Ok(None);
      }

      "(return)".into()
    } else {
      // TODO: this excludes compiler-generated temporaries which we sometimes need to
      // visualize in the case of f(&Some(x)). Need to figure out a good strategy for
      // deciding when a temp should be included.
      if !matches!(
        decl.local_info,
        Some(box LocalInfo::User(ClearCrossCrate::Set(_)))
      ) {
        return Ok(None);
      }

      Place::from_local(local, *self.ecx.tcx)
        .to_string(*self.ecx.tcx, frame.body)
        .unwrap_or_else(|| String::from("(tmp)"))
    };

    let layout = state.layout.get();

    // Ignore dead locals
    let LocalValue::Live(op) = state.value else { return Ok(None) };

    match op {
      // Ignore uninitialized locals
      Operand::Immediate(Immediate::Uninit) => {
        // Special case: a unit struct is considered uninitialized, but we would still like to
        // visualize it at the toplevel, so we handle that here. Might need to make this a configurable thing?
        let not_zst = match layout {
          Some(layout) => !layout.is_zst(),
          None => true,
        };
        if not_zst {
          return Ok(None);
        }
      }

      // If a local is Indirect, meaning there exists a pointer to it,
      // then save its allocation in `MemoryMap::stack_slots`
      Operand::Indirect(mplace) => {
        let mut memory_map = self.memory_map.borrow_mut();
        let (alloc_id, _, _) = self.ecx.ptr_get_alloc_id(mplace.ptr).unwrap();

        // Have to handle the case that a local is uninitialized and indirect
        // by checking the init_mask in the local's allocation
        let (_, allocation) =
          self.ecx.memory.alloc_map().get(alloc_id).unwrap();

        // Some types (like structs, due to alignment) may have uninitialized regions,
        // so checking the entire range is too conservative (will ignore fully initialized types).
        // This hack seems to work for basic structs... but TODO: need to figure out if this robust.
        let range = match layout {
          Some(TyAndLayout { layout, .. }) => match layout.largest_niche() {
            Some(niche) => niche.offset,
            _ => allocation.size(),
          },
          _ => allocation.size(),
        };
        let initialized_status =
          allocation.init_mask().is_range_initialized(AllocRange {
            start: Size::ZERO,
            size: range,
          });
        if let Err(range) = initialized_status {
          log::debug!("Rejecting uninitialized local due to range: {range:?}",);
          return Ok(None);
        }

        memory_map
          .stack_slots
          .insert(alloc_id, (frame_index, name.clone(), layout.unwrap()));
      }
      _ => {}
    };

    let op_ty = self.ecx.local_to_op(frame, local, layout)?;
    Ok(Some((name, op_ty)))
  }

  fn find_locals(&self) -> InterpResult<'tcx, Vec<FrameLocals<'tcx>>> {
    self
      .local_frames()
      .enumerate()
      .map(|(index, (_, frame))| {
        frame
          .locals
          .iter_enumerated()
          .filter_map(|(local, state)| {
            self.test_local(frame, index, local, state).transpose()
          })
          .collect::<InterpResult<'tcx, Vec<_>>>()
      })
      .collect()
  }

  fn build_stack(
    &self,
    current_loc: MirLoc<'tcx>,
  ) -> InterpResult<'tcx, MStack<MirLoc<'tcx>>> {
    let locals = self.find_locals()?;
    let frames = self
      .local_frames()
      .enumerate()
      .zip(locals)
      .map(|((index, (is_last, frame)), locals)| {
        self.build_frame(frame, index, is_last.then_some(current_loc), locals)
      })
      .collect::<InterpResult<'_, _>>()?;
    Ok(MStack { frames })
  }

  fn build_heap(&self) -> MHeap {
    self.memory_map.replace(MemoryMap::default()).heap
  }

  fn build_step(
    &self,
    current_loc: MirLoc<'tcx>,
  ) -> InterpResult<'tcx, Option<MStep<MirLoc<'tcx>>>> {
    log::trace!("Building step for {current_loc:?}");

    log::trace!("Building stack");
    let stack = self.build_stack(current_loc)?;
    if stack.frames.is_empty() {
      return Ok(None);
    }

    log::trace!("Building heap");
    let heap = self.build_heap();

    log::trace!("Step built!");
    Ok(Some(MStep { stack, heap }))
  }

  /// Get the stack frames for functions defined in the local crate
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

  /// Take a single (local) step, internally stepping until we reach a serialization point
  fn step(
    &mut self,
  ) -> InterpResult<'tcx, (Option<MStep<MirLoc<'tcx>>>, bool)> {
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
            let span = body_span(
              *self.ecx.tcx,
              frame.instance.def_id(),
              BodySpanType::Header,
            );
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

  fn beautify_error(
    &mut self,
    e: InterpErrorInfo,
  ) -> Result<MUndefinedBehavior> {
    use UndefinedBehaviorInfo::PointerUseAfterFree;

    Ok(match e.into_kind() {
      InterpError::UndefinedBehavior(ub) => match ub {
        PointerUseAfterFree(alloc_id) => {
          MUndefinedBehavior::PointerUseAfterFree {
            alloc_id: self.remap_alloc_id(alloc_id),
          }
        }
        ub => MUndefinedBehavior::Other(ub.to_string()),
      },
      err => bail!("{err}"),
    })
  }

  /// Evaluate the program to completion, returning a vector of MIR steps for local functions
  pub fn eval(&mut self) -> Result<MTrace<MirLoc<'tcx>>> {
    let mut steps = Vec::new();
    let result = loop {
      match self.step() {
        Ok((step, more_work)) => {
          if let Some(step) = step {
            steps.push(step);
          }
          if !more_work {
            break MResult::Success;
          }
        }
        Err(e) => {
          break MResult::Error(self.beautify_error(e)?);
        }
      }
    };

    Ok(MTrace { steps, result })
  }
}
