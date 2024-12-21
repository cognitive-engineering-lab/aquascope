//! Stepping through a Rust program

use std::{
  cell::RefCell,
  cmp::Ordering,
  collections::{HashMap, HashSet},
};

use anyhow::{anyhow, bail, Context, Result};
use either::Either;
use itertools::Itertools;
use miri::{
  interp_ok, AllocId, AllocMap, AllocRange, Immediate, InterpCx,
  InterpErrorInfo, InterpErrorKind, InterpResult, LocalState, Machine,
  MiriConfig, MiriMachine, OpTy, UndefinedBehaviorInfo,
};
use rustc_abi::{FieldsShape, Size};
use rustc_const_eval::ReportErrorExt;
use rustc_hir::def_id::DefId;
use rustc_middle::{
  mir::{
    self, visit::Visitor, Local, Location, Place, PlaceElem,
    VarDebugInfoContents, RETURN_PLACE,
  },
  ty::{
    layout::{HasTyCtxt, TyAndLayout},
    Instance, TyCtxt,
  },
};
use rustc_session::CtfeBacktrace;
use rustc_span::Span;
use rustc_utils::{source_map::range::CharRange, PlaceExt};
use serde::Serialize;
use ts_rs::TS;

use super::mvalue::{MMemorySegment, MPathSegment, MValue};

#[derive(Serialize, Debug, TS)]
#[ts(export)]
pub struct MLocal {
  name: String,
  value: MValue,
  moved_paths: Vec<Vec<MPathSegment>>,
}

#[derive(Serialize, Debug, TS)]
#[ts(export)]
pub struct MFrame<L> {
  pub name: String,
  pub body_span: CharRange,
  pub location: L,
  pub locals: Vec<MLocal>,
}

#[derive(Serialize, Debug, TS)]
#[ts(export)]
pub struct MStack<L> {
  pub frames: Vec<MFrame<L>>,
}

#[derive(Serialize, Debug, TS, Default)]
#[ts(export)]
pub struct MHeap {
  pub locations: Vec<MValue>,
}

#[derive(Serialize, Debug, TS)]
#[ts(export)]
pub struct MStep<L> {
  pub stack: MStack<L>,
  pub heap: MHeap,
}

#[derive(Serialize, Debug, TS)]
#[serde(tag = "type", content = "value")]
#[ts(export)]
pub enum MUndefinedBehavior {
  PointerUseAfterFree { alloc_id: usize },
  Other(String),
}

#[derive(Serialize, Debug, TS)]
#[serde(tag = "type", content = "value")]
#[ts(export)]
pub enum MResult {
  Success,
  Error(MUndefinedBehavior),
}

#[derive(Serialize, Debug, TS)]
#[ts(export)]
pub struct MTrace<L> {
  pub steps: Vec<MStep<L>>,
  pub result: MResult,
}

pub(crate) type MirLoc<'tcx> = (Instance<'tcx>, Either<Location, Span>);

#[derive(Default)]
pub(crate) struct MemoryMap<'tcx> {
  pub(crate) heap: MHeap,
  pub(crate) place_to_loc:
    HashMap<AllocId, (MMemorySegment, TyAndLayout<'tcx>)>,
  pub(crate) stack_slots: HashMap<AllocId, (usize, String, TyAndLayout<'tcx>)>,
  pub(crate) alloc_id_remapping: HashMap<AllocId, usize>,
}

pub struct MovedPlaces<'tcx>(Vec<HashSet<Place<'tcx>>>);

impl<'tcx> MovedPlaces<'tcx> {
  pub fn new() -> Self {
    MovedPlaces(vec![HashSet::new()])
  }

  pub fn places_at(
    &self,
    index: usize,
  ) -> impl Iterator<Item = Place<'tcx>> + '_ {
    self.0[index].iter().copied()
  }

  pub fn add_place(&mut self, frame: usize, place: Place<'tcx>) {
    self
      .0
      .get_mut(frame)
      .unwrap_or_else(|| {
        panic!("Tried to insert place at missing frame: {frame}")
      })
      .insert(place);
  }

  pub fn push_frame(&mut self) {
    self.0.push(HashSet::new());
  }

  pub fn pop_frame(&mut self) {
    self.0.pop();
  }
}

pub struct VisEvaluator<'tcx> {
  pub(super) ecx: InterpCx<'tcx, MiriMachine<'tcx>>,
  pub(super) memory_map: RefCell<MemoryMap<'tcx>>,
  pub(super) moved_places: RefCell<MovedPlaces<'tcx>>,
}

enum BodySpanType {
  Header,
  Whole,
}

/// Returns the span of a body, either just the header or the entire item
fn body_span(tcx: TyCtxt, def_id: DefId, body_span_type: BodySpanType) -> Span {
  let hir = tcx.hir();
  let fn_node = hir.body_owner(hir.body_owned_by(def_id.expect_local()).id());
  match body_span_type {
    BodySpanType::Header => hir.span(fn_node),
    BodySpanType::Whole => hir.span_with_body(fn_node),
  }
}

type FrameLocals<'tcx> = Vec<(Local, String, OpTy<'tcx>)>;
type MiriFrame<'tcx> =
  miri::Frame<'tcx, miri::Provenance, miri::FrameExtra<'tcx>>;

#[derive(Copy, Clone)]
struct LocalFrame<'a, 'tcx> {
  current: bool,
  local_index: usize,
  global_index: usize,
  frame: &'a MiriFrame<'tcx>,
}

impl<'tcx> VisEvaluator<'tcx> {
  pub fn new(tcx: TyCtxt<'tcx>) -> Result<Self> {
    let (main_id, entry_fn_type) = tcx
      .entry_fn(())
      .context("no main or start function found")?;
    let ecx = miri::create_ecx(tcx, main_id, entry_fn_type, &MiriConfig {
      mute_stdout_stderr: true,
      // have to make sure miri doesn't complain about us poking around memory
      validation: miri::ValidationMode::No,
      borrow_tracker: None,
      ..Default::default()
    })
    .report_err()
    .map_err(|e| {
      anyhow!("{}", e.into_kind().diagnostic_message().as_str().unwrap())
    })?;

    // Ensures we get nice backtraces from miri evaluation errors
    *tcx.sess.ctfe_backtrace.borrow_mut() = CtfeBacktrace::Capture;

    Ok(VisEvaluator {
      ecx,
      memory_map: RefCell::default(),
      moved_places: RefCell::new(MovedPlaces::new()),
    })
  }

  pub(super) fn remap_alloc_id(&self, alloc_id: AllocId) -> usize {
    let mut memory_map = self.memory_map.borrow_mut();
    let n = memory_map.alloc_id_remapping.len();
    *memory_map.alloc_id_remapping.entry(alloc_id).or_insert(n)
  }

  pub(super) fn fn_name(&self, def_id: DefId) -> String {
    self.ecx.tcx.def_path_str(def_id)
  }

  pub(super) fn place_elem_to_path_segment(
    &self,
    elem: PlaceElem<'tcx>,
  ) -> MPathSegment {
    match elem {
      PlaceElem::Field(f, _) => MPathSegment::Field(f.as_usize()),
      PlaceElem::Index(i) => MPathSegment::Index(i.as_usize()),
      PlaceElem::Subslice { from, to, .. } => {
        MPathSegment::Subslice(from as usize, to as usize)
      }
      _ => todo!(),
    }
  }

  fn build_frame(
    &self,
    LocalFrame {
      frame,
      local_index,
      global_index,
      current,
    }: LocalFrame<'_, 'tcx>,
    loc_override: MirLoc<'tcx>,
    locals: FrameLocals<'tcx>,
  ) -> InterpResult<'tcx, MFrame<MirLoc<'tcx>>> {
    log::trace!("Building frame {local_index}");

    let def_id = frame.instance().def_id();
    let name = self.fn_name(def_id);

    let tcx = *self.ecx.tcx;
    let body_span = CharRange::from_span(
      body_span(tcx, frame.instance().def_id(), BodySpanType::Whole),
      tcx.sess.source_map(),
    )
    .unwrap();

    let current_loc = if current {
      loc_override
    } else {
      (frame.instance(), frame.current_loc())
    };

    let moved_places = self.moved_places.borrow();
    let moved_place_map = moved_places
      .places_at(global_index)
      .map(|place| (place.local, place))
      .into_group_map();

    let locals = locals
      .into_iter()
      .map(|(local, name, op_ty)| {
        log::trace!("Reading local {name:?}");
        let value = self.read(&op_ty)?;
        let moved_paths = match moved_place_map.get(&local) {
          Some(moves) => moves
            .iter()
            .map(|place| {
              place
                .projection
                .iter()
                .map(|elem| self.place_elem_to_path_segment(elem))
                .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>(),
          None => Vec::new(),
        };
        interp_ok(MLocal {
          name,
          value,
          moved_paths,
        })
      })
      .collect::<InterpResult<'_, Vec<_>>>()?;

    interp_ok(MFrame {
      name,
      body_span,
      locals,
      location: current_loc,
    })
  }

  pub(super) fn mem_is_initialized(
    &self,
    layout: TyAndLayout<'tcx>,
    allocation: &miri::Allocation<
      miri::Provenance,
      miri::AllocExtra,
      miri::MiriAllocBytes,
    >,
  ) -> bool {
    // TODO: this should be recursive over the type. Only handles one-step right now.
    let ranges = match &layout.fields {
      FieldsShape::Primitive | FieldsShape::Array { .. } => vec![AllocRange {
        start: Size::ZERO,
        size: allocation.size(),
      }],
      FieldsShape::Arbitrary { offsets, .. } => offsets
        .iter()
        .enumerate()
        .map(|(i, offset)| {
          let field = layout.field(&self.ecx, i);
          AllocRange {
            start: *offset,
            size: field.size,
          }
        })
        .collect(),
      FieldsShape::Union(_) => unimplemented!(),
    };

    let init_mask = allocation.init_mask();
    ranges
      .into_iter()
      .all(|range| init_mask.is_range_initialized(range).is_ok())
  }

  fn test_local(
    &self,
    frame: &MiriFrame<'tcx>,
    frame_index: usize,
    local: Local,
    state: &LocalState<'tcx, miri::Provenance>,
  ) -> InterpResult<'tcx, Option<(String, OpTy<'tcx>)>> {
    let decl = &frame.body().local_decls[local];
    let name = if local == RETURN_PLACE {
      // Don't include unit return types in locals
      if decl.ty.is_unit() {
        log::trace!("Ignoring local {local:?} because it's a unit type");
        return interp_ok(None);
      }

      "(return)".into()
    } else {
      // TODO: this excludes compiler-generated temporaries which we sometimes need to
      // visualize in the case of f(&Some(x)). Need to figure out a good strategy for
      // deciding when a temp should be included.
      let has_debug_info = frame
        .body()
        .var_debug_info
        .iter()
        .filter_map(|info| match info.value {
          VarDebugInfoContents::Place(p) => {
            if p.projection.is_empty() {
              Some(p.local)
            } else {
              None
            }
          }
          _ => None,
        })
        .any(|debug| local == debug);
      if !has_debug_info {
        log::trace!(
          "Ignoring local {local:?} because it's not a source-level variable"
        );
        return interp_ok(None);
      }

      Place::from_local(local, *self.ecx.tcx)
        .to_string(*self.ecx.tcx, frame.body())
        .unwrap_or_else(|| String::from("(tmp)"))
    };

    // Ignore dead locals
    let Some(op) = state.as_mplace_or_imm() else {
      log::trace!("Ignoring local {local:?} because it's not live");
      return interp_ok(None);
    };

    let layout = self.ecx.layout_of_local(frame, local, None)?;

    match op {
      // Ignore uninitialized locals
      Either::Right(Immediate::Uninit) => {
        // Special case: a unit struct is considered uninitialized, but we would still like to
        // visualize it at the toplevel, so we handle that here. Might need to make this a configurable thing?
        if !layout.is_zst() {
          log::trace!("Ignoring local {local:?} because it's uninitialized and not zero-sized");
          return interp_ok(None);
        }
      }

      // If a local is Indirect, meaning there exists a pointer to it,
      // then save its allocation in `MemoryMap::stack_slots`
      Either::Left((ptr, _)) => {
        let mut memory_map = self.memory_map.borrow_mut();
        let (alloc_id, _, _) = self
          .ecx
          .ptr_get_alloc_id(ptr, layout.size.bytes() as i64)
          .unwrap();

        // Have to handle the case that a local is uninitialized and indirect
        let (_, allocation) =
          self.ecx.memory.alloc_map().get(alloc_id).unwrap();
        if !self.mem_is_initialized(layout, allocation) {
          log::trace!("Ignoring local {local:?} because it's a pointer to uninitialized memory");
          return interp_ok(None);
        }

        memory_map
          .stack_slots
          .insert(alloc_id, (frame_index, name.clone(), layout));
      }
      _ => {}
    };

    let op_ty = self.ecx.local_at_frame_to_op(frame, local, Some(layout))?;
    interp_ok(Some((name, op_ty)))
  }

  fn find_locals(&self) -> InterpResult<'tcx, Vec<FrameLocals<'tcx>>> {
    self
      .local_frames()
      .map(
        |LocalFrame {
           local_index, frame, ..
         }| {
          frame
            .locals
            .iter_enumerated()
            .filter_map(|(local, state)| {
              let local_data_res = self
                .test_local(frame, local_index, local, state)
                .report_err()
                .transpose()?;
              Some(local_data_res.map(|(name, op)| (local, name, op)).into())
            })
            .collect::<InterpResult<'tcx, Vec<_>>>()
        },
      )
      .collect()
  }

  fn build_stack(
    &self,
    current_loc: MirLoc<'tcx>,
  ) -> InterpResult<'tcx, MStack<MirLoc<'tcx>>> {
    let locals = self.find_locals()?;
    let frames = self
      .local_frames()
      .zip(locals)
      .map(|(frame, locals)| self.build_frame(frame, current_loc, locals))
      .collect::<InterpResult<'_, _>>()?;
    interp_ok(MStack { frames })
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
      return interp_ok(None);
    }

    log::trace!("Building heap");
    let heap = self.build_heap();

    log::trace!("Step built!");
    interp_ok(Some(MStep { stack, heap }))
  }

  /// Get the stack frames for functions defined in the local crate
  fn local_frames(&self) -> impl Iterator<Item = LocalFrame<'_, 'tcx>> {
    let stack = Machine::stack(&self.ecx);
    let n = stack.len();
    stack
      .iter()
      .enumerate()
      .filter(|(_, frame)| frame.instance().def_id().is_local())
      .enumerate()
      .map(move |(local_index, (global_index, frame))| LocalFrame {
        current: global_index == n - 1,
        local_index,
        global_index,
        frame,
      })
  }

  fn collect_moves(&self) -> InterpResult<'tcx, Vec<Place<'tcx>>> {
    let stack = Machine::stack(&self.ecx);
    let Some(frame) = stack.last() else {
      return interp_ok(Vec::new());
    };
    let Either::Left(loc) = frame.current_loc() else {
      return interp_ok(Vec::new());
    };

    struct CollectMoves<'tcx> {
      places: Vec<Place<'tcx>>,
    }

    impl<'tcx> Visitor<'tcx> for CollectMoves<'tcx> {
      fn visit_operand(
        &mut self,
        operand: &mir::Operand<'tcx>,
        _location: Location,
      ) {
        if let mir::Operand::Move(place) = operand {
          self.places.push(*place);
        }
      }
    }

    let mut collector = CollectMoves { places: Vec::new() };
    collector.visit_location(frame.body(), loc);

    interp_ok(collector.places)
  }

  fn handle_moves(
    &mut self,
    n_frames: usize,
    moves: Vec<Place<'tcx>>,
  ) -> InterpResult<'tcx, ()> {
    let n_frames_after = Machine::stack(&self.ecx).len();
    let mut moved_places = self.moved_places.borrow_mut();
    match n_frames_after.cmp(&n_frames) {
      Ordering::Greater => moved_places.push_frame(),
      Ordering::Less => moved_places.pop_frame(),
      Ordering::Equal => {
        for place in moves {
          let place_ty = self.ecx.eval_place(place)?;
          match place_ty.as_mplace_or_local() {
            Either::Left(_mplace) => {
              // todo!()
            }
            Either::Right((local, ..)) => {
              moved_places.add_place(
                n_frames - 1,
                Place::from_local(local, self.ecx.tcx()),
              );
            }
          }
        }
      }
    }

    interp_ok(())
  }

  /// Take a single (local) step, internally stepping until we reach a serialization point
  fn step(
    &mut self,
  ) -> InterpResult<'tcx, (Option<MStep<MirLoc<'tcx>>>, bool)> {
    let get_current_local_loc = |local_frames: &[LocalFrame<'_, 'tcx>]| {
      let LocalFrame { frame, .. } = local_frames.last()?;
      let loc = frame.current_loc();
      Some((frame.instance(), loc))
    };

    loop {
      let local_frames = self.local_frames().collect::<Vec<_>>();
      let n_local_frames = local_frames.len();

      let current_loc_opt = get_current_local_loc(&local_frames);

      let caller_frame_loc = local_frames
        .get(local_frames.len().wrapping_sub(2))
        .map(|LocalFrame { frame, .. }| frame.current_loc());

      let moves = self.collect_moves()?;
      let n_all_frames: usize = Machine::stack(&self.ecx).len();
      let more_work: bool = self.ecx.step()?;
      self.handle_moves(n_all_frames, moves)?;

      let local_frames_after = self.local_frames().collect::<Vec<_>>();
      let current_loc_opt = match local_frames_after.len().cmp(&n_local_frames)
      {
        Ordering::Greater => {
          let LocalFrame { frame, .. } = local_frames_after.last().unwrap();
          let span = body_span(
            *self.ecx.tcx,
            frame.instance().def_id(),
            BodySpanType::Header,
          );

          Some((frame.instance(), Either::Right(span)))
        }
        Ordering::Less => {
          if let Some(caller_frame_loc) = caller_frame_loc {
            let LocalFrame { frame, .. } = local_frames_after.last().unwrap();
            Some((frame.instance(), caller_frame_loc))
          } else {
            current_loc_opt
          }
        }
        Ordering::Equal => current_loc_opt,
      };

      if let Some(current_loc) = current_loc_opt {
        if let Some(step) = self.build_step(current_loc)? {
          return interp_ok((Some(step), more_work));
        }
      }

      if !more_work {
        return interp_ok((None, more_work));
      }
    }
  }

  fn beautify_error(
    &mut self,
    e: InterpErrorInfo,
  ) -> Result<MUndefinedBehavior> {
    use UndefinedBehaviorInfo::PointerUseAfterFree;

    Ok(match e.into_kind() {
      InterpErrorKind::UndefinedBehavior(ub) => match ub {
        PointerUseAfterFree(alloc_id, _) => {
          MUndefinedBehavior::PointerUseAfterFree {
            alloc_id: self.remap_alloc_id(alloc_id),
          }
        }
        ub => MUndefinedBehavior::Other(
          ub.diagnostic_message().as_str().unwrap().to_string(),
        ),
      },
      err => bail!("{}", err.diagnostic_message().as_str().unwrap()),
    })
  }

  /// Evaluate the program to completion, returning a vector of MIR steps for local functions
  pub fn eval(&mut self) -> Result<MTrace<MirLoc<'tcx>>> {
    let mut steps = Vec::new();
    let result = loop {
      match self.step().report_err() {
        Ok((step, more_work)) => {
          if let Some(step) = step {
            steps.push(step);
          }
          if !more_work {
            break MResult::Success;
          }
        }
        Err(e) => {
          // e.print_backtrace();
          break MResult::Error(self.beautify_error(e)?);
        }
      }
    };

    Ok(MTrace { steps, result })
  }
}
