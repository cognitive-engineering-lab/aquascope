//! Types constantly move from miri to rustc_const_eval and change visibility.
//! It's easier to manage that here while it stabilizes and we upstream patches.

pub use miriwasm::{
  create_ecx, AllocExtra, FrameExtra, MiriAllocBytes, MiriConfig, MiriMachine,
  Provenance,
};
pub use rustc_const_eval::{
  interpret::{
    AllocId, AllocKind, AllocMap, AllocRange, Allocation, Frame, Immediate,
    InterpCx, InterpError, InterpErrorInfo, InterpResult, LocalState,
    LocalValue, MPlaceTy, Machine, MemPlaceMeta, MemoryKind, OpTy, Operand,
    Projectable, Provenance as ProvenanceTrait, UndefinedBehaviorInfo,
  },
  ReportErrorExt,
};
