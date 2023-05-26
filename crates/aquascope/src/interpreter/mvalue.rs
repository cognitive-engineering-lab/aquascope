//! Interpreting memory as Rust data types

use miri::{
  AllocKind, AllocMap, Immediate, InterpError, InterpErrorInfo, InterpResult,
  MPlaceTy, MemPlaceMeta, MemoryKind, OpTy, UndefinedBehaviorInfo, Value,
};
use rustc_abi::FieldsShape;
use rustc_apfloat::Float;
use rustc_middle::ty::{
  layout::{LayoutOf, TyAndLayout},
  AdtKind, Ty, TyKind,
};
use rustc_target::abi::Size;
use rustc_type_ir::FloatTy;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use super::{
  miri_utils::{locate_address_in_type, OpTyExt},
  step::VisEvaluator,
};

#[derive(Serialize, Deserialize, Clone, Debug, TS, PartialEq)]
#[serde(tag = "type", content = "value")]
#[ts(export)]
pub enum MMemorySegment {
  Stack { frame: usize, local: String },
  Heap { index: usize },
}

#[derive(Serialize, Deserialize, Clone, Debug, TS, PartialEq)]
#[serde(tag = "type", content = "value")]
#[ts(export)]
pub enum MPathSegment {
  Field(usize),
  Index(usize),
  Subslice(usize, usize),
}

#[derive(Serialize, Deserialize, Clone, Debug, TS, PartialEq)]
#[ts(export)]
pub struct MPath {
  segment: MMemorySegment,
  parts: Vec<MPathSegment>,
}

const ABBREV_MAX: u64 = 12;

#[derive(Serialize, Deserialize, Debug, TS, PartialEq)]
#[serde(tag = "type", content = "value")]
#[ts(export)]
pub enum Abbreviated<T> {
  All(Vec<T>),
  Only(Vec<T>, Box<T>),
}

impl<T> Abbreviated<T> {
  pub fn new<'tcx>(
    n: u64,
    mut mk: impl FnMut(u64) -> InterpResult<'tcx, T>,
  ) -> InterpResult<'tcx, Self> {
    if n <= ABBREV_MAX {
      let elts = (0 .. n).map(mk).collect::<InterpResult<'_, Vec<_>>>()?;
      Ok(Abbreviated::All(elts))
    } else {
      let initial = (0 .. ABBREV_MAX - 1)
        .map(&mut mk)
        .collect::<InterpResult<'tcx, Vec<_>>>()?;
      let last = mk(n - 1)?;
      Ok(Abbreviated::Only(initial, Box::new(last)))
    }
  }

  pub fn map<U>(self, f: impl Fn(T) -> U) -> Abbreviated<U> {
    match self {
      Abbreviated::All(v) => Abbreviated::All(v.into_iter().map(f).collect()),
      Abbreviated::Only(initial, last) => Abbreviated::Only(
        initial.into_iter().map(&f).collect(),
        Box::new(f(*last)),
      ),
    }
  }
}

#[derive(Serialize, Deserialize, Debug, TS, PartialEq, Copy, Clone)]
#[serde(tag = "type", content = "value")]
#[ts(export)]
pub enum MHeapAllocKind {
  String { len: u64 },
  Vec { len: u64 },
  Box,
}

#[derive(Serialize, Deserialize, Debug, TS, PartialEq)]
#[serde(tag = "type", content = "value")]
#[ts(export)]
pub enum MValue {
  // Primitives
  Bool(bool),
  Char(usize),
  Uint(u64),
  Int(i64),
  Float(f64),

  // Composites
  Tuple(Vec<MValue>),
  Array(Abbreviated<MValue>),
  Adt {
    name: String,
    variant: Option<String>,
    fields: Vec<(String, MValue)>,
    alloc_kind: Option<MHeapAllocKind>,
  },
  Pointer {
    path: MPath,
    range: Option<u64>,
  },

  Unallocated {
    alloc_id: Option<usize>,
  },
}

struct Reader<'a, 'mir, 'tcx> {
  ev: &'a VisEvaluator<'mir, 'tcx>,
  heap_alloc_kinds: Vec<MHeapAllocKind>,
}

impl<'tcx> Reader<'_, '_, 'tcx> {
  fn get_path_segments(
    &mut self,
    alloc_size: Size,
    alloc_layout: TyAndLayout<'tcx>,
    mplace: MPlaceTy<'tcx, miri::Provenance>,
    target: Size,
  ) -> Vec<MPathSegment> {
    let segments = locate_address_in_type(
      &self.ev.ecx,
      alloc_layout,
      alloc_size,
      mplace,
      target,
    );
    segments
      .into_iter()
      .map(|segment| self.ev.place_elem_to_path_segment(segment))
      .collect()
  }

  fn read_alloc(
    &mut self,
    base: miri::MPlaceTy<'tcx, miri::Provenance>,
  ) -> InterpResult<'tcx, MValue> {
    Ok(match self.heap_alloc_kinds.last() {
      Some(MHeapAllocKind::String { len }) => {
        let array =
          self.read_array(base, base.layout.size, *len, base.layout.ty)?;
        let MValue::Array(values) = array else { unreachable!() };
        let chars = values.map(|el| {
          let MValue::Uint(c) = el else { unreachable!() };
          MValue::Char(c as usize)
        });
        MValue::Array(chars)
      }
      Some(MHeapAllocKind::Vec { len }) => {
        self.read_array(base, base.layout.size, *len, base.layout.ty)?
      }
      Some(MHeapAllocKind::Box) | None => self.read(&OpTy::from(base))?,
    })
  }

  /// Reads a pointer, registering the pointed data for later use.
  fn read_pointer(
    &mut self,
    mplace: miri::MPlaceTy<'tcx, miri::Provenance>,
  ) -> InterpResult<'tcx, MValue> {
    // Determine the base allocation from the mplace's provenance
    let (alloc_id, offset, _) = self.ev.ecx.ptr_get_alloc_id(mplace.ptr)?;
    let (alloc_size, _, alloc_status) = self.ev.ecx.get_alloc_info(alloc_id);

    if matches!(alloc_status, AllocKind::Dead) {
      log::warn!("Reading a dead allocation");
    }

    // Check if we have seen this allocation before
    let alloc_discovered = self
      .ev
      .memory_map
      .borrow()
      .place_to_loc
      .contains_key(&alloc_id);

    if !alloc_discovered {
      // If we haven't seen this allocation, then use `postprocess` to convert
      // the raw memory value into an understandable MValue.
      let mvalue = self.read_alloc(mplace)?;

      // Get the kind of memory we're looking at (either stack or heap)
      // from the allocation metadata.
      let (memory_kind, _) =
        self.ev.ecx.memory.alloc_map().get(alloc_id).unwrap();

      // Generate a corresponding `MMemorySegment` that locates the base allocation,
      // and a `TyAndLayout` that describes the allocation's entire layout.
      let mut memory_map = self.ev.memory_map.borrow_mut();
      let (segment, layout) = match memory_kind {
        MemoryKind::Stack => {
          // Look up the stack layout in `MemoryMap::stack_slots` which are generated
          // in `VisEvaluator::build_heap`.
          let (frame, local, layout) =
            match memory_map.stack_slots.get(&alloc_id) {
              Some(t) => t.clone(),
              None => {
                drop(memory_map);
                return Ok(MValue::Unallocated {
                  alloc_id: Some(self.ev.remap_alloc_id(alloc_id)),
                });
              }
            };
          (MMemorySegment::Stack { frame, local }, layout)
        }
        MemoryKind::Machine(..) => {
          // Add this value to the heap, assuming that the layout is the same as `mplace`.
          //
          // NOTE: this assumes that a heap value is always first reached via its owner,
          // versus a reference to e.g. a portion of the heap data. Haven't verified whether
          // that property always holds.
          let index = memory_map.heap.locations.len();
          memory_map.heap.locations.push(mvalue);
          (MMemorySegment::Heap { index }, mplace.layout)
        }
        _ => unimplemented!(),
      };

      memory_map.place_to_loc.insert(alloc_id, (segment, layout));
    }

    let (segment, alloc_layout) =
      self.ev.memory_map.borrow().place_to_loc[&alloc_id].clone();

    // The pointer could point anywhere inside the allocation, so we use
    // `get_path_segments` to reverse-engineer a path from the memory location.
    let parts =
      self.get_path_segments(alloc_size, alloc_layout, mplace, offset);
    let path = MPath { segment, parts };

    let range = match mplace.meta {
      MemPlaceMeta::Meta(meta) => Some(meta.to_u64()?),
      MemPlaceMeta::None => None,
    };

    Ok(MValue::Pointer { path, range })
  }

  fn read_array(
    &mut self,
    base: MPlaceTy<'tcx, miri::Provenance>,
    stride: Size,
    len: u64,
    el_ty: Ty<'tcx>,
  ) -> InterpResult<'tcx, MValue> {
    let read = |i: u64| {
      let offset = stride * i;
      let layout = self.ev.ecx.layout_of(el_ty)?;
      let offset_place = base.offset(offset, layout, &self.ev.ecx)?;
      self.read(&offset_place.into())
    };
    let values = Abbreviated::new(len, read)?;
    Ok(MValue::Array(values))
  }

  fn read_vec_len(
    &mut self,
    op: &OpTy<'tcx, miri::Provenance>,
  ) -> InterpResult<'tcx, Option<u64>> {
    let (_, len) = op.field_by_name("len", &self.ev.ecx)?;
    let len = match self.read(&len) {
      Ok(MValue::Unallocated { .. }) => return Ok(None),
      Ok(MValue::Uint(len)) => len,
      _ => unreachable!(),
    };
    Ok(Some(len))
  }

  fn read(
    &mut self,
    op: &OpTy<'tcx, miri::Provenance>,
  ) -> InterpResult<'tcx, MValue> {
    let ty = op.layout.ty;

    let result = match ty.kind() {
      _ if ty.is_box() => {
        self.heap_alloc_kinds.push(MHeapAllocKind::Box);
        let unique = op.project_field(&self.ev.ecx, 0)?;
        let result = self.read(&unique)?;
        self.heap_alloc_kinds.pop();
        MValue::Adt {
          name: "Box".into(),
          variant: None,
          fields: vec![("0".into(), result)],
          alloc_kind: Some(MHeapAllocKind::Box),
        }
      }

      TyKind::Tuple(tys) => {
        let fields = (0 .. tys.len())
          .map(|i| {
            let field_op = op.project_field(&self.ev.ecx, i)?;
            self.read(&field_op)
          })
          .collect::<InterpResult<'tcx, Vec<_>>>()?;

        MValue::Tuple(fields)
      }

      TyKind::Adt(adt_def, _) => {
        let def_id = adt_def.did();
        let name = self.ev.ecx.tcx.item_name(def_id).to_ident_string();

        macro_rules! process_fields {
          ($op:expr, $fields:expr) => {{
            let mut fields = Vec::new();
            for (i, field) in $fields.enumerate() {
              let field_op = $op.project_field(&self.ev.ecx, i)?;

              // Skip ZST fields since they don't exist at runtime
              if field_op.layout.is_zst() {
                continue;
              }

              let field_val = self.read(&field_op)?;
              fields.push((field.name.to_ident_string(), field_val));
            }
            fields
          }};
        }

        match adt_def.adt_kind() {
          AdtKind::Struct => {
            let mut alloc_kind = None;
            match name.as_str() {
              "String" => {
                let (_, vec) = op.field_by_name("vec", &self.ev.ecx)?;
                if let Some(len) = self.read_vec_len(&vec)? {
                  alloc_kind = Some(MHeapAllocKind::String { len });
                }
              }
              "Vec" => {
                if let Some(len) = self.read_vec_len(op)? {
                  if !matches!(
                    self.heap_alloc_kinds.last(),
                    Some(MHeapAllocKind::String { .. })
                  ) {
                    alloc_kind = Some(MHeapAllocKind::Vec { len });
                  }
                }
              }
              _ => {}
            };

            if let Some(kind) = alloc_kind {
              self.heap_alloc_kinds.push(kind);
            }

            let fields = process_fields!(op, adt_def.all_fields());

            if let Some(..) = alloc_kind {
              self.heap_alloc_kinds.pop();
            }

            MValue::Adt {
              name,
              variant: None,
              fields,
              alloc_kind,
            }
          }
          AdtKind::Enum => {
            let (_, variant_idx) = self.ev.ecx.read_discriminant(op)?;
            let casted = op.project_downcast(&self.ev.ecx, variant_idx)?;
            let variant_def = adt_def.variant(variant_idx);
            let variant = variant_def.name.to_ident_string();

            let fields = process_fields!(casted, variant_def.fields.iter());

            MValue::Adt {
              name,
              variant: Some(variant),
              fields,
              alloc_kind: None,
            }
          }
          _ => todo!(),
        }
      }

      _ if ty.is_primitive() => {
        let imm = match self.ev.ecx.read_immediate(op) {
          Ok(imm) => imm,

          // It's possible to read uninitialized data if a data structure
          // is partially initialized, e.g. a tuple `(a, b)` where only `a`
          // is initialized. Therefore we have to handle this case by returning
          // MValue::Unallocated instead of throwing an error.
          Err(e) => match e.into_kind() {
            InterpError::UndefinedBehavior(
              UndefinedBehaviorInfo::InvalidUninitBytes(..),
            ) => return Ok(MValue::Unallocated { alloc_id: None }),
            e => return Err(InterpErrorInfo::from(e)),
          },
        };
        let Immediate::Scalar(scalar) = &*imm else {
          unreachable!()
        };
        match ty.kind() {
          TyKind::Bool => MValue::Bool(scalar.to_bool()?),
          TyKind::Char => MValue::Char(scalar.to_char()? as usize),
          TyKind::Uint(uty) => MValue::Uint(match uty.bit_width() {
            Some(width) => scalar.to_uint(Size::from_bits(width))? as u64,
            None => scalar.to_target_usize(&self.ev.ecx)?,
          }),
          TyKind::Int(ity) => MValue::Int(match ity.bit_width() {
            Some(width) => scalar.to_int(Size::from_bits(width))? as i64,
            None => scalar.to_target_isize(&self.ev.ecx)?,
          }),
          TyKind::Float(fty) => MValue::Float(match fty {
            FloatTy::F32 => {
              f32::from_bits(scalar.to_f32()?.to_bits() as u32) as f64
            }
            FloatTy::F64 => f64::from_bits(scalar.to_f64()?.to_bits() as u64),
          }),
          _ => unreachable!(),
        }
      }

      TyKind::Array(el_ty, _) => {
        let base = op.assert_mem_place();
        let FieldsShape::Array { stride, count } = base.layout.layout.fields() else { unreachable!() };
        self.read_array(base, *stride, *count, *el_ty)?
      }

      TyKind::Str => {
        let mplace = op.assert_mem_place();
        self.read_pointer(mplace)?
      }

      _ if ty.is_any_ptr() => {
        let val = self.ev.ecx.read_immediate(op)?;
        let mplace = self.ev.ecx.ref_to_mplace(&val)?;
        if self.ev.ecx.check_mplace(mplace).is_err() {
          let alloc_id = match self.ev.ecx.ptr_get_alloc_id(mplace.ptr) {
            Ok((alloc_id, _, _)) => Some(self.ev.remap_alloc_id(alloc_id)),
            Err(_) => None,
          };
          return Ok(MValue::Unallocated { alloc_id });
        }
        self.read_pointer(mplace)?
      }

      TyKind::Closure(def_id, substs) => {
        let closure = substs.as_closure();
        let name = self.ev.fn_name(*def_id);

        let upvar_names = match def_id.as_local() {
          Some(def_id) => {
            let tcx = self.ev.ecx.tcx;
            let captures = tcx.closure_captures(def_id);
            captures
              .iter()
              .map(|capture| capture.var_ident.to_string())
              .collect()
          }
          None => vec![String::from("(tmp)"); closure.upvar_tys().count()],
        };

        let env_ty = closure.tupled_upvars_ty();
        let mut env_op = op.clone();
        env_op.layout.ty = env_ty;
        let MValue::Tuple(env) = self.read(&env_op)? else { unreachable!() };
        let fields = upvar_names.into_iter().zip(env).collect();

        MValue::Adt {
          name,
          variant: None,
          fields,
          alloc_kind: None,
        }
      }

      kind => todo!("{:?} / {:?}", **op, kind),
    };

    Ok(result)
  }
}

impl<'tcx> VisEvaluator<'_, 'tcx> {
  pub(super) fn read(
    &self,
    op: &OpTy<'tcx, miri::Provenance>,
  ) -> InterpResult<'tcx, MValue> {
    Reader {
      ev: self,
      heap_alloc_kinds: Vec::new(),
    }
    .read(op)
  }
}
