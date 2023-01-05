//! Interpreting memory as Rust data types

use miri::{
  AllocMap, Immediate, InterpResult, MPlaceTy, MemoryKind, OpTy, Value,
};
use rustc_abi::FieldsShape;
use rustc_apfloat::Float;
use rustc_middle::{
  mir::PlaceElem,
  ty::{
    layout::{LayoutOf, TyAndLayout},
    AdtKind, Ty, TyKind,
  },
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
    mk: impl Fn(u64) -> InterpResult<'tcx, T>,
  ) -> InterpResult<'tcx, Self> {
    if n <= ABBREV_MAX {
      let elts = (0 .. n).map(mk).collect::<InterpResult<'_, Vec<_>>>()?;
      Ok(Abbreviated::All(elts))
    } else {
      let initial = (0 .. ABBREV_MAX - 1)
        .map(&mk)
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

#[derive(Serialize, Deserialize, Debug, TS, PartialEq)]
#[serde(tag = "type", content = "value")]
#[ts(export)]
pub enum MValue {
  // Primitives
  Bool(bool),
  Char(String),
  Uint(u64),
  Int(i64),
  Float(f64),

  // General-purpose composites
  Pointer(MPath),
  Tuple(Vec<MValue>),
  Struct {
    name: String,
    fields: Vec<(String, MValue)>,
  },
  Enum {
    name: String,
    variant: String,
    fields: Vec<(String, MValue)>,
  },

  // Special-case composites
  String(Abbreviated<u64>),
  Array(Abbreviated<MValue>),

  Unallocated,
}

impl<'tcx> VisEvaluator<'_, 'tcx> {
  fn get_path_segments(
    &self,
    alloc_size: Size,
    alloc_layout: TyAndLayout<'tcx>,
    target: Size,
  ) -> Vec<MPathSegment> {
    let segments =
      locate_address_in_type(&self.ecx, alloc_layout, alloc_size, target);
    segments
      .into_iter()
      .map(|segment| match segment {
        PlaceElem::Field(f, _) => MPathSegment::Field(f.as_usize()),
        PlaceElem::Index(i) => MPathSegment::Index(i.as_usize()),
        _ => todo!(),
      })
      .collect()
  }

  /// Reads a pointer, registering the pointed data for later use.
  fn read_pointer(
    &self,
    mplace: miri::MPlaceTy<'tcx, miri::Provenance>,
    postprocess: impl FnOnce(
      miri::MPlaceTy<'tcx, miri::Provenance>,
    ) -> InterpResult<'tcx, MValue>,
  ) -> InterpResult<'tcx, MValue> {
    // Determine the base allocation from the mplace's provenance
    let (alloc_id, offset, _) = self.ecx.ptr_get_alloc_id(mplace.ptr)?;

    // Check if we have seen this allocation before
    let alloc_discovered = self
      .memory_map
      .borrow()
      .place_to_loc
      .contains_key(&alloc_id);

    if !alloc_discovered {
      // If we haven't seen this allocation, then use `postprocess` to convert
      // the raw memory value into an understandable MValue.
      let mvalue = postprocess(mplace)?;

      // Get the kind of memory we're looking at (either stack or heap)
      // from the allocation metadata.
      let (memory_kind, _) = self.ecx.memory.alloc_map().get(alloc_id).unwrap();

      // Generate a corresponding `MMemorySegment` that locates the base allocation,
      // and a `TyAndLayout` that describes the allocation's entire layout.
      let mut memory_map = self.memory_map.borrow_mut();
      let (segment, layout) = match memory_kind {
        MemoryKind::Stack => {
          // Look up the stack layout in `MemoryMap::stack_slots` which are generated
          // in `VisEvaluator::build_heap`.
          let (frame, local, layout) =
            memory_map.stack_slots[&alloc_id].clone();
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

    let (segment, layout) =
      self.memory_map.borrow().place_to_loc[&alloc_id].clone();
    let (alloc_size, _, _) = self.ecx.get_alloc_info(alloc_id);

    // The pointer could point anywhere inside the allocation, so we use
    // `get_path_segments` to reverse-engineer a path from the memory location.
    let parts = self.get_path_segments(alloc_size, layout, offset);

    Ok(MValue::Pointer(MPath { segment, parts }))
  }

  fn read_unique(
    &self,
    op: &OpTy<'tcx, miri::Provenance>,
    postprocess: impl FnOnce(
      miri::MPlaceTy<'tcx, miri::Provenance>,
    ) -> InterpResult<'tcx, MValue>,
  ) -> InterpResult<'tcx, MValue> {
    debug_assert!(op.layout.ty.is_adt());

    let (_, nonnull) = op.field_by_name("pointer", &self.ecx)?;
    let (_, ptr) = nonnull.field_by_name("pointer", &self.ecx)?;

    // This may be unallocated e.g. if a value containing a vector is partially initialized.
    // For example, `let x = (0, vec![1])` will have the vector temporarily invalid, but with
    // space allocated on the stack. So we ensure that doesn't fail.
    let Ok(mplace) = self.ecx.deref_operand(&ptr) else {
      return Ok(MValue::Unallocated);
    };

    self.read_pointer(mplace, postprocess)
  }

  fn read_array(
    &self,
    base: MPlaceTy<'tcx, miri::Provenance>,
    stride: Size,
    len: u64,
    el_ty: Ty<'tcx>,
  ) -> InterpResult<'tcx, MValue> {
    let read = |i: u64| {
      let offset = stride * i;
      let layout = self.ecx.layout_of(el_ty)?;
      let offset_place = base.offset(offset, layout, &self.ecx)?;
      self.read(&offset_place.into())
    };
    let values = Abbreviated::new(len, read)?;
    Ok(MValue::Array(values))
  }

  fn read_raw_vec(
    &self,
    op: &OpTy<'tcx, miri::Provenance>,
    len: u64,
    postprocess: Option<impl Fn(MValue) -> MValue>,
  ) -> InterpResult<'tcx, MValue> {
    let (_, ptr) = op.field_by_name("ptr", &self.ecx)?;
    self.read_unique(&ptr, |base| {
      let array =
        self.read_array(base, base.layout.size, len, base.layout.ty)?;
      Ok(match postprocess {
        Some(f) => f(array),
        None => array,
      })
    })
  }

  fn read_vec(
    &self,
    op: &OpTy<'tcx, miri::Provenance>,
    postprocess: Option<impl Fn(MValue) -> MValue>,
  ) -> InterpResult<'tcx, MValue> {
    let (_, buf) = op.field_by_name("buf", &self.ecx)?;
    let (_, len) = op.field_by_name("len", &self.ecx)?;
    let len = match self.read(&len) {
      Err(_) => return Ok(MValue::Unallocated),
      Ok(MValue::Uint(len)) => len,
      _ => unreachable!(),
    };
    self.read_raw_vec(&buf, len, postprocess)
  }

  pub(super) fn read(
    &self,
    op: &OpTy<'tcx, miri::Provenance>,
  ) -> InterpResult<'tcx, MValue> {
    let ty = op.layout.ty;

    Ok(match ty.kind() {
      _ if ty.is_box() => {
        let unique = op.project_field(&self.ecx, 0)?;
        self.read_unique(&unique, |mplace| self.read(&OpTy::from(mplace)))?
      }

      TyKind::Tuple(tys) => {
        let fields = (0 .. tys.len())
          .map(|i| {
            let field_op = op.project_field(&self.ecx, i)?;
            self.read(&field_op)
          })
          .collect::<InterpResult<'tcx, Vec<_>>>()?;

        MValue::Tuple(fields)
      }

      TyKind::Adt(adt_def, _) => {
        let def_id = adt_def.did();
        let name = self.ecx.tcx.item_name(def_id).to_ident_string();
        match adt_def.adt_kind() {
          AdtKind::Struct => match name.as_str() {
            "String" => {
              let (_, vec) = op.field_by_name("vec", &self.ecx)?;
              self.read_vec(
                &vec,
                Some(&|v: MValue| {
                  let MValue::Array(values) = v else { unreachable!() };
                  let chars = values.map(|el| {
                    let MValue::Uint(c) = el else { unreachable!() };
                    c
                  });
                  MValue::String(chars)
                }),
              )?
            }
            "Vec" => self.read_vec(op, Option::<fn(_) -> _>::None)?,
            _ => {
              let fields = adt_def
                .all_fields()
                .enumerate()
                .map(|(i, field)| {
                  let field_op = op.project_field(&self.ecx, i)?;
                  let field_val = self.read(&field_op)?;
                  Ok((field.name.to_ident_string(), field_val))
                })
                .collect::<InterpResult<'tcx, Vec<_>>>()?;

              MValue::Struct { name, fields }
            }
          },
          AdtKind::Enum => {
            let (_, variant_idx) = self.ecx.read_discriminant(op)?;
            let casted = op.project_downcast(&self.ecx, variant_idx)?;
            let variant_def = adt_def.variant(variant_idx);
            let variant = variant_def.name.to_ident_string();
            let fields = variant_def
              .fields
              .iter()
              .enumerate()
              .map(|(i, field)| {
                let field_op = casted.project_field(&self.ecx, i)?;
                let field_val = self.read(&field_op)?;
                Ok((field.name.to_ident_string(), field_val))
              })
              .collect::<InterpResult<'tcx, Vec<_>>>()?;
            MValue::Enum {
              name,
              variant,
              fields,
            }
          }
          _ => todo!(),
        }
      }

      _ if ty.is_primitive() => {
        let imm = self.ecx.read_immediate(op)?;
        let scalar = match &*imm {
          Immediate::Scalar(scalar) => scalar,
          _ => unreachable!(),
        };
        match ty.kind() {
          TyKind::Bool => MValue::Bool(scalar.to_bool()?),
          TyKind::Char => MValue::Char(scalar.to_char()?.to_string()),
          TyKind::Uint(uty) => MValue::Uint(match uty.bit_width() {
            Some(width) => scalar.to_uint(Size::from_bits(width))? as u64,
            None => scalar.to_machine_usize(&self.ecx)?,
          }),
          TyKind::Int(ity) => MValue::Int(match ity.bit_width() {
            Some(width) => scalar.to_int(Size::from_bits(width))? as i64,
            None => scalar.to_machine_isize(&self.ecx)?,
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
        self.read_pointer(mplace, |mplace| {
          // let length = mplace.meta.unwrap_meta().to_u64()?;
          // eprintln!("{length}");
          let s = self.ecx.read_str(&mplace)?;
          let chars = s.chars().collect::<Vec<_>>();
          let abbrev =
            Abbreviated::new(s.len() as u64, |i| Ok(chars[i as usize] as u64))?;
          Ok(MValue::String(abbrev))
        })?
      }

      _ if ty.is_any_ptr() => {
        let Ok(mplace) = self.ecx.deref_operand(op) else { return Ok(MValue::Unallocated) };
        self.read_pointer(mplace, |mplace| self.read(&OpTy::from(mplace)))?
      }

      kind => todo!("{:?} / {:?}", **op, kind),
    })
  }
}
