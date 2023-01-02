use miri::{
  AllocMap, Immediate, InterpCx, InterpResult, MPlaceTy, Machine, MemoryKind, OpTy, Value,
};
use rustc_abi::FieldsShape;
use rustc_apfloat::Float;
use rustc_middle::mir::interpret::Provenance;

use rustc_middle::ty::layout::LayoutOf;
use rustc_middle::ty::{AdtKind, FieldDef, Ty, TyKind};
use rustc_target::abi::Size;
use rustc_type_ir::FloatTy;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use super::eval::VisEvaluator;

#[derive(Serialize, Deserialize, Clone, Debug, TS, PartialEq)]
#[serde(tag = "type", content = "value")]
#[ts(export)]
pub enum MLocation {
  Stack { frame: usize, local: String },
  Heap { index: usize },
}

const ABBREV_MAX: u64 = 10;

#[derive(Serialize, Deserialize, Debug, TS, PartialEq)]
#[serde(tag = "type", content = "value")]
#[ts(export)]
pub enum Abbreviated<T> {
  All(Vec<T>),
  Only(Vec<T>, Box<T>),
}

impl<T> Abbreviated<T> {
  pub fn new<'tcx>(n: u64, mk: impl Fn(u64) -> InterpResult<'tcx, T>) -> InterpResult<'tcx, Self> {
    if n <= ABBREV_MAX {
      let elts = (0..n).map(mk).collect::<InterpResult<'_, Vec<_>>>()?;
      Ok(Abbreviated::All(elts))
    } else {
      let initial = (0..ABBREV_MAX)
        .map(&mk)
        .collect::<InterpResult<'tcx, Vec<_>>>()?;
      let last = mk(n - 1)?;
      Ok(Abbreviated::Only(initial, Box::new(last)))
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
  Pointer(MLocation),
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
  String(String),
  Array(Abbreviated<MValue>),

  Unallocated,
}

trait OpTyExt<'mir, 'tcx, M: Machine<'mir, 'tcx>>: Sized {
  fn field_by_name(
    &self,
    name: &str,
    ecx: &InterpCx<'mir, 'tcx, M>,
  ) -> InterpResult<'tcx, (&FieldDef, Self)>;
}

impl<'mir, 'tcx, M, Prov: Provenance> OpTyExt<'mir, 'tcx, M> for OpTy<'tcx, Prov>
where
  M: Machine<'mir, 'tcx, Provenance = Prov>,
  'tcx: 'mir,
{
  fn field_by_name(
    &self,
    name: &str,
    ecx: &InterpCx<'mir, 'tcx, M>,
  ) -> InterpResult<'tcx, (&FieldDef, Self)> {
    let adt_def = self.layout.ty.ty_adt_def().unwrap();
    let (i, field) = adt_def
      .all_fields()
      .enumerate()
      .find(|(_, field)| field.name.as_str() == name)
      .unwrap_or_else(|| {
        panic!(
          "Could not find field with name `{name}` out of fields: {:?}",
          adt_def
            .all_fields()
            .map(|field| field.name)
            .collect::<Vec<_>>()
        )
      });
    Ok((field, self.project_field(ecx, i)?))
  }
}

// fn calculate_hash<T: Hash>(t: &T) -> u64 {
//   let mut s = DefaultHasher::new();
//   t.hash(&mut s);
//   s.finish()
// }

impl<'tcx> VisEvaluator<'_, 'tcx> {
  fn add_alloc(
    &self,
    mplace: miri::MPlaceTy<'tcx, miri::Provenance>,
    postprocess: impl FnOnce(miri::MPlaceTy<'tcx, miri::Provenance>) -> InterpResult<'tcx, MValue>,
  ) -> InterpResult<'tcx, MValue> {
    let addr = mplace.ptr.addr();
    let memory_kind = match mplace.ptr.provenance {
      Some(miri::Provenance::Concrete { alloc_id, .. }) => {
        let (memory_kind, _) = self.ecx.memory.alloc_map().get(alloc_id).unwrap();
        memory_kind
      }
      _ => unreachable!(),
    };
    let already_found = self.memory_map.borrow().place_to_loc.contains_key(&addr);
    if !already_found {
      let mvalue = postprocess(mplace)?;
      let mut memory_map = self.memory_map.borrow_mut();
      let location = match memory_kind {
        MemoryKind::Stack => {
          let (frame, local) = memory_map.stack_slots[&addr].clone();
          MLocation::Stack { frame, local }
        }
        MemoryKind::Machine(..) => {
          let index = memory_map.heap.locations.len();
          memory_map.heap.locations.push(mvalue);
          MLocation::Heap { index }
        }
        _ => unimplemented!(),
      };

      memory_map.place_to_loc.insert(addr, location);
    }

    let location = self.memory_map.borrow().place_to_loc[&addr].clone();
    Ok(MValue::Pointer(location))
  }

  fn read_unique(
    &self,
    op: &OpTy<'tcx, miri::Provenance>,
    postprocess: impl FnOnce(miri::MPlaceTy<'tcx, miri::Provenance>) -> InterpResult<'tcx, MValue>,
  ) -> InterpResult<'tcx, MValue> {
    debug_assert!(op.layout.ty.is_adt());

    let (_, nonnull) = op.field_by_name("pointer", &self.ecx)?;
    let (_, ptr) = nonnull.field_by_name("pointer", &self.ecx)?;

    let Ok(mplace) = self.ecx.deref_operand(&ptr) else {
      return Ok(MValue::Unallocated);
    };

    self.add_alloc(mplace, postprocess)
  }

  fn read_array(
    &self,
    base: MPlaceTy<'tcx, miri::Provenance>,
    stride: Size,
    len: u64,
    el_ty: Ty<'tcx>,
    postprocess: Option<&dyn Fn(MValue) -> MValue>,
  ) -> InterpResult<'tcx, MValue> {
    let read = |i: u64| {
      let offset = stride * i;
      let layout = self.ecx.layout_of(el_ty)?;
      let offset_place = base.offset(offset, layout, &self.ecx)?;
      let value = self.read(&offset_place.into())?;
      Ok(match &postprocess {
        Some(f) => f(value),
        None => value,
      })
    };
    let values = Abbreviated::new(len, read)?;

    Ok(MValue::Array(values))
  }

  fn read_raw_vec(
    &self,
    op: &OpTy<'tcx, miri::Provenance>,
    len: u64,
    postprocess: Option<&dyn Fn(MValue) -> MValue>,
  ) -> InterpResult<'tcx, MValue> {
    let (_, ptr) = op.field_by_name("ptr", &self.ecx)?;
    self.read_unique(&ptr, |base| {
      self.read_array(base, base.layout.size, len, base.layout.ty, postprocess)
    })
  }

  fn read_vec(
    &self,
    op: &OpTy<'tcx, miri::Provenance>,
    postprocess: Option<&dyn Fn(MValue) -> MValue>,
  ) -> InterpResult<'tcx, MValue> {
    let (_, buf) = op.field_by_name("buf", &self.ecx)?;
    let (_, len) = op.field_by_name("len", &self.ecx)?;
    let MValue::Uint(len) = self.read(&len)? else { unreachable!() };
    self.read_raw_vec(&buf, len, postprocess)
  }

  pub(super) fn read(&self, op: &OpTy<'tcx, miri::Provenance>) -> InterpResult<'tcx, MValue> {
    let ty = op.layout.ty;

    Ok(match ty.kind() {
      _ if ty.is_box() => {
        let unique = op.project_field(&self.ecx, 0)?;
        self.read_unique(&unique, |mplace| self.read(&OpTy::from(mplace)))?
      }

      TyKind::Adt(adt_def, _subst) => {
        let def_id = adt_def.did();
        let name = self.tcx.item_name(def_id).to_ident_string();
        match adt_def.adt_kind() {
          AdtKind::Struct => match name.as_str() {
            "String" => {
              let (_, vec) = op.field_by_name("vec", &self.ecx)?;
              self.read_vec(
                &vec,
                Some(&|v: MValue| {
                  let MValue::Uint(c) = v else { unreachable!() };
                  MValue::Char(String::from(char::from(c as u8)))
                }),
              )?
            }
            "Vec" => self.read_vec(op, None)?,
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
            None => scalar.to_machine_usize(&self.ecx)? as u64,
          }),
          TyKind::Int(ity) => MValue::Int(match ity.bit_width() {
            Some(width) => scalar.to_int(Size::from_bits(width))? as i64,
            None => scalar.to_machine_isize(&self.ecx)? as i64,
          }),
          TyKind::Float(fty) => MValue::Float(match fty {
            FloatTy::F32 => f32::from_bits(scalar.to_f32()?.to_bits() as u32) as f64,
            FloatTy::F64 => f64::from_bits(scalar.to_f64()?.to_bits() as u64),
          }),
          _ => unreachable!(),
        }
      }

      TyKind::Array(el_ty, _) => {
        let base = op.assert_mem_place();
        let FieldsShape::Array { stride, count } = base.layout.layout.fields() else { unreachable!() };
        self.read_array(base, *stride, *count, *el_ty, None)?
      }

      _ if ty.is_str() => {
        let mplace = op.assert_mem_place();
        self.add_alloc(mplace, |mplace| {
          Ok(MValue::String(self.ecx.read_str(&mplace)?.to_string()))
        })?
      }

      _ if ty.is_any_ptr() => {
        let mplace = self.ecx.deref_operand(&op)?;
        self.add_alloc(mplace, |mplace| self.read(&OpTy::from(mplace)))?
      }

      kind => todo!("{:?} / {:?}", **op, kind),
    })
  }
}
