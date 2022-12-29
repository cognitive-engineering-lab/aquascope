

use std::collections::hash_map::DefaultHasher;
use std::hash::{Hasher, Hash};

use miri::{Immediate, InterpCx, InterpResult, Machine, OpTy, Value, MPlaceTy};
use rustc_apfloat::Float;
use rustc_middle::mir::interpret::Provenance;

use rustc_middle::ty::{AdtKind, FieldDef, TyKind};
use rustc_target::abi::Size;
use rustc_type_ir::FloatTy;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use super::eval::VisEvaluator;

pub type MLocation = usize;

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

  // Special-case composites
  String(String),
  Array(Vec<MValue>),

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
  fn read_unique(&self, op: &OpTy<'tcx, miri::Provenance>) -> InterpResult<'tcx, MValue> {
    debug_assert!(op.layout.ty.is_adt());

    let (_, nonnull) = op.field_by_name("pointer", &self.ecx)?;
    let (_, ptr) = nonnull.field_by_name("pointer", &self.ecx)?;

    let Ok(mplace) = self.ecx.deref_operand(&ptr) else {
      return Ok(MValue::Unallocated) 
    };
    let mut heap_allocs = self.heap_allocs.borrow_mut();

    let key = |m: &MPlaceTy<'tcx, miri::Provenance>| self.ecx.read_immediate(&OpTy::from(*m)).unwrap().to_scalar();

    let idx = match heap_allocs.iter().position(|mp| key(&mplace) == key(mp)) {
      Some(idx) => idx,
      None => {
        heap_allocs.push(mplace);
        heap_allocs.len() - 1
      }
    };
    Ok(MValue::Pointer(idx))
  }

  // fn read_raw_vec(
  //   &self,
  //   op: &OpTy<'tcx, miri::Provenance>,
  //   len: u64,
  // ) -> InterpResult<'tcx, Vec<MValue>> {
  //   let (_, unique_t) = op.field_by_name("ptr", &self.ecx)?;
  //   let place = match self.ecx.deref_operand(&ptr) {
  //     Ok(op) => op,
  //     Err(_) => {
  //       return Ok(vec![MValue::Unallocated]);
  //     }
  //   };

  //   (0..len)
  //     .map(|i| {
  //       let offset = place.layout.size * i;
  //       let offset_place = place.offset(offset, place.layout, &self.ecx)?;
  //       self.read(&offset_place.into())
  //     })
  //     .collect::<InterpResult<'tcx, Vec<_>>>()
  // }

  pub(super) fn read(&self, op: &OpTy<'tcx, miri::Provenance>) -> InterpResult<'tcx, MValue> {
    let ty = op.layout.ty;

    Ok(match ty.kind() {
      _ if ty.is_box() => {
        let unique = op.project_field(&self.ecx, 0)?;
        self.read_unique(&unique)?
      }

      TyKind::Adt(adt_def, _subst) => match adt_def.adt_kind() {
        AdtKind::Struct => {
          let def_id = adt_def.did();
          let name = self.tcx.item_name(def_id).to_ident_string();

          // match self.type_def_ids.get_path(def_id) {
          //   Some(path) => match path.as_str() {
          //     "std::vec::Vec" => {
          //       let (_, len_field) = op.field_by_name("len", &self.ecx)?;
          //       let len = match self.read(&len_field)? {
          //         MValue::Uint(n) => n,
          //         _ => unreachable!(),
          //       };
          //       let (_, buf_field) = op.field_by_name("buf", &self.ecx)?;
          //       let contents = self.read_raw_vec(&buf_field, len)?;
          //       MValue::Vec(contents)
          //     }
          //     _ => todo!(),
          //   },
          //   None => {
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
          // }
          // }
        }
        _ => todo!(),
      },

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

      _ if ty.is_str() => MValue::String(self.ecx.read_str(&op.assert_mem_place())?.to_string()),

      _ if ty.is_any_ptr() => match self.ecx.deref_operand(op) {
        Ok(mplace) => self.read(&mplace.into())?,
        Err(_) => MValue::Unallocated,
      },

      kind => todo!("{:?} / {:?}", **op, kind),
    })
  }
}
