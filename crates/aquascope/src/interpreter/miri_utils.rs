use miri::{
  interpret::Provenance, InterpCx, InterpResult, Machine, OpTy, Value,
};
use rustc_abi::FieldsShape;
use rustc_middle::{
  mir::{Field, Local, PlaceElem},
  ty::{layout::TyAndLayout, AdtKind, FieldDef, TyKind},
};
use rustc_target::abi::Size;

pub trait OpTyExt<'mir, 'tcx, M: Machine<'mir, 'tcx>>: Sized {
  fn field_by_name(
    &self,
    name: &str,
    ecx: &InterpCx<'mir, 'tcx, M>,
  ) -> InterpResult<'tcx, (&FieldDef, Self)>;
}

impl<'mir, 'tcx, M, Prov: Provenance> OpTyExt<'mir, 'tcx, M>
  for OpTy<'tcx, Prov>
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

struct AddressLocator<'a, 'mir, 'tcx> {
  ecx: &'a InterpCx<'mir, 'tcx, miri::MiriMachine<'mir, 'tcx>>,
  target: u64,
  segments: Vec<PlaceElem<'tcx>>,
}

impl<'tcx> AddressLocator<'_, '_, 'tcx> {
  fn locate(&mut self, layout: TyAndLayout<'tcx>, mut offset: u64) {
    let ty = layout.ty;
    match ty.kind() {
      TyKind::Adt(adt_def, _) => {
        let def_id = adt_def.did();
        let name = self.ecx.tcx.item_name(def_id).to_ident_string();
        match adt_def.adt_kind() {
          AdtKind::Struct => match name.as_str() {
            "String" | "Vec" => {}
            _ => {
              for (i, _field) in adt_def.all_fields().enumerate() {
                let field = layout.field(self.ecx, i);
                if offset + field.size.bytes() > self.target {
                  self
                    .segments
                    .push(PlaceElem::Field(Field::from_usize(i), field.ty));
                  self.locate(field, offset);
                  break;
                }

                offset += field.size.bytes();
              }
            }
          },
          AdtKind::Enum => todo!(),
          _ => {}
        }
      }

      TyKind::Array(_, _) => {
        // dbg!(("array", offset, target));
        let FieldsShape::Array { stride, .. } = layout.layout.fields() else { unreachable!() };
        let stride = stride.bytes();
        let array_offset = (self.target - offset) / stride * stride;
        let elem = layout.field(self.ecx, 0);
        let index = (array_offset / stride) as usize;
        dbg!((index, array_offset));
        self
          .segments
          .push(PlaceElem::Index(Local::from_usize(index)));
        self.locate(elem, offset + array_offset);
      }

      TyKind::Tuple(tys) => {
        // dbg!(("tuple", offset, target));
        for i in 0 .. tys.len() {
          let field = layout.field(self.ecx, i);
          if offset + field.size.bytes() > self.target {
            self
              .segments
              .push(PlaceElem::Field(Field::from_usize(i), field.ty));
            self.locate(field, offset);
            break;
          }

          offset += field.size.bytes();
        }
      }

      _ if ty.is_primitive() || ty.is_any_ptr() => {
        debug_assert!(
          offset == self.target,
          "offset {offset} != target {}",
          self.target
        )
      }

      ty => unimplemented!("{ty:#?}"),
    }
  }
}

pub fn locate_address_in_type<'mir, 'tcx>(
  ecx: &InterpCx<'mir, 'tcx, miri::MiriMachine<'mir, 'tcx>>,
  alloc_layout: TyAndLayout<'tcx>,
  alloc_size: Size,
  target: Size,
) -> Vec<PlaceElem<'tcx>> {
  let mut locator = AddressLocator {
    ecx,
    target: target.bytes(),
    segments: Vec::new(),
  };

  if alloc_layout.size.bytes() < alloc_size.bytes() {
    let array_elem_size = alloc_size.bytes();
    let array_elem_offset =
      target.bytes() * (array_elem_size - 1) / array_elem_size;
    let index = array_elem_offset / array_elem_size;
    locator
      .segments
      .push(PlaceElem::Index(Local::from_usize(index as usize)));
  }

  locator.locate(alloc_layout, 0);
  locator.segments
}
