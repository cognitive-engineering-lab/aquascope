use miri::{
  interpret::Provenance, InterpCx, InterpResult, MPlaceTy, Machine,
  MemPlaceMeta, OpTy,
};
use rustc_abi::FieldsShape;
use rustc_middle::{
  mir::{Local, PlaceElem},
  ty::{layout::TyAndLayout, AdtKind, FieldDef, TyKind},
};
use rustc_target::abi::{FieldIdx, Size};

pub trait OpTyExt<'mir, 'tcx: 'mir, M: Machine<'mir, 'tcx>>: Sized {
  fn field_by_name(
    &self,
    name: &str,
    ecx: &InterpCx<'mir, 'tcx, M>,
  ) -> InterpResult<'tcx, (&FieldDef, Self)>;
}

impl<'mir, 'tcx, M, Prov: Provenance + 'static> OpTyExt<'mir, 'tcx, M>
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
    Ok((field, ecx.project_field(self, i)?))
  }
}

struct AddressLocator<'a, 'mir, 'tcx> {
  ecx: &'a InterpCx<'mir, 'tcx, miri::MiriMachine<'mir, 'tcx>>,
  target: u64,
  segments: Vec<PlaceElem<'tcx>>,
}

impl<'tcx> AddressLocator<'_, '_, 'tcx> {
  fn locate(&mut self, layout: TyAndLayout<'tcx>, mut offset: u64) {
    if offset == self.target {
      return;
    }

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
                    .push(PlaceElem::Field(FieldIdx::from_usize(i), field.ty));
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
        let FieldsShape::Array { stride, .. } = layout.layout.fields() else {
          unreachable!()
        };
        let stride = stride.bytes();
        let array_offset = (self.target - offset) / stride * stride;
        let elem = layout.field(self.ecx, 0);
        let index = (array_offset / stride) as usize;
        // dbg!((index, array_offset));
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
              .push(PlaceElem::Field(FieldIdx::from_usize(i), field.ty));
            self.locate(field, offset);
            break;
          }

          offset += field.size.bytes();
        }
      }

      _ if ty.is_primitive() || ty.is_any_ptr() => {
        panic!("offset {offset} != target {}", self.target)
      }

      ty => unimplemented!("{ty:#?}"),
    }
  }
}

pub fn locate_address_in_type<'mir, 'tcx>(
  ecx: &InterpCx<'mir, 'tcx, miri::MiriMachine<'mir, 'tcx>>,
  alloc_layout: TyAndLayout<'tcx>,
  alloc_size: Size,
  mplace: MPlaceTy<'tcx, miri::Provenance>,
  target: Size,
) -> Vec<PlaceElem<'tcx>> {
  // dbg!((alloc_layout, alloc_size, mplace, target));
  let mut locator = AddressLocator {
    ecx,
    target: target.bytes(),
    segments: Vec::new(),
  };

  let mut offset = 0;
  if alloc_layout.size.bytes() < alloc_size.bytes() {
    let array_elem_size = alloc_layout.size.bytes();
    assert!(
      array_elem_size > 0,
      "Array has zero-sized elements: {alloc_layout:#?}"
    );

    offset = target.bytes() / array_elem_size * array_elem_size;
    let index = offset / array_elem_size;
    // dbg!((array_elem_size, offset, index));

    let segment = match mplace.meta {
      MemPlaceMeta::Meta(meta) => {
        let end_offset = meta.to_u64().unwrap();
        let to = index + end_offset / array_elem_size - 1;
        PlaceElem::Subslice {
          from: index,
          to,
          from_end: false,
        }
      }
      MemPlaceMeta::None => PlaceElem::Index(Local::from_usize(index as usize)),
    };

    locator.segments.push(segment);
  }

  locator.locate(alloc_layout, offset);
  locator.segments
}
