use miri::{
  InterpCx, InterpResult, MPlaceTy, Machine, MemPlaceMeta, OpTy, Projectable,
  interp_ok,
};
use rustc_abi::{FieldIdx, FieldsShape, Size};
use rustc_middle::{
  mir::{Local, PlaceElem},
  ty::{FieldDef, Ty, TyKind, layout::TyAndLayout},
};

pub trait OpTyExt<'tcx, M: Machine<'tcx>>: Sized {
  fn field_by_name(
    &self,
    name: &str,
    ecx: &InterpCx<'tcx, M>,
  ) -> InterpResult<'tcx, (&FieldDef, Self)>;
}

impl<'tcx, M> OpTyExt<'tcx, M> for OpTy<'tcx>
where
  M: Machine<'tcx>,
  Self: Projectable<'tcx, M::Provenance>,
{
  fn field_by_name(
    &self,
    name: &str,
    ecx: &InterpCx<'tcx, M>,
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
    let field_op = ecx.project_field(self, FieldIdx::from_usize(i))?;
    interp_ok((field, field_op))
  }
}

struct AddressLocator<'a, 'tcx> {
  ecx: &'a InterpCx<'tcx, miri::MiriMachine<'tcx>>,
  target: u64,
  // Type of the pointed-to place, if it is sized. A value and its first field
  // share an address, so this is what tells `&s` apart from `&s.0`.
  pointee_ty: Option<Ty<'tcx>>,
  segments: Vec<PlaceElem<'tcx>>,
}

impl<'tcx> AddressLocator<'_, 'tcx> {
  /// Whether `locate` walks into the fields of `ty`.
  fn walks_fields(&self, ty: Ty<'tcx>) -> bool {
    match ty.kind() {
      TyKind::Adt(adt_def, _) => {
        adt_def.is_struct()
          && !matches!(
            self.ecx.tcx.item_name(adt_def.did()).as_str(),
            "String" | "Vec"
          )
      }
      TyKind::Tuple(_) => true,
      _ => false,
    }
  }

  fn locate(&mut self, layout: TyAndLayout<'tcx>, offset: u64) {
    if offset == self.target
      && (self.pointee_ty.is_none_or(|ty| ty == layout.ty)
        || !self.walks_fields(layout.ty))
    {
      return;
    }

    let ty = layout.ty;
    match ty.kind() {
      _ if self.walks_fields(ty) => self.locate_field(layout, offset),

      TyKind::Adt(adt_def, _) if adt_def.is_enum() => todo!(),
      TyKind::Adt(..) => {}

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

      _ if ty.is_primitive() || ty.is_any_ptr() => {
        panic!("offset {offset} != target {}", self.target)
      }

      ty => unimplemented!("{ty:#?}"),
    }
  }

  /// Finds the field of a struct or tuple that contains the target, using the
  /// field offsets from the layout (rustc may reorder fields and add padding).
  fn locate_field(&mut self, layout: TyAndLayout<'tcx>, offset: u64) {
    for i in 0 .. layout.fields.count() {
      let field = layout.field(self.ecx, i);
      let start = offset + layout.fields.offset(i).bytes();
      if (start .. start + field.size.bytes()).contains(&self.target) {
        self
          .segments
          .push(PlaceElem::Field(FieldIdx::from_usize(i), field.ty));
        self.locate(field, start);
        return;
      }
    }
  }
}

pub fn locate_address_in_type<'tcx>(
  ecx: &InterpCx<'tcx, miri::MiriMachine<'tcx>>,
  alloc_layout: TyAndLayout<'tcx>,
  alloc_size: Size,
  mplace: MPlaceTy<'tcx>,
  target: Size,
) -> Vec<PlaceElem<'tcx>> {
  // dbg!((alloc_layout, alloc_size, mplace, target));
  let mut locator = AddressLocator {
    ecx,
    target: target.bytes(),
    pointee_ty: matches!(mplace.meta(), MemPlaceMeta::None)
      .then_some(mplace.layout.ty),
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

    let segment = match mplace.meta() {
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
