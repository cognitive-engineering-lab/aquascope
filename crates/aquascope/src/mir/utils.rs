//! A smattering of utilities not yet (or that won't ever be) upstreamed to Flowistry.

use flowistry::mir::utils::PlaceExt as FlowistryPlaceExt;
use rustc_data_structures::captures::Captures;
use rustc_hir::def_id::DefId;
use rustc_middle::{
  mir::{Body, Place},
  ty::{self, subst::GenericArgKind, Region, RegionVid, Ty, TyCtxt},
};
use smallvec::SmallVec;

//------------------------------------------------

/// Extension trait for [`Place`]
pub trait PlaceExt {
  fn is_source_visible(&self, tcx: TyCtxt, body: &Body) -> bool;
}

impl PlaceExt for Place<'_> {
  fn is_source_visible(&self, tcx: TyCtxt, body: &Body) -> bool {
    let local = self.local;
    let local_info = &body.local_decls[local];
    let is_loc = local_info.is_user_variable();
    let from_desugaring = local_info.from_compiler_desugaring();
    let source_info = local_info.source_info;

    // The assumption is that decls whose source_scope should be collapsed
    // (i.e. with that of the outermost expansion site) are coming from a
    // HIR -> MIR expansion OR are being expanded from some macro not
    // actually visible in the source scope.
    let should_collapse = tcx.should_collapse_debuginfo(source_info.span);
    is_loc && !should_collapse && !from_desugaring
  }
}

//------------------------------------------------

/// Extension trait for [`Body`]
pub trait BodyExt<'tcx> {
  type PlacesIter<'a>: Iterator<Item = Place<'tcx>>
  where
    Self: 'a;

  fn all_places(
    &self,
    tcx: TyCtxt<'tcx>,
    def_id: DefId,
  ) -> Self::PlacesIter<'_>;

  type ArgRegionsIter<'a>: Iterator<Item = Region<'tcx>>
  where
    Self: 'a;

  fn regions_in_args(&self) -> Self::ArgRegionsIter<'_>;

  type ReturnRegionsIter: Iterator<Item = Region<'tcx>>;

  fn regions_in_return(&self) -> Self::ReturnRegionsIter;
}

impl<'tcx> BodyExt<'tcx> for Body<'tcx> {
  type ArgRegionsIter<'a> = impl Iterator<Item = Region<'tcx>> + Captures<'tcx> + 'a
    where Self: 'a;

  type ReturnRegionsIter = impl Iterator<Item = Region<'tcx>>;

  type PlacesIter<'a> = impl Iterator<Item = Place<'tcx>> + Captures<'tcx> + 'a
    where Self: 'a;

  fn regions_in_args(&self) -> Self::ArgRegionsIter<'_> {
    self
      .args_iter()
      .flat_map(|arg_local| self.local_decls[arg_local].ty.inner_regions())
  }

  fn regions_in_return(&self) -> Self::ReturnRegionsIter {
    self
      .return_ty()
      .inner_regions()
      .collect::<SmallVec<[Region<'tcx>; 8]>>()
      .into_iter()
  }

  fn all_places(
    &self,
    tcx: TyCtxt<'tcx>,
    def_id: DefId,
  ) -> Self::PlacesIter<'_> {
    self.local_decls.indices().flat_map(move |local| {
      Place::from_local(local, tcx).interior_paths(tcx, self, def_id)
    })
  }
}

//------------------------------------------------

/// Extension trait for [`ty::Ty`]
pub trait TyExt<'tcx> {
  type AllRegionsIter<'a>: Iterator<Item = Region<'tcx>>
  where
    Self: 'a;

  fn inner_regions(&self) -> Self::AllRegionsIter<'_>;
}

impl<'tcx> TyExt<'tcx> for Ty<'tcx> {
  type AllRegionsIter<'a> = impl Iterator<Item = Region<'tcx>> + Captures<'tcx> + 'a
    where Self: 'a;

  fn inner_regions(&self) -> Self::AllRegionsIter<'_> {
    self.walk().filter_map(|part| match part.unpack() {
      GenericArgKind::Lifetime(region) => Some(region),
      _ => None,
    })
  }
}

//------------------------------------------------

pub trait ToRegionVid {
  fn to_region_vid(&self) -> RegionVid;
}

impl<'tcx> ToRegionVid for Region<'tcx> {
  // XXX: when our pinned toolchain is upgraded we can
  // use `Region::as_var` instead to make this simpler.
  fn to_region_vid(&self) -> RegionVid {
    if let ty::ReVar(vid) = self.kind() {
      vid
    } else {
      unreachable!("region is not an ReVar{:?}", self)
    }
  }
}
