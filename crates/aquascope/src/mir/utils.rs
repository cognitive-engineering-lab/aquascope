//! A potpourri of utilities for working with the MIR, primarily exposed as extension traits.

use std::{
  cmp,
  collections::hash_map::Entry,
  hash::Hash,
  io::Write,
  iter,
  ops::ControlFlow,
  path::Path,
  process::{Command, Stdio},
};

use anyhow::{bail, Result};
use either::Either;
use log::{trace, warn};
use rustc_data_structures::fx::{FxHashMap as HashMap, FxHashSet as HashSet};
use rustc_graphviz as dot;
use rustc_hir::def_id::DefId;
use rustc_infer::infer::TyCtxtInferExt;
use rustc_middle::{
  mir::{
    pretty::write_mir_fn,
    visit::{PlaceContext, Visitor},
    MirPass, *,
  },
  traits::ObligationCause,
  ty::{
    self, AdtKind, RegionKind, RegionVid, Ty, TyCtxt, TyKind, TypeAndMut,
    TypeVisitor,
  },
};
use rustc_mir_dataflow::{fmt::DebugWithContext, graphviz, Analysis, Results};
use rustc_span::{
  source_map::SourceMap, BytePos, Pos, Span, SpanData, Symbol, SyntaxContext,
};
use rustc_target::abi::VariantIdx;
use rustc_trait_selection::infer::InferCtxtExt;
use smallvec::SmallVec;

use crate::mir::aliases::UNKNOWN_REGION;

pub trait OperandExt<'tcx> {
  fn to_place(&self) -> Option<Place<'tcx>>;
}

impl<'tcx> OperandExt<'tcx> for Operand<'tcx> {
  fn to_place(&self) -> Option<Place<'tcx>> {
    match self {
      Operand::Copy(place) | Operand::Move(place) => Some(*place),
      Operand::Constant(_) => None,
    }
  }
}

pub fn arg_mut_ptrs<'tcx>(
  args: &[(usize, Place<'tcx>)],
  tcx: TyCtxt<'tcx>,
  body: &Body<'tcx>,
  def_id: DefId,
) -> Vec<(usize, Place<'tcx>)> {
  // let ignore_mut = is_extension_active(|mode| {
  //   mode.mutability_mode == MutabilityMode::IgnoreMut
  // });
  args
    .iter()
    .flat_map(|(i, place)| {
      place
        .interior_pointers(tcx, body, def_id)
        .into_iter()
        .flat_map(|(_, places)| {
          places.into_iter().filter_map(
            |(place, mutability)| match mutability {
              Mutability::Mut => Some(place),
              Mutability::Not => None, // ignore_mut.then(|| place),
            },
          )
        })
        .map(move |place| (*i, tcx.mk_place_deref(place)))
    })
    .collect::<Vec<_>>()
}

pub fn arg_places<'tcx>(args: &[Operand<'tcx>]) -> Vec<(usize, Place<'tcx>)> {
  args
    .iter()
    .enumerate()
    .filter_map(|(i, arg)| arg.to_place().map(move |place| (i, place)))
    .collect::<Vec<_>>()
}

pub fn hashmap_merge<K: Eq + Hash, V>(
  mut h1: HashMap<K, V>,
  h2: HashMap<K, V>,
  conflict: impl Fn(&mut V, V),
) -> HashMap<K, V> {
  for (k, v) in h2.into_iter() {
    match h1.entry(k) {
      Entry::Vacant(entry) => {
        entry.insert(v);
      }
      Entry::Occupied(mut entry) => {
        let entry = entry.get_mut();
        conflict(entry, v);
      }
    }
  }
  h1
}

#[derive(PartialEq, Eq, Debug)]
pub enum PlaceRelation {
  Super,
  Sub,
  Disjoint,
}

impl PlaceRelation {
  pub fn overlaps(&self) -> bool {
    *self != PlaceRelation::Disjoint
  }

  pub fn of<'tcx>(part_place: Place<'tcx>, whole_place: Place<'tcx>) -> Self {
    let locals_match = part_place.local == whole_place.local;
    if !locals_match {
      return PlaceRelation::Disjoint;
    }

    let projections_match = part_place
      .projection
      .iter()
      .zip(whole_place.projection.iter())
      .all(|(elem1, elem2)| {
        use ProjectionElem::*;
        match (elem1, elem2) {
          (Deref, Deref) => true,
          (Field(f1, _), Field(f2, _)) => f1 == f2,
          (Index(_), Index(_)) => true,
          (ConstantIndex { .. }, ConstantIndex { .. }) => true,
          (Subslice { .. }, Subslice { .. }) => true,
          (Downcast(_, v1), Downcast(_, v2)) => v1 == v2,
          _ => false,
        }
      });

    let is_sub_part =
      part_place.projection.len() >= whole_place.projection.len();
    let remaining_projection = if is_sub_part {
      &part_place.projection[whole_place.projection.len() ..]
    } else {
      &whole_place.projection[part_place.projection.len() ..]
    };

    if remaining_projection
      .iter()
      .any(|elem| matches!(elem, ProjectionElem::Deref))
    {
      return PlaceRelation::Disjoint;
    }

    if projections_match {
      if is_sub_part {
        PlaceRelation::Sub
      } else {
        PlaceRelation::Super
      }
    } else {
      PlaceRelation::Disjoint
    }
  }
}

pub struct PlaceCollector<'tcx> {
  pub tcx: TyCtxt<'tcx>,
  pub places: Vec<(Place<'tcx>, Option<PlaceElem<'tcx>>)>,
}

impl<'tcx> Visitor<'tcx> for PlaceCollector<'tcx> {
  fn visit_place(
    &mut self,
    place: &Place<'tcx>,
    _context: PlaceContext,
    _location: Location,
  ) {
    self.places.push((*place, None));
  }

  fn visit_rvalue(&mut self, rvalue: &Rvalue<'tcx>, location: Location) {
    match rvalue {
      Rvalue::Aggregate(
        box AggregateKind::Adt(def_id, idx, substs, _, _),
        ops,
      ) => {
        // In the case of _1 = aggregate { field1: op1, field2: op2, ... }
        // we want to remember which places correspond to which fields so the infoflow
        // analysis can be field-sensitive for constructors.
        let adt_def = self.tcx.adt_def(*def_id);
        let variant = adt_def.variant(*idx);
        let places = variant
          .fields
          .iter()
          .enumerate()
          .zip(ops.iter())
          .filter_map(|((i, field), op)| {
            let place = op.to_place()?;
            let field = ProjectionElem::Field(
              Field::from_usize(i),
              field.ty(self.tcx, substs),
            );
            Some((place, Some(field)))
          });
        self.places.extend(places);
      }
      _ => self.super_rvalue(rvalue, location),
    }
  }
}

pub fn run_dot(path: &Path, buf: Vec<u8>) -> Result<()> {
  let mut p = Command::new("dot")
    .args(&["-Tpdf", "-o", &path.display().to_string()])
    .stdin(Stdio::piped())
    .spawn()?;

  p.stdin.as_mut().unwrap().write_all(&buf)?;
  let status = p.wait()?;

  if !status.success() {
    bail!("dot for {} failed", path.display())
  };

  Ok(())
}

pub fn dump_results<'tcx, A>(
  body: &Body<'tcx>,
  results: &Results<'tcx, A>,
  _def_id: DefId,
  _tcx: TyCtxt<'tcx>,
) -> Result<()>
where
  A: Analysis<'tcx>,
  A::Domain: DebugWithContext<A>,
{
  let graphviz =
    graphviz::Formatter::new(body, results, graphviz::OutputStyle::AfterOnly);
  let mut buf = Vec::new();
  dot::render(&graphviz, &mut buf)?;

  let output_dir = Path::new("target");
  // let fname = tcx.def_path_debug_str(def_id);
  let fname = "results";
  let output_path = output_dir.join(format!("{fname}.png"));

  run_dot(&output_path, buf)
}

pub fn location_to_string(location: Location, body: &Body<'_>) -> String {
  if location.block.as_usize() == body.basic_blocks().len() {
    format!("_{}", location.statement_index)
  } else {
    match body.stmt_at(location) {
      Either::Left(stmt) => format!("{:?}", stmt.kind),
      Either::Right(terminator) => format!("{:?}", terminator.kind),
    }
  }
}

pub struct SimplifyMir;
impl<'tcx> MirPass<'tcx> for SimplifyMir {
  fn run_pass(&self, _tcx: TyCtxt<'tcx>, body: &mut Body<'tcx>) {
    let return_blocks = body
      .all_returns()
      .filter_map(|loc| {
        let bb = &body.basic_blocks()[loc.block];
        (bb.statements.len() == 0).then(|| loc.block)
      })
      .collect::<HashSet<_>>();

    for block in body.basic_blocks_mut() {
      block.statements.retain(|stmt| {
        !matches!(
          stmt.kind,
          StatementKind::StorageLive(..) | StatementKind::StorageDead(..)
        )
      });

      let terminator = block.terminator_mut();
      terminator.kind = match terminator.kind {
        TerminatorKind::FalseEdge { real_target, .. } => TerminatorKind::Goto {
          target: real_target,
        },
        TerminatorKind::FalseUnwind { real_target, .. } => {
          TerminatorKind::Goto {
            target: real_target,
          }
        }
        // Ensures that control dependencies can determine the independence of differnet
        // return paths
        TerminatorKind::Goto { target } if return_blocks.contains(&target) => {
          TerminatorKind::Return
        }
        _ => continue,
      }
    }
  }
}

pub trait PlaceExt<'tcx> {
  fn make(
    local: Local,
    projection: &[PlaceElem<'tcx>],
    tcx: TyCtxt<'tcx>,
  ) -> Self;
  fn from_ref(place: PlaceRef<'tcx>, tcx: TyCtxt<'tcx>) -> Self;
  fn from_local(local: Local, tcx: TyCtxt<'tcx>) -> Self;
  fn is_arg(&self, body: &Body<'tcx>) -> bool;
  fn is_direct(&self, body: &Body<'tcx>) -> bool;
  fn refs_in_projection(
    &self,
  ) -> SmallVec<[(PlaceRef<'tcx>, &[PlaceElem<'tcx>]); 2]>;
  fn place_and_refs_in_projection(
    &self,
    tcx: TyCtxt<'tcx>,
  ) -> SmallVec<[Place<'tcx>; 2]>;
  fn interior_pointers(
    &self,
    tcx: TyCtxt<'tcx>,
    body: &Body<'tcx>,
    def_id: DefId,
  ) -> HashMap<RegionVid, Vec<(Place<'tcx>, Mutability)>>;
  fn interior_places(
    &self,
    tcx: TyCtxt<'tcx>,
    body: &Body<'tcx>,
    def_id: DefId,
  ) -> Vec<Place<'tcx>>;
  fn to_string(&self, tcx: TyCtxt<'tcx>, body: &Body<'tcx>) -> Option<String>;
  fn normalize(&self, tcx: TyCtxt<'tcx>, def_id: DefId) -> Place<'tcx>;
}

impl<'tcx> PlaceExt<'tcx> for Place<'tcx> {
  fn make(
    local: Local,
    projection: &[PlaceElem<'tcx>],
    tcx: TyCtxt<'tcx>,
  ) -> Self {
    Place {
      local,
      projection: tcx.intern_place_elems(projection),
    }
  }

  fn from_ref(place: PlaceRef<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
    Self::make(place.local, place.projection, tcx)
  }

  fn from_local(local: Local, tcx: TyCtxt<'tcx>) -> Self {
    Place::make(local, &[], tcx)
  }

  fn is_arg(&self, body: &Body<'tcx>) -> bool {
    let i = self.local.as_usize();
    i > 0 && i - 1 < body.arg_count
  }

  fn is_direct(&self, body: &Body<'tcx>) -> bool {
    !self.is_indirect() || self.is_arg(body)
  }

  fn refs_in_projection(
    &self,
  ) -> SmallVec<[(PlaceRef<'tcx>, &[PlaceElem<'tcx>]); 2]> {
    self
      .projection
      .iter()
      .enumerate()
      .filter_map(|(i, elem)| match elem {
        ProjectionElem::Deref => {
          let ptr = PlaceRef {
            local: self.local,
            projection: &self.projection[.. i],
          };
          let after = &self.projection[i + 1 ..];
          Some((ptr, after))
        }
        _ => None,
      })
      .collect()
  }

  fn place_and_refs_in_projection(
    &self,
    tcx: TyCtxt<'tcx>,
  ) -> SmallVec<[Place<'tcx>; 2]> {
    iter::once(*self)
      .chain(
        self
          .refs_in_projection()
          .into_iter()
          .map(|(ptr, _)| Place::from_ref(ptr, tcx)),
      )
      .collect()
  }

  fn interior_pointers(
    &self,
    tcx: TyCtxt<'tcx>,
    body: &Body<'tcx>,
    def_id: DefId,
  ) -> HashMap<RegionVid, Vec<(Place<'tcx>, Mutability)>> {
    let ty = self.ty(body.local_decls(), tcx).ty;
    let mut region_collector = CollectRegions {
      tcx,
      def_id,
      local: self.local,
      place_stack: self.projection.to_vec(),
      ty_stack: Vec::new(),
      regions: HashMap::default(),
      places: None,
      types: None,
      stop_at: if
      /*shallow*/
      false {
        StoppingCondition::AfterRefs
      } else {
        StoppingCondition::None
      },
    };
    region_collector.visit_ty(ty);
    region_collector.regions
  }

  fn interior_places(
    &self,
    tcx: TyCtxt<'tcx>,
    body: &Body<'tcx>,
    def_id: DefId,
  ) -> Vec<Place<'tcx>> {
    let ty = self.ty(body.local_decls(), tcx).ty;
    let mut region_collector = CollectRegions {
      tcx,
      def_id,
      local: self.local,
      place_stack: self.projection.to_vec(),
      ty_stack: Vec::new(),
      regions: HashMap::default(),
      places: Some(HashSet::default()),
      types: None,
      stop_at: StoppingCondition::BeforeRefs,
    };
    region_collector.visit_ty(ty);
    region_collector.places.unwrap().into_iter().collect()
  }

  fn to_string(&self, tcx: TyCtxt<'tcx>, body: &Body<'tcx>) -> Option<String> {
    let local_name = if self.local == RETURN_PLACE {
      "RETURN".into()
    } else {
      body
        .var_debug_info
        .iter()
        .filter_map(|info| match info.value {
          VarDebugInfoContents::Place(place) if place.local == self.local => {
            info
              .source_info
              .span
              .as_local(body.span)
              .map(|_| info.name.to_string())
          }
          _ => None,
        })
        .next()?
    };

    let projection_to_string = |s: String,
                                (place, elem): (
      PlaceRef<'tcx>,
      PlaceElem<'tcx>,
    )| match elem {
      ProjectionElem::Deref => format!("*{s}"),
      ProjectionElem::Field(f, _) => {
        let ty = place.ty(&body.local_decls, tcx).ty;
        let default = || format!("{s}.{}", f.as_usize());
        if let Some(def) = ty.ty_adt_def() {
          match def.adt_kind() {
            AdtKind::Struct => {
              let name = def.non_enum_variant().fields[f.as_usize()].ident(tcx);
              format!("{s}.{name}")
            }
            _ => default(),
          }
        } else {
          default()
        }
      }
      ProjectionElem::Downcast(sym, _) => format!(
        "{s} as {}",
        sym.map(|s| s.to_string()).unwrap_or_else(|| "??".into())
      ),
      ProjectionElem::Index(_) => format!("{s}[]"),
      _ => unimplemented!(),
    };

    Some(
      self
        .iter_projections()
        .fold(local_name, projection_to_string),
    )
  }

  fn normalize(&self, tcx: TyCtxt<'tcx>, def_id: DefId) -> Place<'tcx> {
    // Consider a place _1: &'1 <T as SomeTrait>::Foo[2]
    //   we might encounter this type with a different region, e.g. &'2
    //   we might encounter this type with a more specific type for the associated type, e.g. &'1 [i32][0]
    // to account for this variation, we normalize associated types,
    //   erase regions, and normalize projections
    let param_env = tcx.param_env(def_id);
    let place = tcx.erase_regions(*self);
    let place = tcx.infer_ctxt().enter(|infcx| {
      infcx
        .partially_normalize_associated_types_in(
          ObligationCause::dummy(),
          param_env,
          place,
        )
        .value
    });

    let projection = place
      .projection
      .into_iter()
      .filter_map(|elem| match elem {
        // Map all indexes [i] to [0] since they should be considered equal
        ProjectionElem::Index(_) | ProjectionElem::ConstantIndex { .. } => {
          Some(ProjectionElem::Index(Local::from_usize(0)))
        }
        // Ignore subslices, they should be treated the same as the
        // full slice
        ProjectionElem::Subslice { .. } => None,
        _ => Some(elem),
      })
      .collect::<Vec<_>>();

    Place::make(place.local, &projection, tcx)
  }
}

#[derive(Copy, Clone)]
enum StoppingCondition {
  None,
  BeforeRefs,
  AfterRefs,
}

struct CollectRegions<'tcx> {
  tcx: TyCtxt<'tcx>,
  def_id: DefId,
  local: Local,
  place_stack: Vec<PlaceElem<'tcx>>,
  ty_stack: Vec<Ty<'tcx>>,
  places: Option<HashSet<Place<'tcx>>>,
  types: Option<HashSet<Ty<'tcx>>>,
  regions: HashMap<RegionVid, Vec<(Place<'tcx>, Mutability)>>,
  stop_at: StoppingCondition,
}

impl<'tcx> TypeVisitor<'tcx> for CollectRegions<'tcx> {
  fn visit_ty(&mut self, ty: Ty<'tcx>) -> ControlFlow<Self::BreakTy> {
    let tcx = self.tcx;
    if self.ty_stack.iter().any(|visited_ty| ty == *visited_ty) {
      return ControlFlow::Continue(());
    }

    trace!(
      "exploring {:?} with {ty:?}",
      Place::make(self.local, &self.place_stack, tcx)
    );

    self.ty_stack.push(ty);

    match ty.kind() {
      _ if ty.is_box() => {
        self.visit_region(tcx.mk_region(RegionKind::ReVar(UNKNOWN_REGION)));
        self.place_stack.push(ProjectionElem::Deref);
        self.visit_ty(ty.boxed_ty());
        self.place_stack.pop();
      }

      TyKind::Tuple(fields) => {
        for (i, field) in fields.iter().enumerate() {
          self
            .place_stack
            .push(ProjectionElem::Field(Field::from_usize(i), field));
          self.visit_ty(field);
          self.place_stack.pop();
        }
      }

      TyKind::Adt(adt_def, subst) => match adt_def.adt_kind() {
        ty::AdtKind::Struct => {
          for (i, field) in adt_def.all_fields().enumerate() {
            if !field.vis.is_accessible_from(self.def_id, tcx) {
              continue;
            }

            let ty = field.ty(tcx, subst);
            self
              .place_stack
              .push(ProjectionElem::Field(Field::from_usize(i), ty));
            self.visit_ty(ty);
            self.place_stack.pop();
          }
        }
        ty::AdtKind::Union => {
          // unsafe, so ignore
        }
        ty::AdtKind::Enum => {
          for (i, variant) in adt_def.variants().iter().enumerate() {
            let variant_index = VariantIdx::from_usize(i);
            let cast = PlaceElem::Downcast(
              Some(adt_def.variant(variant_index).ident(tcx).name),
              variant_index,
            );
            self.place_stack.push(cast);
            for (j, field) in variant.fields.iter().enumerate() {
              let ty = field.ty(tcx, subst);
              let field = ProjectionElem::Field(Field::from_usize(j), ty);
              self.place_stack.push(field);
              self.visit_ty(ty);
              self.place_stack.pop();
            }
            self.place_stack.pop();
          }
        }
      },

      TyKind::Array(elem_ty, _) | TyKind::Slice(elem_ty) => {
        self
          .place_stack
          .push(ProjectionElem::Index(Local::from_usize(0)));
        self.visit_ty(*elem_ty);
        self.place_stack.pop();
      }

      TyKind::Ref(region, elem_ty, _) => match self.stop_at {
        StoppingCondition::None => {
          self.visit_region(*region);
          self.place_stack.push(ProjectionElem::Deref);
          self.visit_ty(*elem_ty);
          self.place_stack.pop();
        }
        StoppingCondition::AfterRefs => {
          self.visit_region(*region);
        }
        StoppingCondition::BeforeRefs => {}
      },

      TyKind::Closure(_, substs) => {
        self.visit_ty(substs.as_closure().tupled_upvars_ty());
      }

      TyKind::RawPtr(TypeAndMut { ty, .. }) => {
        self.visit_region(tcx.mk_region(RegionKind::ReVar(UNKNOWN_REGION)));
        self.place_stack.push(ProjectionElem::Deref);
        self.visit_ty(*ty);
        self.place_stack.pop();
      }

      TyKind::Projection(_)
      | TyKind::FnDef(_, _)
      | TyKind::FnPtr(_)
      | TyKind::Opaque(_, _)
      | TyKind::Foreign(_)
      | TyKind::Dynamic(_, _)
      | TyKind::Param(_)
      | TyKind::Never => {}

      _ if ty.is_primitive_ty() => {}

      _ => {
        warn!("unimplemented {ty:?} ({:?})", ty.kind());
      }
    };

    // let inherent_impls = tcx.inherent_impls(self.def_id);
    // let traits = tcx.infer_ctxt().enter(|infcx| {
    //   let param_env = tcx.param_env(self.def_id);
    //   self
    //     .tcx
    //     .all_traits()
    //     .filter(|trait_def_id| {
    //       let result = infcx.type_implements_trait(*trait_def_id, ty, params, param_env);
    //       matches!(
    //         result,
    //         EvaluationResult::EvaluatedToOk
    //           | EvaluationResult::EvaluatedToOkModuloRegions
    //       )
    //     })
    //     .collect::<Vec<_>>()
    // });

    // let fns = inherent_impls.iter().chain(&traits).flat_map(|def_id| {
    //   let items = tcx.associated_items(def_id).in_definition_order();
    //   items.filter_map(|item| match item.kind {
    //     AssocKind::Fn => Some(tcx.fn_sig(item.def_id)),
    //     _ => None,
    //   })
    // });

    // // for f in fns {
    // //   f.
    // // }

    if let Some(places) = self.places.as_mut() {
      places.insert(Place::make(self.local, &self.place_stack, tcx));
    }

    if let Some(types) = self.types.as_mut() {
      types.insert(ty);
    }

    self.ty_stack.pop();
    ControlFlow::Continue(())
  }

  fn visit_region(
    &mut self,
    region: ty::Region<'tcx>,
  ) -> ControlFlow<Self::BreakTy> {
    trace!("visiting region {region:?}");
    let region = match region.kind() {
      RegionKind::ReVar(region) => region,
      RegionKind::ReStatic => RegionVid::from_usize(0),
      RegionKind::ReErased | RegionKind::ReLateBound(_, _) => {
        return ControlFlow::Continue(());
      }
      _ => unreachable!("{:?}: {:?}", self.ty_stack.first().unwrap(), region),
    };

    let mutability =
      if self.ty_stack.iter().any(|ty| {
        ty.is_ref() && ty.ref_mutability().unwrap() == Mutability::Not
      }) {
        Mutability::Not
      } else {
        Mutability::Mut
      };

    let place = Place::make(self.local, &self.place_stack, self.tcx);

    self
      .regions
      .entry(region)
      .or_default()
      .push((place, mutability));

    // for initialization setup of Aliases::build
    if let Some(places) = self.places.as_mut() {
      places.insert(self.tcx.mk_place_deref(place));
    }

    ControlFlow::Continue(())
  }
}

pub trait BodyExt<'tcx> {
  type AllReturnsIter<'a>: Iterator<Item = Location>
  where
    Self: 'a;
  fn all_returns(&self) -> Self::AllReturnsIter<'_>;

  type AllLocationsIter<'a>: Iterator<Item = Location>
  where
    Self: 'a;
  fn all_locations(&self) -> Self::AllLocationsIter<'_>;

  type LocationsIter: Iterator<Item = Location>;
  fn locations_in_block(&self, block: BasicBlock) -> Self::LocationsIter;

  fn debug_info_name_map(&self) -> HashMap<Local, Symbol>;

  fn to_string(&self, tcx: TyCtxt<'tcx>) -> Result<String>;
}

impl<'tcx> BodyExt<'tcx> for Body<'tcx> {
  type AllReturnsIter<'a> = impl Iterator<Item = Location>   where Self: 'a;
  fn all_returns(&self) -> Self::AllReturnsIter<'_> {
    self
      .basic_blocks()
      .iter_enumerated()
      .filter_map(|(block, data)| match data.terminator().kind {
        TerminatorKind::Return => Some(Location {
          block,
          statement_index: data.statements.len(),
        }),
        _ => None,
      })
  }

  type AllLocationsIter<'a> = impl Iterator<Item = Location>   where Self: 'a;
  fn all_locations(&self) -> Self::AllLocationsIter<'_> {
    self
      .basic_blocks()
      .iter_enumerated()
      .flat_map(|(block, data)| {
        (0 .. data.statements.len() + 1).map(move |statement_index| Location {
          block,
          statement_index,
        })
      })
  }

  type LocationsIter = impl Iterator<Item = Location>;
  fn locations_in_block(&self, block: BasicBlock) -> Self::LocationsIter {
    let num_stmts = self.basic_blocks()[block].statements.len();
    (0 ..= num_stmts).map(move |statement_index| Location {
      block,
      statement_index,
    })
  }

  fn debug_info_name_map(&self) -> HashMap<Local, Symbol> {
    self
      .var_debug_info
      .iter()
      .filter_map(|info| match info.value {
        VarDebugInfoContents::Place(place) => Some((place.local, info.name)),
        _ => None,
      })
      .collect()
  }

  fn to_string(&self, tcx: TyCtxt<'tcx>) -> Result<String> {
    let mut buffer = Vec::new();
    write_mir_fn(tcx, self, &mut |_, _| Ok(()), &mut buffer)?;
    Ok(String::from_utf8(buffer)?)
  }
}

pub trait SpanExt {
  fn subtract(&self, child_spans: Vec<Span>) -> Vec<Span>;
  fn as_local(&self, outer_span: Span) -> Option<Span>;
  fn overlaps_inclusive(&self, other: Span) -> bool;
  fn trim_end(&self, other: Span) -> Option<Span>;
  fn merge_overlaps(spans: Vec<Span>) -> Vec<Span>;
  fn to_string(&self, tcx: TyCtxt<'_>) -> String;
  fn size(&self) -> u32;
  fn trim_leading_whitespace(
    &self,
    source_map: &SourceMap,
  ) -> Option<Vec<Span>>;
}

impl SpanExt for Span {
  fn trim_end(&self, other: Span) -> Option<Span> {
    let span = self.data();
    let other = other.data();
    if span.lo < other.lo {
      Some(span.with_hi(cmp::min(span.hi, other.lo)))
    } else {
      None
    }
  }

  /// Get spans for regions in `self` not in `child_spans`.
  ///
  /// Example:
  ///  self:          ---------------
  ///  child_spans:    ---      --  -
  ///  output:        -   ------  --
  fn subtract(&self, mut child_spans: Vec<Span>) -> Vec<Span> {
    child_spans.retain(|s| s.overlaps_inclusive(*self));

    let mut outer_spans = vec![];
    if !child_spans.is_empty() {
      // Output will be sorted
      child_spans = Span::merge_overlaps(child_spans);

      if let Some(start) = self.trim_end(*child_spans.first().unwrap()) {
        outer_spans.push(start);
      }

      for children in child_spans.windows(2) {
        outer_spans.push(children[0].between(children[1]));
      }

      if let Some(end) = self.trim_start(*child_spans.last().unwrap()) {
        outer_spans.push(end);
      }
    } else {
      outer_spans.push(*self);
    };

    trace!("outer span for {self:?} with inner spans {child_spans:?} is {outer_spans:?}");

    outer_spans
  }

  fn as_local(&self, outer_span: Span) -> Option<Span> {
    // Before we call source_callsite, we check and see if the span is already local.
    // This is important b/c in print!("{}", y) if the user selects `y`, the source_callsite
    // of that span is the entire macro.
    if outer_span.contains(*self) {
      return Some(*self);
    } else {
      let sp = self.source_callsite();
      if outer_span.contains(sp) {
        return Some(sp);
      }
    }

    None
  }

  fn overlaps_inclusive(&self, other: Span) -> bool {
    let s1 = self.data();
    let s2 = other.data();
    s1.lo <= s2.hi && s2.lo <= s1.hi
  }

  fn merge_overlaps(mut spans: Vec<Span>) -> Vec<Span> {
    spans.sort_by_key(|s| (s.lo(), s.hi()));

    // See note in Span::subtract
    for span in spans.iter_mut() {
      *span = span.with_ctxt(SyntaxContext::root());
    }

    let mut output = Vec::new();
    for span in spans {
      match output
        .iter_mut()
        .find(|other| span.overlaps_inclusive(**other))
      {
        Some(other) => {
          *other = span.to(*other);
        }
        None => {
          output.push(span);
        }
      }
    }
    output
  }

  fn to_string(&self, tcx: TyCtxt<'_>) -> String {
    let source_map = tcx.sess.source_map();
    let lo = source_map.lookup_char_pos(self.lo());
    let hi = source_map.lookup_char_pos(self.hi());
    let snippet = source_map.span_to_snippet(*self).unwrap();
    format!(
      "{snippet} ({}:{}-{}:{})",
      lo.line,
      lo.col.to_usize() + 1,
      hi.line,
      hi.col.to_usize() + 1
    )
  }

  fn size(&self) -> u32 {
    self.hi().0 - self.lo().0
  }

  fn trim_leading_whitespace(
    &self,
    source_map: &SourceMap,
  ) -> Option<Vec<Span>> {
    let snippet = source_map.span_to_snippet(*self).ok()?;
    let mut spans = Vec::new();
    let mut start = self.lo();
    for line in snippet.split('\n') {
      let offset = line
        .chars()
        .take_while(|c| c.is_whitespace())
        .map(|c| c.len_utf8())
        .sum::<usize>();
      let end = (start + BytePos(line.len() as u32)).min(self.hi());
      spans.push(self.with_lo(start + BytePos(offset as u32)).with_hi(end));
      start = end + BytePos(1);
    }
    Some(spans)
  }
}

pub trait SpanDataExt {
  fn size(&self) -> u32;
}

impl SpanDataExt for SpanData {
  fn size(&self) -> u32 {
    self.hi.0 - self.lo.0
  }
}

pub trait MutabilityExt {
  fn more_permissive_than(self, other: Self) -> bool;
}

impl MutabilityExt for Mutability {
  fn more_permissive_than(self, other: Self) -> bool {
    !matches!((self, other), (Mutability::Not, Mutability::Mut))
  }
}

#[cfg(test)]
mod test {
  use rustc_span::BytePos;

  use super::*;

  #[test]
  fn test_span_subtract() {
    rustc_span::create_default_session_if_not_set_then(|_| {
      let mk = |lo, hi| Span::with_root_ctxt(BytePos(lo), BytePos(hi));
      let outer = mk(1, 10);
      let inner: Vec<Span> =
        vec![mk(0, 2), mk(3, 4), mk(3, 5), mk(7, 8), mk(9, 13)];
      let desired: Vec<Span> = vec![mk(2, 3), mk(5, 7), mk(8, 9)];
      assert_eq!(outer.subtract(inner), desired);
    });
  }
}
