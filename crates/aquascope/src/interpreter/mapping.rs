use std::{
  cell::RefCell,
  collections::{HashMap, HashSet},
};

use either::Either;
use flowistry::mir::utils::BodyExt;
use itertools::Itertools;
use miri::InterpCx;
use rustc_hir::{intravisit::Visitor, Body, Expr, ExprKind, HirId, Stmt};
use rustc_middle::{mir::Location, ty::InstanceDef};
use rustc_span::{BytePos, Span};

use super::eval::{MFrame, MStack, MStep, MirLoc};
use crate::analysis::ir_mapper::{GatherDepth, GatherMode, IRMapper};

#[derive(Default)]
struct FindSteppableNodes {
  nodes: HashSet<HirId>,
}

impl Visitor<'_> for FindSteppableNodes {
  fn visit_body(&mut self, b: &Body) {
    let ExprKind::Block(block, _) = b.value.kind else { return };
    self.visit_block(block);
  }

  fn visit_stmt(&mut self, s: &Stmt) {
    self.nodes.insert(s.hir_id);
  }

  fn visit_expr(&mut self, e: &Expr) {
    self.nodes.insert(e.hir_id);
  }
}

pub(crate) type HirLoc = (HirId, Either<HirId, Span>);

struct MapperEntry {
  owner_id: HirId,
  body_mapping: HashMap<Location, HirId>,
}

pub struct Mapper<'a, 'mir, 'tcx> {
  ecx: &'a InterpCx<'mir, 'tcx, miri::MiriMachine<'mir, 'tcx>>,
  mapping: RefCell<HashMap<InstanceDef<'tcx>, MapperEntry>>,
}

impl<'a, 'mir, 'tcx> Mapper<'a, 'mir, 'tcx> {
  pub fn new(
    ecx: &'a InterpCx<'mir, 'tcx, miri::MiriMachine<'mir, 'tcx>>,
  ) -> Self {
    Mapper {
      ecx,
      mapping: RefCell::default(),
    }
  }

  fn build_body_mapping(&self, inst: InstanceDef<'tcx>) -> MapperEntry {
    let mut finder = FindSteppableNodes::default();
    let tcx = *self.ecx.tcx;
    let hir = tcx.hir();
    let body_id = hir.body_owned_by(inst.def_id().expect_local());
    finder.visit_body(hir.body(body_id));

    let body = self.ecx.load_mir(inst, None).unwrap();
    eprintln!("{}", body.to_string(tcx).unwrap());
    let mapper = IRMapper::new(tcx, body, GatherMode::All);

    let body_mapping = finder
      .nodes
      .into_iter()
      .filter_map(|hir_id| {
        let locations =
          mapper.get_mir_locations(hir_id, GatherDepth::Nested)?;
        Some(
          locations
            .values()
            .map(|loc| (loc, hir_id))
            .collect::<Vec<_>>(),
        )
      })
      .flatten()
      .collect();

    MapperEntry {
      owner_id: hir.body_owner(body_id),
      body_mapping,
    }
  }

  fn is_cleanup(
    &self,
    owner_id: HirId,
    inst: InstanceDef<'tcx>,
    location: Location,
  ) -> Option<Span> {
    let body_span = self.ecx.tcx.hir().span_with_body(owner_id);
    let end_brace = body_span.with_lo(body_span.hi() - BytePos(1));
    let body = self.ecx.load_mir(inst, None).unwrap();
    let loc_span = body.source_info(location).span;
    (loc_span == end_brace).then_some(end_brace)
  }

  pub fn abstract_loc(
    &self,
    (inst, loc_or_span): MirLoc<'tcx>,
  ) -> Option<HirLoc> {
    let mut mapping = self.mapping.borrow_mut();
    let MapperEntry {
      owner_id,
      body_mapping,
    } = mapping
      .entry(inst)
      .or_insert_with(|| self.build_body_mapping(inst));
    let hir_body_loc = match loc_or_span {
      Either::Left(location) => {
        match self.is_cleanup(*owner_id, inst, location) {
          Some(span) => Either::Right(span),
          None => Either::Left(*body_mapping.get(&location)?),
        }
      }
      Either::Right(span) => Either::Right(span),
    };

    Some((*owner_id, hir_body_loc))
  }
}

pub fn group_steps<Loc1, Loc2: PartialEq + Clone>(
  steps: Vec<MStep<Loc1>>,
  abstract_loc: impl Fn(Loc1) -> Option<Loc2>,
) -> Vec<MStep<Loc2>> {
  steps
    .into_iter()
    .filter_map(|step| {
      let frames = step
        .stack
        .frames
        .into_iter()
        .map(|frame| {
          let current_loc = abstract_loc(frame.location)?;
          Some(MFrame {
            location: current_loc,
            name: frame.name,
            body_span: frame.body_span,
            locals: frame.locals,
          })
        })
        .collect::<Option<Vec<_>>>()?;
      Some(MStep {
        stack: MStack { frames },
        heap: step.heap,
      })
    })
    .group_by(|step| step.stack.frames.last().unwrap().location.clone())
    .into_iter()
    .map(|(_, group)| group.last().unwrap())
    .collect()
}
