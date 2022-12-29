use flowistry::{mir::utils::BodyExt, source_map::find_bodies};
use itertools::Itertools;
use rustc_hir::{
  def_id::{LocalDefId},
  intravisit::Visitor,
  Body, Expr, ExprKind, HirId, Stmt,
};
use rustc_middle::{
  mir::Location,
  ty::{TyCtxt, WithOptConstParam},
};

use std::collections::{HashMap, HashSet};

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

pub(crate) type HirLoc = (HirId, HirId);

pub struct Mapper {
  mapping: HashMap<LocalDefId, (HirId, HashMap<Location, HirId>)>,
}

impl Mapper {
  pub fn build(tcx: TyCtxt) -> Self {
    let hir = tcx.hir();
    let mapping = find_bodies(tcx)
      .into_iter()
      .map(|(_, body_id)| {
        let owner_id = hir.body_owner(body_id);
        let def_id = hir.body_owner_def_id(body_id);
        (def_id, (owner_id, Self::build_body_mapping(tcx, def_id)))
      })
      .collect::<HashMap<_, _>>();
    Mapper { mapping }
  }

  fn build_body_mapping(tcx: TyCtxt, def_id: LocalDefId) -> HashMap<Location, HirId> {
    let mut finder = FindSteppableNodes::default();
    let hir = tcx.hir();
    let body_id = hir.body_owned_by(def_id);
    finder.visit_body(hir.body(body_id));

    let (body, _) = tcx.mir_promoted(WithOptConstParam::unknown(def_id));
    let body = &body.borrow();
    eprintln!("{}", body.to_string(tcx).unwrap());
    let mapper = IRMapper::new(tcx, body, GatherMode::All);

    finder
      .nodes
      .into_iter()
      .filter_map(|hir_id| {
        let locations = mapper.get_mir_locations(hir_id, GatherDepth::Nested)?;
        Some(
          locations
            .values()
            .map(|loc| (loc, hir_id))
            .collect::<Vec<_>>(),
        )
      })
      .flat_map(|x| x)
      .collect()
  }

  pub fn abstract_loc(&self, (def_id, loc_or_span): MirLoc) -> Option<HirLoc> {
    let location = loc_or_span.left()?;
    let (owner_id, body_mapping) = &self.mapping[&def_id];
    let node_id = if location == Location::START {
      *owner_id
    } else {
      *body_mapping.get(&location)?
    };
    Some((*owner_id, node_id))
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
