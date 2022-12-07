use std::collections::{hash_map::Entry, HashMap};

use flowistry::{
  indexed::impls::LocationOrArg,
  mir::utils::{BodyExt, PlaceExt, SpanExt},
  source_map,
};
use rustc_hir::{
  self as hir,
  intravisit::{self, Visitor as HirVisitor},
  HirId,
};
use rustc_middle::{
  hir::nested_filter::OnlyBodies,
  mir::{self, Body, Location, Place},
  ty::TyCtxt,
};
use rustc_mir_dataflow::{Analysis, JoinSemiLattice, ResultsVisitor};
use rustc_span::Span;

use crate::{
  analysis::permissions::{
    utils::{flow_mir_permissions, PAnalysis, PDomain},
    PermissionsCtxt, PermissionsStateStep, PermsDiff,
  },
  Range,
};

pub fn compute_permission_steps<'tcx>(
  ctxt: &PermissionsCtxt<'_, 'tcx>,
) -> HashMap<HirId, HashMap<Place<'tcx>, PermsDiff>> {
  let results = flow_mir_permissions(ctxt);
  let tcx = ctxt.tcx;
  let body = &ctxt.body_with_facts.body;
  let basic_blocks = body.basic_blocks.indices();

  let mut mir_visitor = MirAnalysisLifter::<'_, 'tcx, PAnalysis<'_, 'tcx>> {
    tcx,
    body,
    map: HashMap::default(),
  };

  results.visit_with(body, basic_blocks, &mut mir_visitor);

  let mir_map = mir_visitor.map;

  let mut hir_visitor = HirPermDiffFlow {
    tcx,
    map: mir_map,
    diff: HashMap::default(),
    last_stmt_perms: ctxt.initial_body_permissions().into(),
  };

  hir_visitor.visit_nested_body(ctxt.body_id);

  hir_visitor
    .diff
    .into_iter()
    .filter_map(|(id, places_to_perms)| {
      let filtered = places_to_perms
        .into_iter()
        .filter(|(place, diff)| {
          let local = place.local;
          let local_info = &body.local_decls[local];
          local_info.is_user_variable() && !diff.is_empty()
        })
        .collect::<HashMap<_, _>>();

      (!filtered.is_empty()).then_some((id, filtered))
    })
    .collect::<HashMap<_, _>>()
}

pub fn prettify_permission_steps<'tcx>(
  ctxt: &PermissionsCtxt<'_, 'tcx>,
  perm_steps: HashMap<HirId, HashMap<Place<'tcx>, PermsDiff>>,
  span_to_range: impl Fn(Span) -> Range,
) -> Vec<PermissionsStateStep> {
  let tcx = ctxt.tcx;
  let hir = tcx.hir();
  let body = &ctxt.body_with_facts.body;
  perm_steps
    .into_iter()
    .map(|(id, place_to_diffs)| {
      let span = hir.span(id);
      let range = span_to_range(span);
      let state = place_to_diffs
        .into_iter()
        .map(|(place, diff)| {
          let s = place
            .to_string(tcx, body)
            .unwrap_or_else(|| String::from("<var>"));
          (s, diff)
        })
        .collect::<Vec<_>>();

      PermissionsStateStep {
        location: range,
        state,
      }
    })
    .collect::<Vec<_>>()
}

struct HirPermDiffFlow<'tcx> {
  tcx: TyCtxt<'tcx>,
  map: HashMap<HirId, PDomain<'tcx>>,
  diff: HashMap<HirId, HashMap<Place<'tcx>, PermsDiff>>,
  last_stmt_perms: PDomain<'tcx>,
}

fn domain_step<'tcx>(
  before: &PDomain<'tcx>,
  after: &PDomain<'tcx>,
) -> HashMap<Place<'tcx>, PermsDiff> {
  before
    .iter()
    .fold(HashMap::default(), |mut acc, (place, p1)| {
      let p2 = after.get(place).unwrap();
      let diff = p1.step(p2);

      match acc.entry(*place) {
        Entry::Occupied(_) => {
          panic!("Permissions step already in output for {place:?}");
        }
        Entry::Vacant(entry) => {
          entry.insert(diff);
        }
      }

      acc
    })
}

impl<'tcx> HirVisitor<'tcx> for HirPermDiffFlow<'tcx> {
  type NestedFilter = OnlyBodies;

  fn nested_visit_map(&mut self) -> Self::Map {
    self.tcx.hir()
  }

  fn visit_stmt(&mut self, stmt: &'tcx hir::Stmt) {
    let id = stmt.hir_id;

    let dmn = self.map.get(&id).unwrap();

    let dmn_diff = domain_step(&self.last_stmt_perms, dmn);

    self.diff.insert(id, dmn_diff);
    self.last_stmt_perms = dmn.clone();

    intravisit::walk_stmt(self, stmt);
  }
}

// ------------------------------------------------
// Binning the MIR locations to HIR statements

struct MirAnalysisLifter<'a, 'tcx: 'a, A>
where
  A: Analysis<'tcx>,
{
  tcx: TyCtxt<'tcx>,
  body: &'a Body<'tcx>,
  map: HashMap<HirId, A::Domain>,
}

impl<'a, 'tcx: 'a, A> MirAnalysisLifter<'a, 'tcx, A>
where
  A: Analysis<'tcx>,
{
  // We want to be able to ignore location which lie in an unwind.
  // If the statements there happen to bin to the same HIR stmt this can
  // really mess up the permissions.
  fn is_on_unwind_path(&self, loc: Location) -> bool {
    let bb = loc.block;
    let bbd = &self.body.basic_blocks[bb];
    bbd.is_cleanup
  }

  fn location_to_stmt(&self, loc: Location) -> Option<HirId> {
    let body = &self.body;
    let hir = self.tcx.hir();
    let mut hir_id = body.location_to_hir_id(loc);

    loop {
      let curr = hir.get(hir_id);

      if let hir::Node::Stmt(_) = curr {
        break;
      }

      let next = hir.get_parent_node(hir_id);

      if next == hir_id {
        log::warn!("reached crate root without finding a Statement");
        return None;
      }

      hir_id = next;
    }

    log::debug!(
      "Location {loc:?}, matched with LocalId: {:?}",
      hir_id.local_id
    );

    Some(hir_id)
  }
}

impl<'a, 'tcx: 'a, A> ResultsVisitor<'_, 'tcx>
  for MirAnalysisLifter<'a, 'tcx, A>
where
  A: Analysis<'tcx>,
{
  type FlowState = A::Domain;

  fn visit_statement_after_primary_effect(
    &mut self,
    state: &Self::FlowState,
    _statement: &mir::Statement<'tcx>,
    location: Location,
  ) {
    if !self.is_on_unwind_path(location) {
      if let Some(id) = self.location_to_stmt(location) {
        match self.map.entry(id) {
          Entry::Occupied(mut entry) => {
            entry.get_mut().join(state);
          }
          Entry::Vacant(entry) => {
            entry.insert(state.clone());
          }
        }
      }
    }
  }

  fn visit_terminator_after_primary_effect(
    &mut self,
    state: &Self::FlowState,
    _terminator: &mir::Terminator<'tcx>,
    location: Location,
  ) {
    if !self.is_on_unwind_path(location) {
      if let Some(id) = self.location_to_stmt(location) {
        match self.map.entry(id) {
          Entry::Occupied(mut entry) => {
            entry.get_mut().join(state);
          }
          Entry::Vacant(entry) => {
            entry.insert(state.clone());
          }
        }
      }
    }
  }
}
