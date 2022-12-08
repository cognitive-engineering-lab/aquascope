use std::{
  cmp::Ordering,
  collections::{hash_map::Entry, HashMap},
};

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
    before_effect: HashMap::default(),
    after_effect: HashMap::default(),
  };

  results.visit_with(body, basic_blocks, &mut mir_visitor);

  let before_states = mir_visitor.before_effect;
  let after_states = mir_visitor.after_effect;

  let mut hir_visitor = HirPermDiffFlow {
    tcx,
    body,
    before_states,
    after_states,
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
      let mut entries = place_to_diffs.into_iter().collect::<Vec<_>>();
      entries
        .sort_by_key(|(place, _)| (place.local.as_usize(), place.projection));
      let state = entries
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

type DomainAtHir<'tcx, A: Analysis<'tcx>> =
  HashMap<HirId, HashMap<Location, A::Domain>>;

struct HirPermDiffFlow<'a, 'tcx> {
  tcx: TyCtxt<'tcx>,
  body: &'a Body<'tcx>,
  before_states: DomainAtHir<'tcx, PAnalysis<'a, 'tcx>>,
  after_states: DomainAtHir<'tcx, PAnalysis<'a, 'tcx>>,
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

impl<'a, 'tcx: 'a> HirVisitor<'tcx> for HirPermDiffFlow<'a, 'tcx> {
  type NestedFilter = OnlyBodies;

  fn nested_visit_map(&mut self) -> Self::Map {
    self.tcx.hir()
  }

  fn visit_stmt(&mut self, stmt: &'tcx hir::Stmt) {
    let id = stmt.hir_id;
    let body = &self.body;

    if let Some(after_states) = self.after_states.get(&id) {
      let before_states = self.before_states.get(&id).unwrap();
      // The child locations represent all of the MIR Points which came from this HIR Stmt.
      // We now want to collapse all of them into a single permissions step for this specific HIR.
      // For debugging, we want to check that in the ordered locations, the after state is the before
      // state of the next location. If this is not the case, then some action could happen
      // between them which does not correspond to this HIR Stmt.
      let mut child_locations = after_states.keys().collect::<Vec<_>>();

      // TODO(gavinleroy): a naive sorting by predecessor relationship will not be sufficient
      // for programs of even mild complexity. Take the following example.
      child_locations.sort_by(|l1, l2| {
        let l1_p = l1.is_predecessor_of(**l2, body);
        let l2_p = l2.is_predecessor_of(**l1, body);
        if l1_p && l2_p {
          log::error!("The MIR Locations for a HIR Stmt did not form a total order {l1:?} {l2:?}");
          Ordering::Equal
        } else if l1_p {
          Ordering::Less
        } else if l2_p {
          Ordering::Greater
        } else {
          log::error!("The MIR Locations for a HIR Stmt are unrelated {l1:?} {l2:?}");
          Ordering::Equal
        }
      });

      let entry_state = &self.last_stmt_perms;

      let mut previous = entry_state;

      // XXX: debugging only.
      // From what I've (gavin) observed is that the pre/post_state for a given
      // MIR Location are identical. I need to look into how the analysis computes
      // these because from my POV they should reflect the actions taken, but the actions
      // seem to have taken effect before the pre_state.
      for loc in child_locations.iter() {
        let pre_state = before_states.get(loc).unwrap();
        let post_state = after_states.get(loc).unwrap();

        // if previous != pre_state {
        //   log::error!("The previous Domain state does not match the current locations previous");
        //   log::error!("Previous: {previous:#?}\n Pre-State: {pre_state:#?}");
        // }

        previous = post_state;
      }

      // `previous` at this point is the output state of the entire
      let output_state = previous;

      let stmt_perm_step = domain_step(entry_state, output_state);

      self.diff.insert(id, stmt_perm_step);
      self.last_stmt_perms = output_state.clone();
    } else {
      log::warn!("No PDomain found for HIR::Stmt: {id:?}");
    }

    intravisit::walk_stmt(self, stmt);
  }
}

// ------------------------------------------------
// Binning the MIR locations to HIR statements

struct MirAnalysisLifter<'a, 'tcx: 'a, A: Analysis<'tcx>> {
  tcx: TyCtxt<'tcx>,
  body: &'a Body<'tcx>,
  before_effect: DomainAtHir<'tcx, A>,
  after_effect: DomainAtHir<'tcx, A>,
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

impl<'a, 'tcx: 'a> ResultsVisitor<'_, 'tcx>
  for MirAnalysisLifter<'a, 'tcx, PAnalysis<'a, 'tcx>>
{
  type FlowState = PDomain<'tcx>;

  // before effect

  fn visit_statement_before_primary_effect(
    &mut self,
    state: &Self::FlowState,
    _statement: &mir::Statement<'tcx>,
    location: Location,
  ) {
    if !self.is_on_unwind_path(location) {
      if let Some(id) = self.location_to_stmt(location) {
        self
          .before_effect
          .entry(id)
          .or_default()
          .insert(location, state.clone());
      }
    }
  }

  fn visit_terminator_before_primary_effect(
    &mut self,
    state: &Self::FlowState,
    _terminator: &mir::Terminator<'tcx>,
    location: Location,
  ) {
    if !self.is_on_unwind_path(location) {
      if let Some(id) = self.location_to_stmt(location) {
        self
          .before_effect
          .entry(id)
          .or_default()
          .insert(location, state.clone());
      }
    }
  }

  // after effect

  fn visit_statement_after_primary_effect(
    &mut self,
    state: &Self::FlowState,
    _statement: &mir::Statement<'tcx>,
    location: Location,
  ) {
    if !self.is_on_unwind_path(location) {
      if let Some(id) = self.location_to_stmt(location) {
        self
          .after_effect
          .entry(id)
          .or_default()
          .insert(location, state.clone());
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
        self
          .after_effect
          .entry(id)
          .or_default()
          .insert(location, state.clone());
      }
    }
  }
}
