use std::collections::hash_map::Entry;

use anyhow::{anyhow, bail, Result};
use flowistry::mir::utils::{PlaceExt as FlowistryPlaceExt, SpanExt};
use rustc_data_structures::{
  captures::Captures,
  fx::{FxHashMap as HashMap, FxHashSet as HashSet},
  graph::{self, iterate},
};
use rustc_hir::{
  self as hir,
  intravisit::{self, Visitor as HirVisitor},
  HirId,
};
use rustc_middle::{
  hir::nested_filter,
  mir::{BasicBlock, BasicBlocks, Body, Location, Place, Successors},
};
use rustc_span::Span;

use super::*;
use crate::{
  analysis::{
    ir_mapper::{CleanedBody, GatherDepth, GatherMode, IRMapper},
    permissions::{
      Permissions, PermissionsCtxt, PermissionsData, PermissionsDomain, Point,
    },
  },
  errors,
  mir::utils::PlaceExt as AquascopePlaceExt,
  Range,
};

pub fn compute_permission_steps<'a, 'tcx>(
  ctxt: &PermissionsCtxt<'a, 'tcx>,
  mode: PermIncludeMode,
  span_to_range: impl Fn(Span) -> Range,
) -> Vec<PermissionsStateStep>
where
  'tcx: 'a,
{
  // FIXME: remove
  crate::analysis::permissions::utils::dump_mir_debug(ctxt);

  let body = &ctxt.body_with_facts.body;
  let _basic_blocks = body.basic_blocks.indices();
  let mut hir_visitor = HirStepPoints::make(ctxt);
  hir_visitor.visit_nested_body(ctxt.body_id);

  log::debug!("Hir Visitor {hir_visitor:?}");

  prettify_permission_steps(
    ctxt,
    hir_visitor.finalize_diffs(),
    mode,
    span_to_range,
  )
}

// Prettify, means:
// - Remove all places that are not source visible
// - Remove all tables which are empty
// - Sanitize spans (mostly for macro invocation)
// - Convert Spans to Ranges
fn prettify_permission_steps<'tcx>(
  ctxt: &PermissionsCtxt<'_, 'tcx>,
  perm_steps: HashMap<Span, HashMap<Place<'tcx>, PermissionsDataDiff>>,
  mode: PermIncludeMode,
  span_to_range: impl Fn(Span) -> Range,
) -> Vec<PermissionsStateStep> {
  let tcx = ctxt.tcx;
  let body = &ctxt.body_with_facts.body;

  let should_keep = |p: &PermissionsDataDiff| -> bool {
    !(matches!(p.is_live, ValueStep::None { value: Some(false) })
      || (mode == PermIncludeMode::Changes && p.is_empty()))
  };

  macro_rules! place_to_string {
    ($p:expr) => {
      $p.to_string(tcx, body)
        .unwrap_or_else(|| String::from("<var>"))
    };
  }

  let first_error_span_opt =
    errors::get_span_of_first_error(ctxt.def_id.expect_local())
      .and_then(|s| s.as_local(ctxt.body_with_facts.body.span));
  let source_map = tcx.sess.source_map();

  perm_steps
    .into_iter()
    .fold(
      HashMap::<Span, Vec<(Place<'tcx>, PermissionsDataDiff)>>::default(),
      |mut acc, (span, place_to_diffs)| {
        // let span = unsanitized_span
        //   .as_local(ctxt.body_with_facts.body.span)
        //   .unwrap_or(unsanitized_span);

        // Attach the span to the end of the line. Later, all permission
        // steps appearing on the same line will be combined.
        let span = source_map.span_extend_to_line(span).shrink_to_hi();

        let entries = place_to_diffs
          .into_iter()
          .filter(|(place, diff)| {
            place.is_source_visible(tcx, body) && should_keep(diff)
          })
          .collect::<Vec<_>>();

        // This could be a little more graceful. The idea is that
        // we want to remove all permission steps which occur after
        // the first error, but the steps involved with the first
        // error could still be helpful. This is why we filter all
        // spans with a LO BytePos greater than the error
        // span HI BytePos.
        if !(entries.is_empty()
          || first_error_span_opt
            .is_some_and(|err_span| err_span.hi() < span.lo()))
        {
          acc.entry(span).or_default().extend(entries);
        }

        acc
      },
    )
    .into_iter()
    .map(|(span, mut entries)| {
      let range = span_to_range(span);

      entries
        .sort_by_key(|(place, _)| (place.local.as_usize(), place.projection));

      let state = entries
        .into_iter()
        .map(|(place, diff)| {
          let s = place_to_string!(place);
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

// NOTE: there must exist a path from location_1 -> location_2
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct MirSegment {
  from: Location,
  to: Location,
}

impl std::fmt::Debug for MirSegment {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "MirSegment({:?} -> {:?})", self.from, self.to)
  }
}

impl MirSegment {
  fn new(l1: Location, l2: Location) -> Self {
    MirSegment { from: l1, to: l2 }
  }

  fn squash_block_path(
    &self,
    basic_blocks: &BasicBlocks,
    path: impl Iterator<Item = BasicBlock>,
  ) -> Vec<Location> {
    path
      .flat_map(|bb| {
        let bbd = &basic_blocks[bb];
        let from = if bb == self.from.block {
          self.from.statement_index
        } else {
          0
        };

        let to = if bb == self.to.block {
          self.to.statement_index
        } else {
          bbd.statements.len()
        };

        (from ..= to).map(move |idx| Location {
          block: bb,
          statement_index: idx,
        })
      })
      .collect::<Vec<_>>()
  }

  fn paths_along_segment(&self, graph: &CleanedBody) -> Vec<Vec<BasicBlock>> {
    graph.paths_from_to(self.from.block, self.to.block)
  }

  fn spanned_locations(&self, body: &Body) -> HashSet<Location> {
    let graph = CleanedBody::make(body);
    let block_paths = self.paths_along_segment(&graph);

    block_paths
      .into_iter()
      .flat_map(|path| {
        self.squash_block_path(&body.basic_blocks, path.into_iter())
      })
      .collect::<HashSet<_>>()
  }

  fn is_valid_segment(&self, graph: &CleanedBody) -> bool {
    let dominators = graph.dominators();
    let post_dominators = graph.post_dominators();
    let vertices =
      iterate::post_order_from_to(graph, self.from.block, Some(self.to.block));

    log::debug!("Validating the segment {self:?} ...");
    log::debug!("path vertices {vertices:#?}");

    let valid_entry = vertices.iter().all(|&b2| {
      self.from.block == b2 || dominators.is_dominated_by(b2, self.from.block)
    });

    let valid_exit = vertices.iter().all(|&b2| {
      self.to.block == b2
        || post_dominators.is_postdominated_by(b2, self.to.block)
    });

    if !valid_entry {
      log::error!("the segment start does not dominate the segment blocks.");
    }

    if !valid_exit {
      log::error!("the segment end does not post-dominate the segment blocks.");
    }

    valid_entry && valid_exit
  }
}

// Fragmented paths, along the segment via the specified paths.
#[derive(Clone)]
struct Fragmentation {
  segment: MirSegment,
  paths: Vec<Vec<BasicBlock>>,
  staged_changes: HashMap<(HirId, Span), Vec<MirSegment>>,
}

/// Associated data for making permission steps.
/// The sole purpose of this structure is to register between
/// which two points in the MIR we would like to make a step, this is
/// driven by the entry / exit locations of a single HIR node.
struct HirStepPoints<'a, 'tcx>
where
  'tcx: 'a,
{
  ctxt: &'a PermissionsCtxt<'a, 'tcx>,
  ir_mapper: IRMapper<'a, 'tcx>,
  graph: CleanedBody<'a, 'tcx>,
  body_segment: MirSegment,
  // A single HirId may have multiple step points.
  diffs_on_hir: HashMap<HirId, HashSet<Span>>,
  // A single span may hay have multiple steps associated with it,
  // a span may also be associated with multiple HIR nodes.
  // XXX: the location represents the *end* location of a step.
  // If it is registered here, there should exist a step in the
  // mir_segments which ends at this location.
  diffs_on_span: HashMap<(HirId, Span), Vec<MirSegment>>,
  // XXX: when combined, the segments cover the entrie MIR.
  mir_segments: HashSet<MirSegment>,
  fragmented_state: Option<Fragmentation>,
}

impl<'a, 'tcx: 'a> HirStepPoints<'a, 'tcx> {
  fn make(ctxt: &'a PermissionsCtxt<'a, 'tcx>) -> Self {
    let tcx = ctxt.tcx;
    let hir = tcx.hir();
    let body = &ctxt.body_with_facts.body;
    let ir_mapper = IRMapper::new(tcx, body, GatherMode::IgnoreCleanup);
    let graph = CleanedBody::make(body);
    let body_hir_id = hir.body(ctxt.body_id).value.hir_id;
    // FIXME: this really shouldn't ever fail but having to
    // `unwrap`s feels dirty.
    let (from, to) = ir_mapper
      .get_mir_locations(body_hir_id, GatherDepth::Nested)
      .unwrap()
      .get_entry_exit_locations()
      .unwrap();

    let body_segment = MirSegment::new(from, to);
    let mut mir_segments = HashSet::default();
    mir_segments.insert(body_segment);

    log::debug!("The body segment for searching: {body_segment:?}");

    HirStepPoints {
      ctxt,
      ir_mapper,
      graph,
      body_segment,
      mir_segments,
      diffs_on_hir: HashMap::default(),
      diffs_on_span: HashMap::default(),
      fragmented_state: None,
    }
  }
}

impl<'a, 'tcx: 'a> HirStepPoints<'a, 'tcx> {
  fn assert_non_fragmented(&mut self) -> Result<()> {
    if let Some(state) = self.fragmented_state.take() {
      if !state.paths.is_empty() {
        self.fragmented_state.replace(state);
        bail!(
          "the HirStepPoints State has remaining fragmented paths {:?}",
          self.fragmented_state,
        );
      }

      self
        .diffs_on_span
        .iter_mut()
        .for_each(|(_, vs)| vs.retain(|seg| *seg != state.segment));

      for ((id, span), ds) in state.staged_changes.into_iter() {
        self.diffs_on_hir.entry(id).or_default().insert(span);
        self.diffs_on_span.entry((id, span)).or_default().extend(ds);
      }
    }
    Ok(())
  }

  fn insert_step_at(
    &mut self,
    id: HirId,
    span: Span,
    location: Location,
  ) -> Result<()> {
    log::debug!("inserting a step! {self:?}");

    let body = &self.ctxt.body_with_facts.body;
    if self.fragmented_state.is_none() {
      // We aren't in a fragmented state. So let's create one!
      log::debug!("inserting location step into a valid state {location:?}");
      let enclosing_segments = self
        .mir_segments
        .drain_filter(|s| Self::segment_contains(body, *s, location))
        .collect::<Vec<_>>();

      log::debug!(
        "location {location:?} enclosed in segment\n{enclosing_segments:?}"
      );

      if enclosing_segments
        .iter()
        .all(|seg| location == seg.from || location == seg.to)
      {
        self.mir_segments.extend(enclosing_segments);
        // HACK FIXME:
        return Ok(());
      }

      let segment = enclosing_segments.first().ok_or_else(|| {
        anyhow!("no enclosing span found for slice point {location:?}")
      })?;

      assert_eq!(enclosing_segments.len(), 1);

      let paths = segment.paths_along_segment(&self.graph);
      let fs = Fragmentation {
        segment: *segment,
        paths,
        staged_changes: HashMap::default(),
      };
      self.fragmented_state.replace(fs);
    } else {
      log::debug!(
        "inserting location step into a fragmented state {location:?}"
      );
    }

    let fragmentation = self.fragmented_state.as_mut().unwrap();
    let MirSegment { from, to } = fragmentation.segment;

    let first_step = MirSegment::new(from, location);
    let second_step = MirSegment::new(location, to);

    log::debug!("\nSlicing range [{from:?} -> {to:?}] into:\n  {first_step:?}\n  {second_step:?}");

    // [INACCURATE] The new segmented paths need to maintaint the invariant that
    // there exists a dominator / post-dominator for every segment.
    //
    // FIXME: how do we want to validate steps? It isn't accurate to say that
    // each segment is valid. Example,
    //
    //       ⬤ [l1]
    //     /   \
    //   ⬤ [l2] ⬤ [l3]
    //    \   /
    //     ⬤ [l4]
    //
    // the segments l1 -> l3 and l1 -> l2 are *invalid*
    // that is, l2 is not a post-dominator of l1. the sum of
    // all the segments is valid, but having these invalid steps
    // is common when slicing up `if`s and `match`es.
    // let f = first_step.is_valid_segment(&self.graph);
    // let s = second_step.is_valid_segment(&self.graph);

    // XXX: these probably shouldn't be bails, I bet we could handle this in some way.
    // if !f {
    //   bail!(
    //     "the new beginning segment from {from:?} -> {location:?} is not valid"
    //   );
    // }

    // if !s {
    //   bail!("the new end segment from {location:?} -> {to:?} is not valid");
    // }

    let removed_paths = fragmentation
      .paths
      .drain_filter(|path| path.contains(&location.block));

    log::debug!(
      "Adding step {from:?} -> {location:?} -> {to:?} removed these paths:"
    );
    for path in removed_paths {
      log::debug!("\n  {path:?}");
    }

    self.mir_segments.insert(first_step);
    self.mir_segments.insert(second_step);

    // Update the previous step
    let old_key_opt = self.diffs_on_span.keys().find(|k| {
      self
        .diffs_on_span
        .get(k)
        .unwrap()
        .contains(&fragmentation.segment)
    });

    if let Some(k) = old_key_opt {
      fragmentation
        .staged_changes
        .entry(*k)
        .or_default()
        .push(second_step);
    }

    fragmentation
      .staged_changes
      .entry((id, span))
      .or_default()
      .push(first_step);

    Ok(())
  }

  // HACK: using this isn't right ...
  fn span_of(&self, id: HirId) -> Span {
    let hir = self.ctxt.tcx.hir();
    let span = hir.span(id);
    let sanitized = span
      .as_local(self.ctxt.body_with_facts.body.span)
      .unwrap_or(span);
    sanitized
  }
  fn segment_contains(body: &Body, segment: MirSegment, loc: Location) -> bool {
    let locs = segment.spanned_locations(body);
    log::debug!("segment {segment:?} contains {loc:?}?\n{locs:#?}");
    locs.contains(&loc)
  }

  fn body_value_id(&self) -> HirId {
    let hir = self.ctxt.tcx.hir();
    hir.body(self.ctxt.body_id).value.hir_id
  }

  fn domain_bottom(&self) -> PermissionsDomain<'tcx> {
    self
      .ctxt
      .domain_places()
      .into_iter()
      .map(|place| {
        (place, PermissionsData {
          is_live: false,
          type_droppable: false,
          type_writeable: false,
          type_copyable: false,
          path_moved: false,
          loan_read_refined: None,
          loan_write_refined: None,
          loan_drop_refined: None,
          permissions: Permissions::bottom(),
        })
      })
      .collect::<HashMap<_, _>>()
      .into()
  }

  fn finalize_diffs(
    self,
  ) -> HashMap<Span, HashMap<Place<'tcx>, PermissionsDataDiff>> {
    if cfg!(debug_assertions) {
      self.check_total_coverage();
    }

    let body_hir_id = self.body_value_id();
    let body_open_brace = self.span_of(body_hir_id).shrink_to_lo();
    let first_point = self.ctxt.location_to_point(self.body_segment.from);
    let first_domain = &self.ctxt.permissions_domain_at_point(first_point);
    let empty_domain = &self.domain_bottom();

    let id_span = self
      .diffs_on_hir
      .into_iter()
      .flat_map(|(id, spans)| spans.into_iter().map(move |span| (id, span)));

    let mut diffs = id_span
      .flat_map(|(id, span)| {
        let segments = self.diffs_on_span.get(&(id, span)).unwrap();

        // assert_eq!(segments.len(), 1);

        segments
          .into_iter()
          .filter_map(move |MirSegment { from, to }| {
            (from != to).then(|| {
              let p0 = self.ctxt.location_to_point(*from);
              let p1 = self.ctxt.location_to_point(*to);
              let before = self
                .ctxt
                .permissions_domain_after_point_effect(p0)
                .unwrap_or_else(|| self.ctxt.permissions_domain_at_point(p0));
              let before = &before;
              let after = &self.ctxt.permissions_domain_at_point(p1);
              (span, before.diff(after))
            })
          })
      })
      .collect::<HashMap<_, _>>();

    // Upon entry, the function parameters are already "live". But we want to
    // special case this, and show that they "come alive" at the opening brace.
    let first_diff = empty_domain.diff(first_domain);
    diffs.insert(body_open_brace, first_diff);

    diffs
  }

  fn check_total_coverage(&self) {
    log::warn!("checking the total coverage of the sliced mir segments");
    let body = &self.ctxt.body_with_facts.body;
    let total_domain = self.body_segment.spanned_locations(body);

    let mut locations = HashSet::default();
    let mut raw_locations = HashSet::default();

    locations.extend(total_domain);

    log::warn!("checking stored mir segment coverage");

    for segment in self.mir_segments.iter() {
      raw_locations.extend(segment.spanned_locations(body));
    }

    assert_eq!(raw_locations.len(), locations.len());
    raw_locations.iter().for_each(|loc| {
      assert!(locations.contains(loc));
    });

    let _ = raw_locations.drain();

    log::warn!("checking visible saved spans coverage");

    for (_, segments) in self.diffs_on_span.iter() {
      for segment in segments.iter() {
        raw_locations.extend(segment.spanned_locations(body));
      }
    }

    assert_eq!(raw_locations.len(), locations.len());
    raw_locations.iter().for_each(|loc| {
      assert!(locations.contains(loc));
    });
  }
}

impl std::fmt::Debug for Fragmentation {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Fragmented along: {:?}\n", self.segment)?;
    write!(f, "  remaining paths\n")?;
    for p in self.paths.iter() {
      write!(f, "    {:?}", p)?;
    }
    write!(f, "\n")
  }
}

impl std::fmt::Debug for HirStepPoints<'_, '_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "\n\n--- HirStepPoints ---\n")?;
    write!(f, "  in state: {:?}\n", self.fragmented_state)?;
    for (id, spans) in self.diffs_on_hir.iter() {
      write!(f, "Attached to {:?}:\n", id.local_id)?;
      for span in spans.iter() {
        write!(f, "  diffs at span: {span:?}\n")?;
        let e = &Vec::default();
        let diffs = self.diffs_on_span.get(&(*id, *span)).unwrap_or(e);
        for l in diffs.iter() {
          write!(f, "    step end at {l:?}\n")?;
        }
      }
      write!(f, "\n")?;
    }
    write!(f, "\nstorage mir segments \n")?;
    for segment in self.mir_segments.iter() {
      write!(f, "  {segment:?}\n")?;
    }
    write!(f, "\n")?;
    Ok(())
  }
}

macro_rules! open_control_flow {
  ($this:tt, $id:expr, $span:expr) => {
    open_constrol_flow!($this, $id, "")
  };

  ($this:tt, $id:expr, $span:expr, $msg:expr) => {
    $this
      .ir_mapper
      .get_mir_locations($id, GatherDepth::Nested)
      .and_then(|mir_order| {
        mir_order.entry_location().map(|entry| {
          $this.insert_step_at($id, $span, entry).expect("");
        })
      })
      .unwrap_or_else(|| {
        if cfg!(debug_assertions) {
          panic!(
            "Expected entry / exit locations but none were found: {:?}",
            $msg
          );
        } else {
          log::error!(
            "Expected entry / exit locations but none were found: {:?}",
            $msg
          );
        }
      });
  };
}

macro_rules! expect_no_fragmentation {
  ($this:tt, $id:expr, $span:expr) => {
    expect_no_fragmentation!($this, $id, "")
  };

  ($this:tt, $id:expr, $span:expr, $msg:expr) => {
    $this
      .ir_mapper
      .get_mir_locations($id, GatherDepth::Nested)
      .and_then(|mir_order| {
        mir_order.exit_location().map(|exit| {
          $this.insert_step_at($id, $span, exit).expect("");
          $this.assert_non_fragmented().expect("");
        })
      })
      .unwrap_or_else(|| {
        if cfg!(debug_assertions) {
          panic!(
            "Expected entry / exit locations but none were found: {:?}",
            $msg
          );
        } else {
          log::error!(
            "Expected entry / exit locations but none were found: {:?}",
            $msg
          );
        }
      });
  };
}

impl<'a, 'tcx: 'a> HirVisitor<'tcx> for HirStepPoints<'a, 'tcx> {
  type NestedFilter = nested_filter::All;

  fn nested_visit_map(&mut self) -> Self::Map {
    self.ctxt.tcx.hir()
  }

  fn visit_stmt(&mut self, stmt: &'tcx hir::Stmt) {
    let hir = self.nested_visit_map();
    let span = self.span_of(stmt.hir_id);
    let error_msg =
      format!("Analyzing statement : {}", hir.node_to_string(stmt.hir_id));

    log::debug!("looking at a stmt!");

    expect_no_fragmentation!(self, stmt.hir_id, span, error_msg);

    intravisit::walk_stmt(self, stmt);
  }

  fn visit_block(&mut self, block: &'tcx hir::Block) {
    let hir = self.ctxt.tcx.hir();

    for stmt in block.stmts.iter() {
      self.visit_stmt(stmt);
    }

    if let Some(expr) = block.expr {
      let id = expr.hir_id;
      let span = self.span_of(id);
      let error_msg =
        format!("end-of-statement expr: {}", hir.node_to_string(id));
      expect_no_fragmentation!(self, id, span, error_msg);
      self.visit_expr(expr);
    }
  }

  // // A Local of the form:
  // //
  // // ```
  // // let symbol = Δ ∪ {symbol} expr else { ⟦ block ⟧ };
  // // ```
  // fn visit_local(&mut self, local: &'tcx hir::Local) {
  //   let id = local.hir_id;
  //   let hir = self.ctxt.tcx.hir();

  //   log::debug!("Visiting LOCAL: {}", hir.node_to_string(id));

  //   // NOTE: We add a "step barrier" for local assignments to make sure the permissions
  //   // for an assigned local don't come alive too early. Consider the following:
  //   // ```
  //   // let x = if <cnd> {
  //   //   <expr:1>
  //   // } else {
  //   //   <expr:2>
  //   // }
  //   // ```
  //   // Due to how the MIR is generated, the MIR statement assigning to `x`,
  //   // happens at the end of the block <expr:1>. This means, that if we don't
  //   // pull those permissions out to the assignment, they will only occur once
  //   // (specifically inside the "then branch") for the whole let.
  //   let mut added_barrier = false;

  //   if let Some(place) = self.ir_mapper.local_assigned_place(local) {
  //     self.step_barriers.push(place);
  //     added_barrier = true;
  //   }

  //   let pre_visibility_ctxt = self.visibility_scopes.clone();

  //   if let Some(expr) = local.init {
  //     self.visit_expr(expr);
  //   }

  //   if let Some(_block) = local.els {
  //     // TODO:
  //     self.visibility_scopes = pre_visibility_ctxt;
  //     unimplemented!("Locals with else branch");
  //   }

  //   if added_barrier {
  //     self.step_barriers.pop();
  //   }
  // }

  fn visit_expr(&mut self, expr: &'tcx hir::Expr) {
    use hir::ExprKind as EK;

    let hir = self.ctxt.tcx.hir();

    match expr.kind {
      EK::If(cnd, then, else_opt) => {
        intravisit::walk_expr(self, cnd);

        let then_span = self.span_of(then.hir_id).shrink_to_lo();
        let error_msg = format!(
          "expecting fragmentation in THEN block: {}",
          hir.node_to_string(then.hir_id)
        );
        open_control_flow!(self, then.hir_id, then_span, error_msg);

        if let Some(els) = else_opt {
          let else_span = self.span_of(els.hir_id).shrink_to_lo();
          let error_msg = format!(
            "expecting fragmentation in ELSE block: {}",
            hir.node_to_string(els.hir_id)
          );
          open_control_flow!(self, els.hir_id, else_span, error_msg);
        }

        self
          .assert_non_fragmented()
          .expect("remaining fragmentation when closing IF");

        intravisit::walk_expr(self, then);
        if let Some(els) = else_opt {
          intravisit::walk_expr(self, els);
        }
      }

      EK::Match(swtch, arms, _source) => {
        intravisit::walk_expr(self, swtch);

        for arm in arms.iter() {
          let arm_span = self.span_of(arm.body.hir_id).shrink_to_lo();
          let error_msg = format!(
            "expecting fragmentation in MATCH ARM {}",
            hir.node_to_string(arm.hir_id)
          );
          open_control_flow!(self, arm.hir_id, arm_span, error_msg);
        }

        self
          .assert_non_fragmented()
          .expect("remaining fragmentation when closing IF");

        for arm in arms.iter() {
          intravisit::walk_arm(self, arm);
        }
      }
      _ => {
        intravisit::walk_expr(self, expr);
      }
    }
  }
}
