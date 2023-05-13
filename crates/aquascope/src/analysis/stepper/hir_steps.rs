//! Experimental algorithm for creating permissions steps.
//!
//! It's main goals are to be:
//! 1. Easier to understand and describe than the previous.
//! 2. Disallow internal panics (something that causes UX problems).
//! 3. Successfuly analyze a larger set of Rust feautres. Loosening the
//!    restriction of multiple returns and infinite loops.

use anyhow::{anyhow, Result};
use rustc_data_structures::{
  self,
  fx::{FxHashMap as HashMap, FxHashSet as HashSet},
};
use rustc_hir::{
  self as hir,
  intravisit::{self, Visitor as HirVisitor},
  HirId,
};
use rustc_middle::{
  hir::nested_filter,
  mir::{self, Local, Location, Place},
};
use rustc_span::Span;
use rustc_utils::{
  source_map::range::CharRange, test_utils::DUMMY_CHAR_RANGE, PlaceExt, SpanExt,
};

use super::{segmented_mir::*, *};
use crate::{
  analysis::{
    ir_mapper::{GatherDepth, IRMapper},
    permissions::PermissionsCtxt,
  },
  errors,
};

/// A single table, unprocessed mapping Places to their differences for a MirSegment.
type PseudoTable<'tcx> = HashMap<Place<'tcx>, PermissionsDataDiff>;

/// At a given Span, there could be multiple tables generated, each for a different
/// MirSegment. These tables can either be merged or displayed side-by-side.
/// See [`prettify_permission_steps`] for merging strategy.
type Tables<'tcx> = HashMap<Span, Vec<(MirSegment, PseudoTable<'tcx>)>>;

// Prettify, means:
// - Remove all places that are not source visible
// - Remove all tables which are empty
// - Convert Spans to Ranges
fn prettify_permission_steps<'tcx>(
  analysis: &AquascopeAnalysis<'_, 'tcx>,
  perm_steps: Tables<'tcx>,
  mode: PermIncludeMode,
) -> Vec<PermissionsLineDisplay> {
  let ctxt = &analysis.permissions;
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
      HashMap::<
        CharRange,
        Vec<(MirSegment, Vec<(Place<'tcx>, PermissionsDataDiff)>)>,
      >::default(),
      |mut acc, (span, vs)| {
        for (segment, place_to_diffs) in vs.into_iter() {
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
            let range = analysis.span_to_range(span);
            acc.entry(range).or_default().push((segment, entries));
          }
        }

        acc
      },
    )
    .into_iter()
    // HACK FIXME: we're at odds with the multi-table setup. This quick
    // hack combines table entries into a single table until the
    // visual explanation gets up-to-speed.
    // Another weird thing about this is that you can have a single
    // table with two changes for one place.
    // ```example
    // # fn main() {
    // let closure = |s: &str| s.len(); // s: +R+O
    //                                  // s: -R-O
    //                                  // closure: +R+O
    // # }
    // ```
    // imagine that the comments to the right of the Let represent
    // a pseudo combined table. The path `s` gains and loses the same
    // set of permissions in the same table. This is kind of weird, we'd
    // rather just show *no change*.
    .filter_map(|(range, mut entries)| {
      for (_, v) in entries.iter_mut() {
        v.sort_by_key(|(place, _)| (place.local.as_usize(), place.projection))
      }

      // let state = entries
      //   .into_iter()
      //   .map(|(MirSegment { from, to }, diffs)| {
      //     let state = diffs
      //       .into_iter()
      //       .map(|(place, diff)| {
      //         let s = place_to_string!(place);
      //         (s, diff)
      //       })
      //       .collect::<Vec<_>>();
      //     let from = analysis.span_to_range(ctxt.location_to_span(from));
      //     let to = analysis.span_to_range(ctxt.location_to_span(to));
      //     PermissionsStepTable { from, to, state }
      //   })
      //   .collect::<Vec<_>>();

      // Conforming to the above HACK this just takes any (from, to) pair.
      let dummy_char_range = DUMMY_CHAR_RANGE.with(|range| *range);
      let (from, to) = entries.first().map_or_else(
        || (dummy_char_range, dummy_char_range),
        |(MirSegment { from, to }, _)| {
          let from = analysis.span_to_range(ctxt.location_to_span(*from));
          let to = analysis.span_to_range(ctxt.location_to_span(*to));
          (from, to)
        },
      );

      let mut master_table: Vec<(Place<'tcx>, PermissionsDataDiff)> =
        Vec::default();

      let is_symmetric_diff =
        |diff1: &PermissionsDataDiff, diff2: &PermissionsDataDiff| -> bool {
          macro_rules! is_symmetric {
            ($v1:expr, $v2:expr) => {
              matches!(
                (&$v1, &$v2),
                (ValueStep::High { .. }, ValueStep::Low { .. })
                  | (ValueStep::Low { .. }, ValueStep::High { .. })
                  | (ValueStep::None { .. }, ValueStep::None { .. })
              )
            };
          }
          let p1 = &diff1.permissions;
          let p2 = &diff2.permissions;
          is_symmetric!(p1.read, p2.read)
            && is_symmetric!(p1.write, p2.write)
            && is_symmetric!(p1.drop, p2.drop)
        };

      // For all tables which fall on the same line, we combine them into a single table
      // and remove all *SYMMETRIC* differences. That is, if you have permission changes such as:
      // - path: +R+O
      // - path: -R-O
      // these are exactly symmetric, and will be removed.
      for (_, diffs) in entries.into_iter() {
        for (place, diff) in diffs.into_iter() {
          let i_opt = master_table.iter().position(|(p, _)| *p == place);
          if let Some(idx) = i_opt {
            let (_, old_diff) = &master_table[idx];
            if is_symmetric_diff(&diff, old_diff) {
              log::debug!(
                "REMOVING place {place:?} with diff {diff:?} into the MT."
              );
              master_table.remove(idx);
              continue;
            }
          }

          log::debug!("ADDING place {place:?} with diff {diff:?} into the MT.");
          master_table.push((place, diff));
        }
      }

      // This means the tables were symmetric and all were removed.
      if master_table.is_empty() {
        return None;
      }

      let master_table = PermissionsStepTable {
        from,
        to,
        state: master_table
          .into_iter()
          .map(|(place, diff)| (place_to_string!(place), diff))
          .collect::<Vec<_>>(),
      };

      Some(PermissionsLineDisplay {
        location: range,
        state: vec![master_table],
      })
    })
    .collect::<Vec<_>>()
}

/// Visitor for creating permission steps in the HIR.
///
/// Visits the HIR in a Nested order, splitting the MIR and accumulating permission steps.
pub(super) struct HirStepPoints<'a, 'tcx>
where
  'tcx: 'a,
{
  ctxt: &'a PermissionsCtxt<'a, 'tcx>,
  ir_mapper: &'a IRMapper<'a, 'tcx>,

  // Error reporting counters
  unsupported_encounter: Option<(Span, String)>,
  fatal_errors: Vec<anyhow::Error>,

  // Actual state of the analysis
  /// Entry location of the body under analysis.
  start_loc: Location,
  locals_at_scope: HashMap<ScopeId, Vec<Local>>,
  /// Stack of the current branch entry points, used
  /// for hinting path steps to the `SegmentedMir`.
  current_branch_start: Vec<Location>,
  mir_segments: SegmentedMirBuilder<'a, 'tcx>,
}

/// Makes calling functions on the SegmentedMir easier.
/// All functions on the `SegmentedMir` return a Result in
/// the case that the internal state gets off. When it does,
/// we should save the error and stop the current computation.
/// As with most error-relevant things, if internally an error
/// state is entered more errors are likely to occur, but it's
/// really the first we care about.
macro_rules! invoke_internal {
  (on_fail -> $ret:expr, $this:ident, $call:ident, $($param:expr),*) => {
    match $this.mir_segments.$call($( $param ),*) {
      Err(e) => {
        $this.fatal_errors.push(e);
        return $ret;
      },
      Ok(v) => v,
    }
  };
  (on_fail -> $ret:expr, $this:ident, $call:ident) => {
    invoke_internal!(on_fail -> $ret, $this, $call,)
  };
  (on_fail -> $ret:expr, $this:ident, $call:ident, $($param:expr),*) => {
    invoke_internal!(on_fail -> $ret, $this, $call, $($param:expr),*)
  };
  ($this:ident, $call:ident) => {
    invoke_internal!(on_fail -> (), $this, $call,)
  };
  ($this:ident, $call:ident, $( $param:expr ),*) => {
    invoke_internal!(on_fail -> (), $this, $call, $( $param ),*)
  };
}

macro_rules! report_unexpected {
  ($this:ident, $($param:expr),*) => {
    $this.fatal_errors.push(anyhow!($( $param ),*))
  }
}

struct TableFinalizer<'a, 'tcx: 'a> {
  ctxt: &'a PermissionsCtxt<'a, 'tcx>,
  mir: &'a SegmentedMir,
  locals_at_scope: HashMap<ScopeId, Vec<Local>>,
}

#[allow(clippy::similar_names)]
impl<'a, 'tcx: 'a> TableFinalizer<'a, 'tcx> {
  fn locals_to_filter(&self, scope: ScopeId) -> HashSet<Local> {
    self
      .mir
      .parent_scopes(scope)
      .filter_map(|sid| self.locals_at_scope.get(&sid))
      .flatten()
      .copied()
      .collect::<HashSet<_>>()
  }

  fn insert_collection(&self, result: &mut Tables<'tcx>, cid: CollectionId) {
    let collection = self.mir.get_collection(cid);

    for &part in collection.data.iter() {
      match part {
        CFKind::Linear(seg_id) => self.insert_segment(result, seg_id),
        CFKind::Branch(branch_id) => self.insert_branch(result, branch_id),
      }
    }
  }

  fn insert_segment(&self, result: &mut Tables<'tcx>, sid: SegmentId)
  // -> ScopeId
  {
    let ctxt = &self.ctxt;
    let &SegmentData {
      segment,
      span,
      scope,
    } = self.mir.get_segment(sid);

    let to_filter = self.locals_to_filter(scope);

    if segment.from == segment.to {
      return;
    }

    let p0 = ctxt.location_to_point(segment.from);
    let p1 = ctxt.location_to_point(segment.to);
    let before = &ctxt.permissions_domain_at_point(p0);
    let after = &ctxt.permissions_domain_at_point(p1);
    let mut diff = before.diff(after);

    let removed = diff
      .drain_filter(|place, _| to_filter.contains(&place.local))
      .collect::<Vec<_>>();

    log::debug!(
      "removed domain places due to attached filter at {:?} {:?}",
      segment.to,
      removed
    );

    result.entry(span).or_default().push((segment, diff));
  }

  fn insert_branch(&self, result: &mut Tables<'tcx>, bid: BranchId) {
    let BranchData {
      reach,
      splits,
      joins,
      nested,
      ..
    } = self.mir.get_branch(bid);

    let mut entire_diff = reach.into_diff(self.ctxt);

    log::debug!("Inserting Branched Collection {:?}:\n\tsplits: {:?}\n\tmiddle: {:?}\n\tjoins: {:?}", reach, splits, nested, joins);

    let temp_middle = &mut Tables::default();
    let temp_joins = &mut Tables::default();

    for &sid in splits.iter() {
      self.insert_segment(temp_middle, sid);
    }

    for &cid in nested.iter() {
      self.insert_collection(temp_middle, cid);
    }

    for &sid in splits.iter() {
      self.insert_segment(temp_joins, sid);
    }

    // Find the locals which were filtered from all scopes. In theory,
    // `all_scopes` should contains the same scope, copied over,
    // but the SegmentedMir doesn't enforce this and there's no
    // scope attached to collections.
    let scope_here = self.mir.get_branch_scope(bid);
    let all_attached = self
      .locals_at_scope
      .get(&scope_here)
      .map(|v| v.iter())
      .unwrap_or_default()
      .collect::<HashSet<_>>();

    let attached_here = entire_diff
      .drain_filter(|place: &Place, _| all_attached.contains(&place.local))
      .collect::<PseudoTable>();

    let diffs_in_tables = |tbls: &Tables| {
      tbls
        .iter()
        .flat_map(|(_, tbls)| {
          tbls
            .iter()
            .flat_map(|(_, ptbl)| ptbl.iter().map(|(_, diff)| *diff))
        })
        .collect::<HashSet<PermissionsDataDiff>>()
    };

    // Flatten all tables to the unique `PermissionsDataDiff`s
    // that exist within them.
    let diffs_in_branches = diffs_in_tables(temp_middle);

    for (_, tbls) in temp_joins.iter_mut() {
      for (_, ptbl) in tbls.iter_mut() {
        let _drained = ptbl
          .drain_filter(|_, diff| diffs_in_branches.contains(diff))
          .collect::<Vec<_>>();
        // log::debug!("drained {drained:#?} because diff exists");
      }
    }

    macro_rules! insert {
      ($m1:expr => $m2:expr) => {
        for (&span, ref mut vs) in $m1.into_iter() {
          $m2.entry(span).or_default().append(vs);
        }
      };
    }

    insert!(temp_middle => result);
    insert!(temp_joins => result);

    // Attach filtered locals
    result
      .entry(reach.span(self.ctxt))
      .or_default()
      .push((*reach, attached_here));
  }
}

impl<'a, 'tcx> HirPermissionStepper<'tcx> for HirStepPoints<'a, 'tcx> {
  fn get_unsupported_feature(&self) -> Option<String> {
    self.unsupported_encounter.as_ref().map(|(_, s)| s.clone())
  }

  fn get_internal_error(&self) -> Option<String> {
    use itertools::Itertools;
    if self.fatal_errors.is_empty() {
      return None;
    }

    Some(
      self
        .fatal_errors
        .iter()
        .map(|e: &anyhow::Error| e.to_string())
        .join("\n"),
    )
  }

  fn finalize(
    self,
    analysis: &AquascopeAnalysis<'_, 'tcx>,
    mode: PermIncludeMode,
  ) -> Result<Vec<PermissionsLineDisplay>> {
    let ctxt = self.ctxt;

    let body_hir_id = self.body_value_id();
    let body_open_brace = self.span_of(body_hir_id).shrink_to_lo();
    let first_point = self.ctxt.location_to_point(self.start_loc);
    let first_domain = &self.ctxt.permissions_domain_at_point(first_point);
    let empty_domain = &self.ctxt.domain_bottom();
    let mir_segments = self.mir_segments.freeze()?;

    // Upon entry, the function parameters are already "live". But we want to
    // special case this, and show that they "come alive" at the opening brace.
    let first_diff = empty_domain.diff(first_domain);

    // Insert a segment into a table filtering defined places.

    let mut diffs = Tables::default();

    let finalizer = TableFinalizer {
      ctxt,
      mir: &mir_segments,
      locals_at_scope: self.locals_at_scope,
    };

    finalizer.insert_collection(&mut diffs, mir_segments.first_collection);

    // We do an unchecked insert here to avoid
    // the segment from getting filtered because the
    // segment from and to locations are equal.
    diffs.insert(body_open_brace, vec![(
      MirSegment::new(self.start_loc, self.start_loc),
      first_diff,
    )]);

    Ok(prettify_permission_steps(analysis, diffs, mode))
  }
}

impl<'a, 'tcx: 'a> HirStepPoints<'a, 'tcx> {
  pub(super) fn make(
    ctxt: &'a PermissionsCtxt<'a, 'tcx>,
    ir_mapper: &'a IRMapper<'a, 'tcx>,
  ) -> Result<Self> {
    let mir_segments = SegmentedMirBuilder::make(ir_mapper);
    let start_loc = mir::START_BLOCK.start_location();

    Ok(HirStepPoints {
      ctxt,
      ir_mapper,
      unsupported_encounter: None,
      fatal_errors: Vec::default(),
      start_loc,
      locals_at_scope: HashMap::default(),
      current_branch_start: Vec::default(),
      mir_segments,
    })
  }

  // Used for tracking path hints of the current branches.

  fn get_path_hint(&self) -> Option<Location> {
    self.current_branch_start.last().copied()
  }

  fn push_branch_start(&mut self, location: Location) {
    self.current_branch_start.push(location)
  }

  fn pop_branch_start(&mut self, expecting: Location) {
    if let Some(popped) = self.current_branch_start.pop() && popped != expecting {
      report_unexpected!(self, "expecting popped location {expecting:?} but got {popped:?}")
    }
  }

  /// Determine whether the traversal should visited nested HIR nodes.
  ///
  /// This method is a sort of HACK to avoid picking apart nodes expanded from
  /// macros, while visiting nodes expanded from expected desugarings (e.g. for / while loops).
  fn should_visit_nested(&self, _id: HirId, span: Span) -> bool {
    use rustc_span::hygiene::DesugaringKind as DK;
    !span.from_expansion()
      || span.is_desugaring(DK::ForLoop)
      || span.is_desugaring(DK::WhileLoop)
  }

  fn span_of(&self, id: HirId) -> Span {
    let hir = self.ctxt.tcx.hir();
    let span = hir.span(id);
    span
      .as_local(self.ctxt.body_with_facts.body.span)
      .unwrap_or(span)
  }

  fn body_value_id(&self) -> HirId {
    let hir = self.ctxt.tcx.hir();
    hir.body(self.ctxt.body_id).value.hir_id
  }

  fn get_node_entry(&self, hir_id: HirId) -> Option<Location> {
    let mir_order = self
      .ir_mapper
      .get_mir_locations(hir_id, GatherDepth::Nested)?;
    mir_order.entry_location()
  }

  fn get_node_exit(&self, hir_id: HirId) -> Option<Location> {
    let mir_order = self
      .ir_mapper
      .get_mir_locations(hir_id, GatherDepth::Nested)?;

    // HACK: shift the exit to the next successor if available.
    //       this way we capture the state changes for a single
    //       operation rather than having an off by one.
    // TODO: a more elegant solution would be to have a way to
    //       specify at which execution point you want the permission
    //       state, before, middle, or after an instruction. This is
    //       similar to what the MIR does but it doesn't provide an
    //       after point, only a start and mid.
    mir_order.exit_location().map(|e| {
      self
        .ir_mapper
        .cleaned_graph
        .location_successor(e)
        .unwrap_or(e)
    })
  }

  fn prettify_node(&self, hir_id: HirId) -> String {
    let hir = self.ctxt.tcx.hir();
    hir.node_to_string(hir_id)
  }

  /// Open a conditional expression for branching. On success, returns
  /// the exit `Location` of the given conditon.
  ///
  /// Examples, given a `EK::If(Expr, Expr, Option<Expr>)`, the given condition expression should
  /// be the first expression in the tuple, which is the condition.
  /// For a `EK::Match(Expr, [Arm], ...)` the given condition should be the first expression
  /// in the tuple which is the match condition.
  fn expr_condition_prelude(
    &mut self,
    cnd: &'tcx hir::Expr,
  ) -> Option<Location> {
    // NOTE: first we need to walk and split the condition. In the
    // case of a more complex condition expression, splitting this
    // first will result in a split location closest to the `SwitchInt`.
    self.visit_expr(cnd);
    let Some(cnd_exit) = self.get_node_exit(cnd.hir_id) else {
      log::warn!(
        "Skipping EXPR, condition has no exit {}",
        self.prettify_node(cnd.hir_id)
      );

      return None;
    };

    invoke_internal!(
      on_fail -> None,
      self,
      insert,
      cnd_exit,
      self.get_path_hint(),
      self.span_of(cnd.hir_id)
    );

    Some(cnd_exit)
  }

  /// Close the entire branching expression which had the condition exit.
  ///
  /// Here, the given expression should be the _entire_ `EK::If` or `EK::Match`.
  fn expr_condition_postlude(&mut self, bid: BranchId, hir_id: HirId) {
    log::warn!(
      "flushing and closing branch steps:\n{}",
      self.prettify_node(hir_id)
    );

    let ss = self.span_of(hir_id).shrink_to_hi();
    invoke_internal!(self, close_branch, bid, |_| ss);
  }

  /// Inserts a step point after the specified `HirId`. This
  /// method is generic and takes the raw span returned by the
  /// `IRMapper`, if a node requires tweaking for the span this
  /// should not be used.
  fn insert_step_at_node_exit(&mut self, hir_id: HirId) {
    if let Some(exit) = self.get_node_exit(hir_id) {
      invoke_internal!(
        self,
        insert,
        exit,
        self.get_path_hint(),
        self.span_of(hir_id)
      );
    } else {
      log::warn!(
        "Node {} doesn't have an exit location.",
        self.prettify_node(hir_id)
      );
    }
  }

  fn condition_produced_switchint(&self, expr: &'tcx hir::Expr) -> bool {
    if let Some(exit) = self.get_node_exit(expr.hir_id) {
      log::debug!(
        "checking location {exit:?} to see if terminator is switchInt"
      );
      self.ir_mapper.is_terminator_switchint(exit)
    } else {
      // If the IRMapper can't determine a single exit location that
      // is most often caused by branching, in this case we just assume
      // that a switchInt was procued. We could do something more robust
      // if we see the need for it.
      true
    }
  }

  // Factored out of the Visitor because this same logic is needed for
  // EK::If and while loop desugarings, just with a different location
  // to span mapping.
  fn handle_expr_if(
    &mut self,
    expr_id: HirId,
    cnd: &'tcx hir::Expr,
    then: &'tcx hir::Expr,
    else_opt: Option<&'tcx hir::Expr>,
    entry_locs_to_spans: HashMap<Location, Span>,
  ) {
    log::debug!(
      "visiting EXPR-IF\n\tCND: {}\n\t\tTHEN: {}\n\t\tELSE: {}",
      self.prettify_node(cnd.hir_id),
      self.prettify_node(then.hir_id),
      else_opt.map_or(String::from("<NONE>"), |e| self.prettify_node(e.hir_id))
    );

    let Some(cnd_exit) = self.expr_condition_prelude(cnd) else {
          return;
        };

    let mapper = self.ir_mapper;
    let branch_id = invoke_internal!(
      self,
      open_branch,
      cnd_exit,
      move |to: &mut Location| {
        entry_locs_to_spans
          .iter()
          .find_map(|(&l, &span)| {
            if mapper.ldominates(*to, l) {
              *to = l;
              Some(span)
            } else {
              None
            }
          })
          .unwrap_or_default()
      }
    );

    if let Some(then_entry) = self.get_node_entry(then.hir_id) {
      self.push_branch_start(then_entry);
      self.visit_expr(then);
      self.pop_branch_start(then_entry);
    } else {
      report_unexpected!(
        self,
        "then-branch doesn't have entry {}",
        self.prettify_node(then.hir_id)
      );
    }

    if let Some(els) = else_opt {
      if let Some(els_entry) = self.get_node_entry(els.hir_id) {
        self.push_branch_start(els_entry);
        self.visit_expr(els);
        self.pop_branch_start(els_entry);
      } else {
        report_unexpected!(
          self,
          "else-branch doesn't have entry {}",
          self.prettify_node(els.hir_id)
        );
      }
    }

    self.expr_condition_postlude(branch_id, expr_id);
  }

  fn handle_expr_match(
    &mut self,
    expr_id: HirId,
    cnd: &'tcx hir::Expr,
    arms: &'tcx [hir::Arm],
    entry_locs_to_spans: HashMap<Location, Span>,
  ) {
    let Some(cnd_exit) = self.expr_condition_prelude(cnd) else {
      return;
    };

    let mapper = self.ir_mapper;
    let branch_id = invoke_internal!(
      self,
      open_branch,
      cnd_exit,
      move |to: &mut Location| {
        entry_locs_to_spans
          .iter()
          .find_map(|(&l, &span)| {
            if mapper.ldominates(*to, l) {
              // Update the location to be the entry of the arm.
              *to = l;
              Some(span)
            } else {
              None
            }
          })
          .unwrap_or(Span::default())
      }
    );

    for arm in arms {
      self.visit_arm(arm);
    }

    self.expr_condition_postlude(branch_id, expr_id);
  }
}

impl<'a, 'tcx: 'a> HirVisitor<'tcx> for HirStepPoints<'a, 'tcx> {
  type NestedFilter = nested_filter::All;

  fn nested_visit_map(&mut self) -> Self::Map {
    self.ctxt.tcx.hir()
  }

  fn visit_body(&mut self, body: &'tcx hir::Body) {
    intravisit::walk_body(self, body);
    self.insert_step_at_node_exit(body.value.hir_id);
  }

  fn visit_block(&mut self, block: &'tcx hir::Block) {
    let scope = invoke_internal!(self, open_scope);
    for stmt in block.stmts.iter() {
      self.visit_stmt(stmt);
    }

    if let Some(expr) = block.expr {
      log::debug!("BLOCK contains final EXPR");
      self.visit_expr(expr);
      self.insert_step_at_node_exit(expr.hir_id);
    }
    invoke_internal!(self, close_scope, scope);
  }

  fn visit_stmt(&mut self, stmt: &'tcx hir::Stmt) {
    use rustc_hir::StmtKind as SK;

    log::debug!(
      "Starting analysis of STMT {}\n",
      self.prettify_node(stmt.hir_id),
    );

    let scope = invoke_internal!(self, open_scope);

    if let SK::Local(local) = stmt.kind {
      let places = self.ir_mapper.local_assigned_place(local);
      let locals = places.into_iter().map(|p| p.local).collect::<Vec<_>>();
      if !locals.is_empty() {
        log::debug!("storing locals at scope {scope:?} {locals:?}");
        self.locals_at_scope.insert(scope, locals);
      }
    }

    if self.should_visit_nested(stmt.hir_id, stmt.span) {
      intravisit::walk_stmt(self, stmt);
    }

    // Close the scope before inserting the final steps.
    invoke_internal!(self, close_scope, scope);

    self.insert_step_at_node_exit(stmt.hir_id);
  }

  fn visit_expr(&mut self, expr: &'tcx hir::Expr) {
    use hir::{ExprKind as EK, LoopSource, MatchSource, StmtKind as SK};
    match expr.kind {
      EK::If(cnd, then, else_opt) => {
        // For the generic case we can take the use the opening brace of each branch
        // target as the span.
        let mut entry_to_spans = HashMap::default();

        // Insert the location and span for the then branch
        if let Some(then_entry) = self.get_node_entry(then.hir_id) {
          let then_span = self.span_of(then.hir_id).shrink_to_lo();
          entry_to_spans.insert(then_entry, then_span);
        }

        // Insert the location and span for the else branch
        if let Some(els) = else_opt && let Some(else_entry) = self.get_node_entry(els.hir_id) {
        let else_span = self.span_of(els.hir_id).shrink_to_lo();
          entry_to_spans.insert(else_entry, else_span);
        }

        self.handle_expr_if(expr.hir_id, cnd, then, else_opt, entry_to_spans);
      }

      // HACK: Special cases for ForLoop and While desugarings.
      //
      // These special cases are needed to _adjust the spans_.
      // Example:
      // ```ignore
      // fn foo(mut s: String) {
      //   s.push_str("looping ")
      //   let b = &mut s;                // - Table 1 -
      //                                  // b: +R +W
      //                                  // s: -R -W -O
      //   while true { /* open */
      //     b.push_str("again... and ");
      //   } /* close */                  // - Table 2 -
      //                                  // b: -R -W
      //                                  // s: +R +W +O
      //   s.push_str("done!");
      //   println!("{s}");
      // }
      // ```
      // If we don't adjust for the desugaring, "Table 2" would
      // be placed on the line labeled "/* open */", but we want
      // it to actually get placed at the end of the loop where
      // it is depicted above. A similar adjustment is needed
      // for `for` loops.

      // While loops need to be detected with the surrounding loop.
      EK::Loop(
        hir::Block {
          stmts: [],
          expr:
            Some(hir::Expr {
              kind: EK::If(cnd, then, Some(els)),
              ..
            }),
          ..
        },
        _,
        LoopSource::While,
        _,
      ) => {
        // For the generic case we can take the use the opening brace of each branch
        // target as the span.
        let mut entry_to_spans = HashMap::default();

        // Insert the location and span for the then branch
        if let Some(then_entry) = self.get_node_entry(then.hir_id) {
          let then_span = self.span_of(then.hir_id).shrink_to_lo();
          entry_to_spans.insert(then_entry, then_span);
        }

        // Insert the location and span for the else branch
        if let Some(else_entry) = self.get_node_entry(els.hir_id) {
          // NOTE: we adjust the span of the break block to
          //       be _after_ the loop.
          let else_span = self.span_of(expr.hir_id).shrink_to_hi();
          entry_to_spans.insert(else_entry, else_span);
        }

        self.handle_expr_if(expr.hir_id, cnd, then, Some(els), entry_to_spans);
      }

      EK::Loop(
        hir::Block {
          stmts:
            [hir::Stmt {
              kind:
                SK::Expr(hir::Expr {
                  kind: EK::Match(cnd, arms @ [none, some], _),
                  ..
                }),
              ..
            }],
          expr: None,
          ..
        },
        _,
        LoopSource::ForLoop,
        _,
      ) => {
        let mut entry_to_spans = HashMap::default();

        let loop_start = self.span_of(some.body.hir_id).shrink_to_lo();
        let loop_end = self.span_of(expr.hir_id).shrink_to_hi();

        // Iterator::next => None, breaking out of the loop
        if let Some(none_entry) = self.get_node_entry(none.body.hir_id) {
          entry_to_spans.insert(none_entry, loop_end);
        }

        // Iterator::next => Some(_), execute loop body
        if let Some(some_entry) = self.get_node_entry(some.body.hir_id) {
          entry_to_spans.insert(some_entry, loop_start);
        }

        #[allow(clippy::needless_borrow)]
        self.handle_expr_match(expr.hir_id, cnd, &arms, entry_to_spans);
      }

      // NOTE: if a match condition doesn't produce a `switchInt`, there
      //       is no need to open a scope for this case. This most
      //       commonly happens when there is a single arm (common for desugarings)
      //       but it can also happen if future arms are elided. However, we
      //       still want to show the steps at the arm locations.
      EK::Match(cnd, [_], MatchSource::ForLoopDesugar)
        if !self.condition_produced_switchint(cnd) =>
      {
        log::debug!(
          "Match condition didn't produce switchInt {}",
          self.prettify_node(cnd.hir_id)
        );
        intravisit::walk_expr(self, expr);
      }

      // TODO this view of how a match branches is too simplistic, and
      //      doesn't accurately reflect reality. There could be many
      //      generated `switchInt`s or there could be none.
      //      Example:
      //      ```ignore
      //      match x {
      //        0 => 1,
      //        1 => 1,
      //        x => x,
      //      }
      //      ```
      //      the above match block would generate NO `switchInt`, just
      //      a series of `goto`s. Contrasted with something such as:
      //
      //      ```ignore
      //      match x {
      //        None => 1,
      //        Some(1) => 1,
      //        Some(x) => x,
      //      }
      //      ```
      //
      //      which will actually generate two `switchInt`s, one for the
      //      discriminant match and another for the inner integer check.
      //      These two cases are relatively simple, but branching for a
      //      generic match is complicated with the current internal API.
      //      What we would want, is automatic opening of a branch,
      //      this would make closing branches more difficult ...
      //      I'm(gavin) currently in thinking mode for this.
      EK::Match(cnd, arms, _) => {
        // This is the generic case and assumes no desugaring.
        // For the span we want to pick the END of the matched pattern,
        // but we choose the location as the entry to the arm body
        // (after all bound variables have been assigned).
        let entry_to_spans = arms
          .iter()
          .filter_map(|arm| {
            let id = arm.body.hir_id;
            self
              .get_node_entry(id)
              .map(|entry| (entry, self.span_of(arm.pat.hir_id).shrink_to_hi()))
          })
          .collect::<HashMap<_, _>>();

        self.handle_expr_match(expr.hir_id, cnd, arms, entry_to_spans);
      }
      _ => {
        intravisit::walk_expr(self, expr);
      }
    }
  }

  // NOTE: it's impotant that arms handle path hinting
  //
  // TODO: handle arm guards!
  fn visit_arm(&mut self, arm: &'tcx hir::Arm) {
    // We use the arm_entry for path hinting, because it's
    // closer the the `switchInt`.
    if let Some(arm_entry) = self.get_node_entry(arm.hir_id) {
      self.push_branch_start(arm_entry);

      // We get the entry of the arm body (or before the arm guard),
      // this is where any arm patterns will be initialized and bound.
      if let Some(entry) = self.get_node_entry(arm.body.hir_id) {
        let span = self.span_of(arm.hir_id).shrink_to_lo();
        invoke_internal!(self, insert, entry, self.get_path_hint(), span);
        self.visit_expr(arm.body);
        // self.insert_step_at_node_exit(arm.hir_id);
      } else {
        intravisit::walk_arm(self, arm);
      }

      self.pop_branch_start(arm_entry);
    } else {
      report_unexpected!(
        self,
        "match-arm doesn't have entry {}",
        self.prettify_node(arm.hir_id)
      );
    }
  }
}
