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

use super::{
  segmented_mir::{
    CollectionIdx, CollectionKind, ScopeIdx, SegmentedMir, BASE_SCOPE,
  },
  *,
};
use crate::{
  analysis::{
    ir_mapper::{GatherDepth, IRMapper},
    permissions::PermissionsCtxt,
  },
  errors,
};

type PseudoTable<'tcx> = HashMap<Place<'tcx>, PermissionsDataDiff>;
// type SpanTableMap<'tcx> = HashMap<Span, (MirSegment, PseudoTable<'tcx>)>;
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
  fatal_errors: Vec<Result<()>>,

  // Actual state of the analysis
  /// Entry location of the body under analysis.
  start_loc: Location,
  locals_at_scope: HashMap<ScopeIdx, Vec<Local>>,
  /// Stack of the current branch entry points, used
  /// for hinting path steps to the `SegmentedMir`.
  current_branch_start: Vec<Location>,
  mir_segments: SegmentedMir<'a, 'tcx>,
}

/// Makes calling functions on the SegmentedMir easier.
/// All functions on the SegmentedMIR could fail, but this
/// doesn't always mean we should stop.
/// This always abstracts away syntax in the case I want
/// to change how the internal failures are reported
/// in the future.
macro_rules! trap_ice {
  ($this:ident, $call:ident) => {
    trap_ice!($this, $call,)
  };
  ($this:ident, $call:ident, $($param:expr),*) => {
    $this.fatal_errors.push($this.mir_segments.$call($( $param ),*))
  };
}

macro_rules! report_unexpected {
  ($this:ident, $($param:expr),*) => {
    $this.fatal_errors.push(Err(anyhow!($( $param ),*)))
  }
}

impl<'a, 'tcx> HirPermissionStepper<'tcx> for HirStepPoints<'a, 'tcx> {
  fn get_unsupported_feature(&self) -> Option<&str> {
    self.unsupported_encounter.as_ref().map(|(_, s)| s.as_str())
  }

  fn get_internal_error(&self) -> Option<String> {
    self
      .fatal_errors
      .iter()
      .find(|r| r.is_err())
      .map(|r| r.as_ref().unwrap_err().to_string())
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

    let locals_attached_on = |scope: ScopeIdx| -> HashSet<Local> {
      let ps = mir_segments.parent_scopes(scope).collect::<Vec<_>>();
      log::debug!("scope {scope:?} paents: {ps:?}");
      ps.iter()
        .filter_map(|idx| self.locals_at_scope.get(idx))
        .flatten()
        .copied()
        .collect::<HashSet<_>>()
    };

    // Upon entry, the function parameters are already "live". But we want to
    // special case this, and show that they "come alive" at the opening brace.
    let first_diff = empty_domain.diff(first_domain);

    // Insert a segment into a table filtering defined places.
    let insert_segment =
      |result: &mut Tables<'tcx>, segment: MirSegment| -> ScopeIdx {
        let Some((span, scope)) = mir_segments.segment_data(segment) else {
        return *BASE_SCOPE;
      };

        let to_filter = locals_attached_on(scope);
        if segment.from != segment.to {
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
        scope
      };

    let inserted_collections = &mut HashSet::default();
    let mut insert_collection =
      |result: &mut Tables<'tcx>, idx: CollectionIdx| {
        // Only insert a collection once.
        if inserted_collections.contains(&idx) {
          return;
        }

        let Some(shape) = mir_segments.collection_data(idx) else {
        return;
      };
        inserted_collections.insert(idx);

        match &shape.kind {
          // TODO: for both types of collecitons, we need a start / end
          // location, as well as a scope so that we can find the attached
          // locals on a scope.

          // For linear collections of segments we can simply
          // insert them into the resulting table.
          CollectionKind::Linear { segments } => {
            for &segment in segments.iter() {
              insert_segment(result, segment);
            }
          }
          CollectionKind::Branch {
            root,
            phi,
            splits,
            middle,
            joins,
          } => {
            // The differences from the start of the condition to the end.
            let reach;
            let mut entire_diff: PseudoTable;
            if let Some(phi) = phi {
              let s = MirSegment::new(*root, *phi);
              reach = Some(s);
              entire_diff = s.into_diff(ctxt);
            } else {
              reach = None;
              entire_diff = HashMap::default();
            };

            let temp_middle: &mut Tables = &mut HashMap::default();
            let temp_joins: &mut Tables = &mut HashMap::default();

            let split_scopes = splits
              .iter()
              .map(|&segment| insert_segment(result, segment));

            let middle_scopes = middle
              .iter()
              .map(|&segment| insert_segment(temp_middle, segment));

            let join_scopes = joins
              .iter()
              .map(|&segment| insert_segment(temp_joins, segment));

            // TODO: the scoped attached locals still don't appear
            //       for if-lets and join segments aren't filtered
            //       if they appear within a different branch.

            let all_scopes =
              split_scopes.chain(middle_scopes).chain(join_scopes);
            let all_attached = all_scopes
              .flat_map(locals_attached_on)
              .collect::<HashSet<_>>();

            let attached_here = entire_diff
              .drain_filter(|place: &Place, _| {
                all_attached.contains(&place.local)
              })
              .collect::<PseudoTable>();

            log::debug!("Locals attached to collection: {attached_here:#?}");

            macro_rules! insert {
              ($m1:expr => into $m2:expr) => {
                for (&span, ref mut vs) in $m1.into_iter() {
                  $m2.entry(span).or_default().append(vs);
                }
              };
            }

            insert!(temp_middle => into result);
            insert!(temp_joins => into result);

            if let Some(reach) = reach {
              result
                .entry(rustc_span::DUMMY_SP)
                .or_default()
                .push((reach, attached_here));
            }
          }
        }
      };

    let mut diffs = HashMap::default();

    diffs.insert(body_open_brace, vec![(
      MirSegment::new(self.start_loc, self.start_loc),
      first_diff,
    )]);

    for &c in mir_segments.collections() {
      insert_collection(&mut diffs, c);
    }

    Ok(prettify_permission_steps(analysis, diffs, mode))
  }
}

impl<'a, 'tcx: 'a> HirStepPoints<'a, 'tcx> {
  pub(super) fn make(
    ctxt: &'a PermissionsCtxt<'a, 'tcx>,
    ir_mapper: &'a IRMapper<'a, 'tcx>,
  ) -> Result<Self> {
    let mir_segments = SegmentedMir::make(ir_mapper);
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

  // fn report_unsupported(&mut self, id: HirId, msg: &str) {
  //   if self.unsupported_encounter.is_none() {
  //     let span = self.span_of(id);
  //     self.unsupported_encounter = Some((span, String::from(msg)));
  //   }
  // }

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

    // XXX: shift the exit to the next successor if available.
    //      this way we capture the state changes for a single
    //      operation rather than having an off by one.
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

  fn exprs_to_entry_spans(
    &self,
    exprs: &[Option<&hir::Expr>],
  ) -> HashMap<Location, Span> {
    exprs
      .iter()
      .filter_map(|expr| {
        expr.and_then(|expr| {
          let id = expr.hir_id;
          self
            .get_node_entry(id)
            .map(|entry| (entry, self.span_of(id).shrink_to_lo()))
        })
      })
      .collect::<HashMap<_, _>>()
  }
}

impl<'a, 'tcx: 'a> HirVisitor<'tcx> for HirStepPoints<'a, 'tcx> {
  type NestedFilter = nested_filter::All;

  fn nested_visit_map(&mut self) -> Self::Map {
    self.ctxt.tcx.hir()
  }

  fn visit_body(&mut self, body: &'tcx hir::Body) {
    intravisit::walk_body(self, body);
  }

  fn visit_block(&mut self, block: &'tcx hir::Block) {
    let scope = self.mir_segments.open_scope();
    for stmt in block.stmts.iter() {
      self.visit_stmt(stmt);
    }

    if let Some(expr) = block.expr {
      log::debug!("BLOCK contains final EXPR");
      self.visit_expr(expr);
      if let Some(exit) = self.get_node_exit(expr.hir_id) {
        trap_ice!(
          self,
          insert,
          exit,
          self.get_path_hint(),
          self.span_of(expr.hir_id)
        );
      }
    }
    trap_ice!(self, close_scope, scope);
  }

  fn visit_stmt(&mut self, stmt: &'tcx hir::Stmt) {
    use rustc_hir::StmtKind as SK;

    log::debug!(
      "Starting analysis of STMT {}\n",
      self.prettify_node(stmt.hir_id),
    );

    let scope = self.mir_segments.open_scope();

    // TODO: have a way to put locally assigned places in a hierarchy so
    //       we can easily filter out the nested assignments.
    let locals = match stmt.kind {
      SK::Local(local) => {
        let places = self.ir_mapper.local_assigned_place(local);
        places.into_iter().map(|p| p.local).collect::<Vec<_>>()
      }
      _ => vec![],
    };

    if !locals.is_empty() {
      log::debug!("storing locals at scope {scope:?} {locals:?}");
      self.locals_at_scope.insert(scope, locals);
    }

    if self.should_visit_nested(stmt.hir_id, stmt.span) {
      intravisit::walk_stmt(self, stmt);
    }

    // Close the scope before inserting the final steps.
    trap_ice!(self, close_scope, scope);

    if let Some(exit_loc) = self.get_node_exit(stmt.hir_id) {
      trap_ice!(
        self,
        insert,
        exit_loc,
        self.get_path_hint(),
        self.span_of(stmt.hir_id)
      );
    } else {
      log::warn!("STMT doesn't have single exit");
    }
  }

  fn visit_expr(&mut self, expr: &'tcx hir::Expr) {
    use hir::ExprKind as EK;

    log::debug!("visiting EXPR : {}", self.prettify_node(expr.hir_id));

    match expr.kind {
      EK::If(cnd, then, else_opt) => {
        // NOTE: first we need to walk and split the condition. In the
        // case of a more complex condition expression, splitting this
        // first will result in a split location closest to the `SwitchInt`.
        self.visit_expr(cnd);
        let cnd_exit_opt = self.get_node_exit(cnd.hir_id);

        if cnd_exit_opt.is_none() {
          log::warn!(
            "Skipping EXPR, IF condition has no exit {}",
            self.prettify_node(cnd.hir_id)
          );

          return;
        }

        let cnd_exit = cnd_exit_opt.unwrap();

        trap_ice!(
          self,
          insert,
          cnd_exit,
          self.get_path_hint(),
          self.span_of(cnd.hir_id)
        );

        let entry_to_spans = self.exprs_to_entry_spans(&[Some(then), else_opt]);
        trap_ice!(self, open_branch, cnd_exit, |MirSegment { to, .. }| {
          entry_to_spans
            .iter()
            .find_map(|(&l, &span)| {
              self.ir_mapper.dominates(to, l).then_some(span)
            })
            .unwrap_or_default()
        });

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

        if self.mir_segments.is_branch_open(cnd_exit) {
          log::warn!(
            "branch still open, flushing and closing steps:\n{}",
            self.prettify_node(expr.hir_id)
          );

          let ss = self.span_of(expr.hir_id).shrink_to_hi();
          trap_ice!(self, close_branch, cnd_exit, |_| ss);
        }
      }

      // TODO: uncomment and implement match
      // - MATCH
      // - For / While (I don't want to special case these but it may be necessary)

      //   EK::Match(swtch, arms, _source) => {
      //     // NOTE: first we need to walk and split the condition. In the
      //     // case of a more complex condition expression, splitting this
      //     // first will result in a split location closest to the `SwitchInt`.
      //     self.visit_expr(swtch);
      //     for arm in arms.iter() {
      //       self.visit_arm(arm);
      //     }

      //   }
      _ => {
        intravisit::walk_expr(self, expr);
      }
    }
  }
}
