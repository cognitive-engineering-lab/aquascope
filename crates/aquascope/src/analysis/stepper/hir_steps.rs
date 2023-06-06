//! Experimental algorithm for creating permissions steps.
//!
//! It's main goals are to be:
//! 1. Easier to understand and describe than the previous.
//! 2. Disallow internal panics (something that causes UX problems).
//! 3. Successfuly analyze a larger set of Rust feautres. Loosening the
//!    restriction of multiple returns and infinite loops.

use anyhow::{anyhow, Result};
use rustc_data_structures::{self, fx::FxHashMap as HashMap};
use rustc_hir::{
  self as hir,
  intravisit::{self, Visitor as HirVisitor},
  HirId,
};
use rustc_middle::{
  hir::nested_filter,
  mir::{self, Local, Location},
};
use rustc_span::Span;
use rustc_utils::SpanExt;

use super::{segmented_mir::*, table_builder::*, *};
use crate::analysis::{
  ir_mapper::{GatherDepth, IRMapper},
  permissions::PermissionsCtxt,
};

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
    let body_hir_id = self.body_value_id();
    let body_span = self.span_of(body_hir_id);

    let mir_segments = self.mir_segments.freeze()?;

    let finalizer = TableBuilder {
      analysis,
      ctxt: &analysis.permissions,
      mir: &mir_segments,
      locals_at_scope: self.locals_at_scope,
    };

    Ok(finalizer.finalize_body(self.start_loc, body_span, mode))
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
