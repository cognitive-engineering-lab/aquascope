use std::{
  cmp::Ordering,
  collections::hash_map::Entry,
  ops::{Deref, DerefMut},
};

use flowistry::mir::utils::{BodyExt, PlaceExt as FlowistryPlaceExt};
use itertools::Itertools;
use rustc_data_structures::fx::FxHashMap as HashMap;
use rustc_hir::{
  self as hir,
  intravisit::{self, Visitor as HirVisitor},
  HirId,
};
use rustc_middle::{
  hir::nested_filter,
  mir::{self, Body, Local, Location, Place},
  ty::TyCtxt,
};
use rustc_mir_dataflow::{Analysis, JoinSemiLattice, ResultsVisitor};
use rustc_span::Span;

use crate::{
  analysis::{
    ir_mapper::{GatherMode, IRMapper},
    permissions::{
      utils::{flow_mir_permissions, PAnalysis},
      Difference, PermissionsCtxt, PermissionsDomain, PermissionsStateStep,
      PermsDiff,
    },
  },
  mir::utils::PlaceExt as AquascopePlaceExt,
  Range,
};

pub fn compute_permission_steps<'a, 'tcx>(
  ctxt: &PermissionsCtxt<'a, 'tcx>,
  span_to_range: impl Fn(Span) -> Range,
) -> Vec<PermissionsStateStep>
where
  'tcx: 'a,
{
  let results = flow_mir_permissions(ctxt);
  let tcx = ctxt.tcx;
  let body = &ctxt.body_with_facts.body;
  let basic_blocks = body.basic_blocks.indices();

  // FIXME REMOVE
  let mut stderr = std::io::stderr();
  rustc_middle::mir::pretty::write_mir_fn(
    tcx,
    body,
    &mut |_, _| Ok(()),
    &mut stderr,
  );

  let ir_mapper =
    &IRMapper::new(tcx, body, ctxt.body_id, GatherMode::IgnoreCleanup);

  let mut hir_visitor = HirPermDiffFlow {
    ctxt,
    ir_mapper,
    mode: SaveDiffs::Yes,
    diff: HashMap::default(),
    entry_state: ctxt.initial_body_permissions().into(),
  };

  log::debug!("HIR IDS:");

  hir_visitor.visit_nested_body(ctxt.body_id);

  // TODO: this is not correct!
  let interim = hir_visitor
    .diff
    .into_iter()
    .filter_map(|(id, places_to_perms)| {
      let filtered = places_to_perms
        .into_iter()
        .filter(|(place, diff)| {
          place.is_source_visible(tcx, body) && !diff.is_empty()
        })
        .collect::<HashMap<_, _>>();

      (!filtered.is_empty()).then_some((id, filtered))
    })
    .collect::<HashMap<_, _>>();

  prettify_permission_steps(ctxt, interim, span_to_range)
}

fn prettify_permission_steps<'tcx>(
  ctxt: &PermissionsCtxt<'_, 'tcx>,
  perm_steps: HashMap<HirId, HashMap<Place<'tcx>, PermsDiff>>,
  span_to_range: impl Fn(Span) -> Range,
) -> Vec<PermissionsStateStep> {
  let tcx = ctxt.tcx;
  let hir = tcx.hir();
  let body = &ctxt.body_with_facts.body;

  macro_rules! place_to_string {
    ($p:expr) => {
      $p.to_string(tcx, body)
        .unwrap_or_else(|| String::from("<var>"))
    };
  }

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

#[allow(type_alias_bounds)]
type DomainAtHir<'tcx, A: Analysis<'tcx>> =
  HashMap<HirId, HashMap<Location, A::Domain>>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum SaveDiffs {
  No,
  Yes,
}

struct HirPermDiffFlow<'a, 'tcx> {
  ctxt: &'a PermissionsCtxt<'a, 'tcx>,
  ir_mapper: &'a IRMapper<'a, 'tcx>,
  mode: SaveDiffs,
  diff: HashMap<HirId, HashMap<Place<'tcx>, PermsDiff>>,
  entry_state: PermissionsDomain<'tcx>,
}

impl<'a, 'tcx: 'a> HirVisitor<'tcx> for HirPermDiffFlow<'a, 'tcx> {
  type NestedFilter = nested_filter::All;

  fn nested_visit_map(&mut self) -> Self::Map {
    self.ctxt.tcx.hir()
  }

  // fn visit_expr(&mut self, expr: &'tcx hir::Expr) {
  //   use hir::{ExprKind as EK, LoopSource, MatchSource, StmtKind as SK};

  //   let id = expr.hir_id;
  //   let body = &self.body;

  //   macro_rules! walk_and_add_pending {
  //     ($e:expr, $id:expr) => {
  //       intravisit::walk_expr(self, $e);
  //       self.stage_bundle($id);
  //       self.commit_bundle($id);
  //     };
  //   }

  //   match expr.kind {
  //     // EK::If(cnd, then, else_opt) => {
  //     //   self.visit_expr(cnd);
  //     //   let branch_before = self.entry_perm_state.clone();
  //     //   self.visit_expr(then);
  //     //   let mut after_if_state = self.entry_perm_state.clone();

  //     //   if let Some(els) = else_opt {
  //     //     self.entry_perm_state = branch_before;
  //     //     self.visit_expr(els);
  //     //     let after_else_state = self.entry_perm_state.clone();
  //     //     after_if_state.join(&after_else_state);
  //     //   }

  //     //   self.entry_perm_state = after_if_state;
  //     //   self.save_for_later(expr.hir_id);
  //     // }

  //     // TODO: special variants to handle control flow
  //     // Match(&'hir Expr<'hir>, &'hir [Arm<'hir>], MatchSource),

  //     // In the AST -> HIR desugaring step, for/while loops get desugared
  //     // into a plain `loop` + `match` construct. Fundamentally, we want to
  //     // break this up into:
  //     // - loop prelude, setting up the iterator.
  //     // - block prelude, setting up variables visible inside the loop body.
  //     // - block postlude, destroying the block visible variables.
  //     // - loop postlude, destroying the loop iterator variables.
  //     //
  //     // We match loops at the expression level because they
  //     // may not ever appear in a Stmt.

  //     //   EK::Loop(
  //     //     hir::Block {
  //     //       stmts: &[],
  //     //       expr:
  //     //         Some(hir::Expr {
  //     //           kind: EK::If(_, _, _),
  //     //           ..
  //     //         }),
  //     //       ..
  //     //     },
  //     //     _,
  //     //     LoopSource::While,
  //     //     _,
  //     //   ) => log::debug!("WHILE LOOP:"),

  //     // EK::Loop(
  //     //   hir::Block {
  //     //     stmts:
  //     //       &[hir::Stmt {
  //     //         kind:
  //     //           SK::Expr(hir::Expr {
  //     //             kind:
  //     //               EK::Match(
  //     //                 switch_expr,
  //     //                 &[hir::Arm {
  //     //                   body: none_expr, ..
  //     //                 }, hir::Arm {
  //     //                   body: some_expr, ..
  //     //                 }],
  //     //                 MatchSource::ForLoopDesugar,
  //     //               ),
  //     //             ..
  //     //           }),
  //     //         ..
  //     //       }],
  //     //     expr: None,
  //     //     ..
  //     //   },
  //     //   _,
  //     //   LoopSource::ForLoop,
  //     //   _,
  //     // ) => {
  //     //   log::debug!("FOR LOOP:");
  //     //   let previous_lift_mode = self.lift_mode;

  //     //   self.visit_expr(switch_expr);

  //     //   let before_arms = self.entry_perm_state.clone();

  //     //   self.lift_mode = LiftBound::Expr;

  //     //   self.visit_expr(some_expr);
  //     //   let after_body = self.entry_perm_state.clone();

  //     //   self.visit_expr(none_expr);
  //     //   let after_break = self.entry_perm_state.clone();

  //     //   self.insert_with_id(switch_expr.hir_id);
  //     //   self.insert_with_id(some_expr.hir_id);
  //     //   self.insert_with_id(none_expr.hir_id);

  //     //   self.lift_mode = previous_lift_mode;
  //     //   self.save_for_later(expr.hir_id);
  //     // }

  //     //   EK::Loop(_, _, LoopSource::Loop, _) => {
  //     //     unimplemented!("implement plain loop desugaring")
  //     //   }
  //     //   EK::Loop(_, _, _, _) => {
  //     //     unimplemented!("Loop desugaring uncovered {expr:#?}")
  //     //   }

  //     //
  //     // - Variants I'd need to think more about.
  //     //
  //     // Closure(&'hir Closure<'hir>),
  //     // Call(&'hir Expr<'hir>, &'hir [Expr<'hir>]),
  //     // MethodCall(&'hir PathSegment<'hir>, &'hir Expr<'hir>, &'hir [Expr<'hir>], Span),
  //     // Struct(&'hir QPath<'hir>, &'hir [ExprField<'hir>], Option<&'hir Expr<'hir>>),
  //     //
  //     // TODO: I'd be surprised if someone were using inline asm but
  //     // we should at least  crash with a "feature not supported" or something.
  //     // InlineAsm(&'hir InlineAsm<'hir>),
  //     _ => {
  //       walk_and_add_pending!(expr, expr.hir_id);
  //     }
  //   }
  // }

  // fn visit_expr(&mut self, expr: &'tcx hir::Expr) {
  //   use hir::{ExprKind as EK, StmtKind as SK};
  //   let id = expr.hir_id;
  //   let body = &self.ctxt.body_with_facts.body;

  //   let nls = self.ir_mapper.get_mir_locations(id).unwrap();

  //   match nls.outer.last() {
  //     Some(loc) => {
  //       let point = self.ctxt.location_to_point(*loc);
  //       let dmn: PermissionsDomain<'tcx> =
  //         self.ctxt.permissions_domain_at_point(point);
  //       let entry_state: &PermissionsDomain<'tcx> = &self.entry_state;
  //       let diff = entry_state.diff(&dmn);
  //       self.diff.insert(id, diff);
  //       self.entry_state = dmn;
  //     }
  //     None => {
  //       intravisit::walk_expr(self, expr);
  //     }
  //   }
  // }

  // The strategy for Locals of the form:
  //
  // Δ: the current permissions context
  // S: represents the set of permission changes.
  //
  // Δ;S :- <init>  : S'   => Δ'
  // Δ;S :- <block> : S''  => Δ''
  // unify(S', S'') : S'''
  // join(Δ', Δ'') : Δ'''
  // -------------------
  // Δ;S :- let <path> = <init> else { <block> } : S''' => Δ'''
  fn visit_local(&mut self, local: &'tcx hir::Local) {
    if local.els.is_some() {
      unimplemented!("TODO: hir::Local with else");
    }
    let id = local.hir_id;
    let body = &self.ctxt.body_with_facts.body;
    let hir = self.ctxt.tcx.hir();

    let nls = self.ir_mapper.get_mir_locations(id).unwrap();

    log::debug!("Visiting: {}", hir.node_to_string(id));

    if let Some(init) = local.init {
      log::debug!("Outer nodes {:?}", nls.outer);
      if let Some(loc) = nls.outer.last() {
        let point = self.ctxt.location_to_point(*loc);
        let dmn = self
          .ctxt
          .permissions_domain_after_point_effect(point)
          .unwrap_or_else(|| self.ctxt.permissions_domain_at_point(point));
        let entry_state = &self.entry_state;
        let diff = entry_state.diff(&dmn);
        self.diff.insert(id, diff);
        self.entry_state = dmn;
      }
    }
  }
}
