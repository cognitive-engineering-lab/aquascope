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
use rustc_mir_dataflow::{Analysis, JoinSemiLattice};
use rustc_span::Span;

use crate::{
  analysis::{
    ir_mapper::{GatherMode, IRMapper},
    permissions::{
      Difference, Permissions, PermissionsCtxt, PermissionsData,
      PermissionsDataDiff, PermissionsDiff, PermissionsDomain,
      PermissionsStateStep,
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
    diff: HashMap::default(),
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
  perm_steps: HashMap<HirId, HashMap<Place<'tcx>, PermissionsDataDiff>>,
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
    .filter_map(|(id, place_to_diffs)| {
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

      Some(PermissionsStateStep {
        location: range,
        state,
      })
    })
    .collect::<Vec<_>>()
}

#[allow(type_alias_bounds)]
type DomainAtHir<'tcx, A: Analysis<'tcx>> =
  HashMap<HirId, HashMap<Location, A::Domain>>;

struct HirPermDiffFlow<'a, 'tcx> {
  ctxt: &'a PermissionsCtxt<'a, 'tcx>,
  ir_mapper: &'a IRMapper<'a, 'tcx>,
  diff: HashMap<HirId, HashMap<Place<'tcx>, PermissionsDataDiff>>,
}

// FIXME: you can simplify this and you know if.
macro_rules! insert_from_to {
  ($this:ident, $id:expr) => {
    if let Some(mir_order) = $this.ir_mapper.get_mir_locations($id) {
      let (loc1, loc2) = mir_order.get_entry_exit_locations();
      let before_point = $this.ctxt.location_to_point(loc1);
      let after_point = $this.ctxt.location_to_point(loc2);
      let dmn_before = &$this.ctxt.permissions_domain_at_point(before_point);
      let dmn_after = &$this
        .ctxt
        .permissions_domain_after_point_effect(after_point)
        .unwrap_or_else(|| $this.ctxt.permissions_domain_at_point(after_point));
      let diff = dmn_before.diff(dmn_after);
      $this.diff.insert($id, diff);
    }
  };
  ($this:ident, before $id1:expr, after $id2:expr) => {
    if let Some(first_order) = $this.ir_mapper.get_mir_locations($id1) {
      if let Some(second_order) = $this.ir_mapper.get_mir_locations($id2) {
        let (loc1, _) = first_order.get_entry_exit_locations();
        let (_, loc2) = second_order.get_entry_exit_locations();
        let before_point = $this.ctxt.location_to_point(loc1);
        let after_point = $this.ctxt.location_to_point(loc2);
        let dmn_before = &$this.ctxt.permissions_domain_at_point(before_point);
        let dmn_after = &$this
          .ctxt
          .permissions_domain_after_point_effect(after_point)
          .unwrap_or_else(|| {
            $this.ctxt.permissions_domain_at_point(after_point)
          });
        let diff = dmn_before.diff(dmn_after);
        $this.diff.insert($id2, diff);
      }
    }
  };
  ($this:ident, after $id1:expr, after $id2:expr) => {
    if let Some(first_order) = $this.ir_mapper.get_mir_locations($id1) {
      if let Some(second_order) = $this.ir_mapper.get_mir_locations($id2) {
        let (_, loc1) = first_order.get_entry_exit_locations();
        let (_, loc2) = second_order.get_entry_exit_locations();
        let before_point = $this.ctxt.location_to_point(loc1);
        let after_point = $this.ctxt.location_to_point(loc2);
        let dmn_before = &$this
          .ctxt
          .permissions_domain_after_point_effect(before_point)
          .unwrap_or_else(|| {
            $this.ctxt.permissions_domain_at_point(before_point)
          });
        let dmn_after = &$this
          .ctxt
          .permissions_domain_after_point_effect(after_point)
          .unwrap_or_else(|| {
            $this.ctxt.permissions_domain_at_point(after_point)
          });
        let diff = dmn_before.diff(dmn_after);
        $this.diff.insert($id2, diff);
      }
    }
  };
}

impl<'a, 'tcx: 'a> HirVisitor<'tcx> for HirPermDiffFlow<'a, 'tcx> {
  type NestedFilter = nested_filter::All;

  fn nested_visit_map(&mut self) -> Self::Map {
    self.ctxt.tcx.hir()
  }

  fn visit_body(&mut self, body: &'tcx hir::Body) {
    if body.generator_kind.is_some() {
      unimplemented!("generators unimplemented");
    }
    log::debug!("Visiting Body");
    self.visit_expr(body.value);
  }

  fn visit_stmt(&mut self, stmt: &'tcx hir::Stmt) {
    use hir::StmtKind as SK;
    let id = stmt.hir_id;
    let hir = self.ctxt.tcx.hir();
    log::debug!("Visiting Stmt: {}", hir.node_to_string(id));

    match stmt.kind {
      SK::Item(_) => {
        unimplemented!("we shouldn't need to do anything for hir::Stmt::Item")
      }
      SK::Local(local) => {
        self.visit_local(local);
        insert_from_to!(self, id);
      }
      SK::Expr(expr) => {
        self.visit_expr(expr);
        insert_from_to!(self, id);
      }
      SK::Semi(expr) => {
        self.visit_expr(expr);
        insert_from_to!(self, id);
      }
    }
  }

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
    log::debug!("Visiting Local: {}", hir.node_to_string(id));
    if let Some(init) = local.init {
      self.visit_expr(init);
    }
    insert_from_to!(self, id);
  }

  fn visit_block(&mut self, block: &'tcx hir::Block) {
    let id = block.hir_id;
    let hir = self.ctxt.tcx.hir();
    log::debug!("Visiting Block: {}", hir.node_to_string(id));

    let mut last_id = id;
    block.stmts.iter().for_each(|stmt| {
      last_id = stmt.hir_id;
      self.visit_stmt(stmt);
    });

    if let Some(expr) = block.expr {
      last_id = expr.hir_id;
      self.visit_expr(expr);
    }
    // Instead of taking the starting / ending positions of the entire block,
    // we'll take the difference between the ending position of the last
    // executed Node, and the end of the block.
    if let Some(block_mir_order) = self.ir_mapper.get_mir_locations(id) {
      if let Some(last_mir_order) = self.ir_mapper.get_mir_locations(id) {
        let (_, loc2) = block_mir_order.get_entry_exit_locations();
        let (_, loc1) = last_mir_order.get_entry_exit_locations();
        let before_point = self.ctxt.location_to_point(loc1);
        let after_point = self.ctxt.location_to_point(loc2);
        let dmn_before = &self
          .ctxt
          .permissions_domain_after_point_effect(before_point)
          .unwrap_or_else(|| {
            self.ctxt.permissions_domain_at_point(before_point)
          });
        let dmn_after = &self
          .ctxt
          .permissions_domain_after_point_effect(after_point)
          .unwrap_or_else(|| {
            self.ctxt.permissions_domain_at_point(after_point)
          });
        let diff = dmn_before.diff(dmn_after);
        self.diff.insert(id, diff);
      }
    }
  }

  // TODO: permission differences are added randomly to make some
  // small examples I have work, but we need a principled way to talk
  // about the different changes for a given HIR Node.
  // I (gavin) think this should include some notion of "all the
  // MIR statements nested under".
  fn visit_expr(&mut self, expr: &'tcx hir::Expr) {
    use hir::{ExprKind as EK, LoopSource, MatchSource, StmtKind as SK};

    let id = expr.hir_id;
    let hir = self.ctxt.tcx.hir();
    log::debug!("Visiting Expr: {}", hir.node_to_string(id));

    match expr.kind {
      EK::Path(_) | EK::Lit(_) => (),

      EK::DropTemps(expr) => {
        self.visit_expr(expr);
      }

      EK::Binary(_, lhs, rhs) => {
        self.visit_expr(lhs);
        self.visit_expr(rhs);
      }

      EK::Unary(_, expr) => {
        self.visit_expr(expr);
      }

      EK::Assign(lhs, rhs, _) => {
        self.visit_expr(rhs);
        self.visit_expr(lhs);
        insert_from_to!(self, id);
      }

      //
      EK::Block(block, _) => {
        self.visit_block(block);
      }

      EK::AddrOf(_, _, expr) => {
        self.visit_expr(expr);
      }

      //
      EK::Call(func, args) => {
        args.iter().for_each(|p| {
          self.visit_expr(p);
        });
        self.visit_expr(func);
        insert_from_to!(self, id);
      }

      EK::MethodCall(_, rcv, args, _) => {
        args.iter().for_each(|p| {
          self.visit_expr(p);
        });
        self.visit_expr(rcv);
        insert_from_to!(self, id);
      }

      //
      EK::Ret(expr_opt) => {
        if let Some(expr) = expr_opt {
          self.visit_expr(expr);
        }
      }

      EK::If(cnd, then, else_opt) => {
        self.visit_expr(cnd);
        self.visit_expr(then);
        if let Some(els) = else_opt {
          self.visit_expr(els);
        }
      }

      // TODO: special variants to handle control flow
      // Match(&'hir Expr<'hir>, &'hir [Arm<'hir>], MatchSource),

      // In the AST -> HIR desugaring step, for/while loops get desugared
      // into a plain `loop` + `match` construct. Fundamentally, we want to
      // break this up into:
      // - loop prelude, setting up the iterator.
      // - block prelude, setting up variables visible inside the loop body.
      // - block postlude, destroying the block visible variables.
      // - loop postlude, destroying the loop iterator variables.
      //
      // We match loops at the expression level because they
      // may not ever appear in a Stmt.

      //   EK::Loop(
      //     hir::Block {
      //       stmts: &[],
      //       expr:
      //         Some(hir::Expr {
      //           kind: EK::If(_, _, _),
      //           ..
      //         }),
      //       ..
      //     },
      //     _,
      //     LoopSource::While,
      //     _,
      //   ) => log::debug!("WHILE LOOP:"),

      // EK::Loop(
      //   hir::Block {
      //     stmts:
      //       &[hir::Stmt {
      //         kind:
      //           SK::Expr(hir::Expr {
      //             kind:
      //               EK::Match(
      //                 switch_expr,
      //                 &[hir::Arm {
      //                   body: none_expr, ..
      //                 }, hir::Arm {
      //                   body: some_expr, ..
      //                 }],
      //                 MatchSource::ForLoopDesugar,
      //               ),
      //             ..
      //           }),
      //         ..
      //       }],
      //     expr: None,
      //     ..
      //   },
      //   _,
      //   LoopSource::ForLoop,
      //   _,
      // ) => unimplemented!(),

      //   EK::Loop(_, _, LoopSource::Loop, _) => {
      //     unimplemented!("implement plain loop desugaring")
      //   }
      //   EK::Loop(_, _, _, _) => {
      //     unimplemented!("Loop desugaring uncovered {expr:#?}")
      //   }

      //
      // - Variants I'd need to think more about.
      //
      // Closure(&'hir Closure<'hir>),
      // Call(&'hir Expr<'hir>, &'hir [Expr<'hir>]),
      // MethodCall(&'hir PathSegment<'hir>, &'hir Expr<'hir>, &'hir [Expr<'hir>], Span),
      // Struct(&'hir QPath<'hir>, &'hir [ExprField<'hir>], Option<&'hir Expr<'hir>>),
      //
      // InlineAsm(&'hir InlineAsm<'hir>),
      _ => unimplemented!("expr {expr:?} unimplemented"),
    }
  }
}
