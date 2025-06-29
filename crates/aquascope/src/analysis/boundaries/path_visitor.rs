//! Visitor to calculate expected permissions for path usages.

use anyhow::{bail, Result};
use fluid_let::{fluid_let, fluid_set};
use rustc_hir::{
  def::Res,
  intravisit::{self, Visitor},
  Block, Body, Expr, ExprKind, HirId, Path, QPath, Stmt, UnOp,
};
use rustc_middle::{
  hir::nested_filter::OnlyBodies,
  ty::{
    adjustment::{Adjust, AutoBorrow},
    TyCtxt, TypeckResults, TypingEnv,
  },
};
use rustc_span::Span;
use rustc_utils::TyExt;

use super::{ExpectedPermissions, PathBoundary};
use crate::analysis::permissions::PermissionsCtxt;

// The current region flow context for outer statements and returns.
fluid_let!(pub static FLOW_CONTEXT: HirId);

struct HirExprScraper<'tcx> {
  tcx: TyCtxt<'tcx>,
  typeck_res: &'tcx TypeckResults<'tcx>,
  typing_env: TypingEnv<'tcx>,
  data: Vec<PathBoundary>,
  unsupported_feature: Option<(Span, String)>,
}

impl HirExprScraper<'_> {
  fn get_adjusted_permissions(&self, expr: &Expr) -> ExpectedPermissions {
    let ty_adj = self.typeck_res.expr_ty_adjusted(expr);
    let adjs = self.typeck_res.expr_adjustments(expr);

    log::debug!("Path TY-ADJ: {:#?} from {:#?}", ty_adj, adjs);

    let is_auto_borrow = adjs.iter().find_map(|adj| {
      if let Adjust::Borrow(AutoBorrow::Ref(m)) = adj.kind {
        Some(m)
      } else {
        None
      }
    });

    if let Some(mutability) = is_auto_borrow {
      return ExpectedPermissions::from_reborrow(mutability);
    }

    // At this point the usage is either a move or a copy. We
    // can determine this whether or not the type of the path
    // is copyable or not.
    if ty_adj.is_copyable(self.tcx, self.typing_env) {
      ExpectedPermissions::from_copy()
    } else {
      ExpectedPermissions::from_move()
    }
  }
}

impl<'tcx> Visitor<'tcx> for HirExprScraper<'tcx> {
  type NestedFilter = OnlyBodies;

  fn nested_visit_map(&mut self) -> Self::Map {
    self.tcx.hir()
  }

  // Visiting statements / body is only used for specifying a
  // region flow context. This would not be used for RWO
  // path boundaries.
  fn visit_body(&mut self, body: &Body<'tcx>) {
    fluid_set!(FLOW_CONTEXT, &body.value.hir_id);
    intravisit::walk_body(self, body);
  }

  fn visit_stmt(&mut self, stmt: &'tcx Stmt<'tcx>) {
    fluid_set!(FLOW_CONTEXT, &stmt.hir_id);
    intravisit::walk_stmt(self, stmt);
  }

  fn visit_block(&mut self, block: &'tcx Block) {
    for stmt in block.stmts.iter() {
      self.visit_stmt(stmt);
    }

    if let Some(expr) = block.expr {
      fluid_set!(FLOW_CONTEXT, expr.hir_id);
      self.visit_expr(expr);
    }
  }

  fn visit_expr(&mut self, expr: &'tcx Expr) {
    let hir_id = expr.hir_id;
    let flow_context = FLOW_CONTEXT.copied().unwrap_or(hir_id);

    log::debug!(
      "visiting {}\n\n",
      self.nested_visit_map().node_to_string(hir_id)
    );

    match expr.kind {
      // Method calls are a form of type-deref coercion which can
      // rely on the adjusted permissions rather than needing to
      // inspect the function signature.
      ExprKind::Match(discr, arms, _)
        if discr.is_place_expr(|e| !matches!(e.kind, ExprKind::Lit(_))) =>
      {
        let expected = ExpectedPermissions::from_discriminant();
        let pb = PathBoundary {
          location: discr.span,
          hir_id: discr.hir_id,
          flow_context,
          is_lhs: false,
          expected,
        };

        self.data.push(pb);

        for a in arms.iter() {
          self.visit_arm(a);
        }
      }

      ExprKind::MethodCall(_, rcvr, args, fn_span)
        if !fn_span.from_expansion()
          && rcvr.is_place_expr(|e| !matches!(e.kind, ExprKind::Lit(_))) =>
      {
        let expected = self.get_adjusted_permissions(rcvr);
        let pb = PathBoundary {
          location: rcvr.span,
          hir_id: rcvr.hir_id,
          flow_context,
          is_lhs: false,
          expected,
        };

        self.data.push(pb);

        for a in args.iter() {
          self.visit_expr(a);
        }
      }

      ExprKind::AddrOf(_, mutability, inner)
        if inner.is_syntactic_place_expr() && !inner.span.from_expansion() =>
      {
        // We don't have to account for adjusted types because
        // taking a borrow provides explicit types.
        let pb = PathBoundary {
          hir_id,
          flow_context,
          is_lhs: false,
          location: inner.span.shrink_to_lo(),
          expected: ExpectedPermissions::from_borrow(mutability),
        };

        self.data.push(pb);
      }

      // If the left-hand-side of the assignment is a deref, then we would
      // expect both read and write permissions from the path. We can say that
      // here because in Rust you cannot have a reference to uninitialized memory,
      // however, if the LHS is a moveable path (no deref) then it *could* be uninitialized.
      ExprKind::Assign(
        lhs @ Expr {
          kind: ExprKind::Unary(UnOp::Deref, _),
          ..
        },
        rhs,
        _,
      ) => {
        let pb = PathBoundary {
          location: lhs.span.shrink_to_lo(),
          hir_id: lhs.hir_id,
          flow_context,
          is_lhs: true,
          expected: ExpectedPermissions::from_assignment(),
        };
        self.data.push(pb);
        self.visit_expr(rhs);
      }

      // It feels natural to say that the LHS of an assignment
      // should expect W permissions. However, this isn't always the case.
      // It's true that the path should be declared as *Mutable*, but
      // this doesn't mean that there's write permissions. Example:
      //
      // ```text
      // let s;
      // s = "all good".to_string();
      // ```
      //
      // `s` would not have write permissions because it is not yet initialized.
      // For now, the LHS is simply ignored from the boundaries analysis.
      ExprKind::Assign(lhs, rhs, _) => {
        log::debug!("ASSIGN: ignoring LHS {lhs:#?}");
        self.visit_expr(rhs);
      }

      ExprKind::AssignOp(_, lhs, rhs) => {
        let lhs_ty = self.typeck_res.expr_ty_adjusted(lhs);
        log::debug!("Type of LHS: {:#?}", lhs_ty);

        let pb = PathBoundary {
          location: lhs.span.shrink_to_lo(),
          hir_id: lhs.hir_id,
          flow_context,
          is_lhs: true,
          expected: ExpectedPermissions::from_assignment(),
        };

        self.data.push(pb);
        self.visit_expr(rhs);
      }

      ExprKind::Unary(UnOp::Deref, inner)
        if inner.is_syntactic_place_expr() && !inner.span.from_expansion() =>
      {
        let pb = PathBoundary {
          hir_id,
          flow_context,
          is_lhs: false,
          // We want the boundary to appear to the left of the deref.
          location: expr.span.shrink_to_lo(),
          expected: self.get_adjusted_permissions(expr),
        };
        self.data.push(pb);
      }

      // We only want to attach permissions to path resolved to `Local` ids.
      ExprKind::Path(QPath::Resolved(
        _,
        Path {
          span,
          res: Res::Local(_),
          ..
        },
      )) if !span.from_expansion() => {
        let pb = PathBoundary {
          hir_id,
          flow_context,
          is_lhs: false,
          location: span.shrink_to_lo(),
          expected: self.get_adjusted_permissions(expr),
        };
        self.data.push(pb);
      }
      _ => {
        intravisit::walk_expr(self, expr);
      }
    }
  }
}

pub(super) fn get_path_boundaries(
  ctxt: &PermissionsCtxt<'_>,
) -> Result<Vec<PathBoundary>> {
  let tcx = ctxt.tcx;
  let body_id = ctxt.body_id;
  let typeck_res = tcx.typeck_body(ctxt.body_id);
  let typing_env = ctxt.typing_env;
  let mut finder = HirExprScraper {
    tcx,
    typing_env,
    typeck_res,
    unsupported_feature: None,
    data: Vec::default(),
  };

  log::debug!("THE BODY OWNER: {:?}", tcx.hir().body_owner(body_id));

  finder.visit_nested_body(body_id);

  if let Some((_, msg)) = finder.unsupported_feature {
    bail!(msg);
  }

  Ok(finder.data)
}
