use anyhow::{bail, Result};
use rustc_hir::{
  def::Res,
  intravisit::{self, Visitor},
  BodyId, Expr, ExprKind, Mutability, Path, QPath,
};
use rustc_middle::{
  hir::nested_filter::OnlyBodies,
  ty::{
    adjustment::{Adjust, AutoBorrow, AutoBorrowMutability},
    TyCtxt, TypeckResults,
  },
};
use rustc_span::Span;

use super::{PathBoundary, Permissions};
use crate::analysis::permissions::PermissionsCtxt;

struct HirExprScraper<'a, 'tcx: 'a> {
  tcx: TyCtxt<'tcx>,
  typeck_res: &'a TypeckResults<'tcx>,
  data: Vec<PathBoundary>,
  unsupported_feature: Option<(Span, String)>,
}

impl<'a, 'tcx: 'a> HirExprScraper<'a, 'tcx> {
  fn get_adjusted_permissions(&self, expr: &Expr) -> Permissions {
    let ty_adj = self.typeck_res.expr_ty_adjusted(expr);
    let adjs = self.typeck_res.expr_adjustments(expr);

    log::debug!("Path TY-ADJ: {:#?} from {:#?}", ty_adj, adjs);

    let is_auto_borrow = adjs.iter().find_map(|adj| {
      if let Adjust::Borrow(AutoBorrow::Ref(_, m)) = adj.kind {
        Some(m)
      } else {
        None
      }
    });

    Permissions {
      read: true,
      write: matches!(is_auto_borrow, Some(AutoBorrowMutability::Mut { .. })),
      // Paths which are not reborrowed are moved.
      drop: is_auto_borrow.is_none(),
    }
  }
}

impl<'a, 'tcx: 'a> Visitor<'tcx> for HirExprScraper<'a, 'tcx> {
  type NestedFilter = OnlyBodies;

  fn nested_visit_map(&mut self) -> Self::Map {
    self.tcx.hir()
  }

  fn visit_expr(&mut self, expr: &'tcx Expr) {
    let hir_id = expr.hir_id;

    log::debug!(
      "visiting {}\n\n",
      self.nested_visit_map().node_to_string(hir_id)
    );

    match expr.kind {
      // For method calls, it's most accurate to get the expected
      // permissions from the method signature declared type.
      ExprKind::MethodCall(_, rcvr, args, fn_span)
        if !fn_span.from_expansion()
          && rcvr.is_place_expr(|e| !matches!(e.kind, ExprKind::Lit(_))) =>
      {
        let def_id = self.typeck_res.type_dependent_def_id(hir_id).unwrap();
        let fn_sig = self.tcx.fn_sig(def_id).skip_binder();

        let pb = PathBoundary {
          location: rcvr.span,
          hir_id,
          expected: fn_sig.inputs()[0].into(),
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
          location: inner.span.shrink_to_lo(),
          expected: Permissions {
            read: true,
            write: matches!(mutability, Mutability::Mut),
            drop: false,
          },
        };

        self.data.push(pb);
      }

      // NOTE: it feels natural to say that the LHS of an assignment
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
      ExprKind::Assign(_, rhs, _) => {
        self.visit_expr(rhs);
      }

      ExprKind::AssignOp(_, lhs, rhs) => {
        let lhs_ty = self.typeck_res.expr_ty_adjusted(lhs);
        log::debug!("Type of LHS: {:#?}", lhs_ty);

        let pb = PathBoundary {
          location: lhs.span.shrink_to_lo(),
          hir_id,
          expected: Permissions {
            read: true,
            write: true,
            drop: false,
          },
        };

        self.data.push(pb);

        intravisit::walk_expr(self, lhs);
        intravisit::walk_expr(self, rhs);
      }

      // XXX: we only want to attach permissions to path resolved to `Local` ids.
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

pub(super) fn get_path_boundaries<'a, 'tcx: 'a>(
  tcx: TyCtxt<'tcx>,
  body_id: BodyId,
  _ctxt: &'a PermissionsCtxt<'a, 'tcx>,
) -> Result<Vec<PathBoundary>> {
  let typeck_res = tcx.typeck_body(body_id);
  let mut finder = HirExprScraper {
    tcx,
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
