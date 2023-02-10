use anyhow::{bail, Result};
use flowistry::mir::utils::SpanExt;
use rustc_hir::{
  def::Res,
  intravisit::{self, Visitor},
  BodyId, Expr, ExprKind, HirId, Local, Mutability, Path, QPath, Stmt,
  StmtKind,
};
use rustc_middle::{
  hir::nested_filter::OnlyBodies,
  ty::{TyCtxt, TypeckResults},
};
use rustc_span::Span;

use super::{AquascopeAnalysis, PathBoundary, Permissions};

struct HirExprScraper<'a, 'tcx: 'a> {
  tcx: TyCtxt<'tcx>,
  typeck_res: &'a TypeckResults<'tcx>,
  data: Vec<PathBoundary<'a, 'tcx>>,
  unsupported_feature: Option<(Span, String)>,
  ctxt: &'a AquascopeAnalysis<'a, 'tcx>,
}

impl<'a, 'tcx: 'a> HirExprScraper<'a, 'tcx> {
  fn span_of(&self, id: HirId) -> Span {
    let hir = self.ctxt.permissions.tcx.hir();
    let span = hir.span(id);
    span
      .as_local(self.ctxt.permissions.body_with_facts.body.span)
      .unwrap_or(span)
  }

  fn report_unsupported(&mut self, id: HirId, msg: &str) {
    if self.unsupported_feature.is_none() {
      let span = self.span_of(id);
      self.unsupported_feature = Some((span, String::from(msg)));
    }
  }
}

impl<'a, 'tcx: 'a> Visitor<'tcx> for HirExprScraper<'a, 'tcx> {
  type NestedFilter = OnlyBodies;

  fn nested_visit_map(&mut self) -> Self::Map {
    self.tcx.hir()
  }

  fn visit_stmt(&mut self, stmt: &'tcx Stmt) {
    if matches!(stmt.kind, StmtKind::Local(Local { ty: Some(_), .. })) {
      self.report_unsupported(
        stmt.hir_id,
        "A local assignment with user-specified type annotation is unsupported",
      )
    } else {
      intravisit::walk_stmt(self, stmt)
    }
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
                && rcvr
                .is_place_expr(|e| !matches!(e.kind, ExprKind::Lit(_))) =>
            {
                let def_id = self.typeck_res.type_dependent_def_id(hir_id).unwrap();
                let fn_sig = self.tcx.fn_sig(def_id).skip_binder();

                let pb = PathBoundary {
                    location: rcvr.span,
                    hir_id,
                    expected: fn_sig.inputs()[0].into(),
                    analysis_ctxt: self.ctxt,
                };

                self.data.push(pb);

                for a in args.iter() {
                    intravisit::walk_expr(self, a);
                }
            }

            ExprKind::AddrOf(_, mutability, inner)
            // FIXME: this will only place bounds on
            // directly referenced paths.
            // The previous logic (only requiring that the inner
            // expression not be from an expansion) failed to filter
            // out the slice reference in the `println!`. Even though,
            // this reference did in fact come from an expansion.
                if inner.is_syntactic_place_expr()
                && !inner.span.from_expansion() =>
            {
                let pb = PathBoundary {
                    hir_id,
                    location: inner.span.shrink_to_lo(),
                    expected: Permissions {
                        read: true,
                        write: matches!(mutability, Mutability::Mut),
                        drop: false,
                    },
                    analysis_ctxt: self.ctxt,
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
            },

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
                    analysis_ctxt: self.ctxt,
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

                log::warn!("Path usage TYPE: {:#?}", self.typeck_res.expr_ty_adjusted(expr));

                let pb = PathBoundary {
                    hir_id,
                    location: span.shrink_to_lo(),
                    expected: Permissions {
                        read: true,
                        write: false,
                        drop: true,
                    },
                    analysis_ctxt: self.ctxt,
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
  ctxt: &'a AquascopeAnalysis<'a, 'tcx>,
) -> Result<Vec<PathBoundary<'a, 'tcx>>> {
  let typeck_res = tcx.typeck_body(body_id);
  let mut finder = HirExprScraper {
    tcx,
    typeck_res,
    unsupported_feature: None,
    ctxt,
    data: Vec::default(),
  };

  finder.visit_nested_body(body_id);

  if let Some((_, msg)) = finder.unsupported_feature {
    bail!(msg);
  }

  Ok(finder.data)
}
