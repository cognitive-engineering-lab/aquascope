use rustc_hir::{
  intravisit::{self, Visitor},
  BodyId, Expr, ExprKind, Mutability,
};
use rustc_middle::{
  hir::nested_filter::OnlyBodies,
  ty::{TyCtxt, TypeckResults},
};

use super::{AquascopeAnalysis, PathBoundary, Permissions};

struct HirExprScraper<'a, 'tcx: 'a> {
  tcx: TyCtxt<'tcx>,
  typeck_res: &'a TypeckResults<'tcx>,
  data: Vec<PathBoundary<'a, 'tcx>>,
  ctxt: &'a AquascopeAnalysis<'a, 'tcx>,
}

impl<'a, 'tcx: 'a> Visitor<'tcx> for HirExprScraper<'a, 'tcx> {
  type NestedFilter = OnlyBodies;

  fn nested_visit_map(&mut self) -> Self::Map {
    self.tcx.hir()
  }

  fn visit_expr(&mut self, expr: &'tcx Expr) {
    let hir_id = expr.hir_id;

    log::debug!(
      "visiting {}",
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

                if !matches!(inner.kind, ExprKind::Path(..)) {
                    intravisit::walk_expr(self, expr);
                }
            }

            ExprKind::AssignOp(_, lhs, rhs) => {
                let lhs_ty = self.typeck_res.expr_ty_adjusted(lhs);
                log::debug!("Type of LHS: {:?}", lhs_ty);


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
            // FIXME: the commented out region produced some hard-to-resolve bugs
            // which I will come back to later. An example,
            // ```
            // fn foo(s: &String, b: &mut String) {}
            //
            // fn main() {
            //   let mut x = "s".to_owned();
            //   let w: &mut String = &mut x;
            //   foo(&x, w);
            // }
            // ```
            // if you allow a permission stack to be placed at the `w` in
            // the Call `foo( &x, w )`, the analysis will currently
            // find the permissions for the path `(*w)`, even though,
            // it is actually reborrowed before the call. Thus, falsely
            // showing that the drop permission is missing.
            //
            // ExprKind::Path(QPath::Resolved(
            //   _,
            //   Path {
            //     span,
            //     res: Res::Local(_),
            //     ..
            //   },
            // )) if !span.from_expansion() => {
            //   let pb = PathBoundary {
            //     hir_id,
            //     location: span.shrink_to_lo(),
            //     expected: Permissions {
            //       read: true,
            //       write: false,
            //       drop: true,
            //     },
            //     analysis_ctxt: self.ctxt,
            //   };
            //   self.data.push(pb);
            // }

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
) -> Vec<PathBoundary<'a, 'tcx>> {
  let typeck_res = tcx.typeck_body(body_id);
  let mut finder = HirExprScraper {
    tcx,
    typeck_res,
    ctxt,
    data: Vec::default(),
  };
  finder.visit_nested_body(body_id);
  finder.data
}
