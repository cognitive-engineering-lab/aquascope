use rustc_hir::{
  intravisit::{self, Visitor},
  BodyId, Expr, ExprKind,
};
use rustc_middle::{
  hir::nested_filter::OnlyBodies,
  ty::{FnSig, TyCtxt, TypeckResults},
};
use rustc_span::Span;

struct MethodCallFinder<'a, 'tcx> {
  tcx: TyCtxt<'tcx>,
  typeck_res: &'a TypeckResults<'tcx>,
  call_spans: Vec<(Span, FnSig<'tcx>)>,
}

impl<'tcx> Visitor<'tcx> for MethodCallFinder<'_, 'tcx> {
  type NestedFilter = OnlyBodies;

  fn nested_visit_map(&mut self) -> Self::Map {
    self.tcx.hir()
  }

  fn visit_expr(&mut self, expression: &'tcx Expr) {
    intravisit::walk_expr(self, expression);

    let hir_id = expression.hir_id;
    if let ExprKind::MethodCall(_ps, _, _, call_span) = expression.kind {
      let def_id = self.typeck_res.type_dependent_def_id(hir_id).unwrap();
      let fn_sig = self.tcx.fn_sig(def_id).skip_binder();
      self.call_spans.push((call_span, fn_sig));
    }
  }
}

pub fn find_method_call_spans(
  tcx: TyCtxt,
  body_id: BodyId,
) -> Vec<(Span, FnSig)> {
  let typeck_res = tcx.typeck_body(body_id);
  let mut finder = MethodCallFinder {
    tcx,
    typeck_res,
    call_spans: Vec::default(),
  };
  // XXX: `visit_all_item_likes_in_crate` visits all bodies, but
  // here we are only searching for bodies in the body that's under
  // analysis.
  finder.visit_nested_body(body_id);
  finder.call_spans
}
