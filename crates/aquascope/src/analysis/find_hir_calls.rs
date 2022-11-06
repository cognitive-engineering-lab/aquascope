use rustc_hir::{
  def_id::LocalDefId,
  intravisit::{self, Visitor},
  Expr, ExprKind, HirId,
};
use rustc_middle::{hir::nested_filter::OnlyBodies, ty::TyCtxt};
use rustc_span::Span;

struct MethodCallFinder<'tcx> {
  tcx: TyCtxt<'tcx>,
  call_spans: Vec<Span>,
}

impl<'tcx> Visitor<'tcx> for MethodCallFinder<'tcx> {
  type NestedFilter = OnlyBodies;

  fn nested_visit_map(&mut self) -> Self::Map {
    self.tcx.hir()
  }

  fn visit_expr(&mut self, expression: &'tcx Expr) {
    intravisit::walk_expr(self, expression);

    let hir_id = expression.hir_id;
    if let ExprKind::MethodCall(_, _, _, call_span) = expression.kind {
      log::trace!("I found a method call!");

      self.call_spans.push(call_span);
    }
  }
}

pub fn find_method_call_spans(tcx: TyCtxt) -> Vec<Span> {
  let mut finder = MethodCallFinder {
    tcx,
    call_spans: Vec::default(),
  };
  tcx.hir().visit_all_item_likes_in_crate(&mut finder);
  finder.call_spans
}
