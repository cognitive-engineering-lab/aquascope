use rustc_hir::{
  intravisit::{self, Visitor},
  BodyId, Expr,
};
use rustc_middle::{hir::nested_filter::OnlyBodies, ty::TyCtxt};

struct HirExprScraper<'tcx, A, F>
where
  F: Fn<(&'tcx Expr<'tcx>,), Output = Option<A>>,
{
  tcx: TyCtxt<'tcx>,
  maybe_scrape: F,
  data: Vec<A>,
}

impl<'tcx, A, F> Visitor<'tcx> for HirExprScraper<'tcx, A, F>
where
  F: Fn<(&'tcx Expr<'tcx>,), Output = Option<A>>,
{
  type NestedFilter = OnlyBodies;

  fn nested_visit_map(&mut self) -> Self::Map {
    self.tcx.hir()
  }

  fn visit_expr(&mut self, expression: &'tcx Expr) {
    intravisit::walk_expr(self, expression);
    if let Some(a) = (self.maybe_scrape)(expression) {
      self.data.push(a);
    }
  }
}

pub fn scrape_expr_data<F, O>(tcx: TyCtxt, body_id: BodyId, f: F) -> Vec<O>
where
  F: for<'tcx> Fn<(&'tcx Expr<'tcx>,), Output = Option<O>>,
{
  let mut finder = HirExprScraper {
    tcx,
    maybe_scrape: f,
    data: Vec::default(),
  };
  // XXX: `visit_all_item_likes_in_crate` visits all bodies, but
  // here we are only searching for bodies in the body that's under
  // analysis.
  finder.visit_nested_body(body_id);
  finder.data
}
