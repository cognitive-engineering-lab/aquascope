use rustc_data_structures::fx::FxHashMap as HashMap;
use rustc_hir::{
  def_id::LocalDefId,
  intravisit::{self, Visitor},
  Expr, ExprKind, HirId,
};
// use rustc_hir_analysis;
use rustc_middle::{hir::nested_filter::OnlyBodies, ty::TyCtxt};

type MultiMap<K, V> = HashMap<K, Vec<V>>;

struct MethodCallFinder<'tcx> {
  tcx: TyCtxt<'tcx>,
  call_node_ids: MultiMap<LocalDefId, HirId>,
}

impl<'tcx> Visitor<'tcx> for MethodCallFinder<'tcx> {
  type NestedFilter = OnlyBodies;

  fn nested_visit_map(&mut self) -> Self::Map {
    self.tcx.hir()
  }

  fn visit_expr(&mut self, expression: &'tcx Expr) {
    intravisit::walk_expr(self, expression);

    let hir_id = expression.hir_id;
    if let ExprKind::MethodCall(..) = expression.kind {
      log::trace!("I found a method call!");

      let hir = self.nested_visit_map();
      let owner: LocalDefId = hir.get_parent_item(hir_id).def_id;
      self.call_node_ids.entry(owner).or_default().push(hir_id);
    }
  }
}

pub fn find_method_calls(tcx: TyCtxt) -> MultiMap<LocalDefId, HirId> {
  let mut finder = MethodCallFinder {
    tcx,
    call_node_ids: MultiMap::default(),
  };
  tcx.hir().visit_all_item_likes_in_crate(&mut finder);
  finder.call_node_ids
}
