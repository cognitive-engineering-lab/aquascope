use rustc_data_structures::fx::FxHashMap as HashMap;
use rustc_hir::{
    intravisit::{self, Visitor},
    BindingAnnotation, HirId, Pat, PatKind,
};
// use rustc_hir_analysis;
use rustc_middle::{hir::nested_filter::OnlyBodies, ty::TyCtxt};

struct BindingFinder<'tcx> {
    tcx: TyCtxt<'tcx>,
    // Mapping a HirId (identifier) with it's binding annotations.
    bindings: HashMap<HirId, BindingAnnotation>,
}

impl<'tcx> Visitor<'tcx> for BindingFinder<'tcx> {
    type NestedFilter = OnlyBodies;

    fn nested_visit_map(&mut self) -> Self::Map {
        self.tcx.hir()
    }

    fn visit_pat(&mut self, pat: &'tcx Pat) {
        intravisit::walk_pat(self, pat);

        if let PatKind::Binding(ba, id, _, _) = pat.kind {
            log::trace!("Binding for {id:?} found!");
            self.bindings.insert(id, ba);
        }
    }
}

pub fn find_bindings(tcx: TyCtxt) -> HashMap<HirId, BindingAnnotation> {
    let mut finder = BindingFinder {
        tcx,
        bindings: HashMap::default(),
    };
    tcx.hir().visit_all_item_likes_in_crate(&mut finder);
    finder.bindings
}
