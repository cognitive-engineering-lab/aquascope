use log::trace;
use rustc_hir::{intravisit::Visitor, BodyId};
use rustc_middle::{hir::nested_filter::OnlyBodies, ty::TyCtxt};
use rustc_span::Span;

use crate::mir::utils::SpanExt;

struct BodyFinder<'tcx> {
  tcx: TyCtxt<'tcx>,
  bodies: Vec<(Span, BodyId)>,
}

impl<'tcx> Visitor<'tcx> for BodyFinder<'tcx> {
  type NestedFilter = OnlyBodies;

  fn nested_visit_map(&mut self) -> Self::Map {
    self.tcx.hir()
  }

  fn visit_nested_body(&mut self, id: BodyId) {
    let hir = self.nested_visit_map();

    // const/static items are considered to have bodies, so we want to exclude
    // them from our search for functions
    if !hir
      .body_owner_kind(hir.body_owner_def_id(id))
      .is_fn_or_closure()
    {
      return;
    }

    let body = hir.body(id);
    self.visit_body(body);

    let hir = self.tcx.hir();
    let span = hir.span_with_body(hir.body_owner(id));
    trace!(
      "Searching body for {:?} with span {span:?} (local {:?})",
      self
        .tcx
        .def_path_debug_str(hir.body_owner_def_id(id).to_def_id()),
      span.as_local(body.value.span)
    );

    if !span.from_expansion() {
      self.bodies.push((span, id));
    }
  }
}

pub fn find_bodies(tcx: TyCtxt) -> Vec<(Span, BodyId)> {
  let mut finder = BodyFinder {
    tcx,
    bodies: Vec::new(),
  };
  tcx.hir().deep_visit_all_item_likes(&mut finder);
  finder.bodies
}

pub fn find_enclosing_bodies(
  tcx: TyCtxt,
  sp: Span,
) -> impl Iterator<Item = BodyId> {
  let mut bodies = find_bodies(tcx);
  bodies.retain(|(other, _)| other.contains(sp));
  bodies.sort_by_key(|(span, _)| span.size());
  bodies.into_iter().map(|(_, id)| id)
}
