use either::Either;
use flowistry::mir::utils::{OperandExt, SpanExt};
use rustc_hir::HirId;
use rustc_middle::mir::{Rvalue, Statement, StatementKind};
use rustc_span::Span;
use serde::Serialize;
use ts_rs::TS;

use crate::{
  analysis::{
    ir_mapper::GatherDepth,
    permissions::{Permissions, PermissionsData},
    AquascopeAnalysis, KeyShifter, LoanKey,
  },
  mir::utils::PlaceExt,
};

/// A point where the permissions reality are checked against their expectations.
#[derive(Debug, Clone, Serialize, TS)]
#[ts(export)]
pub struct PermissionsBoundary {
  // instead of giving the range, the backend should supply the exact location. this will
  // be especially usefull when we have permissions on more than just method calls.
  pub location: usize,
  pub expected: Permissions,
  pub actual: PermissionsData,
}

impl KeyShifter for PermissionsBoundary {
  fn shift_keys(self, loan_shift: LoanKey) -> Self {
    PermissionsBoundary {
      actual: self.actual.shift_keys(loan_shift),
      ..self
    }
  }
}

// A previous implementation of the permission boundaries
// computation allowed for multiple stacks to get generated
// per HirId, this isn't the case currently so we could
// cleanup these types a bit.
trait IntoMany {
  type Elem;
  fn into_many(self) -> Box<dyn Iterator<Item = Self::Elem>>;
}

// ----------------------------------
// Permission boundaries on path uses

struct PathBoundary<'a, 'tcx: 'a> {
  pub hir_id: HirId,
  pub location: Span,
  pub expected: Permissions,
  pub analysis_ctxt: &'a AquascopeAnalysis<'a, 'tcx>,
}

impl std::fmt::Debug for PathBoundary<'_, '_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("PathBoundary")
      .field("location", &self.location)
      .field("hir_id", &self.hir_id)
      .field("expected", &self.expected)
      .finish()
  }
}

impl IntoMany for PathBoundary<'_, '_> {
  type Elem = PermissionsBoundary;
  fn into_many(self) -> Box<dyn Iterator<Item = Self::Elem>> {
    log::debug!("Computing PermissionsBoundary for {self:?}");

    let ctxt = &self.analysis_ctxt.permissions;
    let body = &ctxt.body_with_facts.body;
    let tcx = ctxt.tcx;
    let hir = tcx.hir();
    let ir_mapper = &self.analysis_ctxt.ir_mapper;
    let hir_id = self.hir_id;

    // For a given Path, the MIR location may not be immediately associated with it.
    // For example, in a function call `foo( &x );`, the Hir Node::Path `&x` will not
    // have the MIR locations associated with it, the Hir Node::Call `foo( &x )` will,
    // so we traverse upwards in the tree until we find a location associated with it.
    let search_at_hir_id = |hir_id| {
      let mir_locations_opt =
        ir_mapper.get_mir_locations(hir_id, GatherDepth::Nested);

      let mir_locations = mir_locations_opt?
        .values()
        .filter_map(|loc| {
          if let Either::Left(Statement {
            kind: StatementKind::Assign(box (_, rvalue)),
            ..
          }) = body.stmt_at(loc)
          {
            match rvalue {
              Rvalue::Ref(_, _, place)
                if place.is_source_visible(tcx, body) =>
              {
                Some((loc, *place))
              }
              Rvalue::Use(op) => op.to_place().and_then(|p| {
                p.is_source_visible(tcx, body).then_some((loc, p))
              }),

              Rvalue::CopyForDeref(place)
                if place.is_source_visible(tcx, body) =>
              {
                Some((loc, *place))
              }
              _ => None,
            }
          } else {
            None
          }
        })
        .collect::<Vec<_>>();

      let (loc, place) = mir_locations.first()?;
      log::debug!("Chosen place at location {place:#?} {loc:#?}");
      let point = ctxt.location_to_point(*loc);
      let path = ctxt.place_to_path(place);

      Some((point, path))
    };

    let i = search_at_hir_id(hir_id)
      .or_else(|| {
        hir.parent_iter(hir_id).find_map(|(hir_id, _)| {
          log::debug!("\tsearching upwards in: {hir_id:?}");
          search_at_hir_id(hir_id)
        })
      })
      .map(|(point, path)| {
        let actual = ctxt.permissions_data_at_point(path, point);
        let expected = self.expected;

        log::debug!("Permissions data: {actual:#?}");

        let span = self.location.as_local(body.span).unwrap_or(self.location);

        // NOTE: all permission stacks are placed directly to the _right_ of the path.
        let location = self.analysis_ctxt.span_to_range(span).char_end;

        PermissionsBoundary {
          location,
          actual,
          expected,
        }
      })
      .into_iter();

    Box::new(i)
  }
}

// ----------------------------------
// Entry

pub fn compute_permission_boundaries<'a, 'tcx: 'a>(
  ctxt: &AquascopeAnalysis<'a, 'tcx>,
) -> Vec<PermissionsBoundary> {
  // -----------------------------------
  // TODO cleanup and move mod elsewhere
  // -----------------------------------

  mod path_visitor {
    use rustc_hir::{
      def::Res,
      intravisit::{self, Visitor},
      BodyId, Expr, ExprKind, Mutability, Path, QPath,
    };
    use rustc_middle::{
      hir::nested_filter::OnlyBodies,
      ty::{TyCtxt, TypeckResults},
    };

    use super::*;

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
        let span = expr.span;

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
            args.iter().for_each(|a| {
              intravisit::walk_expr(self, a);
            });
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
  }

  let path_use_points = path_visitor::get_path_boundaries(
    ctxt.permissions.tcx,
    ctxt.permissions.body_id,
    ctxt,
  )
  .into_iter()
  .map(IntoMany::into_many);

  path_use_points.flatten().collect::<Vec<_>>()
}
