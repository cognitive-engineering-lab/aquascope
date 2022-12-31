use either::Either;
use flowistry::mir::utils::OperandExt;
use rustc_hir::{HirId, QPath};
use rustc_middle::mir::{
  Body, Operand, Place, Rvalue, Statement, StatementKind, Terminator,
  TerminatorKind,
};
use rustc_span::Span;

use crate::{
  analysis::{
    find_hir_calls::{find_method_call_spans, scrape_expr_data},
    ir_mapper::{GatherDepth, GatherMode, IRMapper},
    permissions::{
      Permissions, PermissionsBoundary, PermissionsCtxt, PermissionsData, Point,
    },
    AquascopeAnalysis,
  },
  mir::utils::PlaceExt,
  Range,
};

// -------------------------------------
// Permission boundaries on method calls

struct MethodCallBoundary<'a, 'tcx: 'a> {
  pub location: Span,
  pub hir_id: HirId,
  pub expected: Permissions,
  pub analysis_ctxt: &'a AquascopeAnalysis<'a, 'tcx>,
}

impl Into<PermissionsBoundary> for MethodCallBoundary<'_, '_> {
  fn into(self) -> PermissionsBoundary {
    enum RcvrTy<'a> {
      Constant,
      Place(Place<'a>),
    }

    impl<'a> RcvrTy<'a> {
      fn as_place_unchecked(&self) -> Place<'a> {
        match self {
          RcvrTy::Place(place) => *place,
          _ => unreachable!(),
        }
      }

      fn permissions_data_at(
        &self,
        ctxt: &PermissionsCtxt<'_, 'a>,
        point: Point,
      ) -> PermissionsData {
        match self {
          RcvrTy::Place(pl) => {
            let path = ctxt.place_to_path(pl);
            ctxt.permissions_data_at_point(path, point)
          }
          _ => unimplemented!(),
        }
      }
    }

    impl<'a> From<&Operand<'a>> for RcvrTy<'a> {
      fn from(op: &Operand<'a>) -> RcvrTy<'a> {
        op.to_place().map(RcvrTy::Place).unwrap_or(RcvrTy::Constant)
      }
    }

    let ctxt = &self.analysis_ctxt.permissions;
    let body = &ctxt.body_with_facts.body;
    let tcx = ctxt.tcx;
    let ir_mapper = &IRMapper::new(tcx, body, GatherMode::IgnoreCleanup);

    let hir_id = self.hir_id;

    let mir_locations_opt =
      ir_mapper.get_mir_locations(hir_id, GatherDepth::Nested);
    assert!(mir_locations_opt.is_some());

    let mir_locations = mir_locations_opt.unwrap().values().collect::<Vec<_>>();

    // Step 1. find the boundary equivalent
    let function_calls = mir_locations
      .iter()
      .filter(|loc| {
        matches!(
          body.stmt_at(**loc),
          Either::Right(Terminator {
            kind: TerminatorKind::Call {
              from_hir_call: true,
              ..
            },
            ..
          })
        )
      })
      .collect::<Vec<_>>();

    assert_eq!(1, function_calls.len());

    let call_loc = function_calls.first().unwrap();
    let point = ctxt.location_to_point(**call_loc);

    let Either::Right(Terminator {
        kind:
          TerminatorKind::Call {
            args,
            from_hir_call: true,
            ..
          },
        ..
      }) = body.stmt_at(**call_loc) else { unreachable!() };

    let is_terminal = |r: &RcvrTy| -> bool {
      match r {
        RcvrTy::Constant => true,
        RcvrTy::Place(place) => place.is_source_visible(tcx, body),
      }
    };

    let rcvr_op = &args[0];
    let mut rcvr: RcvrTy = rcvr_op.into();

    let mut changed = true;

    // Step 2. traverse the locations until you reach
    while changed && !is_terminal(&rcvr) {
      changed = false;
      let curr_place = rcvr.as_place_unchecked();

      mir_locations.iter().for_each(|loc| {
        match body.stmt_at(*loc) {
          Either::Left(Statement {
            kind: StatementKind::Assign(box (lhs, rhs)),
            ..
          }) if *lhs == curr_place => {
            changed = true;
            rcvr = match rhs {
              Rvalue::Ref(_, _, place) => RcvrTy::Place(*place),
              Rvalue::Use(op) => op.into(),
              Rvalue::CopyForDeref(place) => RcvrTy::Place(*place),

              // TODO: revisit the rest of these which are sure to come up in the wild.
              Rvalue::Aggregate(..) => unimplemented!("Rvalue::aggregate"),
              Rvalue::Repeat(..) => unimplemented!("Rvalue::repeat"),
              Rvalue::ThreadLocalRef(..) => {
                unimplemented!("Rvalue::thread-local-ref")
              }
              Rvalue::AddressOf(..) => unimplemented!("Rvalue::address-of"),
              Rvalue::Len(..) => unimplemented!("Rvalue::len"),
              Rvalue::Cast(..) => unimplemented!("Rvalue::cast"),
              Rvalue::BinaryOp(..) => unimplemented!("Rvalue::binop"),
              Rvalue::CheckedBinaryOp(..) => {
                unimplemented!("Rvalue::checkedbinop")
              }
              Rvalue::NullaryOp(..) => unimplemented!("Rvalue::nullaryop"),
              Rvalue::UnaryOp(..) => unimplemented!("Rvalue::unaryop"),
              Rvalue::Discriminant(..) => {
                unimplemented!("Rvalue::discriminatn")
              }
              Rvalue::ShallowInitBox(..) => {
                unimplemented!("Rvalue::shallow-init-box")
              }
            }
          }
          _ => (),
        }
      })
    }

    let actual = rcvr.permissions_data_at(ctxt, point);
    let expected = self.expected;

    // FIXME HACK: we assume that the `.` is one character to the left of the method call.
    // this is of course not *strictly* true and should be fixed.
    let location =
      self.analysis_ctxt.span_to_range(self.location).char_start - 1;

    PermissionsBoundary {
      location,
      actual,
      expected,
    }
  }
}

pub fn pair_permissions_to_calls<'a, 'tcx: 'a>(
  ctxt: &AquascopeAnalysis<'a, 'tcx>,
) -> Vec<PermissionsBoundary> {
  use rustc_hir::{Expr, ExprKind, Node};

  let typeck_res = ctxt.permissions.tcx.typeck_body(ctxt.permissions.body_id);
  let method_spans =
    find_method_call_spans(ctxt.permissions.tcx, ctxt.permissions.body_id);

  let hir = ctxt.permissions.tcx.hir();

  let method_call_points =
    scrape_expr_data(ctxt.permissions.tcx, ctxt.permissions.body_id, |expr| {
      if let Expr {
        hir_id,
        kind:
          ExprKind::MethodCall(
            _,
            Expr {
              kind: ExprKind::Path(qp),
              ..
            },
            _,
            fn_span,
          ),
        ..
      } = expr
      {
        let def_id = typeck_res.type_dependent_def_id(*hir_id).unwrap();
        let fn_sig = ctxt.permissions.tcx.fn_sig(def_id).skip_binder();

        Some(MethodCallBoundary {
          location: *fn_span,
          hir_id: *hir_id,
          expected: fn_sig.inputs()[0].into(),
          analysis_ctxt: ctxt,
        })
      } else {
        None
      }
    });

  method_call_points
    .into_iter()
    .map(Into::<PermissionsBoundary>::into)
    .collect::<Vec<_>>()
}
