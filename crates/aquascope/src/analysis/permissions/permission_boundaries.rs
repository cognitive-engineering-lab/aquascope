use std::convert::TryInto;

use anyhow::{anyhow, Result};
use either::Either;
use flowistry::mir::utils::OperandExt;
use rustc_hir::HirId;
use rustc_middle::{
  mir::{
    Body, Operand, Place, Rvalue, Statement, StatementKind, Terminator,
    TerminatorKind,
  },
  ty::{Ty, TyCtxt},
};
use rustc_span::Span;

use crate::{
  analysis::{
    ir_mapper::{GatherDepth, GatherMode, IRMapper},
    permissions::{
      Permissions, PermissionsBoundary, PermissionsCtxt, PermissionsData, Point,
    },
    scrape_hir::scrape_expr_data,
    AquascopeAnalysis,
  },
  mir::utils::PlaceExt,
};

// -------------------------------------
// Permission boundaries on method calls

struct MethodCallBoundary<'a, 'tcx: 'a> {
  pub location: Span,
  pub hir_id: HirId,
  pub expected: Permissions,
  pub analysis_ctxt: &'a AquascopeAnalysis<'a, 'tcx>,
}

impl std::fmt::Debug for MethodCallBoundary<'_, '_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("MethodCallBoundary")
      .field("location", &self.location)
      .field("hir_id", &self.hir_id)
      .field("expected", &self.expected)
      .finish()
  }
}

impl TryInto<PermissionsBoundary> for MethodCallBoundary<'_, '_> {
  type Error = anyhow::Error;
  fn try_into(self) -> Result<PermissionsBoundary> {
    enum RcvrTy<'tcx> {
      Constant(Ty<'tcx>),
      Aggregate(Ty<'tcx>),
      Place(Place<'tcx>),
    }

    impl<'tcx> RcvrTy<'tcx> {
      fn as_place_unchecked(&self) -> Place<'tcx> {
        match self {
          RcvrTy::Place(place) => *place,
          _ => unreachable!(),
        }
      }

      fn permissions_data_at(
        &self,
        ctxt: &PermissionsCtxt<'_, 'tcx>,
        point: Point,
      ) -> PermissionsData {
        match self {
          RcvrTy::Place(pl) => {
            let path = ctxt.place_to_path(pl);
            ctxt.permissions_data_at_point(path, point)
          }
          RcvrTy::Constant(ty) | RcvrTy::Aggregate(ty) => {
            ctxt.permissions_for_const_ty(*ty)
          }
        }
      }

      fn from_op(
        op: &Operand<'tcx>,
        body: &Body<'tcx>,
        tcx: TyCtxt<'tcx>,
      ) -> RcvrTy<'tcx> {
        op.to_place()
          .map(RcvrTy::Place)
          .unwrap_or_else(|| RcvrTy::Constant(op.ty(body, tcx)))
      }
    }

    log::debug!("Computing PermissionsBoundary for {self:?}");

    let ctxt = &self.analysis_ctxt.permissions;
    let body = &ctxt.body_with_facts.body;
    let tcx = ctxt.tcx;
    let ir_mapper = &IRMapper::new(tcx, body, GatherMode::IgnoreCleanup);

    let hir_id = self.hir_id;

    let mir_locations_opt =
      ir_mapper.get_mir_locations(hir_id, GatherDepth::Nested);

    // assert!(mir_locations_opt.is_some());

    let mir_locations = mir_locations_opt
      .ok_or_else(|| anyhow!("no associated mir locations"))?
      .values()
      .collect::<Vec<_>>();

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

    log::debug!("function calls: {function_calls:#?}");

    // FIXME: not all method calls have exactly 1 method call. Mulitple calls
    // can be inserted if the receiver is the result of a method call.
    // Example:
    // ```
    // String::from("hello").push_str(" world!");
    // ```
    // taking the last of these calls is a fragile way of handling this.
    assert!(!function_calls.is_empty());
    let call_loc = function_calls.last().unwrap();

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
        RcvrTy::Constant(_) | RcvrTy::Aggregate(_) => true,
        RcvrTy::Place(place) => place.is_source_visible(tcx, body),
      }
    };

    let rcvr_op = &args[0];
    let mut rcvr = RcvrTy::from_op(rcvr_op, body, tcx);
    let mut point = ctxt.location_to_point(**call_loc);
    let mut changed = true;

    // Step 2. traverse the locations until you reach a RValue which
    // is source visible. If the receiver place is not source visible
    // (e.g. constants) we will stop there and
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
            point = ctxt.location_to_point(*loc);
            rcvr = match rhs {
              Rvalue::Ref(_, _, place) => RcvrTy::Place(*place),
              Rvalue::Use(op) => RcvrTy::from_op(op, body, tcx),
              Rvalue::CopyForDeref(place) => RcvrTy::Place(*place),
              Rvalue::Aggregate(..) => RcvrTy::Aggregate(rhs.ty(body, tcx)),

              // TODO: revisit the rest of these which are sure to come up in the wild.
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

    Ok(PermissionsBoundary {
      location,
      actual,
      expected,
    })
  }
}

// ----------------------------------
// Permission boundaries on path uses

struct PathBoundary<'a, 'tcx: 'a> {
  pub location: Span,
  pub hir_id: HirId,
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

impl TryInto<PermissionsBoundary> for PathBoundary<'_, '_> {
  type Error = anyhow::Error;
  fn try_into(self) -> Result<PermissionsBoundary> {
    enum RcvrTy<'tcx> {
      Constant(Ty<'tcx>),
      Aggregate(Ty<'tcx>),
      Place(Place<'tcx>),
    }

    impl<'tcx> RcvrTy<'tcx> {
      fn as_place_unchecked(&self) -> Place<'tcx> {
        match self {
          RcvrTy::Place(place) => *place,
          _ => unreachable!(),
        }
      }

      fn permissions_data_at(
        &self,
        ctxt: &PermissionsCtxt<'_, 'tcx>,
        point: Point,
      ) -> PermissionsData {
        match self {
          RcvrTy::Place(pl) => {
            let path = ctxt.place_to_path(pl);
            ctxt.permissions_data_at_point(path, point)
          }
          RcvrTy::Constant(ty) | RcvrTy::Aggregate(ty) => {
            ctxt.permissions_for_const_ty(*ty)
          }
        }
      }

      fn from_op(
        op: &Operand<'tcx>,
        body: &Body<'tcx>,
        tcx: TyCtxt<'tcx>,
      ) -> RcvrTy<'tcx> {
        op.to_place()
          .map(RcvrTy::Place)
          .unwrap_or_else(|| RcvrTy::Constant(op.ty(body, tcx)))
      }
    }

    log::debug!("Computing PermissionsBoundary for {self:?}");

    let ctxt = &self.analysis_ctxt.permissions;
    let body = &ctxt.body_with_facts.body;
    let tcx = ctxt.tcx;
    let ir_mapper = &IRMapper::new(tcx, body, GatherMode::IgnoreCleanup);

    let hir_id = self.hir_id;

    let mir_locations_opt =
      ir_mapper.get_mir_locations(hir_id, GatherDepth::Nested);

    log::debug!("Finding locations for {hir_id:?}");

    // assert!(mir_locations_opt.is_some());

    let mir_locations = mir_locations_opt
      .ok_or_else(|| anyhow!("No associated MIR locations"))?
      .values()
      .collect::<Vec<_>>();

    log::debug!("MIR LOCATIONS: {mir_locations:#?}");

    // Step 1. find the boundary equivalent
    let mir_rhs = mir_locations
      .iter()
      .filter_map(|loc| {
        if let Either::Left(Statement {
          kind: StatementKind::Assign(box (_, rvalue)),
          ..
        }) = body.stmt_at(*loc)
        {
          match rvalue {
            Rvalue::Ref(_, _, place) if place.is_source_visible(tcx, body) => {
              Some((*loc, *place))
            }
            Rvalue::Use(op) => op.to_place().and_then(|p| {
              p.is_source_visible(tcx, body).then_some((*loc, p))
            }),

            Rvalue::CopyForDeref(place)
              if place.is_source_visible(tcx, body) =>
            {
              Some((*loc, *place))
            }
            _ => None,
          }
        } else {
          None
        }
      })
      .collect::<Vec<_>>();

    log::debug!("MIR Rvalues: {mir_rhs:#?}");

    assert!(!mir_rhs.is_empty());
    assert_eq!(1, mir_rhs.len());

    let (loc, place) = mir_rhs
      .first()
      .ok_or_else(|| anyhow!("No source visible places in assignments"))?;
    let point = ctxt.location_to_point(*loc);
    let path = ctxt.place_to_path(place);

    let actual = ctxt.permissions_data_at_point(path, point);
    let expected = self.expected;

    // FIXME HACK: we assume that the `.` is one character to the left of the method call.
    // this is of course not *strictly* true and should be fixed.
    let location = self.analysis_ctxt.span_to_range(self.location).char_end + 1;

    Ok(PermissionsBoundary {
      location,
      actual,
      expected,
    })
  }
}

// ----------------------------------
// Entry

pub fn pair_permissions_to_calls<'a, 'tcx: 'a>(
  ctxt: &AquascopeAnalysis<'a, 'tcx>,
) -> Vec<PermissionsBoundary> {
  use rustc_hir::{def::Res, Expr, ExprKind, Path, QPath};

  let typeck_res = ctxt.permissions.tcx.typeck_body(ctxt.permissions.body_id);
  let method_call_points =
    scrape_expr_data(ctxt.permissions.tcx, ctxt.permissions.body_id, |expr| {
      if let Expr {
        hir_id,
        kind: ExprKind::MethodCall(_, _, _, fn_span),
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
    })
    .into_iter()
    .map(TryInto::<PermissionsBoundary>::try_into);

  let path_use_points =
    scrape_expr_data(ctxt.permissions.tcx, ctxt.permissions.body_id, |expr| {
      if let Expr {
        hir_id,
        kind:
          ExprKind::Path(QPath::Resolved(
            _,
            Path {
              span,
              res: Res::Local(_),
              ..
            },
          )),
        ..
      } = expr
      {
        log::debug!("Found PATH {expr:#?}");
        let t0 = typeck_res.expr_ty(expr);
        let t1 = typeck_res.expr_ty_adjusted(expr);
        log::debug!("expr ty: {t0:#?} {t1:#?}");

        Some(PathBoundary {
          hir_id: *hir_id,
          location: *span,
          expected: t1.into(),
          analysis_ctxt: ctxt,
        })
      } else {
        None
      }
    })
    .into_iter()
    .map(TryInto::<PermissionsBoundary>::try_into);

  method_call_points
    .chain(path_use_points)
    .flatten()
    .collect::<Vec<_>>()
}
