use either::Either;
use log::trace;
use rustc_middle::mir::{
  self,
  visit::{
    MutatingUseContext, NonMutatingUseContext, NonUseContext, PlaceContext,
    Visitor as MirVisitor,
  },
  Body, FakeReadCause, HasLocalDecls, Place, Statement, StatementKind,
  Terminator, TerminatorKind, RETURN_PLACE,
};
use rustc_span::SpanData;
use smallvec::{smallvec, SmallVec};

use super::Spanner;
use crate::{
  indexed::impls::arg_location,
  mir::utils::{BodyExt, PlaceExt, SpanExt},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MirSpannedPlace<'tcx> {
  pub place: mir::Place<'tcx>,
  pub span: SpanData,
  pub locations: SmallVec<[mir::Location; 1]>,
}

pub struct MirSpanCollector<'a, 'hir, 'tcx>(
  pub &'a mut Spanner<'hir, 'tcx>,
  pub &'a Body<'tcx>,
);

macro_rules! try_span {
  ($self:expr, $span:expr) => {
    match $span.as_local($self.0.item_span) {
      Some(span) if !$self.0.invalid_span(span) => span,
      _ => {
        return;
      }
    }
  };
}

impl<'tcx> MirVisitor<'tcx> for MirSpanCollector<'_, '_, 'tcx> {
  fn visit_body(&mut self, body: &Body<'tcx>) {
    self.super_body(body);

    // Add the return type as a spanned place representing all return locations
    let span = body.local_decls()[RETURN_PLACE].source_info.span;
    let span = try_span!(self, span);
    let locations = body.all_returns().collect::<SmallVec<_>>();
    self.0.mir_spans.push(MirSpannedPlace {
      span: span.data(),
      locations,
      place: Place::from_local(RETURN_PLACE, self.0.tcx),
    })
  }

  fn visit_place(
    &mut self,
    place: &mir::Place<'tcx>,
    context: PlaceContext,
    location: mir::Location,
  ) {
    trace!("place={place:?} context={context:?} location={location:?}");

    // MIR will sometimes include places assigned to unit, e.g.
    //   if true { let x = 1; } else { let x = 2; }
    // then the entire block will have a place with unit value.
    // To avoid letting that block be selectable, we ignore values with unit type.
    // This is a hack, but not sure if there's a better way?
    let body = &self.1;
    if place.ty(body.local_decls(), self.0.tcx).ty.is_unit() {
      return;
    }

    // Three cases, shown by example:
    //   fn foo(x: i32) {
    //     let y = x + 1;
    //   }
    // If the user selects...
    // * "x: i32" -- this span is contained in the LocalDecls for _1,
    //   which is represented by NonUseContext::VarDebugInfo
    // * "x + 1" -- MIR will generate a temporary to assign x into, whose
    //   span is given to "x". That corresponds to MutatingUseContext::Store
    // * "y" -- this corresponds to NonMutatingUseContext::Inspect
    let (span, locations) = match context {
      PlaceContext::MutatingUse(MutatingUseContext::Store)
      | PlaceContext::NonMutatingUse(
        NonMutatingUseContext::Copy | NonMutatingUseContext::Move,
      ) => {
        let source_info = body.source_info(location);
        (source_info.span, smallvec![location])
      }
      PlaceContext::NonMutatingUse(NonMutatingUseContext::Inspect) => {
        let source_info = body.source_info(location);
        // For a statement like `let y = x + 1`, if the user selects `y`,
        // then the only location that contains the source-map for `y` is a `FakeRead`.
        // However, for slicing we want to give the location that actually sets `y`.
        // So we search through the body to find the locations that assign to `y`.
        let locations = match body.stmt_at(location) {
          Either::Left(Statement {
            kind:
              StatementKind::FakeRead(box (
                FakeReadCause::ForLet(_) | FakeReadCause::ForMatchedPlace(_),
                _,
              )),
            ..
          }) => match arg_location(*place, body) {
            Some(arg_location) => smallvec![arg_location],
            None => {
              let locations = assigning_locations(body, *place);
              if locations.len() == 0 {
                log::warn!("FakeRead of {place:?} has no assignments");
                return;
              }
              locations
            }
          },
          _ => {
            return;
          }
        };
        (source_info.span, locations)
      }
      PlaceContext::NonUse(NonUseContext::VarDebugInfo)
        if body.args_iter().any(|local| local == place.local) =>
      {
        let source_info = body.local_decls()[place.local].source_info;
        let location = match arg_location(*place, body) {
          Some(arg_location) => arg_location,
          None => location,
        };
        (source_info.span, smallvec![location])
      }
      _ => {
        return;
      }
    };

    let span = try_span!(self, span);

    let spanned_place = MirSpannedPlace {
      place: *place,
      locations,
      span: span.data(),
    };
    trace!("spanned place: {spanned_place:?}");

    self.0.mir_spans.push(spanned_place);
  }

  // The visit_statement and visit_terminator cases are a backup.
  // Eg in the static_method case, if you have x = Foo::bar(), then
  // then a slice on Foo::bar() would correspond to no places. The best we
  // can do is at least make the slice on x.
  fn visit_statement(
    &mut self,
    statement: &mir::Statement<'tcx>,
    location: mir::Location,
  ) {
    self.super_statement(statement, location);

    if let mir::StatementKind::Assign(box (lhs, _)) = &statement.kind {
      if lhs.ty(self.1.local_decls(), self.0.tcx).ty.is_unit() {
        return;
      }

      let span = try_span!(self, statement.source_info.span);
      let spanned_place = MirSpannedPlace {
        place: *lhs,
        locations: smallvec![location],
        span: span.data(),
      };
      trace!("spanned place (assign): {spanned_place:?}");
      self.0.mir_spans.push(spanned_place);
    }
  }

  fn visit_terminator(
    &mut self,
    terminator: &mir::Terminator<'tcx>,
    location: mir::Location,
  ) {
    self.super_terminator(terminator, location);

    let place = match &terminator.kind {
      mir::TerminatorKind::Call {
        destination: Some((place, _)),
        ..
      } => *place,
      mir::TerminatorKind::DropAndReplace { place, .. } => *place,
      _ => {
        return;
      }
    };

    let span = try_span!(self, terminator.source_info.span);
    let spanned_place = MirSpannedPlace {
      place,
      locations: smallvec![location],
      span: span.data(),
    };
    trace!("spanned place (terminator): {spanned_place:?}");
    self.0.mir_spans.push(spanned_place);
  }
}

fn assigning_locations<'tcx>(
  body: &Body<'tcx>,
  place: mir::Place<'tcx>,
) -> SmallVec<[mir::Location; 1]> {
  body
    .all_locations()
    .filter(|location| match body.stmt_at(*location) {
      Either::Left(Statement {
        kind: StatementKind::Assign(box (lhs, _)),
        ..
      })
      | Either::Right(Terminator {
        kind:
          TerminatorKind::Call {
            destination: Some((lhs, _)),
            ..
          },
        ..
      }) => *lhs == place,
      _ => false,
    })
    .collect::<SmallVec<_>>()
}
