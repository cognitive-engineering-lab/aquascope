//! Core contextual analysis for Aquascope.

pub mod boundaries;
pub mod find_bindings;
pub mod ir_mapper;
pub mod permissions;
mod scrape_hir;
pub mod stepper;

use std::{
  cell::RefCell,
  collections::HashMap,
  ops::{Add, Deref, DerefMut},
};

pub use boundaries::compute_permission_boundaries;
pub use find_bindings::find_bindings;
use flowistry::mir::{
  borrowck_facts::get_body_with_borrowck_facts,
  utils::{BodyExt, SpanExt},
};
use ir_mapper::{GatherMode, IRMapper};
use permissions::{Loan, PermissionsCtxt, Point, RefinementRegion, Refiner};
use rustc_borrowck::consumers::BodyWithBorrowckFacts;
use rustc_hir::BodyId;
use rustc_middle::ty::TyCtxt;
use rustc_span::Span;
use serde::Serialize;
pub use stepper::compute_permission_steps;
use ts_rs::TS;

use crate::Range;

thread_local! {
  pub static BODY_ID_STACK: RefCell<Vec<BodyId>> =
    RefCell::new(Vec::default());
}

// NOTE: these types should be seen as
// little databases that the frontend can use in
// conjunction with other data.

#[derive(
  Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, TS,
)]
#[ts(export)]
pub struct LoanKey(pub u32);

#[derive(
  Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, TS,
)]
#[ts(export)]
pub struct MoveKey(pub u32);

#[derive(Clone, Debug, Serialize, TS)]
#[ts(export)]
pub struct LoanPoints(pub HashMap<LoanKey, Range>);

#[derive(Clone, Debug, Serialize, TS)]
#[ts(export)]
pub struct MovePoints(pub HashMap<LoanKey, Range>);

#[derive(Clone, Debug, Serialize, TS)]
#[ts(export)]
pub struct LoanRegions(pub HashMap<LoanKey, RefinementRegion>);

#[derive(Clone, Debug, Serialize, TS)]
#[ts(export)]
pub struct MoveRegions(pub HashMap<MoveKey, RefinementRegion>);

impl From<&Loan> for LoanKey {
  fn from(f: &Loan) -> LoanKey {
    LoanKey(f.as_u32())
  }
}

impl Add for LoanKey {
  type Output = LoanKey;
  fn add(self, rhs: LoanKey) -> Self::Output {
    let l = self.0;
    let r = rhs.0;
    LoanKey(l + r)
  }
}

impl Deref for LoanKey {
  type Target = u32;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl DerefMut for LoanKey {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

impl Deref for LoanPoints {
  type Target = HashMap<LoanKey, Range>;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl DerefMut for LoanPoints {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

impl Deref for LoanRegions {
  type Target = HashMap<LoanKey, RefinementRegion>;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl DerefMut for LoanRegions {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

// Join two analysis on two separate bodies together!
pub trait BodyAnalysisJoin {
  fn analysis_join(self, other: Self) -> Self;
}

impl<T> BodyAnalysisJoin for Vec<T> {
  fn analysis_join(mut self, other: Self) -> Self {
    self.extend(other.into_iter());
    self
  }
}

impl<O: BodyAnalysisJoin> BodyAnalysisJoin for anyhow::Result<O> {
  fn analysis_join(self, other: Self) -> Self {
    let v1 = self?;
    let v2 = other?;
    Ok(v1.analysis_join(v2))
  }
}

trait KeyShifter {
  fn shift_keys(self, loan_shift: LoanKey) -> Self;
}

impl<O> BodyAnalysisJoin for AnalysisOutput<O>
where
  O: KeyShifter + std::fmt::Debug + Clone + Serialize + TS,
{
  fn analysis_join(mut self, other: Self) -> Self {
    let shift_by = self
      .loan_points
      .iter()
      .fold(LoanKey(1), |acc, (k, _)| std::cmp::max(acc, *k));

    // Shift the RHS values to be greater than those currently stored

    let num_loan_points = self.loan_points.len() + other.loan_points.len();
    let num_loan_regions = self.loan_regions.len() + other.loan_regions.len();

    let loan_points = other
      .loan_points
      .0
      .into_iter()
      .map(|(k, v)| (k + shift_by, v))
      .collect::<HashMap<_, _>>();

    let loan_regions = other
      .loan_regions
      .0
      .into_iter()
      .map(|(k, v)| (k + shift_by, v))
      .collect::<HashMap<_, _>>();

    let values = other.values.into_iter().map(|v| v.shift_keys(shift_by));

    self.values.extend(values);
    self.loan_points.extend(loan_points);
    self.loan_regions.extend(loan_regions);

    assert_eq!(num_loan_points, self.loan_points.len());
    assert_eq!(num_loan_regions, self.loan_regions.len());

    self
  }
}

pub fn compute_permissions<'a, 'tcx>(
  tcx: TyCtxt<'tcx>,
  body_id: BodyId,
  body_with_facts: &'a BodyWithBorrowckFacts<'tcx>,
) -> PermissionsCtxt<'a, 'tcx> {
  BODY_ID_STACK.with(|stack| {
    stack.borrow_mut().push(body_id);

    let permissions = permissions::compute(tcx, body_id, body_with_facts);

    if cfg!(debug_assertions) {
      permissions::utils::dump_permissions_with_mir(&permissions);
    }

    permissions
  })
}

// ------------------------------------------------

pub struct AquascopeAnalysis<'a, 'tcx: 'a> {
  pub(crate) permissions: PermissionsCtxt<'a, 'tcx>,
  pub(crate) ir_mapper: IRMapper<'a, 'tcx>,
}

#[derive(Clone, Debug, Serialize, TS)]
#[ts(export)]
pub struct AnalysisOutput<O>
where
  O: std::fmt::Debug + Clone + Serialize + TS,
{
  pub values: Vec<O>,
  pub loan_points: LoanPoints,
  pub loan_regions: LoanRegions,
}

impl<'a, 'tcx: 'a> AquascopeAnalysis<'a, 'tcx> {
  pub fn run<F, O>(
    tcx: TyCtxt<'tcx>,
    body_id: BodyId,
    f: F,
  ) -> AnalysisOutput<O>
  where
    O: std::fmt::Debug + Clone + Serialize + TS,
    F: Fn(&AquascopeAnalysis<'a, 'tcx>) -> Vec<O>,
  {
    let def_id = tcx.hir().body_owner_def_id(body_id);
    let bwf = get_body_with_borrowck_facts(tcx, def_id);
    let permissions = compute_permissions(tcx, body_id, bwf);
    let body = &permissions.body_with_facts.body;
    let ir_mapper = IRMapper::new(tcx, body, GatherMode::IgnoreCleanup);
    let analysis_ctxt = AquascopeAnalysis {
      permissions,
      ir_mapper,
    };

    // FIXME: remove
    crate::analysis::permissions::utils::dump_mir_debug(
      &analysis_ctxt.permissions,
    );

    let values = f(&analysis_ctxt);
    let (loan_points, loan_regions) = analysis_ctxt.construct_loan_info();
    AnalysisOutput {
      values,
      loan_points,
      loan_regions,
    }
  }

  pub fn span_to_range(&self, span: Span) -> Range {
    let source_map = self.permissions.tcx.sess.source_map();
    flowistry::source_map::Range::from_span(span, source_map)
      .ok()
      .unwrap_or_else(|| {
        log::error!("The span {span:?} could not be turned into a valid Range");
        flowistry::source_map::Range::default()
      })
      .into()
  }

  fn construct_loan_info(&self) -> (LoanPoints, LoanRegions) {
    let loan_regions = &self.permissions.loan_regions.as_ref().unwrap();

    let loans_to_spans = loan_regions
      .iter()
      .filter_map(|(loan, _)| {
        // TODO: using `reserve_location` is not exactly accurate because this
        // could be a two-phase borrow. This needs to use the `activation_location`.
        let loan_loc = self.permissions.borrow_set[*loan].reserve_location;
        let loan_span = self.permissions.location_to_span(loan_loc);

        let span = loan_span
          .as_local(self.permissions.body_with_facts.body.span)
          .unwrap_or(loan_span);

        (!span.is_empty()).then_some((loan, span))
      })
      .collect::<HashMap<_, _>>();

    let loan_to_regions = loans_to_spans
      .iter()
      .map(|(loan, loan_span)| {
        let (p_0, p_e) = loan_regions.get(loan).unwrap();

        let start_loc = self.permissions.point_to_location(*p_0);
        let end_loc = self.permissions.point_to_location(*p_e);

        let start_span = self.permissions.location_to_span(start_loc);
        let end_span = self.permissions.location_to_span(end_loc);

        // XXX: currently trying out using the initial loan location as the activation
        // location. The reason for this can be demonstrated by a simple let.
        // ```
        // let s = String::from("hi");
        // let b = &mut s;
        //
        // == Pseudo MIR ==>
        //
        // s = String::from("hi");
        // _t = &mut s;   <-- loan location
        // b = move _t    <-- initial activation
        // ```
        //
        // The weird thing, is that the actual initial activation occurs at
        // assignment, which is reversed from the source code representation.
        // Therefore, to try and hack my way out of this, just take the "start_span"
        // to be the thing which is first (at the source-level) after the loan issue.
        let start_span = if start_span.lo() < loan_span.lo() {
          *loan_span
        } else {
          start_span
        };

        let active_nodes = self
          .loan_to_spans(**loan, start_span, end_span)
          .into_iter()
          .map(|s| self.span_to_range(s))
          .collect::<Vec<_>>();

        let loan_key: LoanKey = (*loan).into();

        let rr = RefinementRegion {
          refiner_point: Refiner::Loan(loan_key),
          refined_ranges: active_nodes,
        };

        (loan_key, rr)
      })
      .collect::<HashMap<_, _>>();

    let loan_to_ranges = loans_to_spans
      .into_iter()
      .map(|(loan, span)| {
        let loan_key: LoanKey = loan.into();
        let range = self.span_to_range(span);
        (loan_key, range)
      })
      .collect::<HashMap<_, _>>();

    (LoanPoints(loan_to_ranges), LoanRegions(loan_to_regions))
  }

  pub fn loan_to_spans(
    &self,
    loan: Loan,
    min_span: Span,
    max_span: Span,
  ) -> Vec<Span> {
    let points = self
      .permissions
      .polonius_output
      .loan_live_at
      .iter()
      .filter_map(|(point, loans)| loans.contains(&loan).then_some(*point));

    let mut loan_spans = self.points_to_spans(points);

    // Pushing the `min_span` and `max_span` is also a HACK I should
    // get rid of. Only after there are unit tests to make sure the change
    // doesn't break any necessary examples.
    loan_spans.push(min_span);
    loan_spans.push(max_span);

    // HACK: ideally we don't need to use the min / max spans to
    // filter the others. This is needed when you have a HIR span
    // whose outer values come before when we would like them to be shown.
    // ```
    // let x = if true {
    //   `[ &mut y ]`
    // } else {
    //     &mut z
    // }
    //
    // `[ y.abs(); ]` // error
    //
    // `[ use(x); ]`
    //
    // y.abs(); // fine if `x` no longer used
    // ```
    //
    // In the above example, the lines surrounded by `[ ... ]` should be highlighted
    // in the editor. What this means, is that only the "then" child branch of the "if"
    // HIR node should be included in this span. However, if we don't constrain these
    // values it can happen that the entire `[ let x = if true { ... } else { ... }  ]`
    // is included in the returned ranges.
    let loan_spans = loan_spans
      .into_iter()
      .filter(|span| min_span.lo() <= span.lo() && span.hi() <= max_span.hi())
      .collect::<Vec<_>>();

    Self::smooth_spans(loan_spans)
  }

  fn points_to_spans(&self, points: impl Iterator<Item = Point>) -> Vec<Span> {
    let hir = self.permissions.tcx.hir();
    let body = &self.permissions.body_with_facts.body;
    let mut spans = Vec::default();

    points.for_each(|point| {
      let loc = self.permissions.point_to_location(point);

      macro_rules! insert_if_valid {
        ($sp:expr) => {
          if !$sp.is_empty() {
            spans.push($sp);
          }
        };
      }
      let hir_id = body.location_to_hir_id(loc);
      let span = hir.span(hir_id);
      insert_if_valid!(span);
    });

    spans
  }

  fn smooth_spans(mut spans: Vec<Span>) -> Vec<Span> {
    if spans.is_empty() {
      return spans;
    }

    // First, sort the spans by starting value.
    spans.sort_by_key(|a| a.lo());

    let mut smoothed_spans = Vec::default();
    let mut acc = *spans.first().unwrap();

    for span in &spans[1 ..] {
      if acc.overlaps(*span) || acc.hi() == span.lo() {
        acc = acc.to(*span);
      } else {
        smoothed_spans.push(acc);
        acc = *span;
      }
    }

    // don't forget the last accumulator
    smoothed_spans.push(acc);

    smoothed_spans
  }
}
