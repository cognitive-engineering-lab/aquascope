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
  iter::IntoIterator,
  ops::{Add, Deref, DerefMut},
};

pub use boundaries::compute_permission_boundaries;
use boundaries::PermissionsBoundary;
pub use find_bindings::find_bindings;
use ir_mapper::{GatherMode, IRMapper};
use permissions::{
  Loan, Move, PermissionsCtxt, Point, RefinementRegion, Refiner,
};
use rustc_borrowck::consumers::BodyWithBorrowckFacts;
use rustc_data_structures::fx::FxHashMap;
use rustc_hir::BodyId;
use rustc_middle::ty::TyCtxt;
use rustc_span::{self, Span};
use rustc_utils::{
  mir::borrowck_facts,
  source_map::range::{CharPos, CharRange},
  BodyExt, SpanExt,
};
use serde::Serialize;
pub use stepper::compute_permission_steps;
use stepper::PermissionsLineDisplay;
use ts_rs::TS;

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
pub struct LoanPoints(pub HashMap<LoanKey, CharRange>);

#[derive(Clone, Debug, Serialize, TS)]
#[ts(export)]
pub struct MovePoints(pub HashMap<MoveKey, CharRange>);

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

impl From<&Move> for MoveKey {
  fn from(f: &Move) -> MoveKey {
    MoveKey(f.as_u32())
  }
}

impl From<Move> for MoveKey {
  fn from(f: Move) -> MoveKey {
    MoveKey(f.as_u32())
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
  type Target = HashMap<LoanKey, CharRange>;
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

pub trait Bounded {
  type Bound: Ord + Eq;
  fn lo(&self) -> Self::Bound;
  fn hi(&self) -> Self::Bound;
  fn to(&self, other: Self) -> Self;
  fn overlaps(&self, other: Self) -> bool;
}

impl Bounded for Span {
  type Bound = rustc_span::BytePos;
  fn lo(&self) -> Self::Bound {
    Span::lo(*self)
  }

  fn hi(&self) -> Self::Bound {
    Span::hi(*self)
  }

  fn overlaps(&self, other: Self) -> bool {
    Span::overlaps(*self, other)
  }

  fn to(&self, other: Self) -> Self {
    Span::to(*self, other)
  }
}

impl Bounded for CharRange {
  type Bound = CharPos;
  fn lo(&self) -> Self::Bound {
    self.start
  }

  fn hi(&self) -> Self::Bound {
    self.end
  }

  fn overlaps(&self, other: Self) -> bool {
    !(self.end < other.start || other.end < self.start)
  }

  fn to(&self, other: Self) -> Self {
    CharRange {
      start: std::cmp::min(self.start, other.start),
      end: std::cmp::max(self.end, other.end),
      ..*self
    }
  }
}

pub fn smooth_elements<T>(mut elements: Vec<T>) -> Vec<T>
where
  T: Bounded + std::marker::Copy,
{
  if elements.is_empty() {
    return elements;
  }

  // First, sort the elements by starting value.
  elements.sort_by_key(|a| a.lo());

  let mut smoothed_elements = Vec::default();
  let mut acc = *elements.first().unwrap();

  for elem in &elements[1 ..] {
    if acc.overlaps(*elem) || acc.hi() == elem.lo() {
      acc = acc.to(*elem);
    } else {
      smoothed_elements.push(acc);
      acc = *elem;
    }
  }

  // don't forget the last accumulator
  smoothed_elements.push(acc);

  smoothed_elements
}

pub fn compute_permissions<'tcx>(
  tcx: TyCtxt<'tcx>,
  body_id: BodyId,
  body_with_facts: &'tcx BodyWithBorrowckFacts<'tcx>,
) -> PermissionsCtxt<'tcx> {
  BODY_ID_STACK.with(|stack| {
    stack.borrow_mut().push(body_id);

    let permissions = permissions::compute(tcx, body_id, body_with_facts);

    permissions
  })
}

// ------------------------------------------------

#[derive(Clone, Debug, Serialize, TS)]
#[ts(export)]
#[serde(tag = "type")]
pub enum AquascopeError {
  // An error occurred before the intended analysis could run.
  BuildError { range: Option<CharRange> },
  AnalysisError { msg: String },
}

pub type AquascopeResult<T> = ::std::result::Result<T, AquascopeError>;

pub struct AquascopeAnalysis<'tcx> {
  pub(crate) permissions: PermissionsCtxt<'tcx>,
  pub(crate) ir_mapper: IRMapper<'tcx>,
}

impl From<anyhow::Error> for AquascopeError {
  fn from(e: anyhow::Error) -> Self {
    AquascopeError::AnalysisError { msg: e.to_string() }
  }
}

#[derive(Clone, Debug, Serialize, TS)]
#[ts(export)]
pub struct AnalysisOutput {
  pub body_range: CharRange,
  pub boundaries: Vec<PermissionsBoundary>,
  pub steps: Vec<PermissionsLineDisplay>,
  pub loan_points: LoanPoints,
  pub loan_regions: LoanRegions,
  pub move_points: MovePoints,
  pub move_regions: MoveRegions,
}

impl<'tcx> AquascopeAnalysis<'tcx> {
  pub fn new(tcx: TyCtxt<'tcx>, body_id: BodyId) -> Self {
    let def_id = tcx.hir().body_owner_def_id(body_id);
    let bwf = borrowck_facts::get_body_with_borrowck_facts(tcx, def_id);
    let permissions = compute_permissions(tcx, body_id, bwf);
    let body = &permissions.body_with_facts.body;

    let ir_mapper = IRMapper::new(tcx, body, GatherMode::IgnoreCleanup);
    AquascopeAnalysis {
      permissions,
      ir_mapper,
    }
  }

  pub fn run(
    tcx: TyCtxt<'tcx>,
    body_id: BodyId,
  ) -> AquascopeResult<AnalysisOutput> {
    let analysis_ctxt = Self::new(tcx, body_id);
    let body = &analysis_ctxt.permissions.body_with_facts.body;

    if body.tainted_by_errors.is_some() {
      let span = body.span;
      let source_map = tcx.sess.source_map();
      let range = CharRange::from_span(span, source_map).unwrap().into();
      return Err(AquascopeError::BuildError { range });
    }

    crate::analysis::permissions::utils::dump_mir_debug(
      &analysis_ctxt.permissions,
    );

    let boundaries = compute_permission_boundaries(&analysis_ctxt)?;
    let steps = compute_permission_steps(&analysis_ctxt)?;

    let (loan_points, loan_regions) = analysis_ctxt.construct_loan_info();
    let (move_points, move_regions) = analysis_ctxt.construct_move_info();

    let body_range = analysis_ctxt.span_to_range(body.span);

    Ok(AnalysisOutput {
      body_range,
      boundaries,
      steps,
      loan_points,
      loan_regions,
      move_points,
      move_regions,
    })
  }

  pub fn is_span_visible(&self, span: Span) -> bool {
    // let source_map = self.permissions.tcx.sess.source_map();
    span.is_dummy() || span.is_empty() // || !span.is_visible(source_map)
  }

  pub fn span_to_range(&self, span: Span) -> CharRange {
    // if span.is_dummy() || span.is_empty() {
    //   panic!("HERE YOU GO");
    // }
    let source_map = self.permissions.tcx.sess.source_map();
    CharRange::from_span(span, source_map).unwrap()
  }

  fn construct_loan_info(&self) -> (LoanPoints, LoanRegions) {
    let loan_regions = &self.permissions.loan_regions.as_ref().unwrap();

    let loans_to_spans = loan_regions
      .iter()
      .filter_map(|(loan, _)| {
        // TODO: using `reserve_location` is not exactly accurate because this
        // could be a two-phase borrow. This needs to use the `activation_location`.
        let loan_loc = self.permissions.borrow_set[*loan].reserve_location();
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

        let loan_live_at = &self.permissions.polonius_output.loan_live_at;
        let active_nodes = self
          .key_to_spans(
            **loan,
            &loan_live_at.clone().into_iter().collect(),
            start_span,
            end_span,
          )
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

  // FIXME(gavinleroy): the two `construct_XXX` methods could
  // be abstracted away better into one generic algorithm.
  fn construct_move_info(&self) -> (MovePoints, MoveRegions) {
    let ctxt = &self.permissions;
    let move_points = ctxt
      .move_data
      .moves
      .iter_enumerated()
      .filter_map(|(movep, move_out)| {
        let span = ctxt.location_to_span(move_out.source);
        let move_key: MoveKey = movep.into();
        self.is_span_visible(span).then_some((move_key, span))
      })
      .collect::<HashMap<_, _>>();

    let mut move_to_spans = HashMap::<MoveKey, Vec<Point>>::default();

    for (&point, path_to_move) in ctxt.permissions_output.move_refined.iter() {
      for (_, movep) in path_to_move.iter() {
        let move_key = movep.into();
        move_to_spans.entry(move_key).or_default().push(point);
      }
    }

    let move_regions = move_to_spans
      .into_iter()
      .filter_map(|(move_key, points)| {
        // HACK FIXME: visually constraining the region to be
        // strictly after the initial action.
        // Also, if the move point was removed for not being visible then
        // we can just ignore computing the highlighted ranges as well.
        let lo = move_points.get(&move_key).map(|s| s.lo())?;

        let points = self
          .points_to_spans(
            points
              .into_iter()
              .filter(|point| ctxt.is_point_operational(*point)),
          )
          .into_iter()
          .filter(|span| lo <= span.lo())
          .collect::<Vec<_>>();
        let smoothed = smooth_elements(points);
        let refined_ranges = smoothed
          .into_iter()
          .map(|span| self.span_to_range(span))
          .collect::<Vec<_>>();
        let region = RefinementRegion {
          refiner_point: Refiner::Move(move_key),
          refined_ranges,
        };
        Some((move_key, region))
      })
      .collect::<HashMap<_, _>>();

    let move_points = move_points
      .into_iter()
      .map(|(k, span)| (k, self.span_to_range(span)))
      .collect::<HashMap<_, _>>();

    (MovePoints(move_points), MoveRegions(move_regions))
  }

  pub fn key_to_spans<K>(
    &self,
    loan: K,
    live_at: &FxHashMap<Point, Vec<K>>,
    min_span: Span,
    max_span: Span,
  ) -> Vec<Span>
  where
    K: PartialEq + Eq + std::marker::Copy,
  {
    let points = live_at
      .iter()
      .filter_map(|(point, loans)| loans.contains(&loan).then_some(*point));

    let mut spans = self.points_to_spans(points);

    // Pushing the `min_span` and `max_span` is also a HACK I should
    // get rid of. Only after there are unit tests to make sure the change
    // doesn't break any necessary examples.
    spans.push(min_span);
    spans.push(max_span);

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
    let spans = spans
      .into_iter()
      .filter(|span| (min_span.lo() <= span.lo() && span.hi() <= max_span.hi()))
      .map(|span| {
        span
          .as_local(self.permissions.body_with_facts.body.span)
          .unwrap_or(span)
      })
      .collect::<Vec<_>>();

    smooth_elements(spans)
  }

  /// Convert a potentially non-contiguous collection of [`Point`]s into [`Span`]s.
  fn points_to_spans(
    &self,
    points: impl IntoIterator<Item = Point>,
  ) -> Vec<Span> {
    let hir = self.permissions.tcx.hir();
    let body = &self.permissions.body_with_facts.body;
    let mut spans = Vec::default();

    points.into_iter().for_each(|point| {
      let loc = self.permissions.point_to_location(point);

      macro_rules! insert_if_valid {
        ($sp:expr) => {
          if !$sp.is_empty() && !$sp.is_dummy() {
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
}
