use std::cmp::Ordering;

use flowistry::mir::utils::{BodyExt, OperandExt};
use rustc_data_structures::fx::FxHashMap as HashMap;
#[cfg(feature = "rustc-hir-origins")]
use rustc_middle::mir::HirOrigin;
use rustc_middle::mir::{Operand, Rvalue, StatementKind};
use rustc_span::Span;

use crate::{
  analysis::{
    find_hir_calls::find_method_call_spans,
    find_mir_calls::FindCalls,
    permissions::{
      Loan, MissingPermReason, MissingPermsInfo, Path, PermissionsBoundary,
      PermissionsCtxt, Point, RefinementRegion, Refiner,
    },
  },
  Range,
};

pub fn pair_permissions_to_calls(
  ctxt: &PermissionsCtxt,
  span_to_range: impl Fn(Span) -> Range + std::marker::Copy,
) -> Vec<PermissionsBoundary> {
  let locations_to_body_info = ctxt.body_with_facts.body.find_calls();

  let _never_write = &ctxt.permissions_output.never_write;
  let _never_drop = &ctxt.permissions_output.never_drop;

  let method_spans = find_method_call_spans(ctxt.tcx, ctxt.body_id);

  let body = &ctxt.body_with_facts.body;

  // for all method calls foo.bar(..) in the HIR...
  method_spans
    .iter()
    .filter_map(|&(fn_span, fn_sig)| {
      // get the MIR call instructions with an overlapping span
      let mut potential_call_sites = locations_to_body_info
        .iter()
        .filter(|&(_, call_info)| call_info.fn_span.overlaps(fn_span))
        .collect::<Vec<_>>();

      // Order the function calls by which happens first in the CFG.
      potential_call_sites.sort_by(|t1, t2| {
        let t1b = t1.0.is_predecessor_of(*t2.0, body);
        let t2b = t2.0.is_predecessor_of(*t1.0, body);
        if t1b && t2b {
          Ordering::Equal
        } else if t1b {
          Ordering::Less
        } else {
          Ordering::Greater
        }
      });

      let (loc, call_info) = potential_call_sites.first()?;

      // point_call is location of the MIR call
      let point_call = ctxt.location_to_point(**loc);

      // path is the MIR receiver
      let path = &ctxt.place_to_path(&call_info.receiver_place);

      // HACK: there is a small issue with the path retrived from the call site.
      // A piece of code like the following:
      //
      // ```rust
      // let v = vec![];
      // v.push(0);
      // ```
      //
      // will get desugared into code that looks (roughly) like this:
      //
      // ```rust
      // let v = vec![];
      // let _t = &mut v;
      // Vec::push(move _t, 0);
      // ```
      // What this means, is that using `_t` for the permissions at the call site
      // will show that there is *always* enough permissions. Even though, in the
      // above example `v` is missing Write permissions. And the borrow is illegal.
      //
      // The hack below finds the borrowed / moved path which would represent
      // the receiver `v` in the example.

      let place_0 = ctxt.path_to_place(*path);
      let place_0_local = place_0.local;

      // find all the places where the local of the receiver is defined
      let definitions = ctxt
        .polonius_input_facts
        .var_defined_at
        .iter()
        .filter(|(v, _)| (*v == place_0_local))
        .filter_map(|(_, p)| {
          let l = ctxt.point_to_location(*p);
          let stmt = ctxt.body_with_facts.body.stmt_at(l).left()?;
          match &stmt.kind {
            StatementKind::Assign(box (_, rhs)) => Some((*p, rhs)),
            _ => None,
          }
        })
        .collect::<Vec<_>>();

      // XXX: the assumption here is that when assigning to a
      // temporary path for a function call it is either:
      // - a reference that gets borrowed from another place.
      // - a move from another place.
      // - a copy *unimplemented*
      //   copies would create more permissions which we'll need
      //   to handle.
      //
      // However, if the statement at the derived location is
      // a terminator, then the default is to take the original
      // place and point of the method call. This could happen,
      // for example, in a line such as: `Vec::default().push(0)`.
      let mut was_copied = false;
      let (place_1, point) = match definitions.get(0) {
        Some((point_assign, rhs)) => {
          let place = match rhs {
            Rvalue::Ref(_, _, place) => *place,
            Rvalue::Use(op) => {
              if let Operand::Copy(_) = op {
                was_copied = true;
              };
              op.to_place().unwrap()
            }
            _ => unimplemented!(),
          };
          (place, *point_assign)
        }
        None => (place_0, point_call),
      };

      let path = ctxt.place_to_path(&place_1);
      let mut actual = ctxt.permissions_at_point(path, point);

      // If an operand is copied, that means that they "gain" drop permissions.
      // An example of this would be the following:
      // ```
      // fn main() {
      //     let x: &i32 = &0; // x: R--
      //     x.abs();          // (copy x).abs(); // (copy x): R-D
      // }
      // ```
      was_copied &= was_copied != actual.drop;
      actual.drop |= was_copied;
      let expected = fn_sig.inputs()[0].into();

      let explanations =
        build_missing_perms_explainers(ctxt, path, point, span_to_range);

      // FIXME HACK: we assume that the `.` is one character to the left of the method call.
      // this is of course not *strictly* true and should be fixed.
      let location = span_to_range(fn_span).char_start - 1;

      Some(PermissionsBoundary {
        location,
        actual,
        expected,
        was_copied,
        explanations,
      })
    })
    .collect()
}

fn build_missing_perms_explainers(
  ctxt: &PermissionsCtxt,
  path: Path,
  point: Point,
  span_to_range: impl (Fn(Span) -> Range) + std::marker::Copy,
) -> MissingPermsInfo {
  let empty_hash = &HashMap::default();

  // Determine if the max permissions for this path would even allow W/D.
  let never_write = &ctxt.permissions_output.never_write;
  let never_drop = &ctxt.permissions_output.never_drop;

  let cannot_read = ctxt
    .permissions_output
    .cannot_read
    .get(&point)
    .unwrap_or(empty_hash);
  let cannot_write = ctxt
    .permissions_output
    .cannot_write
    .get(&point)
    .unwrap_or(empty_hash);
  let cannot_drop = ctxt
    .permissions_output
    .cannot_drop
    .get(&point)
    .unwrap_or(empty_hash);

  // To determine missing permissions we determine them in order of severity
  // 3. insufficient type
  // 1. refinement due to move
  // 2. refinement due to loan
  use rustc_data_structures::fx::FxHashSet as HashSet;

  let find_insufficient_type = |hshr: &HashSet<Path>| {
    hshr
      .contains(&path)
      .then_some(MissingPermReason::InsufficientType)
  };

  macro_rules! is_loan_refined {
    ($hsh:expr) => {
      find_loan_refinement(ctxt, path, $hsh, span_to_range)
        .map(MissingPermReason::Refined)
    };
  }

  // TODO: compute the same refinement information as loans but for
  // move positions. This may require more information tracking in the
  // permissions analysis.
  macro_rules! is_move_refined {
    () => {
      None
    };
  }

  let read = is_loan_refined!(cannot_read).or_else(|| is_move_refined!());

  let write = find_insufficient_type(never_write)
    .or_else(|| is_loan_refined!(cannot_write))
    .or_else(|| is_move_refined!());

  let drop = find_insufficient_type(never_drop)
    .or_else(|| is_loan_refined!(cannot_drop))
    .or_else(|| is_move_refined!());

  MissingPermsInfo { read, write, drop }
}

pub fn find_loan_refinement(
  ctxt: &PermissionsCtxt,
  path: Path,
  refined_by: &HashMap<Path, Loan>,
  span_to_range: impl Fn(Span) -> Range,
) -> Option<RefinementRegion> {
  let loan_regions = ctxt.loan_regions.as_ref().unwrap();

  // TODO: the permissions_output only keeps one loan per path, however, there could
  // theoretically be several which are refining. When this is fixed, the analysis
  // here should pick the loan which lasts the longest.
  refined_by.get(&path).map(|loan| {
    let (p_0, p_e) = loan_regions.get(loan).unwrap();

    // TODO: using `reserve_location` is not exactly accurate because this
    // could be a two-phase borrow. This needs to use the `activation_location`.
    let loan_loc = ctxt.borrow_set[*loan].reserve_location;
    let start_loc = ctxt.point_to_location(*p_0);
    let end_loc = ctxt.point_to_location(*p_e);

    let loan_span = ctxt.location_to_span(loan_loc);
    let start_span = ctxt.location_to_span(start_loc);
    let end_span = ctxt.location_to_span(end_loc);

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
      loan_span
    } else {
      start_span
    };

    let loan_location = span_to_range(loan_span);
    let start = span_to_range(start_span);
    let end = span_to_range(end_span);

    let active_nodes = loan_to_spans(ctxt, *loan, start_span, end_span)
      .into_iter()
      .map(span_to_range)
      .collect::<Vec<_>>();

    RefinementRegion {
      refiner_point: Refiner::Loan(loan_location),
      refined_ranges: active_nodes,
      start,
      end,
    }
  })
}

pub fn loan_to_spans(
  ctxt: &PermissionsCtxt,
  loan: Loan,
  min_span: Span,
  max_span: Span,
) -> Vec<Span> {
  let points = ctxt
    .polonius_output
    .loan_live_at
    .iter()
    .filter_map(|(point, loans)| loans.contains(&loan).then_some(*point));

  let mut loan_spans = points_to_spans(ctxt, points);

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

  smooth_spans(loan_spans)
}

fn points_to_spans(
  ctxt: &PermissionsCtxt,
  points: impl Iterator<Item = Point>,
) -> Vec<Span> {
  let hir = ctxt.tcx.hir();
  let body = &ctxt.body_with_facts.body;
  let mut spans = Vec::default();

  points.for_each(|point| {
    let loc = ctxt.point_to_location(point);

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
