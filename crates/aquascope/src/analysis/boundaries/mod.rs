//! Analysis for the “Needs-at” relations.
//!
//! In other words, finding expected vs existing permission for a path usage.
//!
//! # Walthrough
//!
//! The “boundaries” analysis is relatively simple and as such poses as a
//! good demonstration for how to use permissions in a larger analysis.
//!
//! This analysis must do three things:
//! 1. Find all places where a path is used at the source-level, determine what
//!    permissions are necessary for this operation to be allowed.
//! 2. Determine the MIR-level [`Place`] and [`Location`] for this usage.
//! 3. Compute the permissions the given Place actually has at the use point.
//!
//! These three steps are represented as two distinct stages. In the [`path_visitor`]
//! module all of the [`PathBoundary`]s are computed. This returns information
//! such as the expected permissions, and the [`HirId`] of the usage. There's some
//! other stuff available in the struct, mostly to resolve the Flow permissions, but
//! those aren't relevant for this basic discussion.
//!
//! ## Finding path usages
//!
//! ### Example
//!
//! Let's walk through what this would look like for a simple function:
//!
//! ```ignore
//! fn append_hello(s: &mut String) {
//!   println!("Adding hello to string { s }");
//!   s.push_str("hello!");
//! }
//! ```
//!
//! Within the function there are two path usages. The first within the `println!`
//! when `s` is read and the second when the method `push_str` is invoked on `s`.
//! Therefore, a call to [`get_path_boundaries`] should return a vector of two elements:
//!
//! ```text
//! [
//!   PathBoundary {
//!     hir_id: { &s }
//!     expected: Permissions { read: true, write: false, drop: false },
//!     ...
//!  },
//!  PathBoundary {
//!    hir_id: { &mut *s }
//!    expected: Permissions { read: true, write: true, drop: false },
//!    ...
//!  },
//! ]
//! ```
//!
//! Let's go through each of these boundaries and discuss what this information means and
//! how it was found. The first usage of `s` occurs within a macro. Macros, and other
//! desugarings, are in tension with how we want to display information. When traversing the
//! HIR, you won't see a nice source code location that looks like `println!("... {s}")`,
//! what you do see is an ugly monster, such as the following:
//!
//! ```text
//! ::std::io::_print(
//!     ::new_v1(
//!       &["... ", "\n"], &[::new_display(&s)]
//!     )
//! );
//! ```
//!
//! When desugaring, the compiler can insert new variables and places which are
//! _invisible_ at the source-level. The current solution to this is to use
//! [`rustc_hir::hir::Expr::is_syntactic_place_expr`] and [`rustc_span::Span::from_expansion`]
//! to find out if the path we're looking is a “syntactic place” (i.e., it looks like a place)
//! and if it came from some sort of expansion. Returning to our example, the HIR node that we
//! are going to find permissions for is `&s`. That is, the shared borrow that occurs within the macro
//! expansion. One last hiccup in the process of finding source spans is the span information
//! available in the HIR. For this macro, if you just look at the source location it will point
//! to somewhere from within rustc. We utilize the [`SpanExt::as_local`] method to sanitize spans
//! and lift them back to original source code.
//! Lastly, the struct [`ExpectedPermissions`] has a series of construction methods
//! which show concisely when certain permissions are expected for the respective uses.
//! In this case, a shared borrow only requires the Read permission.
//!
//! The second boundary returned corresponds to the usage of `s` as the receiver of the
//! invoked meethod `push_str`. At the HIR, this is desugared into a function call
//! passing the  receiver as the first argument, like so: `String::push_str(&mut *s, "hello!")`.
//! There isn't anything tricky about visualizing this information and the code is
//! straightforward, if you want to peruse through the HIR visitor [`path_visitor::HirExprScraper`].
//! The reason method calls are interesting is, at the time of writing, we visualize the
//! boundary stack in-between the receiver and the dot (`.`), instead of to the left of the
//! path like every other case. Note, there's also a reborrow introduced but that's only
//! relevant in the next section.
//!
//! ## Resolving actual permissions
//!
//! The second stage of the boundaries analysis is taking the found [`PathBoundaries`]
//! and converting them into a [`PermissionsBoundary`]. This is the step that does
//! most of the heavy lifting. So try to follow along!
//!
//! The crux of the entire analysis is converting a [`HirId`], specifially a HIR node
//! that we _know_ contains a path use, to the corresponding MIR [`Place`] and [`Location`].
//! Unfortunately, there isn't a “really good way” to do this and before we return to the
//! running example I'll outline the strategy that is currently taken.
//!
//! Given a `HirId`, we can use the [`IRMapper`] to gather all of the MIR instructions
//! that correspond to the given HIR node. That means, given a HIR node such as `let a = &b`,
//! the `IRMapper` can tell you that the below MIR instructions were generated:
//!
//! ```text
//! StorageLive(a);
//! _t0 = &b;
//! a = move _t0;
//! FakeRead(ForLet, a)
//! ```
//!
//! When doing resolution we search through the generated MIR instructions to find
//! all Places that belong to a source-visible path that belongs to a source-visible
//! variable. As you can see in the above mini-example, compiler temporaries are
//! introduced that we don't want to consider. After finding these so-called
//! “candidate places” we need to actually pick one that belongs to the _specific_ usage
//! we're interested in (more on this in the example). To date, every bug reported for
//! the boundaries analysis had to do with picking a place from the list of candidates.
//!
//! ## Example
//!
//! Returning to our example function, remember that we have two `PathBoundaries`,
//! representing `&s` and `&mut *s`.
//!
//! ```ignore
//! fn append_hello(s: &mut String) {
//!   println!("Adding hello to string { s }");
//!   s.push_str("hello!");
//! }
//! ```
//!
//! The first boundary is fortunately very simple. The MIR instructions generated for `&s` would
//! be something such as `_t0 = &s`. This means we have very little to search through, and the
//! list of candidate locations would be `[ s ]`. Thus we can easily resolve the place and location.
//!
//! _A quick side note_, in the above examples I've been using the source-level paths within
//! the MIR, but this **doesn't** happen. It's merely for readability. All paths are replaced
//! by compiler temporaries, and those coming from HIR paths will have extra debug information
//! attached to them. We can use the [`PlaceExt::is_source_visible`] method to see if a MIR
//! `Place` is something with that information attached. The attentive reader will note that
//! I've said “coming from the HIR” which means paths introduced by loop desugarings will
//! also have this attached debug info, this is only a minor inconvenience.
//!
//! The second boundary in our example is the `&mut *s` that occurs within the larger
//! method invocation. For this, the `IRMapper` will tell us that the following MIR
//! instructions are associated:
//!
//! ```text
//! let _t0 = &mut *s;
//! let _t1 = move _t0;
//! ...
//! String::push_str(move _t1, "hello!");
//! ```
//!
//! This demonstrates that there can be a level (or two, or three, ...) between
//! the action, in this case the method invocation, and the first _usage_ being
//! the reborrow. Method calls are quite straightforward because we can take
//! the first use of the path (and it's corresponding location), but for all
//! constructs that's not sufficient (e.g., array accesses first do a
//! bounds check, but the bounds check is on a different `Place` than what we're
//! after). One additional thing to note, however, is that for the method call our
//! resolved `Place` corresponds to `(*s)`, different from the path `s` visible
//! in the source code.
//!
//! For our example, after this selection we will have an exact `Place` and
//! `Location` for a path use. To get the actual permissions, we can use the
//! ever-so-handy [`PermissionsCtxt::permissions_data_at_point`] to get the
//! `PermissionsData`, a struct containing the exact permissions as well as
//! first-order provenance describing any active refinements.
//!
//! The entry location to this process of resolving a HIR path to a MIR place,
//! and retrieving the permissions can be found in the [`path_to_perm_boundary`] function.

pub(crate) mod path_visitor;

use anyhow::Result;
use either::Either;
use path_visitor::get_path_boundaries;
use rustc_hir::HirId;
use rustc_middle::{
  mir::{Body, Location, Mutability, Place, Rvalue, Statement, StatementKind},
  ty::{adjustment::AutoBorrowMutability, TyCtxt},
};
use rustc_span::Span;
use rustc_utils::{
  source_map::range::{BytePos, ByteRange, CharPos, CharRange},
  OperandExt, PlaceExt, SpanExt,
};
use serde::Serialize;
use smallvec::{smallvec, SmallVec};
use ts_rs::TS;

use crate::{
  analysis::{
    ir_mapper::{GatherDepth, IRMapper},
    permissions::{
      flow::FlowEdgeKind, Origin, Permissions, PermissionsCtxt,
      PermissionsData, Point, ENABLE_FLOW_DEFAULT, ENABLE_FLOW_PERMISSIONS,
    },
    AquascopeAnalysis,
  },
  errors,
};

/// A point where a region flow is introduced, potentially resulting in a violation.
#[derive(Debug, Clone, Serialize, TS)]
#[ts(export)]
pub struct FlowBoundary {
  // Used for simplicity in the frontend, later the extra information
  // in the flow kind can be shown with extra details.
  is_violation: bool,
  flow_context: CharRange,
  kind: FlowEdgeKind,
}

/// A point where the permissions reality are checked against their expectations.
#[derive(Debug, Clone, Serialize, TS)]
#[ts(export)]
pub struct PermissionsBoundary {
  pub location: CharPos,
  #[serde(skip)]
  byte_location: BytePos,
  pub expected: Permissions,
  pub actual: Permissions,
  pub data: PermissionsData,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub expecting_flow: Option<FlowBoundary>,
}

impl PermissionsBoundary {
  pub fn is_violation(&self) -> bool {
    macro_rules! is_missing {
      ($this:ident, $perm:ident) => {
        ($this.expected.$perm && !$this.actual.$perm)
      };
    }

    is_missing!(self, read)
      || is_missing!(self, write)
      || is_missing!(self, drop)
  }
}

// ----------------------------------
// Permission boundaries on path uses

#[derive(Copy, Clone, Debug)]
struct ExpectedPermissions(Permissions);

impl ExpectedPermissions {
  pub fn from_assignment() -> Self {
    Self(Permissions {
      read: true,
      write: true,
      drop: false,
    })
  }

  pub fn from_borrow(mutability: Mutability) -> Self {
    Self(Permissions {
      read: true,
      write: matches!(mutability, Mutability::Mut),
      drop: false,
    })
  }

  pub fn from_reborrow(mutability: AutoBorrowMutability) -> Self {
    Self(Permissions {
      read: true,
      write: matches!(mutability, AutoBorrowMutability::Mut { .. }),
      drop: false,
    })
  }

  pub fn from_move() -> Self {
    Self(Permissions {
      read: true,
      write: false,
      drop: true,
    })
  }

  pub fn from_copy() -> Self {
    Self(Permissions {
      read: true,
      write: false,
      drop: false,
    })
  }

  pub fn from_discriminant() -> Self {
    Self(Permissions {
      read: true,
      write: false,
      drop: false,
    })
  }
}

impl From<ExpectedPermissions> for Permissions {
  fn from(ex: ExpectedPermissions) -> Permissions {
    ex.0
  }
}

/// Internal structure for marking nodes as having "expected permissions".
struct PathBoundary {
  /// The [`HirId`] node where we start the search for matching places.
  pub hir_id: HirId,

  /// External context for associated flow constraints.
  pub flow_context: HirId,

  /// A [`HirId`] node that may obstruct the search for place permissions.
  /// The place where this is used is in assignments `*x += y` where
  /// both `*x` and `y` will appear as potential place candidates. We know
  /// at the marking phase that it isn't anything from the `Rvalue` so we
  /// flag it as ignored.
  pub conflicting_node: Option<HirId>,

  /// Exact source span where boundaries should be placed.
  pub location: Span,

  /// The permissions required for the [`Place`] usage.
  pub expected: ExpectedPermissions,
}

impl std::fmt::Debug for PathBoundary {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("PathBoundary")
      .field("location", &self.location)
      .field("hir_id", &self.hir_id)
      .field("expected", &self.expected)
      .finish()
  }
}

/// Get all local places within the body, sorted by their similarity to the given `HirId` place.
fn locals_by_visible_distance<'tcx>(
  tcx: TyCtxt<'tcx>,
  id: HirId,
  body: &Body<'tcx>,
) -> Option<Place<'tcx>> {
  use fuzzy_match::fuzzy_match;
  use rustc_middle::mir::VarDebugInfoContents;

  let map = tcx.hir();
  // Get a textual representation of the path we're dealing with. Note, that this
  // may be something like `*x` or `&x[0]` and not just `x`.
  let node_str = tcx
    .sess
    .source_map()
    .span_to_snippet(map.span(id))
    .unwrap_or_default();

  let all_local_places = body.var_debug_info.iter().filter_map(|debug_info| {
    let VarDebugInfoContents::Place(p) = debug_info.value else {
      return None;
    };
    Some((debug_info.name.as_str(), p))
  });

  fuzzy_match(&node_str, all_local_places)
}

// HACK: this is unsatisfying. Ideally, we would be able to take a (resolved) hir::Path
// and turn it directly into its corresponding mir::Place, I (gavin)
// haven't found a great way to do this, so for now, we consider all
// Places occurring inside of a mapped HirId, and for some cases we can
// remove Places from consideration depending on the hir::Node they came from.
// TODO: this mechanism needs to be built up and inserted into the IRMapper.
// We could make this more robust by doing a union from a hir::Path with a
// mir::Path comparing on *shape*, (number and types of projections).
/// Pick a matching [`Location`] and [`Place`] from the given [`HirId`] use site.
///
/// NOTE: candidates are expected to be given as an
/// [*inorder*](https://en.wikipedia.org/wiki/Tree_traversal) HIR tree traversal.
fn select_candidate_location<'tcx>(
  tcx: TyCtxt<'tcx>,
  body: &Body<'tcx>,
  hir_id: HirId,
  subtract_from: impl FnOnce() -> Vec<(Location, Place<'tcx>)>,
  candidates: &[(Location, Place<'tcx>)],
) -> Option<(Location, Place<'tcx>)> {
  if candidates.len() <= 1 {
    return candidates.first().copied();
  }

  let others = subtract_from();
  // Remove all candidates present in the subtraction set.
  let candidates = candidates.into_iter().filter(|t| !others.contains(t));

  // From the list of local places sorted by similarity to the given `HirId`,
  // pick the first one that matches the base local.
  if let Some(similar_local) = locals_by_visible_distance(tcx, hir_id, body) {
    log::debug!("Similar local found: {similar_local:?}");
    candidates
      .into_iter()
      .find(|(_, can_place)| can_place.local == similar_local.local)
      .copied()
  } else {
    // If no similar local is found, then we can do a "best effort" search.

    let candidates = candidates.collect::<Vec<_>>();
    // The first usage contains the relevant Local,
    // in most cases the first use will also be the desired
    // Place but when indexing an array this isn't true.
    // ```ignore
    // let a = [0];
    // let i0 = a[i];
    //          ^^^ expands to:
    //          // len_a = Len(a)
    //          // assert 0 <= i < len_a
    //          // copy a[i]
    // ```
    // For an array index, the first use is actually getting the
    // length of the array, but we want to make sure to use the
    // actual indexing. To achieve this we filter out all places
    // with a different base Local, then we choose the Place with
    // the *most* projections.
    let base_local = candidates.first()?.1.local;

    let matching_locals = candidates
      .into_iter()
      .filter(|(_, p)| p.local == base_local);

    // We first reverse the iterator because
    // `max_by_key` takes the last matching value
    // when there is a clash but we need the first.
    matching_locals
      .rev()
      .max_by_key(|(_, p)| p.projection.len())
      .copied()
  }
}

/// Return the constraints that occur nested within a [`HirId`].
///
/// Note, constraints involving regions belonging to the same SCC are removed.
fn flow_constraints_at_hir_id<'tcx>(
  ctxt: &PermissionsCtxt<'tcx>,
  ir_mapper: &IRMapper<'tcx>,
  hir_id: HirId,
) -> Option<Vec<(Origin, Origin, Point)>> {
  let mir_locations =
    ir_mapper.get_mir_locations(hir_id, GatherDepth::Nested)?;

  let all_constraints = mir_locations
    .values()
    .flat_map(|loc| {
      let ps = ctxt.location_to_points(loc);
      ctxt
        .polonius_input_facts
        .subset_base
        .iter()
        .filter(move |&(f, t, p)| {
          !ctxt.is_universal_subset((*f, *t)) && ps.contains(p)
        })
        .copied()
    })
    .collect::<Vec<_>>();

  Some(all_constraints)
}

/// If flow permissions are enabled, find expected flow permissions (if any) for the
/// given `hir_id` within the larger `flow_context`.
fn get_flow_permission(
  analysis: &AquascopeAnalysis,
  flow_context: HirId,
  hir_id: HirId,
) -> Option<FlowBoundary> {
  if !ENABLE_FLOW_PERMISSIONS
    .copied()
    .unwrap_or(ENABLE_FLOW_DEFAULT)
  {
    log::warn!("Flow permissions are disabled!");
    return None;
  }

  let ir_mapper = &analysis.ir_mapper;
  let ctxt = &analysis.permissions;
  let hir = ctxt.tcx.hir();
  let body = &ctxt.body_with_facts.body;

  let region_flows = ctxt.region_flows();

  // Do any given constraints have an abstract Origin on the RHS?
  //
  // NOTE: here `is_abstract_member` is used to only look for regions
  // which are themselves part of an abstract SCC, not just containing
  // an abstract region.
  let has_abstract_on_rhs = |flows: &[(Origin, Origin, Point)]| {
    flows
      .iter()
      .any(|&(_, t, _)| region_flows.is_abstract_member(t))
  };

  let context_constraints =
    flow_constraints_at_hir_id(ctxt, ir_mapper, flow_context)?;

  // FIXME: current restriction, only look at constraints when
  // an abstract equivalent region is on the right-hand-side.
  //
  // This covers the cases:
  // - missing abstract-outlives-abstract constraint.
  // - local outlives abstract.
  if !has_abstract_on_rhs(&context_constraints) {
    return None;
  }

  // Search for relevant flows and flow violations.
  let specific_constraints =
    flow_constraints_at_hir_id(ctxt, ir_mapper, hir_id)?;

  {
    let format_with_scc = |v: &[(Origin, Origin, Point)]| {
      v.iter()
        .map(|&(f, t, _)| ((f, region_flows.scc(f)), (t, region_flows.scc(t))))
        .collect::<Vec<_>>()
    };
    log::debug!(
      "flow context constraints:\n{:#?}",
      format_with_scc(&context_constraints)
    );
    log::debug!(
      "flow (HirId)local constraints:\n{:#?}",
      format_with_scc(&specific_constraints)
    );
  }

  let mut flow_violations =
    context_constraints.iter().filter_map(|&(from, to, _)| {
      let fk = region_flows.flow_kind(from, to);

      // We want to look specifically for flows that:
      // - flow to an abstract region (XXX: a current design constraint to be lifter)
      // - are invalid
      // - the local constraints create a context constraint involved in the violation.
      if region_flows.is_abstract_member(to)
        && !fk.is_valid_flow()
        && specific_constraints
          .iter()
          .any(|&(_f, t, _)| t == from || t == to)
      {
        log::debug!("found flow violation: {fk:?} @ {from:?} -> {to:?}");
        Some(fk)
      } else {
        None
      }
    });

  // In theory there could be multiple violations that occur in the context. Multiple could also
  // be triggered by the same local constraints, however, we currently are not providing any
  // visualization for the violation provenance. Therefore we can just take the first one.
  //
  // A brief discussion at:
  // https://github.com/cognitive-engineering-lab/aquascope/pull/51#discussion_r1141095658
  let kind = flow_violations.next().unwrap_or_else(|| {
    log::debug!("No flow edge violation found");
    FlowEdgeKind::Ok
  });

  let raw_span = hir.span(flow_context);
  let span = raw_span.as_local(body.span).unwrap_or(body.span);
  let flow_context = analysis.span_to_range(span);

  Some(FlowBoundary {
    is_violation: !kind.is_valid_flow(),
    flow_context,
    kind,
  })
}

/// Find all of the places used at the MIR-level of the
/// given HIR node. This builds our set of candidate places
/// that we consider for boundary resolution.
#[allow(clippy::wildcard_in_or_patterns)]
fn paths_at_hir_id<'tcx>(
  tcx: TyCtxt<'tcx>,
  body: &'tcx Body<'tcx>,
  ir_mapper: &IRMapper<'tcx>,
  hir_id: HirId,
) -> Option<Vec<(Location, Place<'tcx>)>> {
  type TempBuff<'tcx> = SmallVec<[(Location, Place<'tcx>); 3]>;

  let mir_locations_opt =
    ir_mapper.get_mir_locations(hir_id, GatherDepth::Nested);

  macro_rules! maybe_in_op {
    ($loc:expr, $op:expr) => {
      $op
        .as_place()
        .and_then(|p| p.is_source_visible(tcx, body).then_some(p))
        .map(|p| smallvec![($loc, p)])
        .unwrap_or(smallvec![])
    };
    ($loc:expr, $op1:expr, $op2:expr) => {{
      let mut v: TempBuff = maybe_in_op!($loc, $op1);
      let mut o: TempBuff = maybe_in_op!($loc, $op2);
      v.append(&mut o);
      v
    }};
  }

  let look_in_rvalue = |rvalue: &Rvalue<'tcx>, loc: Location| -> TempBuff {
    match rvalue {
      // Nested operand cases
      Rvalue::Use(op)
        | Rvalue::Repeat(op, _)
        | Rvalue::Cast(_, op, _)
        | Rvalue::UnaryOp(_, op)
        | Rvalue::ShallowInitBox(op, _) => maybe_in_op!(loc, op),

      // Given place cases.
      Rvalue::Ref(_, _, place)
        | Rvalue::Len(place)
        | Rvalue::Discriminant(place)
        | Rvalue::CopyForDeref(place)
        if place.is_source_visible(tcx, body) =>
      {
        smallvec![(loc, *place)]
      }

      // Operand case
      Rvalue::BinaryOp(_, box (left_op, right_op)) => {
        maybe_in_op!(loc, left_op, right_op)
      }

      // Unimplemented cases, ignore nested information for now.
      //
      // These are separated in the or because they aren't implemented,
      // but still silently ignored.
      Rvalue::ThreadLocalRef(..)
        | Rvalue::NullaryOp(..)
        | Rvalue::Aggregate(..)

      // Wildcard for catching the previous guarded matches.
        | _ => {
          log::warn!("couldn't find in RVALUE {rvalue:?}");
          smallvec![]
        }
    }
  };

  let look_in_statement = |stmt: &Statement<'tcx>, loc: Location| -> TempBuff {
    match &stmt.kind {
      StatementKind::Assign(box (lhs_place, ref rvalue)) => {
        let mut found_so_far: TempBuff = look_in_rvalue(rvalue, loc);
        if lhs_place.is_source_visible(tcx, body) {
          found_so_far.push((loc, *lhs_place));
        }
        found_so_far
      }
      StatementKind::SetDiscriminant { place, .. }
        if place.is_source_visible(tcx, body) =>
      {
        smallvec![(loc, **place)]
      }
      StatementKind::FakeRead(box (_, place))
        if place.is_source_visible(tcx, body) =>
      {
        smallvec![(loc, *place)]
      }


      StatementKind::SetDiscriminant { .. }
      | StatementKind::FakeRead(..)
      | StatementKind::PlaceMention(..) // TODO: do we need to handle this new kind

      // These variants are compiler generated, but it would be
      // insufficient to find a source-visible place only in
      // compiler generated statements.
      //
      // They are also unimplemented so if something is missing
      // suspect something in here.
      | StatementKind::Deinit(..)
      | StatementKind::StorageLive(..)
      | StatementKind::StorageDead(..)
      | StatementKind::Retag(..)
      | StatementKind::AscribeUserType(..)
      | StatementKind::Coverage(..)
      | StatementKind::Intrinsic(..)
      | StatementKind::ConstEvalCounter
      | StatementKind::BackwardIncompatibleDropHint { .. }
      | StatementKind::Nop => smallvec![],
    }
  };

  let mir_locations = mir_locations_opt?
    .values()
    .flat_map(|loc| {
      log::debug!("looking at {loc:?}");
      match body.stmt_at(loc) {
        Either::Left(stmt) => look_in_statement(stmt, loc),
        Either::Right(_term) => smallvec![],
      }
    })
    .collect::<Vec<_>>();

  Some(mir_locations)
}

fn path_to_perm_boundary<'tcx>(
  path_boundary: PathBoundary,
  analysis: &AquascopeAnalysis<'tcx>,
) -> Option<PermissionsBoundary> {
  let ctxt = &analysis.permissions;
  let ir_mapper = &analysis.ir_mapper;
  let body = &ctxt.body_with_facts.body;
  let tcx = ctxt.tcx;
  let hir = tcx.hir();
  let hir_id = path_boundary.hir_id;

  log::debug!(
    "Resolving permissions boundary for {}",
    hir.node_to_string(path_boundary.hir_id)
  );

  let search_at_hir_id = |hir_id| {
    let path_locations = paths_at_hir_id(tcx, body, ir_mapper, hir_id)?;

    let (loc, place) = select_candidate_location(
      tcx,
      body,
      hir_id,
      // thunk to compute the places within the conflicting HirId,
      || {
        path_boundary
          .conflicting_node
          .and_then(|hir_id| paths_at_hir_id(tcx, body, ir_mapper, hir_id))
          .unwrap_or_default()
      },
      &path_locations,
    )?;

    let point = ctxt.location_to_point(loc);
    let path = ctxt.place_to_path(&place);

    log::debug!("Chosen place at location {place:#?} {loc:#?} ({point:?},{path:?})\nOther options: {path_locations:#?}");

    Some((point, path))
  };

  // For a given Path, the MIR location may not be immediately associated with it.
  // For example, in a function call `foo( &x );`, the Hir Node::Path `&x` will not
  // have the MIR locations associated with it, the Hir Node::Call `foo( &x )` will,
  // so we traverse upwards in the tree until we find a location associated with it.
  let resolved_boundary = search_at_hir_id(hir_id)
    .or_else(|| {
      hir.parent_iter(hir_id).find_map(|(hir_id, _)| {
        log::debug!("\tsearching upwards in: {}", hir.node_to_string(hir_id));
        search_at_hir_id(hir_id)
      })
    })
    .map(|(point, path)| {
      let data = ctxt.permissions_data_at_point(path, point);
      let expected = path_boundary.expected;
      let actual = data.permissions_ignore_liveness();

      let expecting_flow =
        get_flow_permission(analysis, path_boundary.flow_context, hir_id);

      log::debug!("Permissions data for {}:\n{actual:#?}\n{expected:#?}\n{expecting_flow:#?}", hir.node_to_string(path_boundary.hir_id));

      let span = path_boundary
        .location
        .as_local(body.span)
        .unwrap_or(path_boundary.location);

      // FIXME(gavinleroy): the spans are chosen in the `path_visitor` such that the end
      // of the span is where we want the stack to be placed. I would like to
      // make this a bit more explicit.
      let location = analysis.span_to_range(span).end;
      let byte_location = ByteRange::from_span(span, tcx.sess.source_map())
        .unwrap()
        .end;

      PermissionsBoundary {
        location,
        byte_location,
        expected: expected.into(),
        actual,
        data,
        expecting_flow,
      }
    });

  if resolved_boundary.is_none() {
    log::warn!(
      "Could not resolve a MIR place for expected boundary {}",
      hir.node_to_string(path_boundary.hir_id)
    );
  }

  resolved_boundary
}

#[allow(clippy::module_name_repetitions)]
pub fn compute_permission_boundaries<'tcx>(
  analysis: &AquascopeAnalysis<'tcx>,
) -> Result<Vec<PermissionsBoundary>> {
  let ctxt = &analysis.permissions;

  let path_use_points = get_path_boundaries(ctxt)?
    .into_iter()
    .filter_map(|pb| path_to_perm_boundary(pb, analysis));

  // FIXME: we need a more robust way of filtering by "first error".
  // here (and in the stepper) we do this by diagnostic span from rustc
  // but that can sometimes be a little earlier than we might want.
  let first_error_span_opt =
    errors::get_span_of_first_error(ctxt.def_id.expect_local())
      .and_then(|s| s.as_local(ctxt.body_with_facts.body.span));

  let boundaries = path_use_points
    .filter(|pb| {
      first_error_span_opt.map_or(true, |error_span| {
        pb.expecting_flow.is_some() || {
          let error_range =
            ByteRange::from_span(error_span, ctxt.tcx.sess.source_map())
              .unwrap();
          pb.byte_location <= error_range.end
        }
      })
    })
    .collect::<Vec<_>>();

  Ok(boundaries)
}

#[cfg(test)]
mod test {
  use fuzzy_match::fuzzy_match;

  #[test]
  fn fuzzy_search_0() {
    let search = "&*x[i]";
    let haystack = vec![("x", 0), ("y", 1), ("z", 2)];
    assert_eq!(Some(0), fuzzy_match(search, haystack));
  }

  #[test]
  fn fuzzy_search_1() {
    let search = "&&&&&z";
    let haystack = vec![("x", 0), ("y", 1), ("z", 2)];
    assert_eq!(Some(2), fuzzy_match(search, haystack));
  }

  #[test]
  fn fuzzy_search_2() {
    let search = "a + b + c";
    let haystack = vec![("x", 0), ("y", 1), ("z", 2)];
    assert_eq!(None, fuzzy_match(search, haystack));
  }

  #[test]
  fn fuzzy_search_3() {
    let search = "y[..a]";
    let haystack = vec![("x", 0), ("y", 1), ("z", 2)];
    assert_eq!(Some(1), fuzzy_match(search, haystack));
  }
}
