//! Core analysis for creating permission steps.
//!
//! # Overview
//!
//! Defined on the MIR, a *permissions step* is the difference in permissions between
//! two adjacent MIR [`Point`]s. The difference represents the gains and losses that
//! occur between adjacent permissions states. Permission steps on the MIR are useless
//! to the average user, thus we cluster subsequences of MIR instructions and take the
//! difference between the first and last point to create a larger step.
//!
//! At a high-level, the strategy is to partition the MIR into subsequences (referred to as segments),
//! such that each segment represents a single permission step. I.E. each segment is a cluster
//! of instructions representing one source-level permissions step.
//! After clustering, the steps are easily computed in isolation to create the final permissions
//! steps. As we'll see later, the “isolation” is broken down a little to prevent some specific
//! visual effects.
//!
//! # Splitting Strategy
//!
//! The main goal of the permission stepper is to provide steps that map to logical “steps” in
//! the source code. First, the steps will be determined using HIR language constructs, which are
//! subsequently lowered to fit the more granular MIR language constructs.
//! Starting with the HIR, a so-called “logical step” is roughly defined to be a [`Stmt`](rustc_hir::Stmt).
//! Typically statements fall on their own line and they mark the beginning and end
//! of some potentially permissions-altering operation. This makes up the first loose for
//! finding permissions steps.
//!
//! Statements however, do not cover how permissions change in a control-flow sensitive construct.
//! For example, the statements at the beginning of the then and else branch might execute with
//! different permissions, this sudden change of permissions needs to be communicated to the user,
//! rather than happening on instructions of the CFG these permissions are changed on the *edges*.
//! This forms the second rule of creating a step, namely, a branch in control flow is also a
//! permissions-altering “operation”. The full rules for tracking permissions steps at each
//! respective level of granularity are outlined below.
//!
//! ## Source to HIR
//!
//! In the source code, we'd like a permissions step to be shown after each line and at the
//! opening brace (`{`) of a new block. This requires us to take a permissions step at the
//! following HIR locations.
//!
//! * From before to after each [`Stmt`](rustc_hir::Stmt).
//! * From the last [`Block`](rustc_hir::Block) statement to after the `expr` of a [`Block`](rustc_hir::Block).
//! * From before a branch to *before* the first expression of a branch target. For example, at the source-level,
//!   this would map to a step from before an `if` to the directly after the opening `{` of the
//!   then / else block.
//!
//! Each node in the HIR generates several MIR locations. For information on how to map between the
//! two see the [`IRMapper`]. Important for the stepper, is the ability to find the first, and
//! last, MIR [`Location`] which came from a HIR node. First and last are used in the sense of
//! a [dominator](https://en.wikipedia.org/wiki/Dominator_(graph_theory)) and [post-dominator](https://en.wikipedia.org/wiki/Dominator_(graph_theory)#Postdominance)
//! respectively. The main idea is that the HIR traversal allows us to find the proper *slice points*
//! for the MIR graph.
//!
//! ## HIR to MIR
//!
//! When forming permission steps in the MIR, the most crucial invariant is that the permissions steps
//! form a total cover of the control-flow graph. This invariant remains to ensure that no change in
//! permissions is *missed*. If a change in permissions is not shown (at the source-level),
//! this is due to segmenting steps at the wrong boundaries or at too coarse a granularity.
//! Because of this invariant, the stepper uses a strategy to "slice" the MIR into segments, such that
//! these segments always form a total cover.
//!
//! ### Data Structures Summary
//!
//! The key data structures involvled are the [`MirSegment`] and [`SegmentTree`].
//! The [`MirSegment`] is a simple struct storing the two points, where a permimssion step
//! will step `from` and where steps `to`. This means that a `MirSegment` must lie on a
//! valid path within the MIR.
//!
//! The [`SegmentTree`] (not to be confused with a [segment tree](https://en.wikipedia.org/wiki/Segment_tree))
//! is a tree which holds [`MirSegment`]s in its leaves.
//!
//! ### Slicing
//!
//! The core operation performed on the [`SegmentTree`] is taking a *slice*. There are two kinds of
//! slices:
//!
//! 1. linear slices, those that **do not** contain permissions-altering CFG edges.
//! 2. control-flow slices, those that **only** contain permissions-altering CFG edges.
//!
//! These two slices exist to maintain the invariants of the [`MirSegment`] and [`SegmentTree`].
//! Fundamentally, these slices work on different *shapes* of the underlying graph.
//!
//! #### Linear Slices
//!
//! A *linear slice* slices a portion of the graph which forms a continuous subsequence.
//!
//! Example:
//!
//! ```text
//! before slice:
//!
//!             slice point
//!               |
//! [segment 1]   |
//! ⬤ [l1] ----> ⬤ [l2] ----> ⬤ [l3]
//!               |
//!               |
//!
//!
//! after slice:
//!
//! [segment 1]
//! ⬤ [l1] ----> ⬤ [l2]
//!
//! [segment 2]
//! ⬤ [l2] ----> ⬤ [l3]
//!
//! ```
//!
//! In the above example there exists a linear sequence of control-flow from `l1 ⟶ l2 ⟶ l3`.
//! Depicted, is a *linear slice* of this segment at location `l2`. Linear slices *always*
//! split a single segment, into two new segments which maintain the [`MirSegment`] invariant.
//! These slices are used after [`Stmt`s](rustc_hir::Stmt) and the end of a [`Block` expression](rustc_hir::Block).
//!
//! #### Control-flow slices
//!
//! A *control-flow* slice, then does not slice a continous subsequence but multiple that
//! /span across/ branches of control flow.
//!
//! Example:
//! ```text
//! before slice:
//!
//!                     slice point
//!                        |
//!                        |
//!                        |
//!               ------> ⬤ [l2] -------
//!               |        |            |
//! [segment 1]   |        |            v
//!        ----> ⬤ [l1]    |            ⬤ [l4] ---->
//!               |        |            ^
//!               |        |            |
//!               ------> ⬤ [l3] -------
//!                        |
//!                        |
//!                        |
//!
//!
//! after slice:
//!
//! [segment 1]
//! ⬤ [l1] ----> ⬤ [l2]
//!
//! [segment 2]
//! ⬤ [l1] ----> ⬤ [l3]
//!
//! [segment 3]
//! ⬤ [l2] ----> ⬤ [l4]
//!
//! [segment 4]
//! ⬤ [l3] ----> ⬤ [l4]
//!
//! ```
//! Before the slice in segment 1 there is a graph which rougly captures the shape
//! of an if expression. location `l1` would be the branch point (corresponding
//! to a `SwitchInt`), `l2` and `l3` would be the then and else branches. Here these
//! branches are abstracted to a single point, but in practice they can be any valid
//! [`MirSegment`]. Then location `l4` joins the branches and control flow continues
//! again linearly.
//!
//! In order to slice a control-flow segment properly, a set of locations is required
//! and the function mapping a location to a control-flow path must be bijective.
//! In the above example, the possible paths through this segment (the usliced segment 1) are:
//! 1. `l1`, `l2`, `l4`
//! 2. `l1`, `l3`, `l4`
//!
//! Therefore, in order to perform a proper slice, the set (`l2`, `l3`) is provided.
//! Luckily, these locations are easy to obtain from the structure of the HIR and correspond
//! to the opening block of each branch.
//!
//! After slicing, the result is four segments that form a total cover of the original
//! segment, and each has a clear entry / exit point for *it's specific control flow*.
//!
//! NOTE: one small semantic difference between the resulting segments. The segments
//! which form the so-called "split set" (segments 1 and 2 in the above example) *cannot*
//! be further split. They are treated as **atomic**. This is intuitive if you image that
//! they only contains edges in the CFG (there would be nothing left to spliti).
//!
//! # Finalizing Differences
//!
//! Slicing the MIR into segments is the core task for the stepper and results
//! in a proper [`SegmentTree`]. The last task of the stepper engine is to take the
//! permissions difference between the domain after the segment, and that before.
//! See the [`PermissionsCtxt`] for more information about computing a [`PermissionsDomain`].
//!
//! When computing the differences however, there is an edge case when handling liveness.
//! As a result of the generated MIR, it's possible for the left-hand-side of an assignment
//! to gain permissions before it seems it should. This occurs when the initializer expression
//! is more complex (e.g. an [`If`](rustc_hir::Expr) or [`Block`](rustc_hir::Expr) expression).
//! To ensure initialized places don't gain permissions before the end of the let statement,
//! these places are marked as /attached/ to a specific MIR location, and they are filtered
//! from any nested segment step results.
//!
//! # Known Shortcomings
//!
//! There are a few major known limitations, they can be resolved we just need the time:
//!
//! - Function bodies that contain infinite loops `loop {}` cannot be analyzed.
//!   More general, if there does not contain an exit point to the function the
//!   current algorithm will report this limitation to the user.
//!
//! - The control-flow slicing is too strict, if there exists an `if` without
//!   and `else`, or if there are multiple returns, the algorithm also fails.

use anyhow::{bail, Result};
use rustc_data_structures::{self, fx::FxHashMap as HashMap};
use rustc_hir::{
  self as hir,
  intravisit::{self, Visitor as HirVisitor},
  HirId,
};
use rustc_middle::{
  hir::nested_filter,
  mir::{self, Local, Location, Place},
};
use rustc_span::Span;
use rustc_utils::{
  source_map::range::CharRange, test_utils::DUMMY_CHAR_RANGE, PlaceExt, SpanExt,
};

use super::{
  segment_tree::{MirSegment, SegmentSearchResult, SegmentTree, SplitType},
  *,
};
use crate::{
  analysis::{
    ir_mapper::{GatherDepth, IRMapper},
    permissions::{
      Permissions, PermissionsCtxt, PermissionsData, PermissionsDomain,
    },
  },
  errors,
};

pub fn compute_permission_steps<'a, 'tcx>(
  analysis: &AquascopeAnalysis<'a, 'tcx>,
) -> Result<Vec<PermissionsLineDisplay>>
where
  'tcx: 'a,
{
  let mode = INCLUDE_MODE.copied().unwrap_or(PermIncludeMode::Changes);
  let ctxt = &analysis.permissions;
  let ir_mapper = &analysis.ir_mapper;
  let body = &ctxt.body_with_facts.body;
  let _basic_blocks = body.basic_blocks.indices();
  let mut hir_visitor = HirStepPoints::make(ctxt, ir_mapper)?;
  hir_visitor.visit_nested_body(ctxt.body_id);

  log::debug!(
    "Final tree for permission steps\n{:?}",
    hir_visitor.mir_segments
  );

  if let Some((_, msg)) = hir_visitor.unsupported_encounter {
    bail!(msg);
  }

  if !hir_visitor.fatal_error.is_empty() {
    bail!(hir_visitor.fatal_error);
  }

  Ok(prettify_permission_steps(
    analysis,
    hir_visitor.finalize_diffs(),
    mode,
  ))
}

// Prettify, means:
// - Remove all places that are not source visible
// - Remove all tables which are empty
// - Convert Spans to Ranges
fn prettify_permission_steps<'tcx>(
  analysis: &AquascopeAnalysis<'_, 'tcx>,
  perm_steps: HashMap<
    Span,
    (MirSegment, HashMap<Place<'tcx>, PermissionsDataDiff>),
  >,
  mode: PermIncludeMode,
) -> Vec<PermissionsLineDisplay> {
  let ctxt = &analysis.permissions;
  let tcx = ctxt.tcx;
  let body = &ctxt.body_with_facts.body;

  let should_keep = |p: &PermissionsDataDiff| -> bool {
    !(matches!(p.is_live, ValueStep::None { value: Some(false) })
      || (mode == PermIncludeMode::Changes && p.is_empty()))
  };

  macro_rules! place_to_string {
    ($p:expr) => {
      $p.to_string(tcx, body)
        .unwrap_or_else(|| String::from("<var>"))
    };
  }

  let first_error_span_opt =
    errors::get_span_of_first_error(ctxt.def_id.expect_local())
      .and_then(|s| s.as_local(ctxt.body_with_facts.body.span));
  let source_map = tcx.sess.source_map();

  perm_steps
    .into_iter()
    .fold(
      HashMap::<
        CharRange,
        Vec<(MirSegment, Vec<(Place<'tcx>, PermissionsDataDiff)>)>,
      >::default(),
      |mut acc, (span, (segment, place_to_diffs))| {
        // Attach the span to the end of the line. Later, all permission
        // steps appearing on the same line will be combined.
        let span = source_map.span_extend_to_line(span).shrink_to_hi();
        let entries = place_to_diffs
          .into_iter()
          .filter(|(place, diff)| {
            place.is_source_visible(tcx, body) && should_keep(diff)
          })
          .collect::<Vec<_>>();

        // This could be a little more graceful. The idea is that
        // we want to remove all permission steps which occur after
        // the first error, but the steps involved with the first
        // error could still be helpful. This is why we filter all
        // spans with a LO BytePos greater than the error
        // span HI BytePos.
        if !(entries.is_empty()
          || first_error_span_opt
            .is_some_and(|err_span| err_span.hi() < span.lo()))
        {
          let range = analysis.span_to_range(span);
          acc.entry(range).or_default().push((segment, entries));
        }

        acc
      },
    )
    .into_iter()
    // HACK FIXME: we're at odds with the multi-table setup. This quick
    // hack combines table entries into a single table until the
    // visual explanation gets up-to-speed.
    // Another weird thing about this is that you can have a single
    // table with two changes for one place.
    // ```example
    // # fn main() {
    // let closure = |s: &str| s.len(); // s: +R+O
    //                                  // s: -R-O
    //                                  // closure: +R+O
    // # }
    // ```
    // imagine that the comments to the right of the Let represent
    // a pseudo combined table. The path `s` gains and loses the same
    // set of permissions in the same table. This is kind of weird, we'd
    // rather just show *no change*.
    .filter_map(|(range, mut entries)| {
      for (_, v) in entries.iter_mut() {
        v.sort_by_key(|(place, _)| (place.local.as_usize(), place.projection))
      }

      // let state = entries
      //   .into_iter()
      //   .map(|(MirSegment { from, to }, diffs)| {
      //     let state = diffs
      //       .into_iter()
      //       .map(|(place, diff)| {
      //         let s = place_to_string!(place);
      //         (s, diff)
      //       })
      //       .collect::<Vec<_>>();
      //     let from = analysis.span_to_range(ctxt.location_to_span(from));
      //     let to = analysis.span_to_range(ctxt.location_to_span(to));
      //     PermissionsStepTable { from, to, state }
      //   })
      //   .collect::<Vec<_>>();

      // Conforming to the above HACK this just takes any (from, to) pair.
      let dummy_char_range = DUMMY_CHAR_RANGE.with(|range| *range);
      let (from, to) = entries.first().map_or_else(
        || (dummy_char_range, dummy_char_range),
        |(MirSegment { from, to }, _)| {
          let from = analysis.span_to_range(ctxt.location_to_span(*from));
          let to = analysis.span_to_range(ctxt.location_to_span(*to));
          (from, to)
        },
      );

      let mut master_table: Vec<(Place<'tcx>, PermissionsDataDiff)> =
        Vec::default();

      let is_symmetric_diff =
        |diff1: &PermissionsDataDiff, diff2: &PermissionsDataDiff| -> bool {
          macro_rules! is_symmetric {
            ($v1:expr, $v2:expr) => {
              matches!(
                (&$v1, &$v2),
                (ValueStep::High { .. }, ValueStep::Low { .. })
                  | (ValueStep::Low { .. }, ValueStep::High { .. })
                  | (ValueStep::None { .. }, ValueStep::None { .. })
              )
            };
          }
          let p1 = &diff1.permissions;
          let p2 = &diff2.permissions;
          is_symmetric!(p1.read, p2.read)
            && is_symmetric!(p1.write, p2.write)
            && is_symmetric!(p1.drop, p2.drop)
        };

      // For all tables which fall on the same line, we combine them into a single table
      // and remove all *SYMMETRIC* differences. That is, if you have permission changes such as:
      // - path: +R+O
      // - path: -R-O
      // these are exactly symmetric, and will be removed.
      for (_, diffs) in entries.into_iter() {
        for (place, diff) in diffs.into_iter() {
          let i_opt = master_table.iter().position(|(p, _)| *p == place);
          if let Some(idx) = i_opt {
            let (_, old_diff) = &master_table[idx];
            if is_symmetric_diff(&diff, old_diff) {
              log::debug!(
                "REMOVING place {place:?} with diff {diff:?} into the MT."
              );
              master_table.remove(idx);
              continue;
            }
          }

          log::debug!("ADDING place {place:?} with diff {diff:?} into the MT.");
          master_table.push((place, diff));
        }
      }

      // This means the tables were symmetric and all were removed.
      if master_table.is_empty() {
        return None;
      }

      let master_table = PermissionsStepTable {
        from,
        to,
        state: master_table
          .into_iter()
          .map(|(place, diff)| (place_to_string!(place), diff))
          .collect::<Vec<_>>(),
      };

      Some(PermissionsLineDisplay {
        location: range,
        state: vec![master_table],
      })
    })
    .collect::<Vec<_>>()
}

// ------------------------------------------------

macro_rules! fatal {
  ($this:expr, $( $rest:tt ),*) => {
    let f = format!( $($rest)*);
    $this.report_fatal(&f);
    bail!(f);
  }
}

/// Visitor for creating permission steps in the HIR.
///
/// Visits the HIR in a Nested order, splitting the MIR and accumulating permission steps.
struct HirStepPoints<'a, 'tcx>
where
  'tcx: 'a,
{
  ctxt: &'a PermissionsCtxt<'a, 'tcx>,
  ir_mapper: &'a IRMapper<'a, 'tcx>,
  mir_segments: Box<SegmentTree>,
  unsupported_encounter: Option<(Span, String)>,
  fatal_error: String,
}

impl<'a, 'tcx: 'a> HirStepPoints<'a, 'tcx> {
  fn make(
    ctxt: &'a PermissionsCtxt<'a, 'tcx>,
    ir_mapper: &'a IRMapper<'a, 'tcx>,
  ) -> Result<Self> {
    let tcx = ctxt.tcx;
    let hir = tcx.hir();
    let body = &hir.body(ctxt.body_id);
    let body_hir_id = body.value.hir_id;
    let body_span = body.value.span;

    let mol = ir_mapper
      .get_mir_locations(body_hir_id, GatherDepth::Nested)
      .unwrap();

    // A body must have an entry location.
    let from = mol.entry_location().unwrap();

    // A body with an infinite loop will not generate MIR that
    // contains an exit location.
    let Some(to) = mol.exit_location() else {
      bail!("The function body under analysis has zero (or many) exit points. This currently isn't supported by the permissions stepper; I suggest trying to rewrite the function to contain a single `return`.");
    };

    let body_segment = MirSegment::new(from, to);
    let mir_segments = Box::new(SegmentTree::new(body_segment, body_span));

    Ok(HirStepPoints {
      ctxt,
      ir_mapper,
      mir_segments,
      unsupported_encounter: None,
      fatal_error: String::default(),
    })
  }

  fn report_unsupported(&mut self, id: HirId, msg: &str) {
    if self.unsupported_encounter.is_none() {
      let span = self.span_of(id);
      self.unsupported_encounter = Some((span, String::from(msg)));
    }
  }

  fn report_fatal(&mut self, msg: &str) {
    self.fatal_error.push_str(&"-".repeat(5));
    self.fatal_error.push('\n');
    self.fatal_error.push_str(msg);
  }

  /// Determine whether the traversal should visited nested HIR nodes.
  ///
  /// This method is a sort of HACK to avoid picking apart nodes expanded from
  /// macros, while visiting nodes expanded from expected desugarings (e.g. for / while loops).
  fn should_visit_nested(&self, _id: HirId, span: Span) -> bool {
    use rustc_span::hygiene::DesugaringKind as DK;
    !span.from_expansion()
      || span.is_desugaring(DK::ForLoop)
      || span.is_desugaring(DK::WhileLoop)
  }

  /// Split an already linear segment into two segments.
  ///
  /// Example, a block of statements will produce a graph with the following shape:
  ///
  /// ```text
  ///       ⬤ l1 --> ⬤ l2 --> ⬤ l3
  /// ```
  ///
  /// The above linear sequence could be split at any of the location `l1, l2, l3` and it
  /// would produce two valid segments. For example, splitting the above at `l2` would produce:
  ///
  /// ```text
  /// SegmentTree::Split {
  ///   segments: SplitType::Linear {
  ///     first: MirSegment(l1, l2),
  ///     second: MirSegment(l2, l3),
  ///   },
  ///   reach: MirSegment(l1, l3),
  ///   ...
  /// }
  /// ```
  fn insert_linear_step_at(
    &mut self,
    span: Span,
    location: Location,
    attached_here: Vec<Local>,
  ) -> Result<()> {
    let enclosing_segment = self
      .mir_segments
      .as_ref()
      .find_segment_for_end(location, &self.ir_mapper.cleaned_graph);

    match enclosing_segment {
      SegmentSearchResult::NotFound => {
        fatal!(self, "{location:?} should always be enclosed in the graph");
      }
      SegmentSearchResult::StepExists(segment, ..) => {
        log::warn!(
          "linear step had slice conflict at {location:?} with {segment:?}"
        );
        Ok(())
      }

      SegmentSearchResult::Enclosing(SegmentTree::Single {
        segment,
        span: old_span,
        attached,
      }) => {
        let mut paths =
          segment.paths_along_segment(&self.ir_mapper.cleaned_graph);

        let first_step = SegmentTree::Single {
          segment: MirSegment::new(segment.from, location),
          attached: attached_here,
          span,
        };

        let second_step = SegmentTree::Single {
          segment: MirSegment::new(location, segment.to),
          attached: vec![],
          span: *old_span,
        };

        let _ = paths
          .drain_filter(|path| path.contains(&location.block))
          .collect::<Vec<_>>();

        if !paths.is_empty() {
          fatal!(self, "Inserting a linear segment should not result in fragmentation.\nSplitting segment: {segment:?} at {location:?}. Remaining paths: {paths:#?}");
        }

        let subtree = SegmentTree::Split {
          segments: SplitType::Linear {
            first: Box::new(first_step),
            second: Box::new(second_step),
          },
          reach: *segment,
          span: *old_span,
          attached: attached.clone(),
        };

        let segment = *segment;
        self.mir_segments.as_mut().replace_single(segment, subtree)
      }

      _ => {
        fatal!(self, "Enclosing segments can only be a `Single` variant, this is a stepper bug!");
      }
    }
  }

  /// Split a segment into a series of split / join segments for a piece of control flow.
  ///
  /// Example, a simple `if ... { ... } else { ... }` expression will produce a diamond shaped CFG.
  ///
  /// ```text
  ///       ⬤ l1
  ///     /  \
  ///    ⬤ l2 ⬤ l3
  ///     \  /
  ///      ⬤ l4
  /// ```
  ///
  /// In this diagram, the initial `MirSegment` is `l1` -> `l4`. To produce a well-formed
  /// `SegmentTree::Split` node, the locations `[l2,  l3]` should be provided as arguments.
  ///
  /// The specified locations for splitting should satisfy the following properties.
  /// 1. All locations are enclosed by the same MirSegment, (in the above example `(l1, l4)`).
  /// 2. Each location should correspond to a single path through the control flow. In the above
  ///    example, the two possible paths are `[l1, l2, l4]` and `[l1, l3, l4]`.
  /// 3. The locations should be bijective wrt the possible control-flow paths.
  ///
  /// The above example would produce a SegmentTree with the following shape:
  ///
  /// ```text
  /// SegmentTree::Split {
  ///   segments: SegmentType::ControlFlow {
  ///     splits: vec![
  ///       MirSegment::new(l1, l2),
  ///       MirSegment::new(l1, l3)
  ///     ],
  ///     joins: vec![
  ///       MirSegment::new(l2, l4),
  ///       MirSegment::new(l3, l4)
  ///     ],
  ///   },
  ///   reach: ...,
  ///   span: ...,
  /// }
  /// ```
  fn insert_cf_step_at(&mut self, steps: Vec<(Location, Span)>) -> Result<()> {
    if steps.is_empty() {
      return Ok(());
    }

    let graph = &self.ir_mapper.cleaned_graph;

    let enclosings = steps
      .iter()
      .filter_map(|(location, _)| {
        let res = self.mir_segments.find_segment_for_end(*location, graph);
        if let SegmentSearchResult::Enclosing(SegmentTree::Single {
          segment,
          span,
          attached,
        }) = res
        {
          Some((*segment, *span, attached.clone()))
        } else {
          log::error!(
            "searching for {location:?} came up with no result {res:?}"
          );
          None
        }
      })
      .collect::<Vec<_>>();

    if enclosings.len() < steps.len() {
      fatal!(self, "not every locations step had an enclosing segment.");
    }

    let (segment, old_span, attached) = enclosings.first().unwrap();

    if !enclosings.iter().all(|(s, _, _)| s == segment) {
      fatal!(self, "not all provided locations map to the same enclosing segment: {enclosings:#?}");
    }

    let mut paths = segment.paths_along_segment(&self.ir_mapper.cleaned_graph);

    let mut splits = Vec::default();
    let mut joins = Vec::default();

    for (location, span) in steps.into_iter() {
      let split_step = SegmentTree::Single {
        segment: MirSegment::new(segment.from, location),
        attached: vec![],
        span,
      };

      let join_step = SegmentTree::Single {
        segment: MirSegment::new(location, segment.to),
        attached: vec![],
        span: *old_span,
      };

      let _removed_paths = paths
        .drain_filter(|path| path.contains(&location.block))
        .collect::<Vec<_>>();

      splits.push(split_step);
      joins.push(join_step);
    }

    let subtree = SegmentTree::Split {
      segments: SplitType::ControlFlow { splits, joins },
      reach: *segment,
      span: *old_span,
      attached: attached.clone(),
    };

    self.mir_segments.replace_single(*segment, subtree)
  }

  fn span_of(&self, id: HirId) -> Span {
    let hir = self.ctxt.tcx.hir();
    let span = hir.span(id);
    span
      .as_local(self.ctxt.body_with_facts.body.span)
      .unwrap_or(span)
  }

  fn body_value_id(&self) -> HirId {
    let hir = self.ctxt.tcx.hir();
    hir.body(self.ctxt.body_id).value.hir_id
  }

  /// The [`PermissionsDomain`] ⊥.
  ///
  /// No permissions, anywhere.
  fn domain_bottom(&self) -> PermissionsDomain<'tcx> {
    self
      .ctxt
      .domain_places()
      .into_iter()
      .map(|place| {
        (place, PermissionsData {
          is_live: false,
          type_droppable: false,
          type_writeable: false,
          type_copyable: false,
          path_moved: None,
          path_uninitialized: false,
          loan_read_refined: None,
          loan_write_refined: None,
          loan_drop_refined: None,
          permissions: Permissions::bottom(),
        })
      })
      .collect::<HashMap<_, _>>()
      .into()
  }

  /// Convert the current [`SegmentTree`] into permission steps.
  fn finalize_diffs(
    self,
  ) -> HashMap<Span, (MirSegment, HashMap<Place<'tcx>, PermissionsDataDiff>)>
  {
    let body_hir_id = self.body_value_id();
    let body_open_brace = self.span_of(body_hir_id).shrink_to_lo();
    let first_point = self.ctxt.location_to_point(self.body_segment().from);
    let first_domain = &self.ctxt.permissions_domain_at_point(first_point);
    let empty_domain = &self.domain_bottom();

    // Upon entry, the function parameters are already "live". But we want to
    // special case this, and show that they "come alive" at the opening brace.
    let first_diff = empty_domain.diff(first_domain);

    fn diff_subtree<'tcx>(
      ctxt: &PermissionsCtxt<'_, 'tcx>,
      tree: &SegmentTree,
      result: &mut HashMap<
        Span,
        (MirSegment, HashMap<Place<'tcx>, PermissionsDataDiff>),
      >,
      attached_at: &mut HashMap<Local, Location>,
    ) {
      log::trace!(
        "\ndiff_subtree\n[FILTERS]:\n{attached_at:?}\n[TREE]:{tree:?}"
      );

      macro_rules! is_attached {
        ($set:expr, $place:expr, $loc:expr) => {
          $set.get(&$place.local).map(|l| *l == $loc).unwrap_or(false)
        };
      }

      let mut insert_segment = |segment: MirSegment, span: Span| {
        if segment.from != segment.to {
          let p0 = ctxt.location_to_point(segment.from);
          let p1 = ctxt.location_to_point(segment.to);
          let before = &ctxt.permissions_domain_at_point(p0);
          let after = &ctxt.permissions_domain_at_point(p1);
          let mut diff = before.diff(after);

          let removed = diff
            .drain_filter(|place, _| {
              is_attached!(attached_at, place, segment.to)
            })
            .collect::<Vec<_>>();

          log::debug!(
            "removed domain places due to attached filter at {:?} {:?}",
            segment.to,
            removed
          );

          result.insert(span, (segment, diff));
        }
      };

      match tree {
        SegmentTree::Single { segment, span, .. } => {
          insert_segment(*segment, *span)
        }
        SegmentTree::Split {
          segments,
          attached,
          reach,
          span,
        } => {
          // Add the attached places filter
          for local in attached.iter() {
            log::debug!(
              "filtering Local {local:?} not attached to {:?}",
              reach.to
            );

            let old = attached_at.insert(*local, reach.to);
            assert!(old.is_none());
          }

          match segments {
            SplitType::Linear { first, second } => {
              diff_subtree(ctxt, first, result, attached_at);
              diff_subtree(ctxt, second, result, attached_at);
            }

            // CF Splits with exactly one branch / join are considered linear
            // This happens frequently when there is ForLoop desugaring.
            SplitType::ControlFlow { splits, joins }
              if splits.len() == 1 && joins.len() == 1 =>
            {
              diff_subtree(ctxt, &splits[0], result, attached_at);
              diff_subtree(ctxt, &joins[0], result, attached_at);
            }

            SplitType::ControlFlow { splits, joins } => {
              for subtree in splits.iter() {
                diff_subtree(ctxt, subtree, result, attached_at);
              }

              let mut joined_diff = HashMap::default();
              let mut entire_diff = reach.into_diff(ctxt);

              // Rules for joining two domain differences.
              // 1. We always insert the attached locals.
              let attached_here = entire_diff
                .drain_filter(|place, _| {
                  is_attached!(attached_at, place, reach.to)
                })
                .collect::<HashMap<_, _>>();

              // 2. Differences not found in *any* of the join segments are ignored
              for subtree in joins.iter() {
                let mut temp = HashMap::default();
                diff_subtree(ctxt, subtree, &mut temp, attached_at);

                // HACK: remove any differences that were attached to this span.
                temp.remove(span);

                // HACK: manually remove any attached places which got added.
                for (_, (_, diffs)) in temp.iter_mut() {
                  diffs
                    .drain_filter(|place, _| attached_here.contains_key(place));
                }

                joined_diff.extend(temp);
              }

              assert!(!result.contains_key(span));
              assert!(joined_diff.get(span).is_none());

              // FIXME: the reach is not the correct set of points here.
              // But we don't currently have a good semantic model for
              // what it should be. They aren't currently being
              // displayed by the frontend so this isn't a problem (yet).
              result.insert(*span, (*reach, attached_here));
              result.extend(joined_diff);
            }
          }

          // Remove the attached places filter.
          for local in attached.iter() {
            attached_at.remove(local);
          }
        }
      }
    }

    let mut diffs = HashMap::default();
    let mut attached_at = HashMap::default();
    let dummy_loc = Location {
      block: mir::START_BLOCK,
      statement_index: 0,
    };

    diffs.insert(
      body_open_brace,
      (
        MirSegment {
          from: dummy_loc,
          to: dummy_loc,
        },
        first_diff,
      ),
    );

    diff_subtree(self.ctxt, &self.mir_segments, &mut diffs, &mut attached_at);

    diffs
  }

  fn body_segment(&self) -> &MirSegment {
    match self.mir_segments.as_ref() {
      SegmentTree::Split { reach, .. } => reach,
      SegmentTree::Single { segment, .. } => segment,
    }
  }
}

macro_rules! split_with_control_flow {
  ($this:tt, $ids:expr) => {
    split_with_control_flow!($this, $ids, "CF-SPLIT <anon>")
  };

  ($this:tt, $ids:expr, $msg:expr) => {
    let f = format!("{}\nsplitting the control flow with:\n{:#?}", $msg, $ids);
    $ids
      .into_iter()
      .map(|id| {
        $this
          .ir_mapper
          .get_mir_locations(id, GatherDepth::Nested)
          .and_then(|mir_order| {
            mir_order.entry_location().map(|entry| {
              let span = $this.span_of(id).shrink_to_lo();
              (entry, span)
            })
          })
      })
      .fold(Some(Vec::default()), |acc, step| {
        if let (Some(mut acc), Some(step)) = (acc, step) {
          acc.push(step);
          Some(acc)
        } else {
          None
        }
      })
      .and_then(|steps| $this.insert_cf_step_at(steps).ok())
      .unwrap_or_else(|| {
        $this.report_fatal(&f);
      });
  };
}

macro_rules! split_with_linear {
  ($this:tt, $id:expr) => {
    split_with_linear!($this, $id, "splitting linearly")
  };

  ($this:tt, $id:expr, $msg:expr) => {
    split_with_linear!($this, $id, $msg, vec![])
  };

  ($this:tt, $id:expr, $msg:expr, $attached:expr) => {
    $this
      .ir_mapper
      .get_mir_locations($id, GatherDepth::Nested)
      .and_then(|mir_order| {
        mir_order.exit_location().map(|exit| {
          let span = $this.span_of($id);
          let exit = $this
            .ir_mapper
            .cleaned_graph
            .location_successor(exit)
            .unwrap_or(exit);
          $this
            .insert_linear_step_at(span, exit, $attached)
            .expect("");
        })
      })
      .unwrap_or_else(|| {
        log::warn!(
          "Expected entry / exit locations but none were found: {:?}",
          $msg
        );
      });
  };
}

impl<'a, 'tcx: 'a> HirVisitor<'tcx> for HirStepPoints<'a, 'tcx> {
  type NestedFilter = nested_filter::All;

  fn nested_visit_map(&mut self) -> Self::Map {
    self.ctxt.tcx.hir()
  }

  fn visit_stmt(&mut self, stmt: &'tcx hir::Stmt) {
    use rustc_hir::StmtKind as SK;
    let hir = self.nested_visit_map();
    let error_msg =
      format!("Analyzing statement : {}", hir.node_to_string(stmt.hir_id));

    let locals = match stmt.kind {
      SK::Local(local) => {
        let places = self.ir_mapper.local_assigned_place(local);
        places.into_iter().map(|p| p.local).collect::<Vec<_>>()
      }
      _ => vec![],
    };

    split_with_linear!(self, stmt.hir_id, error_msg, locals);

    if self.should_visit_nested(stmt.hir_id, stmt.span) {
      intravisit::walk_stmt(self, stmt);
    }
  }

  fn visit_block(&mut self, block: &'tcx hir::Block) {
    let hir = self.ctxt.tcx.hir();

    for stmt in block.stmts.iter() {
      self.visit_stmt(stmt);
    }

    if let Some(expr) = block.expr {
      let error_msg =
        format!("end-of-statement expr: {}", hir.node_to_string(expr.hir_id));
      split_with_linear!(self, expr.hir_id, error_msg);
      self.visit_expr(expr);
    }
  }

  fn visit_expr(&mut self, expr: &'tcx hir::Expr) {
    use hir::{ExprKind as EK, LoopSource, StmtKind as SK};

    let hir = self.nested_visit_map();
    let error_msg =
      format!("Analyzing expr : {}", hir.node_to_string(expr.hir_id));

    match expr.kind {
      // Special case for While Loop desugaring, this shouldn't be necessary
      // when generic loops are handled.
      EK::Loop(
        hir::Block {
          stmts: [],
          expr:
            Some(hir::Expr {
              kind: EK::If(cnd, then, Some(els)),
              ..
            }),
          ..
        },
        _label,
        LoopSource::While,
        _loop_span,
      ) => {
        self
          .ir_mapper
          .get_mir_locations(then.hir_id, GatherDepth::Nested)
          .and_then(|mir_order| {
            mir_order.entry_location().map(|then_entry| {
              self
                .ir_mapper
                .get_mir_locations(els.hir_id, GatherDepth::Nested)
                .and_then(|mir_order| {
                  mir_order.exit_location().map(|else_exit| {
                    let loop_end = self.span_of(expr.hir_id).shrink_to_hi();
                    let if_start = self.span_of(then.hir_id).shrink_to_lo();

                    let ls =
                      vec![(then_entry, if_start), (else_exit, loop_end)];

                    self.insert_cf_step_at(ls).expect("");
                  })
                })
                .unwrap();
            })
          })
          .unwrap();

        // Skip the else block, it only contains the break statement.
        intravisit::walk_expr(self, cnd);
        intravisit::walk_expr(self, then);
      }

      // Special case for For Loop desugaring, this shouldn't be necessary
      // when generic loops are handled.
      EK::Loop(
        hir::Block {
          stmts:
            [hir::Stmt {
              kind:
                SK::Expr(hir::Expr {
                  kind: EK::Match(cnd, [none, some], _),
                  ..
                }),
              ..
            }],
          expr: None,
          ..
        },
        _label,
        LoopSource::ForLoop,
        _loop_span,
      ) => {
        self
          .ir_mapper
          .get_mir_locations(some.body.hir_id, GatherDepth::Nested)
          .and_then(|mir_order| {
            mir_order.entry_location().map(|then_entry| {
              self
                .ir_mapper
                .get_mir_locations(none.body.hir_id, GatherDepth::Nested)
                .and_then(|mir_order| {
                  mir_order.exit_location().map(|else_exit| {
                    let loop_end = self.span_of(expr.hir_id).shrink_to_hi();
                    let loop_start =
                      self.span_of(some.body.hir_id).shrink_to_lo();

                    let ls =
                      vec![(then_entry, loop_start), (else_exit, loop_end)];

                    self.insert_cf_step_at(ls).expect("");
                  })
                })
                .unwrap();
            })
          })
          .unwrap();

        // ignore the none branch as it just contains the break.
        intravisit::walk_expr(self, cnd);
        intravisit::walk_arm(self, some);
      }

      // TODO: have a split strategy for bare loops. They could be infinite, and
      // thus have no exit block. This shouldn't be an issue but it currently is.
      EK::Loop(_block, _label, LoopSource::Loop, _span) => {
        self.report_unsupported(expr.hir_id, "Bare loops aren't working yet, sorry! Can I interest you in a `for` or `while` loop?");
      }

      EK::If(cnd, then, else_opt) => {
        // NOTE: first we need to walk and split the condition. In the
        // case of a more complex condition expression, splitting this
        // first will result in a split location closest to the `SwitchInt`.
        intravisit::walk_expr(self, cnd);

        let ids = [Some(then), else_opt]
          .iter()
          .flatten()
          .map(|n| n.hir_id)
          .collect::<Vec<_>>();

        split_with_control_flow!(self, ids, error_msg);

        intravisit::walk_expr(self, then);
        if let Some(els) = else_opt {
          intravisit::walk_expr(self, els);
        }
      }

      EK::Match(swtch, arms, _source) => {
        // NOTE: first we need to walk and split the condition. In the
        // case of a more complex condition expression, splitting this
        // first will result in a split location closest to the `SwitchInt`.
        intravisit::walk_expr(self, swtch);

        let ids = arms
          .iter()
          .map(|arm| {
            if arm.guard.is_some() {
              self.report_unsupported(
                arm.hir_id,
                "Arm guards are not supported, sorry!",
              )
            }

            arm.body.hir_id
          })
          .collect::<Vec<_>>();

        split_with_control_flow!(self, ids, error_msg);

        for arm in arms.iter() {
          intravisit::walk_arm(self, arm);
        }
      }
      _ => {
        intravisit::walk_expr(self, expr);
      }
    }
  }
}
