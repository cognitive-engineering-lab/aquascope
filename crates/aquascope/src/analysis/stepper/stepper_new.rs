//! Core analysis for creating permission steps.
//!
//! # Overview
//!
//! A *permissions step* is the difference in permissions between two MIR `Point`s.
//! Specifically, a step represents the changes needing to happen to reach a subsequent
//! state in the permissions static analysis.
//!
//! At a high-level, the strategy is to partition the MIR into pieces (referred to as segments),
//! such that each piece represents a single permission step. This means, that (theoretically)
//! after the graph has been partitioned the differences are taken in isolation.
//! It isn't quite this straightforward so additional details are provided below.
//!
//! # Splitting Strategy
//!
//! The main goal of the permission stepper is to provide steps that map to logical "steps" in
//! the source code. First, the steps will be determined using HIR language constructs, which are
//! subsequently lowered to fit the more granular MIR language constructs.
//! Starting with the HIR, a so-called "logical step" is roughly defined to be a [`Stmt`](rustc_hir::Stmt).
//! Typically statements fall on their own line and they mark the beginning and end
//! of some potentially permissions-altering operation. This makes up the first loose rule for
//! finding permissions steps.
//!
//! Statements however, do not cover how permissions change in a control-flow sensitive setting.
//! For example, the statements at the beginning of the then and else branch might execute with
//! different permissions, this sudden change of permissions needs to be communicated to the user.
//! This leads to another rule, namely, a branch in control flow can also be a permissions-altering
//! "operation". The full rules for tracking permissions steps at each respective level of granularity
//! are outlined below.
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
//! Each node in the HIR generates several MIR locationss. For information on how to map between the
//! two see the [`IRMapper`]. Important for the stepper, is the ability to find the first, and
//! last, MIR [`Location`] which came from a HIR node. First and last are used in the sense of
//! a [dominator](https://en.wikipedia.org/wiki/Dominator_(graph_theory)) and [post-dominator](https://en.wikipedia.org/wiki/Dominator_(graph_theory)#Postdominance)
//! respectively. These locations will be used later (described below) when slicing the MIR graph.
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
//! ### Data Structures and Invariants
//!
//! The key data structures involvled are the [`MirSegment`] and [`SegmentTree`].
//! The [`MirSegment`] TODO what even is a segment?
//!
//! The [`SegmentTree`] (not to be confused with a [segment tree](https://en.wikipedia.org/wiki/Segment_tree))
//! is a tree which holds [`MirSegment`]s in its leaves.
//!
//! ### Slicing
//!
//! The core operation performed on the [`SegmentTree`] is taking a *slice*. There are two kinds of
//! slices:
//!
//! 1. linear slices, those that **do not** span several branches of control-flow.
//! 2. control-flow slices, those that **do** span several branches of control-flow.
//!
//! These two slices exist to maintain the invariants of the [`MirSegment`] and [`SegmentTree`].
//! Fundamentally, these slices work on different *shapes* of the underlying graph. A *linear slice*
//! would slice a portion of the graph which forms a line.
//!
//! Example:
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
//! In the above example there exists a linear sequence of control-flow from `l1 ⟶ l2 ⟶ l3`.
//! Depicted, is a *linear slice* of this segment at location `l2`. Linear slices *always*
//! split a single segment, into two new segments which maintain the [`MirSegment`] invariant.
//! These slices are used after [`Stmt`s](rustc_hir::Stmt) and the end of a [`Block` expression](rustc_hir::Block).
//!
//! A *control-flow* slice, then does not slice a linear sequence of locations but multiple that
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
//! be further split. They are treated as **atomic**. TODO: (elaborate) in practice they currently
//! wouldn't ever be split again, but if the analysis changes they need to remain that way.
//!
//! # Finalizing Differences
//!
//! Slicing the MIR into segments is the core task for the stepper and results
//! in a proper [`SegmentTree`]. The last task of the stepper engine is to take the
//! permissions difference between the domain after the segment, and that before.
//! See the [`PermissionsCtxt`] for more information about computing a [`PermissionsDomain`].
//!
//! When computing the differences however, there are two edge cases to be handled. First,
//! as a result of the generated MIR, it's possible for the left-hand-side of an assignment
//! to gain permissions before it seems it should. This occurs when the initializer expression
//! is more complex (e.g. an [`If`](rustc_hir::Expr) or [`Block`](rustc_hir::Expr) expression).
//! To ensure initialized places don't gain permissions before the end of the let statement,
//! these places are marked as /attached/ to a specific MIR location, and they are filtered
//! from any nested segment step results.
//!
//! The second case is TODO: NOT YET HANDLED.
//!
//! (but here's the scoop if you're still reading)
//! The second case is the join points of a sliced control-flow segment. This results in multiple
//! steps being placed on the same span, however semantically represent different steps. This can
//! either be resolved visually, e.g. by placing multiple tables at the end of a join and
//! differentiating them somehow. Another option would be to ignore the control-flow specific
//! steps and only show the step from the beginning of the control-flow to the join point.

use anyhow::{bail, Result};
use flowistry::mir::utils::{PlaceExt as FlowistryPlaceExt, SpanExt};
use rustc_data_structures::{
  self,
  fx::{FxHashMap as HashMap, FxHashSet as HashSet},
};
use rustc_hir::{
  self as hir,
  intravisit::{self, Visitor as HirVisitor},
  HirId,
};
use rustc_middle::{
  hir::nested_filter,
  mir::{BasicBlock, BasicBlocks, Local, Location, Place},
};
use rustc_span::Span;

use super::*;
use crate::{
  analysis::{
    ir_mapper::{CleanedBody, GatherDepth, GatherMode, IRMapper},
    permissions::{
      Permissions, PermissionsCtxt, PermissionsData, PermissionsDomain,
    },
  },
  errors,
  mir::utils::PlaceExt as AquascopePlaceExt,
  Range,
};

pub fn compute_permission_steps<'a, 'tcx>(
  ctxt: &PermissionsCtxt<'a, 'tcx>,
  mode: PermIncludeMode,
  span_to_range: impl Fn(Span) -> Range,
) -> Vec<PermissionsStateStep>
where
  'tcx: 'a,
{
  // FIXME: remove
  crate::analysis::permissions::utils::dump_mir_debug(ctxt);

  let body = &ctxt.body_with_facts.body;
  let _basic_blocks = body.basic_blocks.indices();
  let mut hir_visitor = HirStepPoints::make(ctxt);
  hir_visitor.visit_nested_body(ctxt.body_id);

  log::debug!("Final tree\n{:?}", hir_visitor.mir_segments);

  prettify_permission_steps(
    ctxt,
    hir_visitor.finalize_diffs(),
    mode,
    span_to_range,
  )
}

// Prettify, means:
// - Remove all places that are not source visible
// - Remove all tables which are empty
// - Convert Spans to Ranges
fn prettify_permission_steps<'tcx>(
  ctxt: &PermissionsCtxt<'_, 'tcx>,
  perm_steps: HashMap<Span, HashMap<Place<'tcx>, PermissionsDataDiff>>,
  mode: PermIncludeMode,
  span_to_range: impl Fn(Span) -> Range,
) -> Vec<PermissionsStateStep> {
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
      HashMap::<Span, Vec<(Place<'tcx>, PermissionsDataDiff)>>::default(),
      |mut acc, (span, place_to_diffs)| {
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
          acc.entry(span).or_default().extend(entries);
        }

        acc
      },
    )
    .into_iter()
    .map(|(span, mut entries)| {
      let range = span_to_range(span);

      entries
        .sort_by_key(|(place, _)| (place.local.as_usize(), place.projection));

      let state = entries
        .into_iter()
        .map(|(place, diff)| {
          let s = place_to_string!(place);
          (s, diff)
        })
        .collect::<Vec<_>>();

      PermissionsStateStep {
        location: range,
        state,
      }
    })
    .collect::<Vec<_>>()
}

// ------------------------------------------------

/// Represents a segment of the MIR control-flow graph.
///
/// A `MirSegment` corresponds directly to locations where a permissions step
/// will be made. However, a segment is also control-flow specific.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct MirSegment {
  from: Location,
  to: Location,
}

/// The types of splits that can be performed on a [`SegmentTre::Single`].
#[derive(Clone)]
enum SplitType {
  /// A split of a segment that is not due to control flow.
  /// Example, after each `Stmt` a step is created, this is simply
  /// a step in a linear sequence.
  Linear {
    first: Box<SegmentTree>,
    second: Box<SegmentTree>,
  },

  /// Split of a complex control flow.
  /// For example, the `split_segments` of an `ExprKind::If` would be the segments
  /// from the if condition, to the start of the then / else blocks.
  /// The `join_segments` are all the those that end at the same join point.
  ///
  /// NOTE: any segment stored in the `splits` of a SplitType::ControlFlow
  /// can not be split again, these are *atomic*.
  ControlFlow {
    splits: Vec<SegmentTree>,
    joins: Vec<SegmentTree>,
  },
}

/// A `SegmentTree` represents the control flow graph of a MIR `Body`.
/// It's used to keep track of the entire graph as it is sliced during
/// the permission steps analysis.
#[derive(Clone)]
enum SegmentTree {
  /// An inner tree node with children.
  Split {
    segments: SplitType,
    reach: MirSegment,
    span: Span,
    attached: Vec<Local>,
  },

  /// A leaf segment that is expected to be split again later.
  Single {
    segment: MirSegment,
    span: Span,
    attached: Vec<Local>,
  },
}

#[derive(Clone, Debug)]
enum SegmentSearchResult<'a> {
  Enclosing(&'a SegmentTree),
  StepExisits(MirSegment, Span),
  NotFound,
}

// ------------------------------------------------
// Debugging pretty printers

impl std::fmt::Debug for MirSegment {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "MirSegment({:?} -> {:?})", self.from, self.to)
  }
}

impl std::fmt::Debug for SegmentTree {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    fn print_loop(
      f: &mut std::fmt::Formatter,
      tree: &SegmentTree,
      spaces: usize,
    ) -> std::fmt::Result {
      let indent_size = 4;
      match tree {
        SegmentTree::Single {
          segment, attached, ..
        } => {
          write!(
            f,
            "{}SegmentTree::Single: {segment:?}\n",
            " ".repeat(spaces)
          )?;
          write!(
            f,
            "{}-locals attached to end {:?}\n",
            " ".repeat(spaces),
            attached,
          )
        }
        SegmentTree::Split {
          segments: SplitType::Linear { first, second },
          reach,
          attached,
          ..
        } => {
          write!(
            f,
            "{}SegmentTree::Split [LINEAR]: {reach:?}\n",
            " ".repeat(spaces)
          )?;
          write!(
            f,
            "{}-locals attached to end {:?}\n",
            " ".repeat(spaces),
            attached,
          )?;
          print_loop(f, first, spaces + indent_size)?;
          write!(f, "\n")?;
          print_loop(f, second, spaces + indent_size)?;
          write!(f, "\n")?;

          Ok(())
        }

        SegmentTree::Split {
          segments: SplitType::ControlFlow { splits, joins },
          reach,
          attached,
          ..
        } => {
          write!(
            f,
            "{}SegmentTree::Split [CF]: {reach:?}\n",
            " ".repeat(spaces)
          )?;
          write!(
            f,
            "{}-locals attached to end {:?}\n",
            " ".repeat(spaces),
            attached,
          )?;
          write!(f, "{}Splits:\n", " ".repeat(spaces))?;
          for tree in splits.iter() {
            print_loop(f, tree, spaces + indent_size)?;
            write!(f, "\n")?;
          }
          write!(f, "\n")?;

          write!(f, "{}Joins:\n", " ".repeat(spaces))?;
          for tree in joins.iter() {
            print_loop(f, tree, spaces + indent_size)?;
            write!(f, "\n")?;
          }

          Ok(())
        }
      }
    }

    print_loop(f, self, 0)
  }
}

// ------------------------------------------------
// Impls

impl MirSegment {
  fn new(l1: Location, l2: Location) -> Self {
    MirSegment { from: l1, to: l2 }
  }

  fn squash_block_path(
    &self,
    basic_blocks: &BasicBlocks,
    path: impl Iterator<Item = BasicBlock>,
  ) -> Vec<Location> {
    path
      .flat_map(|bb| {
        let bbd = &basic_blocks[bb];
        let from = if bb == self.from.block {
          self.from.statement_index
        } else {
          0
        };

        let to = if bb == self.to.block {
          self.to.statement_index
        } else {
          bbd.statements.len()
        };

        (from ..= to).map(move |idx| Location {
          block: bb,
          statement_index: idx,
        })
      })
      .collect::<Vec<_>>()
  }

  fn paths_along_segment(&self, graph: &CleanedBody) -> Vec<Vec<BasicBlock>> {
    graph.paths_from_to(self.from.block, self.to.block)
  }

  fn spanned_locations(&self, graph: &CleanedBody) -> HashSet<Location> {
    let block_paths = self.paths_along_segment(&graph);
    let body = graph.body();
    block_paths
      .into_iter()
      .flat_map(|path| {
        self.squash_block_path(&body.basic_blocks, path.into_iter())
      })
      .collect::<HashSet<_>>()
  }

  fn into_diff<'tcx>(
    &self,
    ctxt: &PermissionsCtxt<'_, 'tcx>,
  ) -> HashMap<Place<'tcx>, PermissionsDataDiff> {
    let p0 = ctxt.location_to_point(self.from);
    let p1 = ctxt.location_to_point(self.to);
    let before = &ctxt.permissions_domain_at_point(p0);
    let after = &ctxt.permissions_domain_at_point(p1);
    before.diff(after)
  }
}

impl SegmentTree {
  fn new(body: MirSegment, span: Span) -> Self {
    Self::Single {
      segment: body,
      span,
      attached: vec![],
    }
  }

  /// Find a [`SegmentTree::Single`] node which matches *exactly* the given segment.
  fn find_single(&mut self, segment: MirSegment) -> Option<&mut SegmentTree> {
    let node = &mut *self;

    match node {
      SegmentTree::Single { segment: seg, .. } if *seg == segment => Some(node),
      SegmentTree::Single { .. } => None,
      SegmentTree::Split {
        segments: SplitType::ControlFlow { joins, .. },
        ..
      } => {
        // NOTE: the split set is regarded as atomic so
        // it isn't included in the search.
        for s in joins.iter_mut() {
          let r = s.find_single(segment);
          if r.is_some() {
            return r;
          }
        }

        None
      }

      SegmentTree::Split {
        segments: SplitType::Linear { first, second },
        ..
      } => first
        .as_mut()
        .find_single(segment)
        .or_else(|| second.as_mut().find_single(segment)),
    }
  }

  /// Replace a [`SegmentTree::Single`] node which matches *exactly* the given segment.
  /// The subtree must fragment the [`MirSegment`] correctly, otherwise the tree
  /// will enter an invalid state.
  fn replace_single(
    &mut self,
    to_replace: MirSegment,
    subtree: SegmentTree,
  ) -> Result<()> {
    // TODO better error handling here.
    let node = self.find_single(to_replace);

    if node.is_none() {
      bail!("the provided mir segment to replace doesn't exist {to_replace:?}");
    }

    let node = node.unwrap();

    if let SegmentTree::Single { segment, .. } = node {
      assert_eq!(to_replace, *segment);
    } else {
      bail!("SegmentTree::find_single can only return a Single variant. This is an implementation bug");
    }

    *node = subtree;

    Ok(())
  }

  fn subtree_contains(&self, location: Location, graph: &CleanedBody) -> bool {
    let segment = match self {
      SegmentTree::Split { reach, .. } => reach,
      SegmentTree::Single { segment, .. } => segment,
    };
    let locs = segment.spanned_locations(graph);
    locs.contains(&location)
  }

  /// Find the /leaf/ [`MirSegment`] and it's corresponding `Span` that enclose
  /// `location`. The `location` is expected to be used as the end of step.
  fn find_segment_for_end<'a>(
    &'a self,
    location: Location,
    graph: &CleanedBody,
  ) -> SegmentSearchResult<'a> {
    match self {
      SegmentTree::Single { segment, .. } if segment.to != location => {
        SegmentSearchResult::Enclosing(self)
      }

      SegmentTree::Single { segment, span, .. } => {
        SegmentSearchResult::StepExisits(*segment, *span)
      }

      SegmentTree::Split {
        segments: SplitType::Linear { first, second },
        ..
      } => {
        if first.subtree_contains(location, graph) {
          first.find_segment_for_end(location, graph)
        } else if second.subtree_contains(location, graph) {
          second.find_segment_for_end(location, graph)
        } else {
          SegmentSearchResult::NotFound
        }
      }

      SegmentTree::Split {
        segments: SplitType::ControlFlow { joins, .. },
        ..
      } =>
      // NOTE: the split locations are atomic and cannot be split.
      {
        joins
          .iter()
          .find(|s| s.subtree_contains(location, graph))
          .map(|next| next.find_segment_for_end(location, graph))
          .unwrap_or(SegmentSearchResult::NotFound)
      }
    }
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
  ir_mapper: IRMapper<'a, 'tcx>,
  mir_segments: Box<SegmentTree>,
}

impl<'a, 'tcx: 'a> HirStepPoints<'a, 'tcx> {
  fn make(ctxt: &'a PermissionsCtxt<'a, 'tcx>) -> Self {
    let tcx = ctxt.tcx;
    let hir = tcx.hir();
    let body = &ctxt.body_with_facts.body;
    let ir_mapper = IRMapper::new(tcx, body, GatherMode::IgnoreCleanup);
    let body = &hir.body(ctxt.body_id);
    let body_hir_id = body.value.hir_id;
    let body_span = body.value.span;
    // FIXME: this really shouldn't ever fail but having to
    // `unwrap`s feels dirty.
    let (from, to) = ir_mapper
      .get_mir_locations(body_hir_id, GatherDepth::Nested)
      .unwrap()
      .get_entry_exit_locations()
      .unwrap();

    let body_segment = MirSegment::new(from, to);
    let mir_segments = Box::new(SegmentTree::new(body_segment, body_span));

    HirStepPoints {
      ctxt,
      ir_mapper,
      mir_segments,
    }
  }
}

impl<'a, 'tcx: 'a> HirStepPoints<'a, 'tcx> {
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
      SegmentSearchResult::NotFound => unreachable!("{location:?} should always be enclosed in the graph. This is an implementation bug."),
      SegmentSearchResult::StepExisits(segment, ..) =>
        Ok(()),
        // bail!("linear steps should not conflict at step locations {location:?} at {segment:?}"),

      SegmentSearchResult::Enclosing(SegmentTree::Single {
        segment, span: old_span, attached,
      }) => {
        let mut paths = segment.paths_along_segment(&self.ir_mapper.cleaned_graph);

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
          bail!("Inserting a linear segment should not result in fragmentation.\nSplitting segment: {segment:?} at {location:?}. Remaining paths: {paths:#?}");
        }

        let subtree = SegmentTree::Split {
          segments: SplitType::Linear {
            first: Box::new(first_step),
            second: Box::new(second_step),
          },
          reach: *segment,
          span: *old_span,
          attached: attached.to_vec(),
        };

        let segment = *segment;
        self.mir_segments.as_mut().replace_single(segment, subtree)
      }

      _ => unreachable!(),
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
    log::debug!("Current segment tree:\n{:?}", self.mir_segments);

    if steps.is_empty() {
      return Ok(());
    }

    log::debug!("inserting steps {steps:?}");

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
      bail!("not every locations step had an enclosing segment.");
    }

    let (segment, old_span, attached) = enclosings.first().unwrap();

    if !enclosings.iter().all(|(s, _, _)| s == segment) {
      bail!("not all provided locations map to the same enclosing segment: {enclosings:#?}");
    }

    let mut paths = segment.paths_along_segment(&self.ir_mapper.cleaned_graph);

    log::debug!("the paths along the segment {segment:?} are:\n{paths:#?}");

    let mut splits = Vec::default();
    let mut joins = Vec::default();

    for (location, span) in steps.into_iter() {
      // if paths.is_empty() {
      //   bail!("no remaining paths for {location:?} to cover");
      // }

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

      let removed_paths = paths
        .drain_filter(|path| path.contains(&location.block))
        .collect::<Vec<_>>();

      // if removed_paths.is_empty() {
      //   bail!("location {location:?} didn't cover any new paths");
      // }

      splits.push(split_step);
      joins.push(join_step);
    }

    let subtree = SegmentTree::Split {
      segments: SplitType::ControlFlow { splits, joins },
      reach: *segment,
      span: *old_span,
      attached: attached.to_vec(),
    };

    self.mir_segments.replace_single(*segment, subtree)
  }

  fn span_of(&self, id: HirId) -> Span {
    let hir = self.ctxt.tcx.hir();
    let span = hir.span(id);
    let sanitized = span
      .as_local(self.ctxt.body_with_facts.body.span)
      .unwrap_or(span);
    sanitized
  }

  fn body_value_id(&self) -> HirId {
    let hir = self.ctxt.tcx.hir();
    hir.body(self.ctxt.body_id).value.hir_id
  }

  /// The [`PermissionDomain`] ⊥.
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
          path_moved: false,
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
  ) -> HashMap<Span, HashMap<Place<'tcx>, PermissionsDataDiff>> {
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
      result: &mut HashMap<Span, HashMap<Place<'tcx>, PermissionsDataDiff>>,
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

          result.insert(span, diff);
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
              for subtree in splits.into_iter() {
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
              for subtree in joins.into_iter() {
                let mut temp = HashMap::default();

                diff_subtree(ctxt, subtree, &mut temp, attached_at);

                temp.get(&span).map(|diff_here| {
                  entire_diff.drain_filter(|place, diff| {
                    !diff.is_empty()
                      && diff_here
                        .get(place)
                        .map(|perm_diff| perm_diff.is_empty())
                        .unwrap_or(false)
                  })
                });

                // HACK: remove any differences that were attached to this span.
                temp.remove(span);

                // HACK: manually remove any attached places which got added.
                for (_, diffs) in temp.iter_mut() {
                  diffs
                    .drain_filter(|place, _| attached_here.contains_key(place));
                }

                joined_diff.extend(temp);
              }

              let to_insert = attached_here.into_iter().chain(entire_diff);
              result.entry(*span).or_default().extend(to_insert);

              assert!(joined_diff.get(span).is_none());

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
    diffs.insert(body_open_brace, first_diff);

    diff_subtree(&self.ctxt, &self.mir_segments, &mut diffs, &mut attached_at);

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
    split_with_control_flow!($this, $ids, "splitting control flow")
  };

  ($this:tt, $ids:expr, $msg:expr) => {
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
        unreachable!("splitting control flow failed");
        // log::error!();
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
        if cfg!(debug_assertions) {
          panic!(
            "Expected entry / exit locations but none were found: {:?}",
            $msg
          );
        } else {
          log::error!(
            "Expected entry / exit locations but none were found: {:?}",
            $msg
          );
        }
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

    match stmt.kind {
      SK::Local(local) => {
        let places = self.ir_mapper.local_assigned_place(local);
        let locals = places.into_iter().map(|p| p.local).collect::<Vec<_>>();
        split_with_linear!(self, stmt.hir_id, error_msg, locals);
      }
      _ => {
        split_with_linear!(self, stmt.hir_id, error_msg);
      }
    }

    intravisit::walk_stmt(self, stmt);
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
    use hir::{ExprKind as EK, LoopSource, MatchSource, StmtKind as SK};

    let hir = self.ctxt.tcx.hir();

    let error_msg =
      format!("splitting expr segment {}", hir.node_to_string(expr.hir_id));

    match expr.kind {
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

        intravisit::walk_expr(self, cnd);
        intravisit::walk_expr(self, then);
        // Skip the else block, it only contains the break statement.
      }

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

        intravisit::walk_expr(self, cnd);
        intravisit::walk_arm(self, some);
        // ignore the none branch as it just contains the break.
      }

      EK::Loop(_block, _label, LoopSource::Loop, _span) => {
        todo!("bare loops aren't working yet, sorry!")
      }

      EK::If(cnd, then, else_opt) => {
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
        intravisit::walk_expr(self, swtch);

        let ids = arms
          .iter()
          .map(|arm| {
            if let Some(_) = &arm.guard {
              todo!("Arm guards are not supported, sorry!");
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
