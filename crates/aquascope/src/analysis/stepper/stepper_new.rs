//! Core analysis for creating Permission Steps.
//!
//! # Overview
//!
//! A *permissions step* is the difference in permissions between two MIR `Point`s.
//!
//! At a high-level, the strategy is to partition the MIR into pieces
//! (referred to as segments), such that each piece represents a single
//! permission step. This means, that (theoretically) after the graph has been partitioned the
//! differences are taken in isolation. It isn't quite this straightforward so additional details are
//! provided below.
//!
//! # Splitting Strategy
//!
//! TODO
//!
//! # Finalizing Differences
//!
//! TODO
//!
//!

use std::collections::hash_map::Entry;

use anyhow::{anyhow, bail, Result};
use flowistry::mir::utils::{PlaceExt as FlowistryPlaceExt, SpanExt};
use rustc_data_structures::{
  captures::Captures,
  fx::{FxHashMap as HashMap, FxHashSet as HashSet},
  graph::{self, iterate},
};
use rustc_hir::{
  self as hir,
  intravisit::{self, Visitor as HirVisitor},
  HirId,
};
use rustc_middle::{
  hir::nested_filter,
  mir::{BasicBlock, BasicBlocks, Body, Local, Location, Place, Successors},
};
use rustc_span::Span;

use super::*;
use crate::{
  analysis::{
    ir_mapper::{CleanedBody, GatherDepth, GatherMode, IRMapper},
    permissions::{
      Permissions, PermissionsCtxt, PermissionsData, PermissionsDomain, Point,
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
// - Sanitize spans (mostly for macro invocation)
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
        // let span = unsanitized_span
        //   .as_local(ctxt.body_with_facts.body.span)
        //   .unwrap_or(unsanitized_span);

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

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct MirSegment {
  from: Location,
  to: Location,
}

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
  ControlFlow {
    splits: Vec<SegmentTree>,
    joins: Vec<SegmentTree>,
  },
}

#[derive(Clone)]
/// A `SegmentTree` represents the control flow graph of a MIR `Body`.
/// It's used to keep track of the entire graph as it is sliced during
/// the permission steps analysis.
enum SegmentTree {
  /// An inner tree node with children.
  Split {
    segments: SplitType,
    reach: MirSegment,
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
            "{}  locals attached to end {:?}\n",
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
            "{}  locals attached to end {:?}\n",
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
          ..
        } => {
          write!(
            f,
            "{}SegmentTree::Split [CF]: {reach:?}\n",
            " ".repeat(spaces)
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
        segments: SplitType::ControlFlow { splits, joins },
        ..
      } => {
        for s in splits.iter_mut().chain(joins.iter_mut()) {
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
      SegmentTree::Single { segment, span, .. } if segment.to != location => {
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
        } else {
          second.find_segment_for_end(location, graph)
        }
      }

      SegmentTree::Split {
        segments: SplitType::ControlFlow { splits, joins },
        ..
      } => splits
        .iter()
        .chain(joins)
        .find(|s| s.subtree_contains(location, graph))
        .map(|next| next.find_segment_for_end(location, graph))
        .unwrap_or(SegmentSearchResult::NotFound),
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
  fn insert_linear_step_at(
    &mut self,
    span: Span,
    location: Location,
    attached_here: Vec<Local>,
  ) -> Result<()> {
    log::debug!("inserting linear step {location:?}");

    let enclosing_segment = self
      .mir_segments
      .as_ref()
      .find_segment_for_end(location, &self.ir_mapper.cleaned_graph);

    match enclosing_segment {
      SegmentSearchResult::NotFound => unreachable!("{location:?} should always be enclosed in the graph. This is an implementation bug."),
      SegmentSearchResult::StepExisits(segment, ..) => bail!("linear steps should not conflict at step locations {location:?} at {segment:?}"),

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

        log::debug!("\nSlicing range [{:?} -> {:?}] into:\n  {first_step:?}\n  {second_step:?}", segment.from, segment.to);

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
    log::debug!("inserting a step! {:?}", self.mir_segments);

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
      bail!("not every locations step had an enclosing segment.");
    }

    let (segment, old_span, attached) = enclosings.first().unwrap();

    if !enclosings.iter().all(|(s, _, _)| s == segment) {
      bail!("not all provided locations map to the same enclosing segment: {enclosings:#?}");
    }

    let mut paths = segment.paths_along_segment(&self.ir_mapper.cleaned_graph);

    let mut splits = Vec::default();
    let mut joins = Vec::default();

    for (location, span) in steps.into_iter() {
      if paths.is_empty() {
        bail!("no remaining paths for {location:?} to cover");
      }

      let split_step = SegmentTree::Single {
        segment: MirSegment::new(segment.from, location),
        attached: vec![],
        span,
      };

      let join_step = SegmentTree::Single {
        segment: MirSegment::new(location, segment.to),
        attached: attached.clone(),
        span: *old_span,
      };

      let removed_paths = paths
        .drain_filter(|path| path.contains(&location.block))
        .collect::<Vec<_>>();

      if removed_paths.is_empty() {
        bail!("location {location:?} didn't cover any new paths");
      }

      splits.push(split_step);
      joins.push(join_step);
    }

    let subtree = SegmentTree::Split {
      segments: SplitType::ControlFlow { splits, joins },
      reach: *segment,
      attached: vec![],
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
      let mut insert_segment = |MirSegment { from, to }: MirSegment,
                                span: Span| {
        if from != to {
          let p0 = ctxt.location_to_point(from);
          let p1 = ctxt.location_to_point(to);
          let before = &ctxt.permissions_domain_at_point(p0);
          let after = &ctxt.permissions_domain_at_point(p1);
          let mut diff = before.diff(after);

          let removed = diff
            .drain_filter(|place, _| {
              attached_at
                .get(&place.local)
                .map(|l| *l != to)
                .unwrap_or(false)
            })
            .collect::<Vec<_>>();

          log::debug!(
            "removed domain places due to attached filter at {to:?} {removed:?}"
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
          ..
        } => {
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
            SplitType::ControlFlow { splits, joins } => {
              for subtree in splits.into_iter() {
                diff_subtree(ctxt, subtree, result, attached_at);
              }

              for subtree in joins.into_iter() {
                diff_subtree(ctxt, subtree, result, attached_at);
              }
            }
          }

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
    log::debug!("looking at a stmt!");

    match stmt.kind {
      SK::Local(local) => {
        log::debug!("pattern: {:?}", local.pat);
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

  // // A Local of the form:
  // //
  // // ```
  // // let symbol = Δ ∪ {symbol} expr else { ⟦ block ⟧ };
  // // ```
  // fn visit_local(&mut self, local: &'tcx hir::Local) {
  //   let id = local.hir_id;
  //   let hir = self.ctxt.tcx.hir();

  //   log::debug!("Visiting LOCAL: {}", hir.node_to_string(id));

  //   // NOTE: We add a "step barrier" for local assignments to make sure the permissions
  //   // for an assigned local don't come alive too early. Consider the following:
  //   // ```
  //   // let x = if <cnd> {
  //   //   <expr:1>
  //   // } else {
  //   //   <expr:2>
  //   // }
  //   // ```
  //   // Due to how the MIR is generated, the MIR statement assigning to `x`,
  //   // happens at the end of the block <expr:1>. This means, that if we don't
  //   // pull those permissions out to the assignment, they will only occur once
  //   // (specifically inside the "then branch") for the whole let.
  //   let mut added_barrier = false;

  //   if let Some(place) = self.ir_mapper.local_assigned_place(local) {
  //     self.step_barriers.push(place);
  //     added_barrier = true;
  //   }

  //   let pre_visibility_ctxt = self.visibility_scopes.clone();

  //   if let Some(expr) = local.init {
  //     self.visit_expr(expr);
  //   }

  //   if let Some(_block) = local.els {
  //     // TODO:
  //     self.visibility_scopes = pre_visibility_ctxt;
  //     unimplemented!("Locals with else branch");
  //   }

  //   if added_barrier {
  //     self.step_barriers.pop();
  //   }
  // }

  fn visit_expr(&mut self, expr: &'tcx hir::Expr) {
    use hir::ExprKind as EK;

    let hir = self.ctxt.tcx.hir();

    let error_msg =
      format!("splitting expr segment {}", hir.node_to_string(expr.hir_id));

    match expr.kind {
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
            // FIXME: this should get turned into an analysis error,
            // we aren't supporting guards currently.
            assert!(arm.guard.is_none());
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
