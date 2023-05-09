use anyhow::{bail, Result};
use rustc_data_structures::{
  self,
  fx::{FxHashMap as HashMap, FxHashSet as HashSet},
};
use rustc_middle::mir::{BasicBlock, BasicBlocks, Local, Location, Place};
use rustc_span::Span;

use super::*;
use crate::analysis::{ir_mapper::CleanedBody, permissions::PermissionsCtxt};

/// The types of splits that can be performed on a [`SegmentTre::Single`].
#[derive(Clone)]
pub enum SplitType {
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
pub enum SegmentTree {
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

/// Search result when trying to find the smallest enclosing segment for a location.
///
/// NOTE: this is used under the assumption that the location cannot be the
/// ending location of a step (this would result in a zero distance step).
#[derive(Clone, Debug)]
pub enum SegmentSearchResult<'a> {
  Enclosing(&'a SegmentTree),
  StepExists(MirSegment, Span),
  NotFound,
}

// ------------------------------------------------
// Debugging pretty printers

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
          segment,
          attached,
          span,
          ..
        } => {
          writeln!(
            f,
            "{}SegmentTree::Single: {segment:?} {span:?}",
            " ".repeat(spaces)
          )?;
          writeln!(
            f,
            "{}-locals attached to end {:?}",
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
          writeln!(
            f,
            "{}SegmentTree::Split [LINEAR]: {reach:?}",
            " ".repeat(spaces)
          )?;
          writeln!(
            f,
            "{}-locals attached to end {:?}",
            " ".repeat(spaces),
            attached,
          )?;
          print_loop(f, first, spaces + indent_size)?;
          writeln!(f)?;
          print_loop(f, second, spaces + indent_size)?;
          writeln!(f)?;

          Ok(())
        }

        SegmentTree::Split {
          segments: SplitType::ControlFlow { splits, joins },
          reach,
          attached,
          ..
        } => {
          writeln!(
            f,
            "{}SegmentTree::Split [CF]: {reach:?}",
            " ".repeat(spaces)
          )?;
          writeln!(
            f,
            "{}-locals attached to end {:?}",
            " ".repeat(spaces),
            attached,
          )?;
          writeln!(f, "{}Splits:", " ".repeat(spaces))?;
          for tree in splits.iter() {
            print_loop(f, tree, spaces + indent_size)?;
            writeln!(f)?;
          }
          writeln!(f)?;

          writeln!(f, "{}Joins:", " ".repeat(spaces))?;
          for tree in joins.iter() {
            print_loop(f, tree, spaces + indent_size)?;
            writeln!(f)?;
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
  /// Expand the path through the segment to a full set of [`Location`]s.
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

  pub(crate) fn paths_along_segment(
    &self,
    graph: &CleanedBody,
  ) -> Vec<Vec<BasicBlock>> {
    graph.paths_from_to(self.from.block, self.to.block)
  }

  fn spanned_locations(&self, graph: &CleanedBody) -> HashSet<Location> {
    let block_paths = self.paths_along_segment(graph);
    let body = graph.body();
    block_paths
      .into_iter()
      .flat_map(|path| {
        self.squash_block_path(&body.basic_blocks, path.into_iter())
      })
      .collect::<HashSet<_>>()
  }

  pub fn into_diff<'tcx>(
    self,
    ctxt: &PermissionsCtxt<'_, 'tcx>,
  ) -> HashMap<Place<'tcx>, PermissionsDataDiff> {
    let p0 = ctxt.location_to_point(self.from);
    let p1 = ctxt.location_to_point(self.to);
    let before = &ctxt.permissions_domain_at_point(p0);
    let after = &ctxt.permissions_domain_at_point(p1);
    before.diff(after)
  }
}

#[allow(dead_code)]
impl SegmentTree {
  pub fn new(body: MirSegment, span: Span) -> Self {
    Self::Single {
      segment: body,
      span,
      attached: vec![],
    }
  }

  /// Find a [`SegmentTree::Single`] node which matches *exactly* the given segment.
  pub fn find_single(
    &mut self,
    segment: MirSegment,
  ) -> Option<&mut SegmentTree> {
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
  pub fn replace_single(
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

  pub(crate) fn subtree_contains(
    &self,
    location: Location,
    graph: &CleanedBody,
  ) -> bool {
    let segment = match self {
      SegmentTree::Split { reach, .. } => reach,
      SegmentTree::Single { segment, .. } => segment,
    };
    let locs = segment.spanned_locations(graph);
    locs.contains(&location)
  }

  /// Find the /leaf/ [`MirSegment`] and it's corresponding `Span` that enclose
  /// `location`. The `location` is expected to be used as the end of step.
  pub(crate) fn find_segment_for_end<'a>(
    &'a self,
    location: Location,
    graph: &CleanedBody,
  ) -> SegmentSearchResult<'a> {
    match self {
      SegmentTree::Single { segment, .. } if segment.to != location => {
        SegmentSearchResult::Enclosing(self)
      }

      SegmentTree::Single { segment, span, .. } => {
        SegmentSearchResult::StepExists(*segment, *span)
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
          .map_or(SegmentSearchResult::NotFound, |next| {
            next.find_segment_for_end(location, graph)
          })
      }
    }
  }
}
