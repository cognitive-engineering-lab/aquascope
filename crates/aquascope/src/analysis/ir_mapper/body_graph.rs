use rustc_data_structures::graph::*;
use rustc_middle::mir::{
  BasicBlock, BasicBlockData, BasicBlocks, Body, Location, Terminator,
  TerminatorKind,
};
use smallvec::SmallVec;

/// A /cleaned/ graph representation for working with the MIR.
///
/// A `CleanedBody` represents MIR locations that are reachable via
/// regular control-flow. This removes cleanup blocks or those which
/// fall in unwind paths. When mapping back to source-level constructs
/// this is almost certainly what you want to use.
pub(crate) struct CleanedBody<'tcx>(pub &'tcx Body<'tcx>);

#[allow(dead_code)]
impl<'tcx> CleanedBody<'tcx> {
  pub fn body(&self) -> &'tcx Body<'tcx> {
    self.0
  }

  /// Compute the locations successor.
  ///
  /// If the specified location lies in the middle of a `BasicBlock`,
  /// the successor location is simply the Location with the next statement index.
  /// When analyzing terminators, if the jump location is not clear, `None` will
  /// be returned. (NOTE: jump locations are computed based on the cleaned graph).
  pub(crate) fn location_successor(
    &self,
    location: Location,
  ) -> Option<Location> {
    let b = location.block;
    let si = location.statement_index;
    let bbd = &self.0.basic_blocks[b];

    if si < bbd.statements.len() {
      Some(location.successor_within_block())
    } else {
      // The location is a terminator. If there exists
      // only a singular block successor in the cleaned graph,
      // then we can take the first statement in the block,
      // otherwise we just give up.
      let nexts = self.successors(location.block).collect::<Vec<_>>();
      if nexts.len() == 1 {
        Some(Location {
          block: nexts[0],
          statement_index: 0,
        })
      } else {
        log::debug!("No Location (or too many) successor(s) found: {nexts:?}");
        None
      }
    }
  }

  pub fn terminator_in_block(&self, block: BasicBlock) -> &Terminator<'tcx> {
    self.body().basic_blocks[block].terminator()
  }

  pub fn blocks(&self) -> impl Iterator<Item = BasicBlock> + use<'tcx, '_> {
    self
      .0
      .basic_blocks
      .reverse_postorder()
      .iter()
      .rev()
      .filter(|bb| CleanedBody::keep_block(&self.0.basic_blocks[**bb]))
      .copied()
  }

  pub fn is_false_edge(&self, bb: BasicBlock) -> bool {
    matches!(
      self.0.basic_blocks[bb].terminator().kind,
      TerminatorKind::FalseEdge { .. }
    )
  }

  fn keep_block(bb: &BasicBlockData) -> bool {
    !bb.is_cleanup && !bb.is_unreachable()
  }

  fn is_imaginary_target(
    from_data: &BasicBlockData,
    target: BasicBlock,
  ) -> bool {
    let TerminatorKind::FalseEdge {
      imaginary_target, ..
    } = from_data.terminator().kind
    else {
      return false;
    };

    imaginary_target == target
  }
}

// -----------
// Graph impls

impl DirectedGraph for CleanedBody<'_> {
  type Node = BasicBlock;

  fn num_nodes(&self) -> usize {
    self.0.basic_blocks.len()
  }
}

impl StartNode for CleanedBody<'_> {
  fn start_node(&self) -> Self::Node {
    self.0.basic_blocks.start_node()
  }
}

impl Successors for CleanedBody<'_> {
  fn successors(&self, node: Self::Node) -> impl Iterator<Item = Self::Node> {
    <BasicBlocks as Successors>::successors(&self.0.basic_blocks, node)
      .filter(|bb| {
        let from_data = &self.0.basic_blocks[*bb];
        CleanedBody::keep_block(from_data)
          && !CleanedBody::is_imaginary_target(from_data, *bb)
      })
      .collect::<SmallVec<[BasicBlock; 4]>>()
      .into_iter()
  }
}

impl Predecessors for CleanedBody<'_> {
  fn predecessors(&self, node: Self::Node) -> impl Iterator<Item = Self::Node> {
    <BasicBlocks as Predecessors>::predecessors(&self.0.basic_blocks, node)
      .filter(|bb| CleanedBody::keep_block(&self.0.basic_blocks[*bb]))
      .collect::<SmallVec<[BasicBlock; 4]>>()
      .into_iter()
  }
}

trait BasicBlockDataExt {
  fn is_unreachable(&self) -> bool;
}

impl BasicBlockDataExt for BasicBlockData<'_> {
  fn is_unreachable(&self) -> bool {
    matches!(self.terminator().kind, TerminatorKind::Unreachable)
  }
}

#[cfg(test)]
mod test {
  use rustc_utils::BodyExt;

  use super::{super::AllPostDominators, *};
  use crate::test_utils as tu;

  // CleanedBody tests

  #[test]
  fn cleaned_body_simple_if() {
    // EXPECTED MIR:
    // -------------
    // bb0: {
    //     StorageLive(_2);
    //     _2 = const 0_i32;
    //     FakeRead(ForLet(None), _2);
    //     StorageLive(_3);
    //     StorageLive(_4);
    //     _4 = const true;
    //     switchInt(move _4) -> [0: bb3, otherwise: bb1];
    // }
    //
    // bb1: {
    //     _5 = CheckedAdd(_2, const 1_i32);
    //     assert(!move (_5.1: bool), <removed>) -> [success: bb2, unwind: bb5];
    // }
    //
    // bb2: {
    //     _2 = move (_5.0: i32);
    //     _3 = const ();
    //     goto -> bb4;
    // }
    //
    // bb3: {
    //     _3 = const ();
    //     goto -> bb4;
    // }
    //
    // bb4: {
    //     StorageDead(_4);
    //     StorageDead(_3);
    //     _0 = _2;
    //     StorageDead(_2);
    //     return;
    // }
    //
    // bb5 (cleanup): {
    //     resume;
    // }

    tu::compile_normal(
      r#"
fn foo() -> i32 {
  let mut v1 = 0;
  if true {
    v1 += 1;
  }
  return v1;
}
"#,
      |tcx| {
        tu::for_each_body(tcx, |_, wfacts| {
          let cleaned_graph = CleanedBody(&wfacts.body);

          let post_doms = AllPostDominators::<BasicBlock>::build(
            &cleaned_graph,
            wfacts.body.all_returns().map(|loc| loc.block),
          );

          let cleaned_blocks = cleaned_graph.blocks().collect::<Vec<_>>();

          let bb0 = BasicBlock::from_usize(0);
          let bb1 = BasicBlock::from_usize(1);
          let bb2 = BasicBlock::from_usize(2);
          let bb3 = BasicBlock::from_usize(3);
          let bb4 = BasicBlock::from_usize(4);
          let bb5 = BasicBlock::from_usize(5);

          assert!(cleaned_blocks.contains(&bb0));
          assert!(cleaned_blocks.contains(&bb1));
          assert!(cleaned_blocks.contains(&bb2));
          assert!(cleaned_blocks.contains(&bb3));
          assert!(cleaned_blocks.contains(&bb4));
          // Cleanup  blocks
          assert!(!cleaned_blocks.contains(&bb5));

          for &bb in vec![bb0, bb1, bb2, bb3, bb4].iter() {
            assert!(post_doms.is_postdominated_by(bb, bb4));
          }

          assert!(!post_doms.is_postdominated_by(bb0, bb2));
          assert!(!post_doms.is_postdominated_by(bb0, bb3));
          assert!(post_doms.is_postdominated_by(bb1, bb2));
          assert!(!post_doms.is_postdominated_by(bb1, bb3));
        })
      },
    );
  }
}
