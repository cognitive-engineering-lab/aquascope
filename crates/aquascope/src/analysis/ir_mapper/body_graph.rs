use itertools::Itertools;
use rustc_data_structures::{fx::FxHashMap as HashMap, graph::*};
use rustc_middle::mir::{
  BasicBlock, BasicBlockData, BasicBlocks, Body, Location,
};
use smallvec::SmallVec;

/// A /cleaned/ graph representation for working with the MIR.
///
/// A `CleanedBody` represents MIR locations that are reachable via
/// regular control-flow. This removes cleanup blocks or those which
/// fall in unwind paths. When mapping back to source-level constructs
/// this is almost certainly what you want to use.
pub(crate) struct CleanedBody<'a, 'tcx: 'a>(pub &'a Body<'tcx>);

#[allow(dead_code)]
impl<'a, 'tcx: 'a> CleanedBody<'a, 'tcx> {
  pub fn body(&self) -> &'a Body<'tcx> {
    self.0
  }

  // TODO: cache the results
  pub(crate) fn paths_from_to(
    &self,
    from: BasicBlock,
    to: BasicBlock,
  ) -> Vec<Vec<BasicBlock>> {
    DFSFinder::find_paths_from_to(self, from, to)
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
        None
      }
    }
  }

  fn keep_block(bb: &BasicBlockData) -> bool {
    !bb.is_cleanup && !bb.is_empty_unreachable()
  }
}

// -----------
// Graph impls

impl DirectedGraph for CleanedBody<'_, '_> {
  type Node = BasicBlock;
}

impl WithStartNode for CleanedBody<'_, '_> {
  fn start_node(&self) -> Self::Node {
    self.0.basic_blocks.start_node()
  }
}

impl<'tcx> WithNumNodes for CleanedBody<'_, 'tcx> {
  fn num_nodes(&self) -> usize {
    self.0.basic_blocks.len()
  }
}

impl<'tcx> GraphSuccessors<'_> for CleanedBody<'_, 'tcx> {
  type Item = BasicBlock;
  type Iter = smallvec::IntoIter<[BasicBlock; 4]>;
}

impl<'tcx> WithSuccessors for CleanedBody<'_, 'tcx> {
  fn successors(
    &self,
    node: Self::Node,
  ) -> <Self as GraphSuccessors<'_>>::Iter {
    <BasicBlocks as WithSuccessors>::successors(&self.0.basic_blocks, node)
      .filter(|bb| CleanedBody::keep_block(&self.0.basic_blocks[*bb]))
      .collect::<SmallVec<[BasicBlock; 4]>>()
      .into_iter()
  }
}

impl<'tcx> GraphPredecessors<'_> for CleanedBody<'_, 'tcx> {
  type Item = BasicBlock;
  type Iter = smallvec::IntoIter<[BasicBlock; 4]>;
}

impl<'tcx> WithPredecessors for CleanedBody<'_, 'tcx> {
  fn predecessors(
    &self,
    node: Self::Node,
  ) -> <Self as GraphSuccessors<'_>>::Iter {
    <BasicBlocks as WithPredecessors>::predecessors(&self.0.basic_blocks, node)
      .filter(|bb| CleanedBody::keep_block(&self.0.basic_blocks[*bb]))
      .collect::<SmallVec<[BasicBlock; 4]>>()
      .into_iter()
  }
}

/// Finds all paths between two nodes.
///
/// This DFS will find all unique paths between two nodes. This
/// includes allowing loops to be traversed (at most once).
/// This is quite a HACK to briefly satisfy the needs of the
/// [stepper](crate::analysis::stepper::compute_permission_steps).
struct DFSFinder<'graph, G>
where
  G: ?Sized + DirectedGraph + WithNumNodes + WithSuccessors,
{
  graph: &'graph G,
  paths: Vec<Vec<G::Node>>,
  stack: Vec<G::Node>,
  visited: HashMap<G::Node, u8>,
}

impl<'graph, G> DFSFinder<'graph, G>
where
  G: ?Sized + DirectedGraph + WithNumNodes + WithSuccessors,
{
  pub fn new(graph: &'graph G) -> Self {
    Self {
      graph,
      paths: vec![],
      stack: vec![],
      visited: HashMap::default(),
    }
  }

  pub fn find_paths_from_to(
    graph: &'graph G,
    from: G::Node,
    to: G::Node,
  ) -> Vec<Vec<G::Node>> {
    let mut dfs = Self::new(graph);
    dfs.search(from, to);
    dfs.paths.into_iter().unique().collect::<Vec<_>>()
  }

  fn insert(&mut self, n: G::Node) -> bool {
    let v = self.visited.entry(n).or_default();
    if *v >= 2 {
      return false;
    }
    *v += 1;
    true
  }

  fn remove(&mut self, n: G::Node) {
    let v = self.visited.entry(n).or_default();
    assert!(*v > 0);
    *v -= 1;
  }

  fn search(&mut self, from: G::Node, to: G::Node) {
    if !self.insert(from) {
      return;
    }

    self.stack.push(from);

    if from == to {
      self.paths.push(self.stack.clone());
      self.remove(to);
      self.stack.pop().unwrap();
      return;
    }

    for v in self.graph.successors(from) {
      self.search(v, to);
    }

    self.stack.pop().unwrap();
    self.remove(from);
  }
}

#[cfg(test)]
mod test {
  use rustc_data_structures::graph::vec_graph::VecGraph;

  use super::*;

  #[test]
  fn if_shape() {
    // Diamond shaped IF.
    let graph = VecGraph::new(6, vec![
      (0u32, 1u32),
      (1u32, 2u32),
      (1u32, 3u32),
      (2u32, 4u32),
      (3u32, 4u32),
      (4u32, 5u32),
    ]);

    let paths_0_5 = vec![vec![0, 1, 2, 4, 5], vec![0, 1, 3, 4, 5]];

    assert_eq!(DFSFinder::find_paths_from_to(&graph, 0, 5), paths_0_5);
  }

  #[test]
  fn while_loop_shape() {
    // While loop shape:
    // 0 -> 1 -> 2 -> 3 -> 5
    //           ^    |
    //           |    v
    //           |-- 4
    let graph = VecGraph::new(6, vec![
      (0u32, 1u32),
      (1u32, 2u32),
      (2u32, 3u32),
      (3u32, 5u32),
      (3u32, 4u32),
      (4u32, 2u32),
    ]);

    let paths_0_5 = vec![vec![0, 1, 2, 3, 5], vec![0, 1, 2, 3, 4, 2, 3, 5]];
    let mut paths = DFSFinder::find_paths_from_to(&graph, 0, 5);
    paths.sort_by_key(|l| l.len());

    assert_eq!(paths, paths_0_5);
  }
}
