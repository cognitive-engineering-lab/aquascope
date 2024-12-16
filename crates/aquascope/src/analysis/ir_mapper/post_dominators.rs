use rustc_data_structures::graph::*;
use rustc_index::{
  bit_set::{ChunkedBitSet, SparseBitMatrix},
  Idx,
};
use rustc_utils::mir::control_dependencies::PostDominators;

/// Computes the intersection of the post-dominators across all exits
/// to a graph.
pub(crate) struct AllPostDominators<Node: Idx>(SparseBitMatrix<Node, Node>);
impl<Node: Idx> AllPostDominators<Node> {
  pub(crate) fn build<G: ControlFlowGraph<Node = Node>>(
    graph: &G,
    exits: impl IntoIterator<Item = Node>,
  ) -> Self {
    let mut pdom = SparseBitMatrix::new(graph.num_nodes());
    let all_nodes = (0 .. graph.num_nodes()).map(|i| Node::new(i));
    for node in all_nodes.clone() {
      pdom.insert_all_into_row(node)
    }
    for exit in exits {
      let exit_pdom = PostDominators::build(graph, exit);
      for node in all_nodes.clone() {
        let mut is_pdom = ChunkedBitSet::new_empty(graph.num_nodes());
        if let Some(iter) = exit_pdom.post_dominators(node) {
          for other in iter {
            is_pdom.insert(other);
          }
        }
        pdom.intersect_row(node, &is_pdom);
      }
    }
    AllPostDominators(pdom)
  }

  pub(crate) fn is_postdominated_by(&self, node: Node, dom: Node) -> bool {
    match self.0.row(node) {
      Some(set) => set.contains(dom),
      None => false,
    }
  }
}

#[cfg(test)]
mod tests {
  use rustc_data_structures::graph::{vec_graph::VecGraph, *};

  use super::*;

  struct VG<N: Idx> {
    source: N,
    forward: VecGraph<N>,
    backward: VecGraph<N>,
  }

  impl<N: Idx + Ord> VG<N> {
    fn make(size: usize, source: N, edges: Vec<(N, N)>) -> Self {
      let rev = edges.iter().map(|&(f, s)| (s, f)).collect::<Vec<_>>();
      VG {
        source,
        forward: VecGraph::new(size, edges),
        backward: VecGraph::new(size, rev),
      }
    }
  }

  impl<N: Idx> DirectedGraph for VG<N> {
    type Node = N;

    fn num_nodes(&self) -> usize {
      self.forward.num_nodes()
    }
  }

  impl<N: Idx> StartNode for VG<N> {
    fn start_node(&self) -> Self::Node {
      self.source
    }
  }

  impl<N: Idx + Ord> Successors for VG<N> {
    fn successors(
      &self,
      node: Self::Node,
    ) -> impl Iterator<Item = Self::Node> {
      self
        .forward
        .successors(node)
        .iter()
        .copied()
        .collect::<smallvec::SmallVec<[N; 10]>>()
        .into_iter()
    }
  }

  impl<N: Idx + Ord> Predecessors for VG<N> {
    fn predecessors(
      &self,
      node: Self::Node,
    ) -> impl Iterator<Item = Self::Node> {
      self
        .backward
        .successors(node)
        .iter()
        .copied()
        .collect::<smallvec::SmallVec<[N; 10]>>()
        .into_iter()
    }
  }

  #[test]
  fn pdom_diamond() {
    let diamond = VG::<usize>::make(4, 0, vec![(0, 1), (0, 2), (1, 3), (2, 3)]);
    let post_doms = AllPostDominators::build(&diamond, std::iter::once(3));
    for b in 0 ..= 2 {
      assert!(post_doms.is_postdominated_by(b, 3));
    }
  }

  #[test]
  fn pdom_linear() {
    let nodes = 100;
    let edges = (0 .. nodes).zip(1 ..).collect::<Vec<_>>();
    let line = VG::<usize>::make(nodes, 0, edges);
    let post_doms = AllPostDominators::build(&line, std::iter::once(nodes - 1));
    for i in 0 .. nodes {
      for j in i + 1 .. nodes {
        assert!(
          post_doms.is_postdominated_by(i, j),
          "{j} should post-dominate {i}"
        );
      }
    }
  }

  #[test]
  fn pdom_double_diamond() {
    //         2     5
    // 0 -> 1     4     7 ->
    //         3     6
    let dd = VG::<usize>::make(8, 0, vec![
      (0, 1),
      (1, 2),
      (1, 3),
      (2, 4),
      (3, 4),
      (4, 5),
      (4, 6),
      (5, 7),
      (6, 7),
    ]);
    let post_doms = AllPostDominators::build(&dd, std::iter::once(7));

    assert!(post_doms.is_postdominated_by(0, 1));
    assert!(post_doms.is_postdominated_by(0, 4));
    assert!(post_doms.is_postdominated_by(0, 7));
    assert!(post_doms.is_postdominated_by(1, 4));
    assert!(post_doms.is_postdominated_by(1, 7));
    assert!(post_doms.is_postdominated_by(4, 7));

    for i in 0 .. 8 {
      for &bad in &[2, 3, 5, 6] {
        if i != bad {
          assert!(
            !post_doms.is_postdominated_by(i, bad),
            "{bad} should NOT post-dominate {i}"
          );
        }
      }
    }
  }
}
