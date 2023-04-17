use rustc_data_structures::graph::*;
use rustc_index::{
  bit_set::{HybridBitSet, SparseBitMatrix},
  vec::Idx,
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
        let mut is_pdom = HybridBitSet::new_empty(graph.num_nodes());
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
