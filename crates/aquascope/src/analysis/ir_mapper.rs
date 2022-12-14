//! Main data structure for mapping HIR to MIR and vice-versa.
use std::{cmp::Ordering, collections::hash_map::Entry, fmt};

use flowistry::mir::utils::{BodyExt, PlaceExt as FlowistryPlaceExt};
use itertools::Itertools;
use rustc_data_structures::{
  fx::{FxHashMap as HashMap, FxHashSet as HashSet},
  graph::{dominators::Dominators, vec_graph::VecGraph, *},
};
use rustc_hir::{
  self as hir,
  intravisit::{self, Visitor as HirVisitor},
  HirId,
};
use rustc_index::bit_set::{BitSet, HybridBitSet, SparseBitMatrix};
use rustc_middle::{
  hir::nested_filter,
  mir::{
    self, visit::Visitor as MirVisitor, BasicBlock, Body, Local, Location,
    Place,
  },
  ty::TyCtxt,
};
use rustc_mir_dataflow::{Analysis, JoinSemiLattice, ResultsVisitor};
use rustc_span::Span;

use crate::{mir::utils::PlaceExt as AquascopePlaceExt, Range};

pub struct IRMapper<'a, 'tcx> {
  // flowistry::Spanner
  tcx: TyCtxt<'tcx>,
  body: &'a Body<'tcx>,
  body_id: hir::BodyId,
  hir_to_mir: HashMap<HirId, HashSet<Location>>,
  gather_mode: GatherMode,
  dominators: Dominators<BasicBlock>,
  post_dominators: PostDominators,
}

// TODO: I want to decompose this into more specific regions.
// E.g. we really want to only ever group locations together
// which have a total order. Example, an IF expr whould have
// a pseudo location as such:
// ```
//            : { locations associated with setup }
// if <cnd> {
//   <if-body>   : {locations associated with then branch}
// } else {
//   <else-body>   : { locations associated with the else branch }
// }          : { locations associated with the join }
//
// ```
// Splitting up the children locations (<cnd>, <if-body>, <else-body>)
// isn't necessary, because they can all be grouped into a
// single "child set", but we would want to know that there
// are distinct locations for the if prelude and postlude.
//
// A decomposition like this would be more useful for
// advacned structures like loops and matches.
pub struct HirNodeLocations {
  pub outer: Vec<Location>,
}

struct NestedLocations<'tcx> {
  tcx: TyCtxt<'tcx>,
  target: HirId,
  locs: Vec<HirId>,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum GatherMode {
  IgnoreCleanup,
  All,
}

pub struct MirOrderedLocations {
  entry_block: BasicBlock,
  exit_block: BasicBlock,
  // associated indices must remain sorted
  locations: HashMap<BasicBlock, Vec<usize>>,
}

impl MirOrderedLocations {
  fn minimum(&self) -> Location {
    let block = self.entry_block;
    let statement_index = *self
      .locations
      .get(&block)
      .expect("Block with no associated locations")
      .first()
      .unwrap();
    Location {
      block,
      statement_index,
    }
  }

  fn maximum(&self) -> Location {
    let block = self.exit_block;
    let statement_index = *self
      .locations
      .get(&block)
      .expect("Block with no associated locations")
      .last()
      .unwrap();
    Location {
      block,
      statement_index,
    }
  }

  pub fn get_entry_exit_locations(&self) -> (Location, Location) {
    let m = self.minimum();
    let mx = self.maximum();
    (m, mx)
  }
}

impl<'a, 'tcx> IRMapper<'a, 'tcx>
where
  'tcx: 'a,
{
  pub fn new(
    tcx: TyCtxt<'tcx>,
    body: &'a Body<'tcx>,
    body_id: hir::BodyId,
    gather_mode: GatherMode,
  ) -> Self {
    let dominators = body.basic_blocks.dominators();
    let control_dependencies = PostDominators::build(body);
    let mut ir_map = IRMapper {
      tcx,
      body,
      body_id,
      dominators,
      post_dominators: control_dependencies,
      hir_to_mir: HashMap::default(),
      gather_mode,
    };

    ir_map.visit_body(body);

    let hir = tcx.hir();
    for (id, locs) in ir_map.hir_to_mir.iter() {
      let hirs = hir.node_to_string(*id);
      log::debug!("Associated: {hirs} {locs:#?}");
    }

    #[cfg(debug_assertions)]
    ir_map.check_invariants();

    ir_map
  }

  /// Produces a MirOrderedLocations which is defined as follows.
  /// The `entry_block` represents the `BasicBlock` which post-dominates all
  /// blocks in the given set of locations and conversely the `exit_block`
  /// dominates all blocks in the set.
  ///
  /// This works under the assumption that there exists a global
  /// maximum in the (post-)dominator lattice.
  ///
  /// See: https://en.wikipedia.org/wiki/Dominator_(graph_theory)
  pub fn get_mir_locations(
    &self,
    hir_id: HirId,
  ) -> Option<MirOrderedLocations> {
    let empty_set = &HashSet::default();
    let outer = self.hir_to_mir.get(&hir_id).unwrap_or(empty_set);

    if outer.is_empty() {
      return None;
    }

    log::debug!("Gathering outer nodes {:#?}", outer);

    let mut total_location_map: HashMap<BasicBlock, Vec<usize>> =
      outer.into_iter().fold(HashMap::default(), |mut acc, loc| {
        let bb = loc.block;
        let idx = loc.statement_index;
        acc.entry(bb).or_default().push(idx);
        acc
      });

    for idxs in total_location_map.values_mut() {
      idxs.sort_unstable();
    }

    let basic_blocks = total_location_map.keys().collect::<Vec<_>>();

    let entry_block = basic_blocks
      .iter()
      .find(|&&&b1| {
        basic_blocks
          .iter()
          .all(|&&b2| b1 == b2 || self.dominators.is_dominated_by(b2, b1))
      })
      .unwrap();

    let exit_block = basic_blocks
      .iter()
      .find(|&&&b1| {
        basic_blocks.iter().all(|&&b2| {
          b1 == b2 || self.post_dominators.is_postdominated_by(b1, b2)
        })
      })
      .unwrap();

    Some(MirOrderedLocations {
      entry_block: **entry_block,
      exit_block: **exit_block,
      locations: total_location_map,
    })
  }

  // Check the given invariants that I am assuming hold about this data structure.
  // This method should be extremely slow, inefficient, exhaustive, and only
  // be run when debugging :)
  fn check_invariants(&self) {
    log::warn!("Running invariant checks on the IRMapper");

    // 1. No Location is associated with multiple HIR nodes.
    let disjoint_locations = self.hir_to_mir.iter().all(|(id1, locs1)| {
            !self.hir_to_mir.iter().any(|(id2, locs2)| {
                let intersection = locs1.intersection(locs2).collect::<HashSet<_>>();
                let cnd = id1 != id2 && !intersection.is_empty();
                if cnd {
                    log::debug!("HirId {id1:?} and {id2:?} contain the following overlapping locations {intersection:#?}");
                }
                cnd
            })
        });

    assert!(disjoint_locations);
  }
}

// -------------------------------------------
// Graph for post-dominator set.
// HACK: copied from flowistry, see if you can
// reuse bits.

struct BodyReversed<'a, 'tcx> {
  body: &'a Body<'tcx>,
  ret: BasicBlock,
  unreachable: BitSet<BasicBlock>,
}

impl DirectedGraph for BodyReversed<'_, '_> {
  type Node = BasicBlock;
}

impl WithStartNode for BodyReversed<'_, '_> {
  fn start_node(&self) -> Self::Node {
    self.ret
  }
}

impl WithNumNodes for BodyReversed<'_, '_> {
  fn num_nodes(&self) -> usize {
    self.body.basic_blocks.len()
  }
}

impl<'graph> GraphSuccessors<'graph> for BodyReversed<'_, '_> {
  type Item = BasicBlock;
  type Iter = Box<dyn Iterator<Item = BasicBlock> + 'graph>;
}

impl WithSuccessors for BodyReversed<'_, '_> {
  fn successors(
    &self,
    node: Self::Node,
  ) -> <Self as GraphSuccessors<'_>>::Iter {
    Box::new(
      self.body.basic_blocks.predecessors()[node]
        .iter()
        .filter(|bb| !self.unreachable.contains(**bb))
        .copied(),
    )
  }
}

impl<'graph> GraphPredecessors<'graph> for BodyReversed<'_, '_> {
  type Item = BasicBlock;
  type Iter = Box<dyn Iterator<Item = BasicBlock> + 'graph>;
}

impl WithPredecessors for BodyReversed<'_, '_> {
  fn predecessors(
    &self,
    node: Self::Node,
  ) -> <Self as GraphPredecessors<'_>>::Iter {
    Box::new(
      self.body.basic_blocks[node]
        .terminator()
        .successors()
        .filter(|bb| !self.unreachable.contains(*bb)),
    )
  }
}

fn compute_post_dominators(
  body: &Body,
  ret: BasicBlock,
) -> HashMap<BasicBlock, HashSet<BasicBlock>> {
  let nblocks = body.basic_blocks.len();
  let mut graph = BodyReversed {
    body,
    ret,
    unreachable: BitSet::new_empty(nblocks),
  };

  let reachable = iterate::post_order_from(&graph, ret);
  graph.unreachable.insert_all();
  for n in &reachable {
    graph.unreachable.remove(*n);
  }

  let dominators = dominators::dominators(graph);
  reachable
    .into_iter()
    .map(|n| (n, dominators.dominators(n).collect()))
    .collect()
}

pub struct PostDominators(SparseBitMatrix<BasicBlock, BasicBlock>);

impl fmt::Debug for PostDominators {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for (i, (bb, bbs)) in self
      .0
      .rows()
      .enumerate()
      .filter_map(|(i, bb)| self.0.row(bb).map(move |bbs| (i, (bb, bbs))))
    {
      if i > 0 {
        write!(f, ", ")?;
      }
      write!(f, "{bb:?}: {{")?;
      for (j, bb2) in bbs.iter().enumerate() {
        if j > 0 {
          write!(f, ", ")?;
        }
        write!(f, "{bb2:?}")?;
      }
      write!(f, "}}")?;
    }
    Ok(())
  }
}

impl PostDominators {
  pub fn build(body: &Body) -> Self {
    PostDominators(
      body
        .all_returns()
        .map(|loc| {
          let block = loc.block;
          assert!(!body.basic_blocks[block].is_cleanup);
          PostDominators::build_for_return(body, loc.block)
        })
        .fold(
          SparseBitMatrix::new(body.basic_blocks.len()),
          |mut deps1, deps2| {
            for block in deps2.rows() {
              if let Some(set) = deps2.row(block) {
                deps1.union_row(block, set);
              }
            }
            deps1
          },
        ),
    )
  }

  fn build_for_return(
    body: &Body,
    ret: BasicBlock,
  ) -> SparseBitMatrix<BasicBlock, BasicBlock> {
    let doms = compute_post_dominators(body, ret);
    log::debug!("doms={doms:?}");

    let n = body.basic_blocks.len();
    let mut domsp = SparseBitMatrix::<BasicBlock, BasicBlock>::new(n);

    for (b1, doms) in doms.iter() {
      for b2 in doms.iter() {
        domsp.insert(*b1, *b2);
      }
    }

    domsp
  }

  pub fn is_postdominated_by(&self, b1: BasicBlock, pdom: BasicBlock) -> bool {
    self.0.row(pdom).map_or(false, |row| row.contains(b1))
  }
}

// ---------------------------------------------------------
// Gather all nested HirIds under the NestedLocations.target

macro_rules! remember_nested {
  ($me:ident, $with_id:expr) => {
    if $me
      .tcx
      .hir()
      .parent_id_iter($me.target)
      .any(|e| e == $me.target)
    {
      $me.locs.push($with_id.hir_id);
    }
  };
}

impl<'tcx> HirVisitor<'tcx> for NestedLocations<'tcx> {
  type NestedFilter = nested_filter::All;

  fn nested_visit_map(&mut self) -> Self::Map {
    self.tcx.hir()
  }

  fn visit_block(&mut self, v: &'tcx hir::Block<'tcx>) {
    remember_nested!(self, v);
    intravisit::walk_block(self, v);
  }

  fn visit_local(&mut self, v: &'tcx hir::Local<'tcx>) {
    remember_nested!(self, v);
    intravisit::walk_local(self, v);
  }

  fn visit_stmt(&mut self, v: &'tcx hir::Stmt<'tcx>) {
    remember_nested!(self, v);
    intravisit::walk_stmt(self, v);
  }

  fn visit_arm(&mut self, v: &'tcx hir::Arm<'tcx>) {
    remember_nested!(self, v);
    intravisit::walk_arm(self, v);
  }

  fn visit_expr(&mut self, v: &'tcx hir::Expr<'tcx>) {
    remember_nested!(self, v);
    intravisit::walk_expr(self, v);
  }

  fn visit_let_expr(&mut self, v: &'tcx hir::Let<'tcx>) {
    remember_nested!(self, v);
    intravisit::walk_let_expr(self, v);
  }
}

// -------------------------------------------------------------------
// Gather the HIR -> MIR relationships for statements and terminators.

impl<'tcx> MirVisitor<'tcx> for IRMapper<'_, 'tcx> {
  fn visit_basic_block_data(
    &mut self,
    block: mir::BasicBlock,
    data: &mir::BasicBlockData<'tcx>,
  ) {
    match self.gather_mode {
      GatherMode::All => self.super_basic_block_data(block, data),
      GatherMode::IgnoreCleanup if !data.is_cleanup => {
        self.super_basic_block_data(block, data)
      }
      GatherMode::IgnoreCleanup => {
        log::debug!("Ignoring cleanup block {block:?}");
      }
    }
  }

  fn visit_statement(
    &mut self,
    terminator: &mir::Statement<'tcx>,
    location: Location,
  ) {
    let hir_id = self.body.location_to_hir_id(location);
    self.hir_to_mir.entry(hir_id).or_default().insert(location);
  }

  fn visit_terminator(
    &mut self,
    terminator: &mir::Terminator<'tcx>,
    location: Location,
  ) {
    let hir_id = self.body.location_to_hir_id(location);
    self.hir_to_mir.entry(hir_id).or_default().insert(location);
  }
}
