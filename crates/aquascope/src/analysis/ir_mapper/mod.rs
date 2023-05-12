//! Main data structure for mapping HIR to MIR and vice-versa.
pub(crate) mod body_graph;
#[allow(dead_code)]
pub(crate) mod mir_locations;
pub(crate) mod post_dominators;
// pub(crate) mod region_name;

pub(crate) use body_graph::CleanedBody;
pub(crate) use mir_locations::MirOrderedLocations;
use post_dominators::AllPostDominators;
use rustc_data_structures::{
  fx::{FxHashMap as HashMap, FxHashSet as HashSet},
  graph::{dominators::Dominators, *},
};
use rustc_hir::{self as hir, HirId};
use rustc_middle::{
  mir::{
    self, visit::Visitor as MirVisitor, BasicBlock, Body, Location, Place,
  },
  ty::TyCtxt,
};
use rustc_utils::BodyExt;

pub struct IRMapper<'a, 'tcx> {
  pub(crate) cleaned_graph: CleanedBody<'a, 'tcx>,
  tcx: TyCtxt<'tcx>,
  body: &'a Body<'tcx>,
  hir_to_mir: HashMap<HirId, HashSet<Location>>,
  gather_mode: GatherMode,
  dominators: Dominators<BasicBlock>,
  post_dominators: AllPostDominators<BasicBlock>,
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

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum GatherMode {
  IgnoreCleanup,
  All,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum GatherDepth {
  Outer,
  Nested,
}

// FIXME rever these changes, they shouldn't be needed.
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum ExitJoinSearch {
  None,
  Successor,
}

impl<'a, 'tcx> IRMapper<'a, 'tcx>
where
  'tcx: 'a,
{
  pub fn new(
    tcx: TyCtxt<'tcx>,
    body: &'a Body<'tcx>,
    gather_mode: GatherMode,
  ) -> Self {
    let cleaned_graph = CleanedBody(body);
    let dominators = dominators::dominators(&cleaned_graph);
    let post_dominators = AllPostDominators::build(
      &cleaned_graph,
      body.all_returns().map(|loc| loc.block),
    );

    let mut ir_map = IRMapper {
      tcx,
      body,
      dominators,
      post_dominators,
      hir_to_mir: HashMap::default(),
      gather_mode,
      cleaned_graph,
    };

    ir_map.visit_body(body);

    let hir = tcx.hir();
    for (id, _locs) in ir_map.hir_to_mir.iter() {
      let _hirs = hir.node_to_string(*id);
    }

    if cfg!(debug_assertions) {
      ir_map.check_invariants();
    }

    ir_map
  }

  // --------------------------------------------------------
  // TODO: these utilities probably belong in the CleanedBody

  pub fn dominates(&self, dom: Location, node: Location) -> bool {
    if dom.block == node.block {
      return dom.statement_index <= node.statement_index;
    }

    self.dominators.is_reachable(node.block)
      && self.dominators.dominates(dom.block, node.block)
  }

  pub fn post_dominates(&self, dom: Location, node: Location) -> bool {
    if dom.block == node.block {
      return dom.statement_index >= node.statement_index;
    }

    self
      .post_dominators
      .is_postdominated_by(node.block, dom.block)
  }

  /// Returns true if the terminator in the location's block is a `switchInt`.
  pub fn is_terminator_switchint(&self, location: Location) -> bool {
    matches!(
      self.cleaned_graph.terminator_in_block(location.block).kind,
      mir::TerminatorKind::SwitchInt { .. }
    )
  }

  pub fn local_assigned_place(&self, local: &hir::Local) -> Vec<Place<'tcx>> {
    use either::Either;
    use mir::{FakeReadCause as FRC, StatementKind as SK};
    let id = local.hir_id;
    self.get_mir_locations(id, GatherDepth::Outer).map_or_else(
      Vec::default,
      |mol| {
        mol
          .values()
          .filter_map(|loc| match self.body.stmt_at(loc) {
            Either::Left(mir::Statement {
              kind: SK::FakeRead(box (FRC::ForLet(_), place)),
              ..
            }) => Some(*place),
            _ => None,
          })
          .collect::<Vec<_>>()
      },
    )
  }

  // Determines whether or not a block was inserted solely as a
  // `FalseEdge` or `FalseUnwind`. These were making the post-dominator
  // analysis fail for conditional terminators.
  fn is_false_location(&self, loc: Location) -> bool {
    use mir::TerminatorKind as TK;
    let bb = loc.block;
    let data = &self.body.basic_blocks[bb];
    let term = data.terminator();
    data.statements.is_empty()
      && matches!(term.kind, TK::FalseUnwind { .. } | TK::FalseEdge { .. })
  }

  /// Search for entry/exit locations allowing for a tolerance on the exit block.
  ///
  /// See [`get_mir_locations`] for a description of the non-tolerant version,
  /// here, we give the option of relaxing the initial set of basic blocks
  /// to search from.
  /// When branching control-flow has a return in one of the branches, it's
  /// possible that the entire HIR node doesn't include the block which joins
  /// these branches.
  pub(crate) fn get_mir_locations_fuzzy(
    &self,
    hir_id: HirId,
    depth: GatherDepth,
    exit_search: ExitJoinSearch,
  ) -> Option<MirOrderedLocations> {
    let empty_set = &HashSet::default();
    let outer = self.hir_to_mir.get(&hir_id).unwrap_or(empty_set);
    let mut locations = outer.clone();
    match depth {
      GatherDepth::Outer => (),
      // Gather all the mir locations for every HirId nested under this one.
      GatherDepth::Nested => {
        let hir = self.tcx.hir();
        self.hir_to_mir.iter().for_each(|(child_id, locs)| {
          if hir.parent_id_iter(*child_id).any(|id| id == hir_id) {
            for l in locs.iter() {
              locations.insert(*l);
            }
          }
        });
      }
    };

    if locations.is_empty() {
      return None;
    }

    let mut total_location_map: HashMap<BasicBlock, Vec<usize>> = locations
      .into_iter()
      .filter(|loc| !self.is_false_location(*loc))
      .fold(HashMap::default(), |mut acc, loc| {
        let bb = loc.block;
        let idx = loc.statement_index;
        if !self.is_block_unreachable(bb) {
          acc.entry(bb).or_default().push(idx);
        }
        acc
      });

    for idxs in total_location_map.values_mut() {
      idxs.sort_unstable();
    }

    let basic_blocks = total_location_map.keys().copied().collect::<Vec<_>>();

    let entry_block = basic_blocks
      .iter()
      .find(|&&candidate_dom| {
        basic_blocks.iter().all(|&block| {
          self.dominators.is_reachable(block)
            && (block == candidate_dom
              || self.dominators.dominates(candidate_dom, block))
        })
      })
      .copied();

    let find_exit_from = |basic_blocks: &[BasicBlock]| -> Option<BasicBlock> {
      basic_blocks
        .iter()
        .find(|&&candidate_postdom| {
          basic_blocks.iter().all(|&block| {
            block == candidate_postdom
              || self
                .post_dominators
                .is_postdominated_by(block, candidate_postdom)
          })
        })
        .copied()
    };

    let exit_block = find_exit_from(&basic_blocks);

    let exit_block = match (exit_block, exit_search) {
      (None, ExitJoinSearch::Successor) => {
        let successor_blocks = basic_blocks
          .iter()
          .flat_map(|&bb| self.cleaned_graph.successors(bb));
        let basic_blocks = successor_blocks
          .chain(total_location_map.keys().copied())
          .collect::<Vec<_>>();
        find_exit_from(&basic_blocks)
      }
      (eb, _) => eb,
    };

    log::debug!("Gathering MIR location entry / exit blocks: {entry_block:?}{exit_block:?}");

    if exit_block.is_none() {
      log::debug!("Found locations: {total_location_map:#?}");
      log::warn!(
        "No post-dominator: Entry: {entry_block:?} Exit {exit_block:?}"
      );
    }

    Some(MirOrderedLocations {
      entry_block,
      exit_block,
      locations: total_location_map,
    })
  }

  /// Produces a MirOrderedLocations which is defined as follows.
  /// The `entry_block` represents the `BasicBlock` which post-dominates all
  /// blocks in the given set of locations and conversely the `exit_block`
  /// dominates all blocks in the set.
  ///
  /// This works under the assumption that there exists a global
  /// maximum in the (post-)dominator lattice.
  ///
  /// See: <https://en.wikipedia.org/wiki/Dominator_(graph_theory)>
  pub fn get_mir_locations(
    &self,
    hir_id: HirId,
    depth: GatherDepth,
  ) -> Option<MirOrderedLocations> {
    self.get_mir_locations_fuzzy(hir_id, depth, ExitJoinSearch::None)
  }

  fn is_block_unreachable(&self, block: BasicBlock) -> bool {
    let block_data = &self.body.basic_blocks[block];
    let term = block_data.terminator();
    matches!(term.kind, mir::TerminatorKind::Unreachable)
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
    _terminator: &mir::Statement<'tcx>,
    location: Location,
  ) {
    let hir_id = self.body.location_to_hir_id(location);
    self.hir_to_mir.entry(hir_id).or_default().insert(location);
  }

  fn visit_terminator(
    &mut self,
    _terminator: &mir::Terminator<'tcx>,
    location: Location,
  ) {
    let hir_id = self.body.location_to_hir_id(location);
    self.hir_to_mir.entry(hir_id).or_default().insert(location);
  }
}
