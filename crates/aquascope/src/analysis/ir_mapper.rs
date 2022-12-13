//! Main data structure for mapping HIR to MIR and vice-versa.
use std::{
  cmp::Ordering,
  collections::{hash_map::Entry, HashMap},
};

use flowistry::mir::utils::{BodyExt, PlaceExt as FlowistryPlaceExt};
use itertools::Itertools;
use rustc_data_structures::fx::FxHashSet as HashSet;
use rustc_hir::{
  self as hir,
  intravisit::{self, Visitor as HirVisitor},
  HirId,
};
use rustc_middle::{
  hir::nested_filter,
  mir::{self, visit::Visitor as MirVisitor, Body, Local, Location, Place},
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
  // pub nested: Vec<Location>,
  // pub node: hir::Node<'hir>,
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
    let mut ir_map = IRMapper {
      tcx,
      body,
      body_id,
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

  // Given a HIR node, get the corresponding nodes for:
  // - Itself, and it's nested children.
  // - QUESTION: in which case would the caller be interested in the children?
  //   it seems that this isn't information I want to actually give back here.
  pub fn get_mir_locations(&self, hir_id: HirId) -> Option<HirNodeLocations> {
    let empty_set = &HashSet::default();
    let outer = self.hir_to_mir.get(&hir_id).unwrap_or(empty_set);

    // let mut nested_ids = NestedLocations {
    //   tcx: self.tcx,
    //   target: hir_id,
    //   locs: Vec::default(),
    // };

    // nested_ids.visit_nested_body(self.body_id);

    // let unioned = nested_ids
    //   .locs
    //   .into_iter()
    //   .flat_map(|id| self.hir_to_mir.get(&id).unwrap_or(empty_set).iter())
    //   .copied()
    //   .collect::<HashSet<_>>();

    // let mut inner = unioned.difference(&outer).copied().collect::<Vec<_>>();

    // let node = self.tcx.hir().find(hir_id).unwrap();

    let mut outer = outer.into_iter().copied().collect::<Vec<_>>();

    outer.mir_order_sort(self.body);

    Some(HirNodeLocations {
      outer,
      // nested: inner,
      // node,
    })
  }

  // TODO: Also queries about the given node's span.

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

trait MirOrder {
  fn mir_order_sort(&mut self, body: &mir::Body);
}

impl MirOrder for Vec<Location> {
  // TODO: in general, it isn't always possible to just take
  // a vector of locations and put them in some sensible order.
  // This function should either:
  // - have some serious preconditions
  // - (more likely) not sort in place, but return several
  //   resulting vectors which do have a sensible order.
  //
  // XXX: This implementation was taken at face value from
  // the previous permission_steps impl.
  fn mir_order_sort(&mut self, body: &mir::Body) {
    let mut exists_cycle = false;

    self.sort_by(|&l1, &l2| {

        if l1.block == l2.block {
          return l1.statement_index.cmp(&l2.statement_index);
        }

        let l1_p = l1.is_predecessor_of(l2, body);
        let l2_p = l2.is_predecessor_of(l1, body);
        if l1_p && l2_p {
          log::error!("The MIR Locations for a HIR Stmt did not form a total order {l1:?} {l2:?}");
          exists_cycle = true;
          Ordering::Equal
        } else if l1_p {
          Ordering::Less
        } else if l2_p {
          Ordering::Greater
        } else {
          log::error!("The MIR Locations for a HIR Stmt are unrelated {l1:?} {l2:?}");
          exists_cycle = true;
          Ordering::Equal
        }
      });

    if exists_cycle {
      log::error!("These locations contains a cycle: {self:#?}");
    }
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
