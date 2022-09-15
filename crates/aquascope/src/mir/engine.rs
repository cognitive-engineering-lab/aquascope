//! This module re-implements [`rustc_mir_dataflow::Engine`](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_mir_dataflow/struct.Engine.html) for performance reasons.
//!
//! The Engine implementation in rustc optimizes for minimizing memory usage
//! by only materializing results at the start of basic blocks, and recomputing
//! the analysis when visiting results. However, this strategy involves a lot of
//! creation and deletion of the [analysis domain](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_mir_dataflow/trait.AnalysisDomain.html).
//!
//! The information flow analysis has a large domain of size `O(|places| * |locations|)`.
//! Profiling showed that a significant portion of analysis time was just the engine
//! allocating / cloning / dropping the domain, not doing computation. Therefore this
//! engine improves performance but increases memory usage by up-front materializing
//! the domain at every [`Location`].

use std::rc::Rc;

use either::Either;
use rustc_data_structures::{graph::WithSuccessors, work_queue::WorkQueue};
use rustc_index::vec::IndexVec;
use rustc_middle::{
  mir::{traversal, Body, Location},
  ty::TyCtxt,
};
use rustc_mir_dataflow::{
  Analysis, Direction, JoinSemiLattice, ResultsVisitor,
};

use crate::indexed::{
  impls::{LocationDomain, LocationIndex},
  IndexedDomain,
};

/// An alternative implementation of
/// [`rustc_mir_dataflow::Results`](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_mir_dataflow/struct.Results.html).
pub struct AnalysisResults<'tcx, A: Analysis<'tcx>> {
  pub analysis: A,
  location_domain: Rc<LocationDomain>,
  state: IndexVec<LocationIndex, A::Domain>,
}

impl<'tcx, A: Analysis<'tcx>> AnalysisResults<'tcx, A> {
  pub fn visit_reachable_with<'mir, V>(
    &self,
    body: &'mir Body<'tcx>,
    visitor: &mut V,
  ) where
    V: ResultsVisitor<'mir, 'tcx, FlowState = A::Domain>,
  {
    for (block, data) in traversal::reachable(body) {
      for statement_index in 0 ..= data.statements.len() {
        let location = Location {
          block,
          statement_index,
        };
        let loc_index = self.location_domain.index(&location);
        let state = &self.state[loc_index];

        if statement_index == 0 {
          visitor.visit_block_start(state, data, block);
        }

        if statement_index == data.statements.len() {
          visitor.visit_terminator_after_primary_effect(
            state,
            data.terminator(),
            location,
          )
        } else {
          visitor.visit_statement_after_primary_effect(
            state,
            &data.statements[statement_index],
            location,
          )
        }
      }
    }
  }

  /// Gets the computed [`AnalysisDomain`](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_mir_dataflow/trait.AnalysisDomain.html)
  /// at a given [`Location`].
  pub fn state_at(&self, location: Location) -> &A::Domain {
    &self.state[self.location_domain.index(&location)]
  }
}

/// Runs a given [`Analysis`](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_mir_dataflow/trait.Analysis.html) to a fixpoint over the given [`Body`].
///
/// A reimplementation of [`rustc_mir_dataflow::framework::engine::iterate_to_fixpoint`](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_mir_dataflow/framework/engine/struct.Engine.html#method.iterate_to_fixpoint).
pub fn iterate_to_fixpoint<'tcx, A: Analysis<'tcx>>(
  _tcx: TyCtxt<'tcx>,
  body: &Body<'tcx>,
  location_domain: Rc<LocationDomain>,
  analysis: A,
) -> AnalysisResults<'tcx, A> {
  let bottom_value = analysis.bottom_value(body);

  // `state` materializes the analysis domain for *every* location, which is the crux
  // of this implementation strategy.
  let num_locs = location_domain.num_real_locations();
  let mut state = IndexVec::from_elem_n(bottom_value, num_locs);

  analysis.initialize_start_block(
    body,
    &mut state[location_domain.index(&Location::START)],
  );

  let mut dirty_queue: WorkQueue<LocationIndex> =
    WorkQueue::with_none(num_locs);
  if A::Direction::is_forward() {
    for (block, data) in traversal::reverse_postorder(body) {
      for statement_index in 0 ..= data.statements.len() {
        dirty_queue.insert(location_domain.index(&Location {
          block,
          statement_index,
        }));
      }
    }
  }

  while let Some(loc_index) = dirty_queue.pop() {
    let location = *location_domain.value(loc_index);
    let next_locs = match body.stmt_at(location) {
      Either::Left(statement) => {
        analysis.apply_statement_effect(
          &mut state[loc_index],
          statement,
          location,
        );
        vec![location.successor_within_block()]
      }
      Either::Right(terminator) => {
        analysis.apply_terminator_effect(
          &mut state[loc_index],
          terminator,
          location,
        );
        body
          .successors(location.block)
          .map(|block| Location {
            block,
            statement_index: 0,
          })
          .collect::<Vec<_>>()
      }
    };

    for next_loc in next_locs {
      let next_loc_index = location_domain.index(&next_loc);
      let (cur_state, next_state) = state.pick2_mut(loc_index, next_loc_index);
      let changed = next_state.join(cur_state);
      if changed {
        dirty_queue.insert(next_loc_index);
      }
    }
  }

  AnalysisResults {
    analysis,
    location_domain,
    state,
  }
}
