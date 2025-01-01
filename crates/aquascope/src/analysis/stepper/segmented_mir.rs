//! Internal state for managing permissions steps.
//!
//! The `SegmentedMir` aids the stepper in making sure that
//! steps made are always _valid_. In this context a step is defined
//! as a `MirSegment`, a simple struct that contains a `from` and `to`
//! location defining the step. The finished segmented mir is valid if
//! it satisfies the following criteria:
//!
//! 1. All segments are valid (more on this later).
//! 2. Segments form a total cover of the body under analysis.
//! 3. No location is included in multiple steps (see exceptions to this below).
//!
//! Segment validity is the main crux of the above definition and this is
//! split into three separate definitions. There exist three different kinds
//! of segments (spiritually, they are the same in the code):
//!
//! - Linear segments: a segment representing a linear piece of control flow.
//!   A linear segment has a single point of entry and a single exit. Formally,
//!   this is defined as:
//!       Given a `MirSegment { from, to }`, it is linear iff:
//!       `from` dominates `to` and `to` post-dominates `from`
//!   These segments are what we ultimately want.
//!
//! - Split segments: a segment representing the start of conditional control-flow.
//!   These segments relax the definition of a linear segment, in that the `to`
//!   location *does not* post-dominate `from`. These segments are important when
//!   representing control-flow given by a `switchInt`. In brief, a `switchInt`
//!   will have multiple jump targets based on its argument, and each one of these
//!   targets will be made into a split segment, stepping `from` the `switchInt`
//!   and stepping `to` the target location.
//!
//! - Join segments: a segment representing the close of conditional control-flow.
//!   These segments are the opposite of split segments, and relax the definition
//!   of a linear segment by lifting the requirement that `from` dominates `to`. After
//!   control-flow has been split (by say, a `switchInt`) join segments represent the
//!   steps needed to unify the control-flow again.
//!
//! Unless specified, the word 'segment' or 'step' always refers to a linear segment.
//! Whenever the stepper says "insert a step ending at location L", this will _always_
//! result in a linear step as the other two variants need to be explicitly handled.
//!
//! To maintain validity we use a recursive tree that incrementally builds up sequences
//! of linear steps. The tree layout looks (roughly) as follows:
//!
//! ```text
//! type LinearSegment = MirSegment
//! type SplitSegment  = MirSegment
//! type JoinSegment   = MirSegment
//!
//! data ControlFlow = Linear LinearSegment
//!                  | Branch
//!                      { splits :: [SplitSegment]
//!                      , joins  :: [JoinSegment]
//!                      , nested :: Collection
//!                      }
//!
//! data Collection = [ControlFlow]
//! ```
//!
//! To build this tree we manage a set of `CollectionBuilder`s, these
//! store the last `Location` from a step, and only allow inserting a
//! linear step into a collection. The exact process won't be outlined here,
//! but the stepper will open a branch when it encounters an `if` or `match`,
//! this opening will then create a new builder for each branch target. Builders
//! are then destroyed when either (1) it has reached a stopping point as
//! previously specified by the stepper, or (2) the branch that spawned the builder
//! is being closed.
//!
//! There is a little more to the process than this, for example: making sure that
//! branches and segments are created within the natural structure of the MIR and only
//! inserting steps in previously "unstepped" areas. But for those really curious
//! feel free to start at the [`SegmentedMirBuilder::insert`] function and explore
//! from there.

use anyhow::{anyhow, bail, ensure, Result};
use rustc_data_structures::{
  frozen::Frozen,
  fx::{FxHashMap as HashMap, FxHashSet as HashSet},
  graph::*,
  transitive_relation::{TransitiveRelation, TransitiveRelationBuilder},
  unify::{InPlaceUnificationTable, UnifyKey},
};
use rustc_index::{Idx, IndexVec};
use rustc_middle::mir::{BasicBlock, Location};
use rustc_span::Span;

use super::MirSegment;
use crate::analysis::ir_mapper::IRMapper;

// --------------------------
// Decls sections

rustc_index::newtype_index! {
  pub(super) struct SegmentId {}
}

rustc_index::newtype_index! {
  pub(super) struct BranchId {}
}

rustc_index::newtype_index! {
  /// Collections are groups of segments thare nest.
  /// E.g., when a branch contains another branch.
  /// These are controlled internally.
  pub(super) struct CollectionId {}
}

rustc_index::newtype_index! {
  /// Scopes are controlled at the segment-level
  /// and controlled by the caller.
  pub(super) struct ScopeId {}
}

rustc_index::newtype_index! {
  pub(super) struct TableId {}
}

impl UnifyKey for TableId {
  type Value = ();

  fn index(&self) -> u32 {
    self.as_u32()
  }

  fn from_index(i: u32) -> Self {
    Self::from_u32(i)
  }

  fn tag() -> &'static str {
    "TableId"
  }
}

lazy_static::lazy_static! {
  static ref BASE_SCOPE: ScopeId = ScopeId::new(0);
}

#[derive(Copy, Clone, Debug)]
#[allow(dead_code)]
enum LengthKind {
  Bounded {
    /// Entry location for the collection, location
    /// must dominate all locations contained within the collection.
    root: Location,
    phi: Location,
  },
  Unbounded {
    /// Exit location (if it exists) where control flow must leave,
    /// if a phi exists then it must post-dominate all locations
    /// contained within the collection.
    root: Location,
  },
}

#[derive(Debug)]
pub(super) struct SegmentData {
  pub(super) segment: MirSegment,
  pub(super) span: Span,
  pub(super) scope: ScopeId,
}

#[derive(Debug)]
pub(super) struct BranchData {
  table_id: TableId,
  pub(super) reach: MirSegment,

  /// Split segments, `from` dominates `to` but `to` does not post-dominate `from`.
  pub(super) splits: Vec<SegmentId>,

  // NOTE: join segments aren't currently used for anything. Previously we
  //       had lots of complex logic dictating when the join steps should be
  //       included but through lots of testing it seemed that the visual results
  //       we wanted _never_ used the join steps. We still keep them around in
  //       case a counterexample to that is found, or until I(gavinleroy) can
  //       come up with a sufficient formal reason why we don't need them.
  //       See the documentation in `table_builder` for more details.
  /// Join segments, `to` post-dominates `from` but `from` does not post-dominate `to`.
  #[allow(dead_code)]
  pub(super) joins: Vec<SegmentId>,

  pub(super) nested: Vec<CollectionId>,
}

#[derive(Copy, Clone, Debug)]
pub(super) enum CFKind {
  Linear(SegmentId),
  Branch(BranchId),
}

#[derive(Debug)]
pub(super) struct Collection {
  pub(super) data: Vec<CFKind>,
  kind: LengthKind,
}

#[derive(Copy, Clone, Debug)]
struct CollectionBuilder {
  collection: CollectionId,
  current_location: Location,
}

#[derive(Copy, Clone, Debug)]
struct BuilderIdx(usize);

#[derive(Copy, Clone)]
enum FindResult {
  None,
  NonLinear(BranchId, Location),
  Linear(BuilderIdx),
}

#[derive(Debug, Default)]
struct OpenCollections(Vec<CollectionBuilder>);

type BranchSpannerMap<'a> =
  HashMap<BranchId, Box<dyn Fn(&mut Location) -> Span + 'a>>;

pub(super) struct SegmentedMirBuilder<'a, 'tcx: 'a> {
  mapper: &'a IRMapper<'tcx>,
  first_collection: CollectionId,
  root_mappings: BranchSpannerMap<'a>,
  collections: IndexVec<CollectionId, Collection>,
  branches: IndexVec<BranchId, BranchData>,
  segments: IndexVec<SegmentId, SegmentData>,
  processing: OpenCollections,
  branch_roots: InPlaceUnificationTable<TableId>,
  scope_graph: TransitiveRelationBuilder<ScopeId>,
  open_scopes: Vec<ScopeId>,
  next_scope: ScopeId,
}

pub(super) struct SegmentedMir {
  pub(super) first_collection: CollectionId,
  collections: Frozen<IndexVec<CollectionId, Collection>>,
  branches: Frozen<IndexVec<BranchId, BranchData>>,
  segments: Frozen<IndexVec<SegmentId, SegmentData>>,
  scopes: TransitiveRelation<ScopeId>,
}

// --------------------------
// Impl sections

impl BranchData {
  pub fn new(tid: TableId, root: Location, phi: Option<Location>) -> Self {
    let to = phi.unwrap_or(root);
    BranchData {
      table_id: tid,
      reach: MirSegment::new(root, to),
      splits: Vec::default(),
      joins: Vec::default(),
      nested: Vec::default(),
    }
  }
}

#[allow(dead_code)]
impl OpenCollections {
  pub fn push(&mut self, c: CollectionBuilder) {
    self.0.push(c)
  }

  pub fn iter(&self) -> impl Iterator<Item = &CollectionBuilder> + '_ {
    // Open collections behaves like a stack, so we need to reverse the iterator.
    self.0.iter().rev()
  }

  pub fn enumerate(
    &self,
  ) -> impl Iterator<Item = (BuilderIdx, &CollectionBuilder)> + '_ {
    // Open collections behaves like a stack, so we need to reverse the iterator.
    self
      .0
      .iter()
      .enumerate()
      .map(|(i, o)| (BuilderIdx(i), o))
      .rev()
  }

  pub fn iter_mut(
    &mut self,
  ) -> impl Iterator<Item = &mut CollectionBuilder> + '_ {
    // Open collections behaves like a stack, so we need to reverse the iterator.
    self.0.iter_mut().rev()
  }

  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  pub fn len(&self) -> usize {
    self.0.len()
  }

  pub fn drain_collections<'a, 'this: 'a>(
    &'this mut self,
    cids: &'a HashSet<CollectionId>,
  ) -> impl Iterator<Item = CollectionBuilder> + 'a {
    self.0.extract_if(|cb| cids.contains(&cb.collection))
  }

  pub fn get(&self, i: BuilderIdx) -> &CollectionBuilder {
    &self.0[i.0]
  }

  pub fn get_mut(&mut self, i: BuilderIdx) -> &mut CollectionBuilder {
    &mut self.0[i.0]
  }

  pub fn clear(&mut self) {
    self.0.clear()
  }
}

impl std::fmt::Debug for SegmentedMirBuilder<'_, '_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "#<SegmentedMir: TODO>")
  }
}

impl SegmentedMir {
  pub(super) fn segments(&self) -> impl Iterator<Item = MirSegment> + '_ {
    self.segments.iter().map(|sd| sd.segment)
  }

  pub fn get_branch_scope(&self, bid: BranchId) -> ScopeId {
    let branch = self.get_branch(bid);
    let sid = branch.splits[0];
    let segment = self.get_segment(sid);
    segment.scope
  }

  pub fn get_collection(&self, cid: CollectionId) -> &Collection {
    &self.collections[cid]
  }

  pub fn get_segment(&self, sid: SegmentId) -> &SegmentData {
    &self.segments[sid]
  }

  pub fn get_branch(&self, bid: BranchId) -> &BranchData {
    &self.branches[bid]
  }

  /// Returns all ancestor scopes excluding `scope`.
  pub fn parent_scopes(
    &self,
    scope: ScopeId,
  ) -> impl Iterator<Item = ScopeId> + '_ {
    self.scopes.reachable_from(scope).into_iter()
  }
}

enum GetSpanner<'a> {
  GetFrom(BranchId),
  InsertNew(Box<dyn Fn(&mut Location) -> Span + 'a>),
}

impl<'a, 'tcx: 'a> SegmentedMirBuilder<'a, 'tcx> {
  pub fn make(mapper: &'a IRMapper<'tcx>) -> Self {
    let from = mapper.cleaned_graph.start_node().start_location();

    let mut collections = IndexVec::new();

    // We start with an empty linear collection.
    // XXX: we could also try to find the exit location for the
    // entire body but having this information isn't useful
    // for the end of the body. Phi nodes are used to make
    // sure we don't accidentally jump past the end of a
    // join but with the return there isn't anything after.
    let first_collection = collections.push(Collection {
      data: Vec::default(),
      kind: LengthKind::Unbounded { root: from },
    });

    let mut this = Self {
      first_collection,
      mapper,
      root_mappings: HashMap::default(),
      collections,
      branches: IndexVec::default(),
      segments: IndexVec::default(),
      processing: OpenCollections::default(),
      branch_roots: InPlaceUnificationTable::default(),
      scope_graph: TransitiveRelationBuilder::default(),
      // NOTE: this maintains that there is always
      // an open scope that the visitor cannot close.
      open_scopes: vec![*BASE_SCOPE],
      next_scope: BASE_SCOPE.plus(1),
    };

    this.processing.push(CollectionBuilder {
      collection: first_collection,
      current_location: mapper.cleaned_graph.start_node().start_location(),
    });

    this
  }

  fn finish_first_collection(&mut self) -> Result<()> {
    log::debug!("Finishing, open collections: {:#?}", self.processing);
    ensure!(
      self.processing.len() == 1,
      "More than one collection open {:#?}",
      self.processing
    );
    self.processing.clear();
    Ok(())
  }

  pub fn freeze(mut self) -> Result<SegmentedMir> {
    self.finish_first_collection()?;

    Ok(SegmentedMir {
      first_collection: self.first_collection,
      segments: Frozen::freeze(self.segments),
      branches: Frozen::freeze(self.branches),
      collections: Frozen::freeze(self.collections),
      scopes: self.scope_graph.freeze(),
    })
  }

  fn next_scope(&mut self) -> ScopeId {
    let next = self.next_scope;
    // The scope graph is used to find _parent scopes_.
    self.scope_graph.add(next, self.current_scope());
    self.next_scope.increment_by(1);
    next
  }

  // ------------------------------------------------
  // Scope operations
  //
  // NOTE: scopes are controlled by the HIR Visitor
  // so we don't need to sanitize them at all.
  // They return Results to match the interface
  // of everything else though.

  // NOTE: After starting a body analysis this should never be None.
  fn current_scope(&self) -> ScopeId {
    *self.open_scopes.last().unwrap()
  }

  pub fn open_scope(&mut self) -> Result<ScopeId> {
    let next_scope = self.next_scope();
    self.open_scopes.push(next_scope);
    Ok(next_scope)
  }

  pub fn close_scope(&mut self, idx: ScopeId) -> Result<()> {
    ensure!(idx != *BASE_SCOPE, "cannot close base scope");

    let last_open = self.open_scopes.last().ok_or(anyhow!("no open scopes"))?;

    ensure!(
      *last_open == idx,
      "closing wrong scope expected: {last_open:?} given: {idx:?}"
    );

    self.open_scopes.pop();

    Ok(())
  }

  // -----------------
  // Branch operations

  /// Finds the basic block that is the last post-dominator of the successors of `root`.
  fn least_post_dominator(&self, root: BasicBlock) -> Option<BasicBlock> {
    log::debug!("Finding the least post-dominator for root {root:?}");

    let mapper = &self.mapper;

    // Find all basic blocks that are reachable from the root.
    let reachable = depth_first_search(&mapper.cleaned_graph, root)
      .filter(|&to| mapper.dominates(root, to))
      .collect::<HashSet<_>>();

    log::debug!("Reachable from root: {reachable:?}");

    // Find the blocks that is the _most_ post-dominating,
    // this is a point that must post-dominate everything else.
    let most_post_dominating = reachable
      .iter()
      .find(|&can| reachable.iter().all(|&n| mapper.post_dominates(*can, n)))?;

    log::debug!("Most post-dominating: {most_post_dominating:?}");

    // If a block dominates the "most post-dominator" that means that this
    // block also post-dominates all branches that occur after the root.
    // We exclude the (1) root itself, and (2) any false edges. False edges
    // are common in loop lowering but the borrowck semantics indicate that
    // we should consider points  *after* the false edges as having left the branches.
    let candidate_leasts = reachable
      .iter()
      .filter(|&can| {
        *can != root
          && !mapper.cleaned_graph.is_false_edge(*can)
          && mapper.dominates(*can, *most_post_dominating)
      })
      .collect::<Vec<_>>();

    log::debug!("Candidate least-dominating: {candidate_leasts:?}");

    // The least post-dominator dominates all the other post-dominators.
    candidate_leasts
      .iter()
      .find(|&can| {
        candidate_leasts
          .iter()
          .all(|&n| mapper.dominates(**can, *n))
      })
      .copied()
      .copied()
  }

  fn find_next_switchint(&self, current: Location) -> Option<Location> {
    use rustc_middle::mir::{Terminator, TerminatorKind as TK};
    let body = self.mapper.cleaned_graph.body();

    log::debug!("Locating switchInt after {current:?}");

    let has_switchint = |b: &BasicBlock| {
      matches!(body.basic_blocks[*b].terminator(), Terminator {
        kind: TK::SwitchInt { .. },
        ..
      })
    };

    let mut current = Some(current.block);
    let mut linear_blocks = std::iter::from_fn(move || {
      let previous = current?;
      let ss = self
        .mapper
        .cleaned_graph
        .successors(previous)
        .collect::<Vec<_>>();
      current = if ss.len() == 1 { Some(ss[0]) } else { None };
      Some(previous)
    });

    linear_blocks.find(has_switchint).map(|block| Location {
      block,
      statement_index: body.basic_blocks[block].statements.len(),
    })
  }

  fn mk_branch(
    &mut self,
    location: Location,
    get_span: GetSpanner<'a>,
  ) -> Result<BranchId> {
    let mapper = &self.mapper;
    let scope = self.current_scope();

    let switch_location =
      self.find_next_switchint(location).unwrap_or(location);

    log::debug!("switchInt located at: {switch_location:?}");

    // The convergence of all branching paths.
    let phi_opt = self
      .least_post_dominator(switch_location.block)
      .map(|bb| bb.start_location());

    log::debug!("Chosen least-post-dominator: {phi_opt:?}");

    let builder_opt = self
      .processing
      .iter_mut()
      .find(|cb| mapper.ldominates(cb.current_location, location));

    let Some(builder) = builder_opt else {
      bail!("no open collection dominates root location {location:?}");
    };

    ensure!(
      builder.current_location == location,
      "opening a branch missed a step, expected {:?} given: {:?}",
      builder.current_location,
      location
    );

    // Make a new branch
    let tid = self.branch_roots.new_key(());
    let bid =
      self
        .branches
        .push(BranchData::new(tid, switch_location, phi_opt));
    let branch = &mut self.branches[bid];

    // Save the Location -> Span mappings under this root BranchId.
    let get_span = match get_span {
      GetSpanner::InsertNew(b) => {
        self.root_mappings.insert(bid, b);
        &self.root_mappings[&bid]
      }
      GetSpanner::GetFrom(bid) => &self.root_mappings[&bid],
    };

    // Push the new Branch as a control flow kind on
    // the current collection's data set.
    self.collections[builder.collection]
      .data
      .push(CFKind::Branch(bid));

    let length_kind = if let Some(phi) = phi_opt {
      builder.current_location = phi;
      LengthKind::Bounded {
        root: location,
        phi,
      }
    } else {
      // TODO: how should we update the collection if there
      // isn't a phi? My current feeling is that we should
      // just close the collection.
      LengthKind::Unbounded { root: location }
    };

    // For each of the target BasicBlocks of the switchInt:
    for sblock in mapper.cleaned_graph.successors(switch_location.block) {
      log::debug!("Adding segment to switch successor: {sblock:?}");

      // 1. insert the split segment into the branch
      let mut to = sblock.start_location();
      let span = get_span(&mut to);
      let sid = self.segments.push(SegmentData {
        segment: MirSegment::new(location, to),
        span,
        scope,
      });
      branch.splits.push(sid);

      // 2. Open a new Collection with it's starting
      //    location at the branch target location.
      let cid = self.collections.push(Collection {
        data: Vec::default(),
        kind: length_kind,
      });

      // 3. Store this new collection in the branch middle section.
      branch.nested.push(cid);

      // 4. Put a new collection builder on the open collection stack.
      self.processing.push(CollectionBuilder {
        collection: cid,
        current_location: to,
      });
    }

    Ok(bid)
  }

  /// Opens a branch of control flow rooted at `location`.
  ///
  /// The function implicitly adds a new segment for all split steps
  /// and `get_span` should return the associated Span for these split steps.
  pub fn open_branch(
    &mut self,
    location: Location,
    get_span: impl Fn(&mut Location) -> Span + 'a,
  ) -> Result<BranchId> {
    log::debug!("opening user initiated branch at {location:?}");
    log::debug!("open branches BEFORE {:#?}", self.processing);

    let r = self.mk_branch(location, GetSpanner::InsertNew(Box::new(get_span)));

    log::debug!("open branches AFTER {:#?}", self.processing);
    r
  }

  fn open_child_branch(
    &mut self,
    parent: BranchId,
    root: Location,
  ) -> Result<()> {
    log::debug!("opening implicit branch at {root:?}");

    let child = self.mk_branch(root, GetSpanner::GetFrom(parent))?;
    let parent_tid = self.branches[parent].table_id;
    let child_tid = self.branches[child].table_id;
    self.branch_roots.union(parent_tid, child_tid);
    Ok(())
  }

  /// Closes a branch of control flow with an origin root of `location`.
  ///
  /// Contrary to previous implementations, the function does not implicitly
  /// add a new segment for all split steps.
  pub fn close_branch(&mut self, bid: BranchId) -> Result<()> {
    let table_root = self.branches[bid].table_id;

    let branches_to_close = self
      .branches
      .iter_enumerated()
      .filter_map(|(bid, bd)| {
        (table_root == self.branch_roots.find(bd.table_id)).then_some(bid)
      })
      .collect::<Vec<_>>();

    for bid in branches_to_close.into_iter() {
      let branch = &mut self.branches[bid];

      let nested_collections =
        branch.nested.iter().copied().collect::<HashSet<_>>();

      let closed_builders = self
        .processing
        .drain_collections(&nested_collections)
        .collect::<Vec<_>>();

      log::debug!("Closed builders {:#?}", closed_builders);
    }

    log::debug!("State after closing branches {:#?}", self.processing);

    Ok(())
  }

  fn find_containing_branch(&self, cid: CollectionId) -> Option<BranchId> {
    self
      .branches
      .iter_enumerated()
      .find_map(|(bid, branch)| branch.nested.contains(&cid).then_some(bid))
  }

  /// Search through the list of open builders and return the one that can
  /// be used to insert a new step ending at `location`.
  fn find_suitable_collection(&mut self, location: Location) -> FindResult {
    let mapper = &self.mapper;

    // We can insert into a collection where the last location
    // was the dominates the new location to insert.
    let builder_opt = self.processing.enumerate().find_map(|(i, cb)| {
      log::debug!("Trying to find open collection: {cb:?}");
      mapper
        .ldominates(cb.current_location, location)
        .then_some((i, cb))
    });

    // No collection found
    let Some((builder_i, builder)) = builder_opt else {
      return FindResult::None;
    };

    // Return the found builder to create a new linear step.
    if mapper.lpost_dominates(location, builder.current_location) {
      log::debug!(
        "location post-dominates builder: {location:?} {:?} {:?}",
        builder.current_location,
        builder_i
      );
      return FindResult::Linear(builder_i);
    }

    // Fallback case for when we  want to open an implicit branch. However,
    // if there doesn't exist a parent branch, this is just an internal error.
    match self.find_containing_branch(builder.collection) {
      None => {
        log::warn!("couldn't find branch containing {:?}", builder.collection);
        FindResult::None
      }
      Some(bid) => FindResult::NonLinear(bid, builder.current_location),
    }
  }

  // ----------
  // Insertions

  /// Insert a step ending at the given `Location`.
  ///
  /// It's the `SegmentedMir`s job to find out where the step came from,
  /// in the case of ambiguity the given path hint can be used, this
  /// proves most usefull when an implicit branch child needs to be spawned.
  /// See the doc comment for further details.
  pub fn insert(
    &mut self,
    location: Location,
    path_hint: Option<Location>,
    span: Span,
  ) -> Result<()> {
    log::debug!(
      "starting insertion with hint {path_hint:?} at {location:?} \ninto: {:?}",
      self.processing
    );

    match self.find_suitable_collection(location) {
      // BAD case, no dominating locations where we can insert.
      //
      // XXX: returning an internal error here is too limiting. It seems
      //      that if control-flow constructs are (mis)-used, then the MIR
      //      is already more simplified than we would expect. This approach
      //      siliently ignores these insertions, but we leave a log warning
      //      to help debugging if something bad happens.
      //
      //      This was changed from an Error with the introduction
      //      of the weird expr test cases. Making this change has not
      //      knowingly made previously failing test cases pass, nor has it
      //      affected the steps produced by the test suite.
      FindResult::None => {
        log::warn!(
          "no suitable collection for location {location:?} {:#?}",
          self.processing
        );

        Ok(())
      }

      // RARE case: spawn a new child branch and retry the insert.
      //      These automatic branches are used to handle match expressions
      //      that compile to a series of `switchInt`s.
      FindResult::NonLinear(parent, branch_loc) => {
        self.open_child_branch(parent, branch_loc)?;
        self.insert(location, path_hint, span)
      }

      // COMMON case: we can insert a linear segment into the found builder.
      FindResult::Linear(builder_idx) => {
        let scope = self.current_scope();
        let builder = self.processing.get_mut(builder_idx);
        let collection = &mut self.collections[builder.collection];

        let mut insert_to = |to| {
          let segment = MirSegment::new(builder.current_location, to);
          let segment_data = SegmentData {
            segment,
            span,
            scope,
          };
          log::debug!(
            "Inserting {segment:?} into builder {builder:?} {builder_idx:?}"
          );

          let segid = self.segments.push(segment_data);
          collection.data.push(CFKind::Linear(segid));
          builder.current_location = to;
        };

        match collection.kind {
          // If the step attempts to go past its previously computed bound
          // we will cut it short. I(gavinleroy) haven't yet seen this happen,
          // but in theory it's possible and is bad because it bypasses the
          // branching mechanisms.
          LengthKind::Bounded { phi, .. }
            if self.mapper.ldominates(phi, location) =>
          {
            log::warn!(
              "Linear insert is stepping past the join point {location:?} {phi:?}"
            );

            insert_to(phi)
          }

          _ => insert_to(location),
        }

        Ok(())
      }
    }
  }
}

#[cfg(test)]
pub(crate) mod test_exts {
  use rustc_data_structures::{
    captures::Captures, graph::iterate::post_order_from_to,
  };
  use rustc_middle::mir::BasicBlockData;

  use super::*;

  pub trait SegmentedMirTestExt {
    fn validate(&self, mapper: &IRMapper) -> Result<(), InvalidReason>;
  }

  #[allow(dead_code)]
  #[derive(Debug)]
  pub enum InvalidReason {
    MissingLocations {
      missing: Vec<Location>,
    },
    // DuplicateLocation {
    //   at: Location,
    // },
    InvalidSegment {
      segment: MirSegment,
      kind: BadSegmentKind,
    },
  }

  #[derive(Debug)]
  #[allow(clippy::enum_variant_names)]
  pub enum BadSegmentKind {
    SplitNoDom,
    JoinNoPostDom,
    LinearNoDom,
    LinearNoPostDom,
  }

  fn explode_block<'a, 'tcx: 'a>(
    bb: BasicBlock,
    block: &'a BasicBlockData<'tcx>,
    from: Option<usize>,
    to: Option<usize>,
  ) -> impl Iterator<Item = Location> + Captures<'tcx> + 'a {
    // End is an inclusive index.
    let start = from.unwrap_or(0);
    let end = to.unwrap_or(block.statements.len());
    (start ..= end).map(move |i| Location {
      block: bb,
      statement_index: i,
    })
  }

  impl MirSegment {
    fn explode<'a, 'tcx: 'a>(
      self,
      mapper: &'a IRMapper<'tcx>,
    ) -> impl Iterator<Item = Location> + Captures<'tcx> + 'a {
      let sb = self.from.block;
      let eb = self.to.block;
      let graph = &mapper.cleaned_graph;
      let mut block_path = post_order_from_to(graph, sb, Some(eb));
      // The target block is never added in the post-order.
      block_path.push(eb);

      block_path.into_iter().flat_map(move |bb| {
        let body = &mapper.cleaned_graph.body();
        let from = (bb == sb).then_some(self.from.statement_index);
        let to = (bb == eb).then_some(self.to.statement_index);
        explode_block(bb, &body.basic_blocks[bb], from, to)
      })
    }
  }

  impl SegmentedMir {
    fn is_valid_collection(
      &self,
      cid: CollectionId,
      ssf: &mut HashSet<Location>,
      mapper: &IRMapper,
    ) -> Result<(), InvalidReason> {
      let collection = self.get_collection(cid);
      for kind in collection.data.iter() {
        match kind {
          CFKind::Linear(sid) => self.is_valid_segment(*sid, ssf, mapper)?,
          CFKind::Branch(bid) => self.is_valid_branch(*bid, ssf, mapper)?,
        }
      }

      Ok(())
    }

    fn is_valid_split_segment(
      &self,
      sid: SegmentId,
      ssf: &mut HashSet<Location>,
      mapper: &IRMapper,
    ) -> Result<(), InvalidReason> {
      let SegmentData { segment: s, .. } = self.get_segment(sid);

      if !mapper.ldominates(s.from, s.to) {
        return Err(InvalidReason::InvalidSegment {
          segment: *s,
          kind: BadSegmentKind::SplitNoDom,
        });
      }

      for at in s.explode(mapper) {
        ssf.insert(at);
      }

      Ok(())
    }

    fn is_valid_join_segment(
      &self,
      sid: SegmentId,
      ssf: &mut HashSet<Location>,
      mapper: &IRMapper,
    ) -> Result<(), InvalidReason> {
      let SegmentData { segment: s, .. } = self.get_segment(sid);

      if !mapper.lpost_dominates(s.to, s.from) {
        return Err(InvalidReason::InvalidSegment {
          segment: *s,
          kind: BadSegmentKind::JoinNoPostDom,
        });
      }

      for at in s.explode(mapper) {
        ssf.insert(at);
      }

      Ok(())
    }

    fn is_valid_segment(
      &self,
      sid: SegmentId,
      ssf: &mut HashSet<Location>,
      mapper: &IRMapper,
    ) -> Result<(), InvalidReason> {
      let SegmentData { segment: s, .. } = self.get_segment(sid);
      if !mapper.ldominates(s.from, s.to) {
        return Err(InvalidReason::InvalidSegment {
          segment: *s,
          kind: BadSegmentKind::LinearNoDom,
        });
      }

      if !mapper.lpost_dominates(s.to, s.from) {
        return Err(InvalidReason::InvalidSegment {
          segment: *s,
          kind: BadSegmentKind::LinearNoPostDom,
        });
      }

      for at in s.explode(mapper) {
        ssf.insert(at);
      }

      Ok(())
    }

    fn is_valid_branch(
      &self,
      bid: BranchId,
      ssf: &mut HashSet<Location>,
      mapper: &IRMapper,
    ) -> Result<(), InvalidReason> {
      let branch = self.get_branch(bid);

      for &sid in branch.splits.iter() {
        self.is_valid_split_segment(sid, ssf, mapper)?;
      }

      for &sid in branch.joins.iter() {
        self.is_valid_join_segment(sid, ssf, mapper)?;
      }

      for &cid in branch.nested.iter() {
        self.is_valid_collection(cid, ssf, mapper)?;
      }

      Ok(())
    }
  }

  impl SegmentedMirTestExt for SegmentedMir {
    /// See the module documentation for a sense of what valid means. Here
    /// the below three basic things are checked. In the future, these guarantees
    /// will hopefully only ever get stronger, and never weaker.
    ///
    /// 1. All segments are valid regarding where they appear in the collection.
    /// 2. The segments form a total cover of the body.
    /// 3. At each branch location (`switchInt`) there must exist a split segment
    ///    for each possible branch target.
    fn validate(&self, mapper: &IRMapper) -> Result<(), InvalidReason> {
      let body = &mapper.cleaned_graph.body();
      let seen_so_far = &mut HashSet::default();

      let all_locations = mapper
        .cleaned_graph
        .blocks()
        .flat_map(|block| {
          explode_block(block, &body.basic_blocks[block], None, None)
        })
        .collect::<HashSet<_>>();

      self.is_valid_collection(self.first_collection, seen_so_far, mapper)?;
      let missing = all_locations
        .difference(&*seen_so_far)
        .copied()
        .collect::<Vec<_>>();
      if missing.is_empty() {
        Ok(())
      } else {
        Err(InvalidReason::MissingLocations { missing })
      }
    }
  }
}
