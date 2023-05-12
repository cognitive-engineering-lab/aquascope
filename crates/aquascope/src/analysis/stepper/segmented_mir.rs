//! Basic relaxation of MIR fragmentation storage.

use std::{
  collections::hash_map::Entry,
  ops::{Deref, DerefMut},
};

use anyhow::{anyhow, bail, ensure, Result};
use rustc_data_structures::{
  frozen::Frozen,
  fx::FxHashMap as HashMap,
  graph::*,
  transitive_relation::{TransitiveRelation, TransitiveRelationBuilder},
};
use rustc_index::vec::Idx;
use rustc_middle::mir::{Location, START_BLOCK};
use rustc_span::Span;

use super::MirSegment;
use crate::analysis::ir_mapper::IRMapper;

// --------------------------
// Decls sections

rustc_index::newtype_index! {
  /// Scopes are controlled at the segment-level
  /// and controlled by the caller.
  pub(super) struct ScopeIdx {}
}

rustc_index::newtype_index! {
  /// Collections are groups of segments thare nest.
  /// E.g., when a branch contains another branch.
  /// These are controlled internally.
  pub(super) struct CollectionIdx {}
}

lazy_static::lazy_static! {
  pub(super) static ref BASE_SCOPE: ScopeIdx = ScopeIdx::new(0);
}

#[derive(Clone, Debug)]
struct UnifiedState {
  idx: CollectionIdx,
  inserted: Vec<MirSegment>,
  from: Location,
}

/// The state of a current branch.
///
/// A branch splits the control flow at the given `Location`
/// and steps will be inserted along each open path until
/// they converge at `phi`. If there is not join node, e.g.
/// infinite loops or (TODO finish writing).
#[derive(Clone, Debug)]
struct BranchedState {
  idx: CollectionIdx,

  // Finalized segments
  splits: Vec<MirSegment>,
  joins: Vec<MirSegment>,
  middle: Vec<MirSegment>,

  // In-progress data
  root: Location,
  open_paths: Vec<Location>,
  phi: Option<Location>,
}

/// Currently processing steps within a control-flow branch.
///
/// Each open branch needs to be completed before it can be
/// popped off the stack and the previous continued. On exit
/// of the last branch the state returns to `Unified`.
#[derive(Clone, Debug)]
struct BranchStack(Vec<BranchedState>);

#[derive(Clone, Debug)]
enum StateKind {
  Unified(UnifiedState),
  Branched(BranchStack),
}

#[derive(Clone, Debug)]
struct State {
  kind: StateKind,
}

#[derive(Clone, Debug)]
pub enum CollectionKind {
  Linear {
    segments: Vec<MirSegment>,
  },
  Branch {
    root: Location,
    phi: Option<Location>,
    splits: Vec<MirSegment>,
    middle: Vec<MirSegment>,
    joins: Vec<MirSegment>,
  },
}

#[derive(Clone, Debug)]
pub struct Collection {
  pub(super) idx: CollectionIdx,
  pub(super) kind: CollectionKind,
}

pub(super) struct FrozenMir {
  scopes: TransitiveRelation<ScopeIdx>,
  nesting: TransitiveRelation<CollectionIdx>,
  collections: Frozen<HashMap<CollectionIdx, Collection>>,
  segments: Frozen<HashMap<MirSegment, (Span, ScopeIdx)>>,
}

pub(super) struct SegmentedMir<'a, 'tcx: 'a> {
  mapper: &'a IRMapper<'a, 'tcx>,
  shapes: HashMap<CollectionIdx, Collection>,
  segments: HashMap<MirSegment, (Span, ScopeIdx)>,
  state: State,
  scope_graph: TransitiveRelationBuilder<ScopeIdx>,
  collection_graph: TransitiveRelationBuilder<CollectionIdx>,
  open_scopes: Vec<ScopeIdx>,
  next_collection: CollectionIdx,
  next_scope: ScopeIdx,
}

// --------------------------
// Impl sections

impl From<UnifiedState> for Collection {
  fn from(state: UnifiedState) -> Self {
    Collection {
      idx: state.idx,
      kind: CollectionKind::Linear {
        segments: state.inserted,
      },
    }
  }
}

impl UnifiedState {
  pub fn new(idx: CollectionIdx, from: Location) -> Self {
    Self {
      idx,
      inserted: Vec::default(),
      from,
    }
  }
}

// XXX: only used so we can use `std::mem::take`.
impl Default for UnifiedState {
  fn default() -> Self {
    Self {
      idx: CollectionIdx::from(0u32),
      inserted: Vec::default(),
      from: START_BLOCK.start_location(),
    }
  }
}

impl From<BranchedState> for Collection {
  fn from(state: BranchedState) -> Self {
    Collection {
      idx: state.idx,
      kind: CollectionKind::Branch {
        root: state.root,
        phi: state.phi,
        splits: state.splits,
        middle: state.middle,
        joins: state.joins,
      },
    }
  }
}

impl BranchedState {
  /// Makes a new `BranchedState` that is rooted at the given location.
  ///
  /// Building a `BranchedState` could fail if the given root
  /// comes from a block without a `switchInt` terminator. The
  /// HIR Visitor should of course make sure this prerequisite holds.
  fn make(
    mapper: &IRMapper,
    idx: CollectionIdx,
    root: Location,
  ) -> Result<Self> {
    ensure!(
      mapper.is_terminator_switchint(root),
      "terminator in block {root:?} is not a `switchInt`"
    );

    let open_branches = mapper
      .cleaned_graph
      .successors(root.block)
      .map(|bb| bb.start_location())
      .collect::<Vec<_>>();

    let potential_joins = mapper
      .cleaned_graph
      .blocks()
      .filter_map(|bb| {
        open_branches
          .iter()
          .all(|loc| mapper.post_dominates(bb.start_location(), *loc))
          .then_some(bb.start_location())
      })
      .collect::<Vec<_>>();

    let close = potential_joins
      .iter()
      .find(|&before| {
        potential_joins
          .iter()
          .all(|after| mapper.dominates(*before, *after))
      })
      .copied();

    log::debug!("creating new branch:\n\troot: {root:?}\n\tpath_starts: {open_branches:?}\n\tphi: {close:?}");

    Ok(BranchedState {
      idx,
      joins: Vec::default(),
      middle: Vec::default(),
      splits: Vec::default(),
      root,
      open_paths: open_branches,
      phi: close,
    })
  }

  pub fn location_dominators(
    &self,
    mapper: &IRMapper,
    location: Location,
  ) -> Vec<(usize, &Location)> {
    self
      .open_paths
      .iter()
      .enumerate()
      .filter(|&(_, start)| mapper.dominates(*start, location))
      .collect::<Vec<_>>()
  }

  /// Gets the index of the path that dominates the given `Location`.
  ///
  /// Returns `Err` if no dominating path is found, or if _multiple_
  /// are found, this indicates that the bookkeeping went awry or
  /// my mental model of the problem broke down.
  pub fn get_dominating_path_idx(
    &self,
    mapper: &IRMapper,
    location: Location,
    path_hint: Option<Location>,
  ) -> Result<usize> {
    let expect_unique = |branches: &[(usize, &Location)]| -> Result<usize> {
      match branches.len() {
        0 => bail!("no branches dominate {location:?} {self:#?}"),
        1 => Ok(branches[0].0),
        _ => bail!("multiple dominating branches {location:?} {branches:#?}"),
      }
    };

    log::debug!(
      "finding open paths that dominate {location:?} with hint {path_hint:?}\noptions: {:#?}",
      self.open_paths
    );

    let containing_branches = self.location_dominators(mapper, location);
    match (expect_unique(&containing_branches), path_hint) {
      (r @ Ok(_), _) | (r, None) => r,
      (_, Some(hint)) => {
        let hint_block_start = hint.block.start_location();
        let hinted_branches = containing_branches
          .into_iter()
          .filter(|(_, l)| mapper.dominates(hint_block_start, **l))
          .collect::<Vec<_>>();
        expect_unique(&hinted_branches)
      }
    }
  }

  pub fn is_closed_by(&self, mapper: &IRMapper, location: Location) -> bool {
    if let Some(phi) = self.phi {
      mapper.dominates(phi, location)
    } else {
      false
    }
  }
}

impl Deref for BranchStack {
  type Target = Vec<BranchedState>;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl DerefMut for BranchStack {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

impl BranchStack {
  pub fn new(initial: BranchedState) -> Self {
    Self(vec![initial])
  }

  pub fn get_current_branch(&self) -> Result<&BranchedState> {
    self.last().ok_or(anyhow!("branch stack empty"))
  }

  pub fn get_current_branch_mut(&mut self) -> Result<&mut BranchedState> {
    self.last_mut().ok_or(anyhow!("branch stack empty"))
  }
}

impl State {
  pub fn get_unified_mut(&mut self) -> Result<&mut UnifiedState> {
    match self.kind {
      StateKind::Unified(ref mut from) => Ok(from),
      _ => bail!("expected unified state"),
    }
  }

  pub fn get_branched(&self) -> Result<&BranchStack> {
    match self.kind {
      StateKind::Branched(ref stack) => Ok(stack),
      _ => bail!("expected branched state"),
    }
  }

  pub fn get_branched_mut(&mut self) -> Result<&mut BranchStack> {
    match self.kind {
      StateKind::Branched(ref mut stack) => Ok(stack),
      _ => bail!("expected branched state"),
    }
  }

  pub fn get_current_branch(&self) -> Result<&BranchedState> {
    self.get_branched()?.get_current_branch()
  }
}

impl std::fmt::Debug for SegmentedMir<'_, '_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "#<SegmentedMir: TODO>")
  }
}

impl FrozenMir {
  pub fn collections(&self) -> impl Iterator<Item = &CollectionIdx> + '_ {
    self.collections.keys()
  }

  pub fn collection_data(
    &self,
    collection: CollectionIdx,
  ) -> Option<&Collection> {
    self.collections.get(&collection)
  }

  pub fn segment_data(&self, segment: MirSegment) -> Option<(Span, ScopeIdx)> {
    self.segments.get(&segment).copied()
  }

  pub fn parent_scopes(
    &self,
    scope: ScopeIdx,
  ) -> impl Iterator<Item = ScopeIdx> + '_ {
    let mut parents = self.scopes.reachable_from(scope);
    parents.push(scope);
    parents.into_iter()
  }

  pub fn nested_collections(
    &self,
    collection: CollectionIdx,
  ) -> impl Iterator<Item = CollectionIdx> + '_ {
    self.nesting.reachable_from(collection).into_iter()
  }
}

// Using as a macro to avoid double mutable refs
macro_rules! insert_segment_unchecked {
  ($this:ident, $seg:expr, $sp:expr, $sc:expr) => {
    match $this.segments.entry($seg) {
      Entry::Occupied(entry) => {
        log::warn!(
          "inserting segment failed because it already existed {:?} {:?}",
          $seg,
          entry
        );
      }
      Entry::Vacant(entry) => {
        entry.insert(($sp, $sc));
      }
    }
  };
}

impl<'a, 'tcx: 'a> SegmentedMir<'a, 'tcx> {
  pub fn make(mapper: &'a IRMapper<'a, 'tcx>) -> Self {
    let from = mapper.cleaned_graph.start_node().start_location();
    let initial_collection = CollectionIdx::new(0);
    Self {
      mapper,
      state: State {
        kind: StateKind::Unified(UnifiedState::new(initial_collection, from)),
      },
      scope_graph: TransitiveRelationBuilder::default(),
      collection_graph: TransitiveRelationBuilder::default(),
      // NOTE: this maintains that there is always
      //       an open scope that the visitor cannot close.
      open_scopes: vec![*BASE_SCOPE],
      next_scope: BASE_SCOPE.plus(1),
      next_collection: initial_collection.plus(1),
      shapes: HashMap::default(),
      segments: HashMap::default(),
    }
  }

  pub fn freeze(mut self) -> Result<FrozenMir> {
    // TODO: Do validation on the end state of things.

    match self.state.kind {
      StateKind::Unified(unified) => {
        let collection: Collection = unified.into();
        self.shapes.insert(collection.idx, collection);
      }
      StateKind::Branched(stack) if !stack.is_empty() => {
        bail!("finishing computation with open branches")
      }
      // empty branch stack, which shouldn't happen but
      // we can ignore that here if it does.
      _ => (),
    };

    Ok(FrozenMir {
      scopes: self.scope_graph.freeze(),
      nesting: self.collection_graph.freeze(),
      collections: Frozen::freeze(self.shapes),
      segments: Frozen::freeze(self.segments),
    })
  }

  // ----------------
  // Idx operations

  fn next_scope(&mut self) -> ScopeIdx {
    let next = self.next_scope;
    // The scope graph is used to find _parent scopes_.
    self.scope_graph.add(next, self.current_scope());
    self.next_scope.increment_by(1);
    next
  }

  fn next_collection(&mut self) -> CollectionIdx {
    let next = self.next_collection;
    if let Ok(branch) = self.state.get_current_branch() {
      // The collection graph is used to find _nested branches_.
      self.collection_graph.add(branch.idx, next);
    }
    self.next_collection.increment_by(1);
    next
  }

  // ----------------
  // Scope operations

  /// After starting a body analysis this should never be None.
  fn current_scope(&self) -> ScopeIdx {
    *self.open_scopes.last().unwrap()
  }

  pub fn open_scope(&mut self) -> Result<ScopeIdx> {
    let next_scope = self.next_scope();
    self.open_scopes.push(next_scope);
    Ok(next_scope)
  }

  pub fn close_scope(&mut self, idx: ScopeIdx) -> Result<()> {
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

  fn is_branch_open(&self, root: Location) -> bool {
    if let StateKind::Branched(stack) = &self.state.kind {
      stack.iter().any(|bs| bs.root == root)
    } else {
      false
    }
  }

  /// Opens a branch of control flow rooted at `location`.
  ///
  /// The function implicitly adds a new segment for all split steps
  /// and `get_span` should return the associated Span for these split steps.
  pub fn open_branch(
    &mut self,
    location: Location,
    get_span: impl Fn(&mut Location) -> Span,
  ) -> Result<()> {
    log::debug!("creating new branch rooted at {location:?}");
    let next_collection = self.next_collection();

    let new_branch =
      BranchedState::make(self.mapper, next_collection, location);

    // Put the current state into a branched state building a
    // new branch node on the stack.
    //
    // If we were in a unifying state this location must be the
    // last one where a step ended. If it isn't we could either:
    // 1. insert the step (bad idea IMO)
    // 2. fail
    //
    // From a already branched state this location needs to be
    // reachable from exactly one variant and (TODO: finish writing).
    // NOTE: a linear state is not returned to, it is replaced by a
    //       new linear state. Branch states stack up until they are
    //       all finished.
    match self.state.kind {
      StateKind::Unified(ref mut ustate) => {
        ensure!(
          location == ustate.from,
          "opened control flow missed a step: unified @ {:?} CF {:?}",
          ustate.from,
          location
        );

        // Store the shape of collected mir segments
        let unified = std::mem::take(ustate);
        let collection: Collection = unified.into();
        self.shapes.insert(collection.idx, collection);

        // Swap to the new branched state
        self.state.kind = StateKind::Branched(BranchStack::new(new_branch?));
      }
      StateKind::Branched(ref mut stack) => {
        let branch = stack.get_current_branch_mut()?;
        let _ = branch.get_dominating_path_idx(self.mapper, location, None)?;
        // TODO: we should update the branch we're going from
        stack.push(new_branch?);
      }
    };

    let scope = self.current_scope();
    let stack = self.state.get_branched_mut()?;
    let branch = stack.get_current_branch_mut()?;
    log::debug!("flushing branch split steps");

    // NOTE: we are passing a `&mut Location` for a reason.
    //       The HIR node that starts the branch entry often
    //       has a Location in the middle of a BasicBlock, not
    //       right at the beginning. The mut ref allows the HIR
    //       to do small corrections on the location as necessary.
    for to in branch.open_paths.iter_mut() {
      let span = get_span(to); // Update loc first!
      let segment = MirSegment::new(branch.root, *to);
      log::debug!("inserting split step {segment:?}");
      branch.splits.push(segment);
      insert_segment_unchecked!(self, segment, span, scope);
    }

    Ok(())
  }

  /// Opens a branch of control flow rooted at `location`.
  ///
  /// The function implicitly adds a new segment for all split steps
  /// and `get_span` should return the associated Span for these split steps.
  pub fn close_branch(
    &mut self,
    root: Location,
    get_span: impl Fn(MirSegment) -> Span,
  ) -> Result<()> {
    ensure!(
      self.is_branch_open(root),
      "no open branch rooted at {root:?}"
    );

    let scope = self.current_scope();
    let stack = self.state.get_branched_mut()?;
    let mut branch = stack.pop().ok_or(anyhow!("branch stack empty"))?;

    ensure!(
      branch.root == root,
      "expecting to close branch rooted at {root:?} but given {branch:#?}"
    );

    log::debug!("Flushing branch state {branch:#?}");

    // Flush any remaining open paths to step back to the phi node.
    if let Some(to) = branch.phi {
      for &from in branch.open_paths.iter() {
        let segment = MirSegment::new(from, to);
        log::debug!("flushing join step {segment:?}");
        branch.joins.push(segment);
        insert_segment_unchecked!(self, segment, get_span(segment), scope);
      }
    }

    // If we're closing the last branch then we need to put the
    // state back to uniform, the last branch node should always
    // have a valid phi.
    if stack.is_empty() {
      if let Some(from) = branch.phi {
        let idx = self.next_collection();
        self.state.kind = StateKind::Unified(UnifiedState::new(idx, from));
      } else {
        bail!("closing last branch without a phi node");
      }
    }

    let collection: Collection = branch.into();
    self.shapes.insert(collection.idx, collection);

    Ok(())
  }

  // ----------
  // Insertions

  pub fn insert(
    &mut self,
    location: Location,
    path_hint: Option<Location>,
    span: Span,
  ) -> Result<()> {
    log::debug!("starting insertion with hint {path_hint:?} at {location:?}");

    match &self.state.kind {
      StateKind::Unified(UnifiedState { from, .. }) => {
        self.insert_linear(*from, location, span)
      }
      StateKind::Branched(..) => {
        self.insert_in_branch(location, path_hint, span)
      }
    }
  }

  /// Insert a step where all control flow start at `from` and ends at `to`.
  ///
  /// This means that `from` dominates `to`, and `to` post-dominates `from`.
  fn insert_linear(
    &mut self,
    from: Location,
    to: Location,
    span: Span,
  ) -> Result<()> {
    log::debug!("starting linear insertion step {from:?} -> {to:?}");

    ensure!(
      self.mapper.dominates(from, to),
      "linear insert dominance condition unsatisfied {from:?} -> {to:?}"
    );

    ensure!(
      self.mapper.post_dominates(to, from),
      "linear insert post-dominance condition unsatisfied {from:?} -> {to:?}"
    );

    let unified = self.state.get_unified_mut()?;

    ensure!(
      unified.from == from,
      "missed step in unified state {:?}: {:?} -> {:?}",
      unified.from,
      from,
      to
    );

    let segment = MirSegment::new(from, to);
    log::debug!("inserting {segment:?} into unified state");

    // Update the UnifiedState
    unified.from = to;
    unified.inserted.push(segment);

    let scope = self.current_scope();
    insert_segment_unchecked!(self, segment, span, scope);

    Ok(())
  }

  fn insert_in_branch(
    &mut self,
    to: Location,
    path_hint: Option<Location>,
    span: Span,
  ) -> Result<()> {
    log::debug!("trying branched insertion with hint {path_hint:?} at {to:?}");

    let branch_stack = self.state.get_branched_mut()?;
    let branch = branch_stack.get_current_branch_mut()?;
    let path_idx =
      branch.get_dominating_path_idx(self.mapper, to, path_hint)?;
    let from = branch.open_paths[path_idx];

    // If the step will go past the phi node, or to the phi
    // node exactly, then we will close this open path and
    // limit the step to only reach the phi node.
    let to = if let Some(phi) = branch.phi && self.mapper.dominates(phi, to) {
      // We've gone past the phi node, close the open path and
      // return phi as the to location
      log::debug!("location {to:?} closed branch path {from:?} -> {phi:?}");
      branch.open_paths.remove(path_idx);
      phi
    } else {
      branch.open_paths[path_idx] = to;
      to
    };

    let segment = MirSegment::new(from, to);
    // Update the branch state
    branch.middle.push(segment);

    log::debug!("inserting {segment:?} into current branch {branch:#?}");
    let scope = self.current_scope();
    insert_segment_unchecked!(self, segment, span, scope);

    Ok(())
  }
}
