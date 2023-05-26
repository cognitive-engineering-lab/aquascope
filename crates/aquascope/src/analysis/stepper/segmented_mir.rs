//! Internal state for managing permissions steps.
//!
//! TODO: points to touch on:
//! - the recursive structure of the data
//! - branches
//! - inserting into linear segments
//! - managing "builders"
//! - the various mini-analyses we need for
//!   finding the right blocks.

use anyhow::{anyhow, bail, ensure, Result};
use rustc_data_structures::{
  frozen::Frozen,
  fx::{FxHashMap as HashMap, FxHashSet as HashSet},
  graph::*,
  transitive_relation::{TransitiveRelation, TransitiveRelationBuilder},
  unify::{InPlaceUnificationTable, UnifyKey},
};
use rustc_index::vec::{Idx, IndexVec};
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
  /// Split segments, `from` dominates `to` but
  /// `to` does not post-dominate `from`.
  pub(super) splits: Vec<SegmentId>,
  /// Join segments, `to` post-dominates `from` but
  /// `from` does not post-dominate `to`.
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
  mapper: &'a IRMapper<'a, 'tcx>,
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
    // Open collections are pushed on the end, but we want to search
    // in the most recently pushed by reverse the Vec::iter
    self.0.iter().rev()
  }

  pub fn enumerate(
    &self,
  ) -> impl Iterator<Item = (BuilderIdx, &CollectionBuilder)> + '_ {
    // Open collections are pushed on the end, but we want to search
    // in the most recently pushed by reverse the Vec::iter
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
    // Open collections are pushed on the end, but we want to search
    // in the most recently pushed by reverse the Vec::iter
    self.0.iter_mut().rev()
  }

  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  pub fn drain_collections<'a, 'this: 'a>(
    &'this mut self,
    cids: &'a HashSet<CollectionId>,
  ) -> impl Iterator<Item = CollectionBuilder> + 'a {
    self.0.drain_filter(|cb| cids.contains(&cb.collection))
  }

  pub fn get(&self, i: BuilderIdx) -> &CollectionBuilder {
    &self.0[i.0]
  }

  pub fn get_mut(&mut self, i: BuilderIdx) -> &mut CollectionBuilder {
    &mut self.0[i.0]
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
  pub fn make(mapper: &'a IRMapper<'a, 'tcx>) -> Self {
    let from = mapper.cleaned_graph.start_node().start_location();

    let mut collections = IndexVec::new();

    // We start with an empty linear collection.
    // TODO: try to find the bound for the entire body
    //       if it exists, only use LengthKind::Unbounded
    //       as a default.
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
      //       an open scope that the visitor cannot close.
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
    // TODO: make sure that the only open collection is the first collection.
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
  //       so we don't need to sanitize them at all.
  //       They return Results to match the interface
  //       of everything else though.

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

  // TODO: we can use the post-dominating tree to get this information.
  /// Finds the basic block that is the last post-dominator of the successors of `root`.
  fn least_post_dominator(&self, root: BasicBlock) -> Option<BasicBlock> {
    log::debug!("Finding the least post-dominator for root {root:?}");
    let mapper = &self.mapper;

    let reachable = mapper
      .cleaned_graph
      .depth_first_search(root)
      .filter(|&to| mapper.dominates(root, to))
      .collect::<HashSet<_>>();
    log::debug!("reachable from {root:?} {reachable:#?}");

    let most_post_dominating = reachable
      .iter()
      .find(|&can| reachable.iter().all(|&n| mapper.post_dominates(*can, n)))?;

    log::debug!("Most post-dominating {most_post_dominating:?}");

    let candidate_leasts = reachable
      .iter()
      .filter(|&can| {
        *can != root
          && !mapper.cleaned_graph.is_false_edge(*can)
          && mapper.dominates(*can, *most_post_dominating)
      })
      .collect::<Vec<_>>();

    log::debug!("Candidate least-post-dominators {candidate_leasts:#?}");

    candidate_leasts
      .iter()
      .find(|&can| {
        candidate_leasts
          .iter()
          .all(|&n| mapper.dominates(**can, *n))
      })
      .copied()
      .copied()

    // let open_blocks = mapper.cleaned_graph.successors(root).collect::<Vec<_>>();

    // log::debug!("Successors: {open_blocks:?}");

    // // Those blocks that are reachable from each successor.
    // let mut reachable_from_successors = open_blocks.iter().map(|&bb| {
    //   let reachable = mapper
    //     .cleaned_graph
    //     .depth_first_search(bb)
    //     .filter(|&to| mapper.dominates(bb, to))
    //     .collect::<HashSet<_>>();

    //   log::debug!("reachable from {bb:?} {reachable:#?}");

    //   reachable
    // });

    // let first_set: HashSet<BasicBlock> = reachable_from_successors.next()?;

    // let reachable_from_successors =
    //   reachable_from_successors.collect::<Vec<_>>();

    // // Take the intersection of all reachable sets.
    // let reachable_from_all_successors = first_set
    //   .into_iter()
    //   .filter(|e| reachable_from_successors.iter().all(|s| s.contains(e)))
    //   .collect::<Vec<_>>();

    // log::debug!(
    //   "These BasicBlocks are reachable from all successors: {:#?}",
    //   reachable_from_all_successors
    // );

    // reachable_from_all_successors
    //   .iter()
    //   .find(|&can_pdom| {
    //     reachable_from_all_successors
    //       .iter()
    //       .all(|&node| mapper.post_dominates(*can_pdom, node))
    //   })
    //   .copied()
  }

  fn mk_branch(
    &mut self,
    location: Location,
    get_span: GetSpanner<'a>,
  ) -> Result<BranchId> {
    let mapper = &self.mapper;
    let scope = self.current_scope();

    ensure!(
      self.mapper.is_terminator_switchint(location),
      "terminator in block {location:?} is not a `switchInt`"
    );

    // The convergence of all branching paths.
    let phi_opt = self
      .least_post_dominator(location.block)
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
    let bid = self.branches.push(BranchData::new(tid, location, phi_opt));
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
      //       isn't a phi? My current feeling is that we should
      //       just close the collection.
      LengthKind::Unbounded { root: location }
    };

    // For each of the target BasicBlocks of the switchInt:
    for sblock in mapper.cleaned_graph.successors(location.block) {
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
  /// The function implicitly adds a new segment for all split steps
  /// and `get_span` should return the associated Span for these split steps.
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

      let closed_builders =
        self.processing.drain_collections(&nested_collections);

      log::debug!(
        "closing builders: {:#?}",
        closed_builders.collect::<Vec<_>>()
      );
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

  fn find_suitable_collection(&mut self, location: Location) -> FindResult {
    let mapper = &self.mapper;

    // We can insert into a collection where the last location
    // was the dominates the new location to insert.
    let builder_opt = self.processing.enumerate().find_map(|(i, cb)| {
      log::debug!("TRYING TO FIND OPEN COLLECTION: {cb:?}");
      mapper
        .ldominates(cb.current_location, location)
        .then_some((i, cb))
    });

    // No collection found
    let Some((builder_i, builder)) = builder_opt else {
      return FindResult::None;
    };

    // Easy case! We can use a linear insert.
    if mapper.lpost_dominates(location, builder.current_location) {
      log::debug!(
        "location post-dominates builder: {location:?} {:?} {:?}",
        builder.current_location,
        builder_i
      );
      return FindResult::Linear(builder_i);
    }

    match self.find_containing_branch(builder.collection) {
      None => {
        log::error!("couldn't find branch containing {:?}", builder.collection);
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
      FindResult::None => bail!(
        "no suitable collection for location {location:?} {:#?}",
        self.processing
      ),

      // RARE case:
      // Spawn a new child branch and retry the insert.
      FindResult::NonLinear(parent, branch_loc) => {
        self.open_child_branch(parent, branch_loc)?;
        self.insert(location, path_hint, span)
      }

      // EASY case:
      // We can insert a segment _linearly_ into the list of segments,
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
          LengthKind::Bounded { phi, .. }
            if self.mapper.ldominates(phi, location) =>
          {
            insert_to(phi)
          }
          // All other cases can use the given location as the `to` location.
          _ => insert_to(location),
        }

        Ok(())
      }
    }
  }
}
