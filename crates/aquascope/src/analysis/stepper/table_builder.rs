//! Convert permissions steps into tables viewable by the frontend.

use rustc_data_structures::{
  self,
  fx::{FxHashMap as HashMap, FxHashSet as HashSet},
};
use rustc_middle::mir::{Local, Location, Place};
use rustc_span::Span;
use rustc_utils::{test_utils::DUMMY_CHAR_RANGE, PlaceExt, SpanExt};

use super::{segmented_mir::*, *};
use crate::{analysis::permissions::PermissionsCtxt, errors};

/// A single unprocessed table, mapping Places to their differences for a MirSegment.
#[derive(Debug)]
pub(super) struct Table<'tcx> {
  span: Span,
  segment: MirSegment,
  data: HashMap<Place<'tcx>, PermissionsDataDiff>,
}

/// A series of tables, identified by the _ending location_ of the step.
///
/// Except in branchess, ending locations should only contains a
/// single table. These tables are currently collapsed into a single
/// larger table and shows per-line, though, this restriction could
/// be relaxed in the future.
///
/// See [`prettify_permission_steps`] for how tables get merged.
pub(super) type Tables<'tcx> = HashMap<Location, Vec<Table<'tcx>>>;

pub(super) struct TableBuilder<'a, 'tcx: 'a> {
  pub(super) analysis: &'a AquascopeAnalysis<'a, 'tcx>,
  pub(super) ctxt: &'a PermissionsCtxt<'a, 'tcx>,
  pub(super) mir: &'a SegmentedMir,
  pub(super) locals_at_scope: HashMap<ScopeId, Vec<Local>>,
}

#[allow(clippy::similar_names)]
impl<'a, 'tcx: 'a> TableBuilder<'a, 'tcx> {
  pub(super) fn finalize_body(
    &self,
    start_loc: Location,
    body_span: Span,
    mode: PermIncludeMode,
  ) -> Vec<PermissionsLineDisplay> {
    let first_point = self.ctxt.location_to_point(start_loc);
    let first_domain = &self.ctxt.permissions_domain_at_point(first_point);
    let empty_domain = &self.ctxt.domain_bottom();
    let body_open_brace = body_span.shrink_to_lo();

    // Upon entry, the function parameters are already "live". But we want to
    // special case this, and show that they "come alive" at the opening brace.
    let first_diff = empty_domain.diff(first_domain);

    // Insert a segment into a table filtering defined places.
    let mut diffs = Tables::default();

    // We do an unchecked insert here to avoid
    // the segment from getting filtered because the
    // segment from and to locations are equal.
    let seg = MirSegment::new(start_loc, start_loc);
    diffs.entry(seg.to).or_default().push(Table {
      segment: seg,
      span: body_open_brace,
      data: first_diff,
    });
    self.insert_collection(&mut diffs, self.mir.first_collection);

    prettify_permission_steps(self.analysis, diffs, mode)
  }

  fn locals_to_filter(&self, scope: ScopeId) -> HashSet<Local> {
    self
      .mir
      .parent_scopes(scope)
      .filter_map(|sid| self.locals_at_scope.get(&sid))
      .flatten()
      .copied()
      .collect::<HashSet<_>>()
  }

  fn insert_collection(&self, result: &mut Tables<'tcx>, cid: CollectionId) {
    let collection = self.mir.get_collection(cid);

    for &part in collection.data.iter() {
      match part {
        CFKind::Linear(seg_id) => self.insert_segment(result, seg_id),
        CFKind::Branch(branch_id) => self.insert_branch(result, branch_id),
      }
    }
  }

  fn insert_segment(&self, result: &mut Tables<'tcx>, sid: SegmentId) {
    let ctxt = &self.ctxt;
    let &SegmentData {
      segment,
      span,
      scope,
    } = self.mir.get_segment(sid);

    let to_filter = self.locals_to_filter(scope);

    if segment.from == segment.to {
      return;
    }

    let p0 = ctxt.location_to_point(segment.from);
    let p1 = ctxt.location_to_point(segment.to);
    let before = &ctxt.permissions_domain_at_point(p0);
    let after = &ctxt.permissions_domain_at_point(p1);
    let mut diff = before.diff(after);

    let removed = diff
      .extract_if(|place, _| to_filter.contains(&place.local))
      .collect::<Vec<_>>();

    if !removed.is_empty() {
      log::debug!(
        "removed domain places due to attached filter at {:?} {:?}",
        segment.to,
        removed
      );
    }

    let table = Table {
      segment,
      span,
      data: diff,
    };

    log::info!("saving segment diff {segment:?}");
    result.entry(segment.to).or_default().push(table);
  }

  // NOTE: when inserting a branch we currently ignore join steps. Within the
  //       function the previous code is left commented out. It was left in case
  //       we need to quickly bring it back, but through testing I found
  //       it was a lot of complex logic that removed all join steps, every time.
  //       Therefore, to save time, we just ignore them! We did this filtering
  //       to remove any weird permissions changes that were branch sensitive in
  //       order to avoid showing the same change in permissions multiple times.
  //       Should we decide to change this then this code will become relevant again.
  fn insert_branch(&self, result: &mut Tables<'tcx>, bid: BranchId) {
    let BranchData {
      reach,
      splits,
      // joins,
      nested,
      ..
    } = self.mir.get_branch(bid);

    let mut entire_diff = reach.into_diff(self.ctxt);

    log::debug!(
      "Inserting Branched Collection {:?}:\n\tsplits: {:?}\n\tmiddle: {:?}",
      reach,
      splits,
      nested
    );

    let mut temp_middle = Tables::default();
    // let mut temp_joins = Tables::default();

    for &sid in splits.iter() {
      self.insert_segment(&mut temp_middle, sid);
    }

    for &cid in nested.iter() {
      self.insert_collection(&mut temp_middle, cid);
    }

    // for &sid in joins.iter() {
    //   self.insert_segment(&mut temp_joins, sid);
    // }

    // Find the locals which were filtered from all scopes. In theory,
    // `all_scopes` should contains the same scope, copied over,
    // but the SegmentedMir doesn't enforce this and there's no
    // scope attached to collections.
    let scope_here = self.mir.get_branch_scope(bid);
    let all_attached = self
      .locals_at_scope
      .get(&scope_here)
      .map(|v| v.iter())
      .unwrap_or_default()
      .collect::<HashSet<_>>();

    let attached_here = entire_diff
      .extract_if(|place: &Place, _| all_attached.contains(&place.local))
      .collect::<HashMap<_, _>>();

    // let diffs_in_tables = |tbls: &Tables| {
    //   tbls
    //     .iter()
    //     .flat_map(|(_, v)| v.iter().flat_map(|tbl| tbl.data.values()))
    //     .copied()
    //     .collect::<HashSet<PermissionsDataDiff>>()
    // };

    // Flatten all tables to the unique `PermissionsDataDiff`s
    // that exist within them.

    // let diffs_in_branches = diffs_in_tables(&mut temp_middle);
    // for (_, v) in temp_joins.iter_mut() {
    //   for tbl in v.iter_mut() {
    //     let drained = tbl
    //       .data
    //       .drain_filter(|_, diff| diffs_in_branches.contains(diff))
    //       .map(|(p, _)| p)
    //       .collect::<Vec<_>>();
    //     log::debug!("diffs at join loc removed for redundancy {drained:#?}");
    //   }
    // }

    result.extend(temp_middle);
    // result.extend(temp_joins);

    // Attach filtered locals
    result.entry(reach.to).or_default().push(Table {
      span: reach.span(self.ctxt),
      segment: *reach,
      data: attached_here,
    });
  }
}

// Prettify, means:
// - Remove all places that are not source visible
// - Remove all tables which are empty
// - Convert Spans to Ranges
#[allow(clippy::if_not_else)]
pub(super) fn prettify_permission_steps<'tcx>(
  analysis: &AquascopeAnalysis<'_, 'tcx>,
  perm_steps: Tables<'tcx>,
  mode: PermIncludeMode,
) -> Vec<PermissionsLineDisplay> {
  let ctxt = &analysis.permissions;
  let tcx = ctxt.tcx;
  let body = &ctxt.body_with_facts.body;

  let should_keep = |p: &PermissionsDataDiff| -> bool {
    !(matches!(p.is_live, ValueStep::None { value: Some(false) })
      || (mode == PermIncludeMode::Changes && p.is_empty()))
  };

  macro_rules! place_to_string {
    ($p:expr) => {
      $p.to_string(tcx, body)
        .unwrap_or_else(|| String::from("<var>"))
    };
  }

  let first_error_span_opt =
    errors::get_span_of_first_error(ctxt.def_id.expect_local())
      .and_then(|s| s.as_local(ctxt.body_with_facts.body.span));
  let source_map = tcx.sess.source_map();

  let mut semi_filtered = HashMap::<
    usize,
    Vec<(MirSegment, Span, Vec<(Place<'tcx>, PermissionsDataDiff)>)>,
  >::default();

  // Goal: filter out differences for Places that
  // aren't source-visible. As well as those that come
  // after the first error span.
  // Group these intermediate tables by line numbers to make
  // collapsing them easier.
  for (_, v) in perm_steps.into_iter() {
    for Table {
      segment,
      span,
      data,
    } in v.into_iter()
    {
      // Attach the span to the end of the line. Later, all permission
      // steps appearing on the same line will be combined.
      let span = source_map.span_extend_to_line(span).shrink_to_hi();
      let entries = data
        .into_iter()
        .filter(|(place, diff)| {
          place.is_source_visible(tcx, body) && should_keep(diff)
        })
        .collect::<Vec<_>>();

      // This could be a little more graceful. The idea is that
      // we want to remove all permission steps which occur after
      // the first error, but the steps involved with the first
      // error could still be helpful. This is why we filter all
      // spans with a LO BytePos greater than the error
      // span HI BytePos.
      if !(entries.is_empty()
        || first_error_span_opt
          .is_some_and(|err_span| err_span.hi() < span.lo()))
      {
        // We'll store things by line number
        let line_num = source_map.lookup_line(span.hi()).unwrap().line;
        semi_filtered
          .entry(line_num)
          .or_default()
          .push((segment, span, entries));
      } else {
        log::debug!(
          "segment diff at {segment:?} was empty or follows an error"
        );
      }
    }
  }

  // NOTE: we're at odds with the multi-table setup. This quick
  // hack combines table entries into a single table until the
  // visual explanation gets up-to-speed.
  // Another weird thing about this is that you can have a single
  // table with two changes for one place.
  // ```example
  // # fn main() {
  // let closure = |s: &str| s.len(); // s: +R+O
  //                                  // s: -R-O
  //                                  // closure: +R+O
  // # }
  // ```
  // imagine that the comments to the right of the Let represent
  // a pseudo combined table. The path `s` gains and loses the same
  // set of permissions in the same table. This is kind of weird, we'd
  // rather just show *no change*.

  semi_filtered
    .into_iter()
    .filter_map(|(line, entries)| {

      // Conforming to the above HACK this just takes any (from, to) pair.
      let dummy_char_range = DUMMY_CHAR_RANGE.with(|range| *range);
      let (from, to, range) = entries.first().map_or_else(
        || (dummy_char_range, dummy_char_range, dummy_char_range),
        |(MirSegment { from, to }, span, _)| {
          let range = analysis.span_to_range(*span);
          let from = analysis.span_to_range(ctxt.location_to_span(*from));
          let to = analysis.span_to_range(ctxt.location_to_span(*to));
          (from, to, range)
        },
      );

      let mut combined_table =
        HashMap::<Place<'tcx>, PermissionsDataDiff>::default();

      // For all tables which fall on the same line, we combine them into a single table
      // and remove all *SYMMETRIC* differences. That is, if you have permission changes such as:
      // - path: +R+O
      // - path: -R-O
      // these are exactly symmetric, and will be removed.
      log::debug!("Finishing the combined table for line {line}");
      for (segment, _, diffs) in entries.into_iter() {
        for (place, diff) in diffs.into_iter() {
          match combined_table.entry(place) {
            Entry::Vacant(o) => {
              log::debug!("- Place: {place:?} Segment {segment:?}\n\t\t{diff:?}");
              o.insert(diff);
            }
            Entry::Occupied(o) => {
              let old_diff = o.get();
              if diff.is_symmetric_diff(old_diff) {
                log::debug!(
                  "X Place {place:?} had a symmetric difference."
                );
                o.remove();
                // master_table.remove(idx);
                continue;
              } else {
                  log::warn!("Clashing places on a step table were not symmetric: {place:?}");
              }
            }
          };
        }
      }

      // This means the tables were symmetric and all were removed.
      if combined_table.is_empty() {
        return None;
      }

      let mut master_table_vec = combined_table
        .into_iter()
        .collect::<Vec<_>>();

      master_table_vec
            .sort_by_key(|(place, _)| (place.local.as_usize(), place.projection));

      let master_table = PermissionsStepTable {
        from,
        to,
        state: master_table_vec
          .into_iter()
          .map(|(place, diff)| (place_to_string!(place), diff))
          .collect::<Vec<_>>(),
      };

      Some(PermissionsLineDisplay {
        location: range,
        state: vec![master_table],
      })
    })
    .collect::<Vec<_>>()
}
