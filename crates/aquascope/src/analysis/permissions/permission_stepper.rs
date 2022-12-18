use flowistry::mir::utils::PlaceExt as FlowistryPlaceExt;
use rustc_data_structures::fx::FxHashMap as HashMap;
use rustc_hir::{
  self as hir,
  intravisit::{self, Visitor as HirVisitor},
  HirId,
};
use rustc_middle::{
  hir::nested_filter,
  mir::{Location, Place},
};
use rustc_mir_dataflow::Analysis;
use rustc_span::Span;

use crate::{
  analysis::{
    ir_mapper::{GatherDepth, GatherMode, IRMapper},
    permissions::{
      Difference, PermissionsCtxt, PermissionsDataDiff, PermissionsStateStep,
    },
  },
  mir::utils::PlaceExt as AquascopePlaceExt,
  Range,
};

pub fn compute_permission_steps<'a, 'tcx>(
  ctxt: &PermissionsCtxt<'a, 'tcx>,
  span_to_range: impl Fn(Span) -> Range,
) -> Vec<PermissionsStateStep>
where
  'tcx: 'a,
{
  let tcx = ctxt.tcx;
  let body = &ctxt.body_with_facts.body;
  let _basic_blocks = body.basic_blocks.indices();

  // FIXME REMOVE
  let mut stderr = std::io::stderr();
  rustc_middle::mir::pretty::write_mir_fn(
    tcx,
    body,
    &mut |_, _| Ok(()),
    &mut stderr,
  );

  let ir_mapper = &IRMapper::new(tcx, body, GatherMode::IgnoreCleanup);

  let mut hir_visitor = HirPermDiffFlow {
    ctxt,
    ir_mapper,
    diff: HashMap::default(),
    step_barriers: Vec::default(),
    visibility_scopes: Vec::default(),
  };

  log::debug!("HIR IDS:");

  hir_visitor.visit_nested_body(ctxt.body_id);

  // TODO: this is not correct!
  let interim = hir_visitor
    .diff
    .into_iter()
    .filter_map(|(id, places_to_perms)| {
      let filtered = places_to_perms
        .into_iter()
        .filter(|(place, diff)| {
          place.is_source_visible(tcx, body) && !diff.is_empty()
        })
        .collect::<HashMap<_, _>>();

      (!filtered.is_empty()).then_some((id, filtered))
    })
    .collect::<HashMap<_, _>>();

  prettify_permission_steps(ctxt, interim, span_to_range)
}

fn prettify_permission_steps<'tcx>(
  ctxt: &PermissionsCtxt<'_, 'tcx>,
  perm_steps: HashMap<HirId, HashMap<Place<'tcx>, PermissionsDataDiff>>,
  span_to_range: impl Fn(Span) -> Range,
) -> Vec<PermissionsStateStep> {
  let tcx = ctxt.tcx;
  let hir = tcx.hir();
  let body = &ctxt.body_with_facts.body;

  macro_rules! place_to_string {
    ($p:expr) => {
      $p.to_string(tcx, body)
        .unwrap_or_else(|| String::from("<var>"))
    };
  }

  perm_steps
    .into_iter()
    .filter_map(|(id, place_to_diffs)| {
      let span = hir.span(id);

      let range = span_to_range(span);
      let mut entries = place_to_diffs.into_iter().collect::<Vec<_>>();

      entries
        .sort_by_key(|(place, _)| (place.local.as_usize(), place.projection));
      let state = entries
        .into_iter()
        .map(|(place, diff)| {
          let s = place_to_string!(place);
          (s, diff)
        })
        .collect::<Vec<_>>();

      Some(PermissionsStateStep {
        location: range,
        state,
      })
    })
    .collect::<Vec<_>>()
}

#[allow(type_alias_bounds)]
type DomainAtHir<'tcx, A: Analysis<'tcx>> =
  HashMap<HirId, HashMap<Location, A::Domain>>;

struct HirPermDiffFlow<'a, 'tcx>
where
  'tcx: 'a,
{
  ctxt: &'a PermissionsCtxt<'a, 'tcx>,
  ir_mapper: &'a IRMapper<'a, 'tcx>,
  diff: HashMap<HirId, HashMap<Place<'tcx>, PermissionsDataDiff>>,
  step_barriers: Vec<Place<'tcx>>,
  visibility_scopes: Vec<HashMap<Place<'tcx>, PermissionsDataDiff>>,
}

macro_rules! filter_exec_commit {
  ($this:tt, $id:expr, $scopes:ident, $inner:expr) => {
    // For the current visibility we remove any place which is part of a barrier,
    // or if that place had *no change* (e.g. an empty difference).
    $scopes.retain(|place, df| {
      !(df.is_empty() || $this.step_barriers.iter().any(|p| p.local == place.local))
    });

    // For each previous visibility scope, remove the permission differences
    // that are visible at the current level. This ensures that the permission
    // change is attached to the most nested HirId possible.
    for scope in $this.visibility_scopes.iter_mut() {
      scope.retain(|place, _df| {
        // TODO: is it sufficient to just compare the place?
        // Try and come up with a counter example or a proof for why this is [un]safe.
        !$scopes.contains_key(place)
      });
    }
    $this.visibility_scopes.push($scopes);
    // Execute the inner block
    $inner;
    let $scopes = $this.visibility_scopes.pop().unwrap();
    $this.diff.insert($id, $scopes);
  }
}

macro_rules! in_visibility_scope_context {
  ($this:tt, $id:tt, $inner:block) => {
    // From the beginning to the point *after* the end location of `id`, `visibility_here`
    // represents all the places where a permissions change was visible. Specifically,
    // this is the difference in PermissionsDomain @ (after_point - before_point).
    let mut visibility_here =
      $this.ir_mapper.get_mir_locations($id, GatherDepth::Nested).map(|mir_order| {
        let (loc1, loc2) = mir_order.get_entry_exit_locations();
        let before_point = $this.ctxt.location_to_point(loc1);
        let after_point = $this.ctxt.location_to_point(loc2);
        let dmn_before = &$this.ctxt.permissions_domain_at_point(before_point);
        let dmn_after = &$this
          .ctxt
          .permissions_domain_after_point_effect(after_point)
          .unwrap_or_else(|| $this.ctxt.permissions_domain_at_point(after_point));
        dmn_before.diff(dmn_after)
      }).unwrap_or_else(|| HashMap::default());
    filter_exec_commit!($this, $id, visibility_here, $inner);
  };
}

macro_rules! with_seamless_branching {
  ($this:tt, $k:ident, $targets:expr, $branch_cnd:expr) => {
    let id = $branch_cnd.hir_id;
    // From the beginning to the point *after* the end location of `id`, `visibility_here`
    // represents all the places where a permissions change was visible. Specifically,
    // this is the difference in PermissionsDomain @ (after_point - before_point).
    let (mut visibility_here, after_branch_point) =
      $this.ir_mapper.get_mir_locations(id, GatherDepth::Nested).map(|mir_order| {
        let (loc1, loc2) = mir_order.get_entry_exit_locations();
        let before_point = $this.ctxt.location_to_point(loc1);
        let after_point = $this.ctxt.location_to_point(loc2);
        let dmn_before = &$this.ctxt.permissions_domain_at_point(before_point);
        let dmn_after = &$this.ctxt.permissions_domain_at_point(after_point);
        (dmn_before.diff(dmn_after), after_point)
      }).unwrap_or_else(|| {
        // FIXME
        panic!("this shouldn't ever ever ever happen")
      });
    filter_exec_commit!($this, id, visibility_here, {
      intravisit::walk_expr($this, $branch_cnd);
    });
    // - for each landing pad:
    // --- diff the domain starting from the conditions "after point"
    // --- rinse and repeat
    for target in $targets.iter() {
      let id = target.hir_id;
      // From the beginning to the point *after* the end location of `id`, `visibility_here`
      // represents all the places where a permissions change was visible. Specifically,
      // this is the difference in PermissionsDomain @ (after_point - before_point).
      let mut visibility_here =
        $this.ir_mapper.get_mir_locations(id, GatherDepth::Nested).map(|mir_order| {
          let (_, loc2) = mir_order.get_entry_exit_locations();
          let before_point = after_branch_point;
          let after_point = $this.ctxt.location_to_point(loc2);
          let dmn_before = &$this.ctxt.permissions_domain_at_point(before_point);
          let dmn_after = &$this
            .ctxt
            .permissions_domain_after_point_effect(after_point)
            .unwrap_or_else(|| $this.ctxt.permissions_domain_at_point(after_point));
          dmn_before.diff(dmn_after)
        }).unwrap_or_else(|| {
          // FIXME
          panic!("this shouldn't ever ever ever happen")
        });
      filter_exec_commit!($this, id, visibility_here, {
        intravisit::$k($this, target);
      });
    }
  }
}

// NOTE: in the following traversal a HIR Node (N) which should be wrapped in a
// visibility context, e.g. one which can have permission changes stuck to
// the end of it, will be denoted with the following notation: `⟦ N ⟧`
//
// That is for an initial permissions domain Γ and difference set S,
// the execution of N will produce some context Γ' where Γ' - Γ = S.
// Nested contexts `⟦ ⟦ N ⟧: S_1 ⟧: S_0` ensure that S_0 ∪ S_1 = ∅ .
// Meaning that permission changes are sucked into the most nested context.
//
// Additionally, there is a barrier context Δ, which forbids moving a `Variable`'s
// permission changes into a nested context.
// In the following statement we ensure that S_1 doesnot include
// any permission changes for the variable `s`.
// ```
// Γ; Δ ∪ {s}  :- ⟦ E ⟧: S_1 => Γ';Δ
// --------------------------------------
// Γ;Δ :-  ⟦ let s = E; ⟧ : S_0 => Γ';Δ
// ```
impl<'a, 'tcx: 'a> HirVisitor<'tcx> for HirPermDiffFlow<'a, 'tcx> {
  type NestedFilter = nested_filter::All;

  fn nested_visit_map(&mut self) -> Self::Map {
    self.ctxt.tcx.hir()
  }

  fn visit_body(&mut self, body: &'tcx hir::Body) {
    intravisit::walk_body(self, body);
  }

  // Statements:
  // Can have 4 different kinds: Local, Item, Expr, Semi.
  //
  // For all of these different variants, we want to attach a
  // visibility scope to the outside of the statement. This ensures,
  // that any statement that has a change in permissions will show
  // this as a step at the statement level.
  //
  // ```
  // ⟦ <stmt> ⟧
  // ```
  fn visit_stmt(&mut self, stmt: &'tcx hir::Stmt) {
    let id = stmt.hir_id;
    let hir = self.ctxt.tcx.hir();

    log::debug!("Visiting STMT: {}", hir.node_to_string(id));

    in_visibility_scope_context!(self, id, {
      intravisit::walk_stmt(self, stmt);
    });
  }

  // Blocks:
  // ```
  // ⟦ { s_1, s_2, ... s_n, ⟦ expr? ⟧ } ⟧
  // ```
  fn visit_block(&mut self, block: &'tcx hir::Block) {
    let id = block.hir_id;
    let hir = self.ctxt.tcx.hir();

    log::debug!("Visiting BLOCK: {}", hir.node_to_string(id));

    in_visibility_scope_context!(self, id, {
      for stmt in block.stmts.iter() {
        self.visit_stmt(stmt);
      }

      if let Some(expr) = block.expr {
        let id = expr.hir_id;
        in_visibility_scope_context!(self, id, {
          self.visit_expr(expr);
        });
      }
    });
  }

  // A Local of the form:
  //
  // ```
  // let symbol = Δ ∪ {symbol} expr else { ⟦ block ⟧ };
  // ```
  fn visit_local(&mut self, local: &'tcx hir::Local) {
    let id = local.hir_id;
    let hir = self.ctxt.tcx.hir();

    log::debug!("Visiting LOCAL: {}", hir.node_to_string(id));

    // NOTE: We add a "step barrier" for local assignments to make sure the permissions
    // for an assigned local don't come alive too early. Consider the following:
    // ```
    // let x = if <cnd> {
    //   <expr:1>
    // } else {
    //   <expr:2>
    // }
    // ```
    // Due to how the MIR is generated, the MIR statement assigning to `x`,
    // happens at the end of the block <expr:1>. This means, that if we don't
    // pull those permissions out to the assignment, they will only occur once
    // (specifically inside the "then branch") for the whole let.
    let mut added_barrier = false;

    if let Some(place) = self.ir_mapper.local_assigned_place(local) {
      self.step_barriers.push(place);
      added_barrier = true;
    }

    let pre_visibility_ctxt = self.visibility_scopes.clone();

    if let Some(expr) = local.init {
      self.visit_expr(expr);
    }

    if let Some(_block) = local.els {
      // TODO:
      self.visibility_scopes = pre_visibility_ctxt;
      unimplemented!("Locals with else branch");
    }

    if added_barrier {
      self.step_barriers.pop();
    }
  }

  fn visit_expr(&mut self, expr: &'tcx hir::Expr) {
    use hir::ExprKind as EK;
    let id = expr.hir_id;
    let hir = self.ctxt.tcx.hir();
    log::debug!("Visiting EXPR: {}", hir.node_to_string(id));

    match expr.kind {
      // ```
      // if ⟦ cnd ⟧  {
      //   ⟦ e_then ⟧
      // } else {
      //   ⟦ e_else? ⟧
      // }
      // ```
      EK::If(cnd, then, else_opt) => {
        in_visibility_scope_context!(self, id, {
          let landing_pads = [Some(then), else_opt]
            .into_iter()
            .flatten()
            .collect::<Vec<_>>();
          with_seamless_branching!(self, walk_expr, landing_pads, cnd);
        });
      }

      EK::Match(swtch, arms, _source) => {
        in_visibility_scope_context!(self, id, {
          with_seamless_branching!(self, walk_arm, arms, swtch);
        });
      }

      // - Variants I'd need to think more about.
      EK::Closure(..) => unimplemented!("CLOSURES"),

      _ => {
        intravisit::walk_expr(self, expr);
      }
    }
  }
}
