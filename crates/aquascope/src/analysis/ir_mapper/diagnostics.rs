//! IRMapping for Diagnostic capabilities.
use std::{
  cell::RefCell,
  fmt::{self, Display},
  iter,
  ops::Deref,
};

use rustc_hir::{
  self as hir,
  def::{DefKind, Res},
};
use rustc_middle::{
  mir::Local,
  ty::{
    self,
    print::RegionHighlightMode,
    subst::{GenericArgKind, SubstsRef},
    RegionVid, Ty,
  },
};
use rustc_span::{
  symbol::{kw, Symbol},
  Span, DUMMY_SP,
};

use super::IRMapper;
use crate::mir::utils::{BodyExt, ToRegionVid};

/// A name for a particular region used in emitting diagnostics. This name could be a generated
/// name like `'1`, a name used by the user like `'a`, or a name like `'static`.
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct RegionName {
  /// The name of the region (interned).
  pub(crate) name: Symbol,
  /// Where the region comes from.
  pub source: RegionNameSource,
}

/// Denotes the source of a region that is named by a `RegionName`. For example, a free region that
/// was named by the user would get `NamedFreeRegion` and `'static` lifetime would get `Static`.
/// This helps to print the right kinds of diagnostics.
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum RegionNameSource {
  /// A bound (not free) region that was substituted at the def site (not an HRTB).
  NamedEarlyBoundRegion(Span),
  /// A free region that the user has a name (`'a`) for.
  NamedFreeRegion(Span),
  /// The `'static` region.
  Static,
  /// The free region corresponding to the environment of a closure.
  SynthesizedFreeEnvRegion(Span, &'static str),
  /// The region corresponding to an argument.
  AnonRegionFromArgument(RegionNameHighlight),
  /// The region corresponding to a closure upvar.
  AnonRegionFromUpvar(Span, Symbol),
  /// The region corresponding to the return type of a closure.
  AnonRegionFromOutput(RegionNameHighlight, &'static str),
  /// The region from a type yielded by a generator.
  AnonRegionFromYieldTy(Span, String),
  /// An anonymous region from an async fn.
  AnonRegionFromAsyncFn(Span),
  /// An anonymous region from an impl self type or trait
  AnonRegionFromImplSignature(Span, &'static str),
}

/// Describes what to highlight to explain to the user that we're giving an anonymous region a
/// synthesized name, and how to highlight it.
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum RegionNameHighlight {
  /// The anonymous region corresponds to a reference that was found by traversing the type in the HIR.
  MatchedHirTy(Span),
  /// The anonymous region corresponds to a `'_` in the generics list of a struct/enum/union.
  MatchedAdtAndSegment(Span),
  /// The anonymous region corresponds to a region where the type annotation is completely missing
  /// from the code, e.g. in a closure arguments `|x| { ... }`, where `x` is a reference.
  CannotMatchHirTy(Span, String),
  /// The anonymous region corresponds to a region where the type annotation is completely missing
  /// from the code, and *even if* we print out the full name of the type, the region name won't
  /// be included. This currently occurs for opaque types like `impl Future`.
  Occluded(Span, String),
}

impl Display for RegionName {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.name)
  }
}

/// Describes where the free region you're looking for is located.
#[derive(Debug, Copy, Clone)]
pub enum RegionLocation {
  /// The region is located in the argument at `usize` index. Note,
  /// that this is the index for the HIR which doesn't include 0
  /// as the return location (as in the MIR).
  Arg(usize),
  Return,
}

/// Diagnostic extensions for the [`IRMapper`].
///
/// Note, most of the functions provided here were taken (at face value or lightly modified)
/// from: <https://github.com/rust-lang/rust/blob/master/compiler/rustc_borrowck/src/diagnostics/region_name.rs>
/// Hopefully, one day, diagnostic reporting in the borrowck module will be made public
/// and we won't need to maintain our own set of HACKS.
pub struct IRDiagnosticMapper<'a, 'tcx: 'a> {
  /// The underlying [`IRMapper`].
  ir_mapper: &'a IRMapper<'a, 'tcx>,

  /// The counter for generating new region names.
  next_region_name: RefCell<usize>,
}

impl<'a, 'tcx: 'a> Deref for IRDiagnosticMapper<'a, 'tcx> {
  type Target = IRMapper<'a, 'tcx>;
  fn deref(&self) -> &Self::Target {
    self.ir_mapper
  }
}

impl<'a, 'tcx: 'a> IRDiagnosticMapper<'a, 'tcx> {
  pub fn new(ir_mapper: &'a IRMapper<'a, 'tcx>) -> Self {
    IRDiagnosticMapper {
      ir_mapper,
      next_region_name: RefCell::new(1),
    }
  }

  pub(crate) fn mir_def_id(&self) -> hir::def_id::LocalDefId {
    self.body.source.def_id().expect_local()
  }

  pub(crate) fn mir_hir_id(&self) -> hir::HirId {
    self.tcx.hir().local_def_id_to_hir_id(self.mir_def_id())
  }

  // Self added functions which we will need minimal support for.

  /// Get the Span of the [`RegionVid`] inside the type for the given argument.
  pub(crate) fn get_fn_sig_region_highlight(
    &self,
    fr: RegionVid,
    index: RegionLocation,
  ) -> RegionNameHighlight {
    log::debug!("finding highlight for origin {fr:?} in arg: {index:?}");
    let (arg_ty, hir_index) = match index {
      RegionLocation::Arg(idx) => (
        self.body.local_decls[Local::from_usize(idx)].ty,
        // the HIR FnSig doesn't use 0 as the return Ty, so we
        // decrement from the index given for the MIR local.
        RegionLocation::Arg(idx - 1),
      ),
      RegionLocation::Return => (self.body.return_ty(), index),
    };

    log::debug!("ArgTy: {arg_ty:?}");

    self
      .get_hir_ty_for_highlighting(hir_index)
      .and_then(|arg_hir_ty| {
        self.highlight_if_we_can_match_hir_ty(fr, arg_ty, arg_hir_ty)
      })
      .unwrap_or_else(|| {
        // `highlight_if_we_cannot_match_hir_ty` needs to know the number we will give to
        // the anonymous region. If it succeeds, the `synthesize_region_name` call below
        // will increment the counter, "reserving" the number we just used.
        // FIXME: this was hacked together from the original rustc version
        // so we aren't keeping track of implicit region vids.
        let counter = *self.next_region_name.try_borrow().unwrap();
        let span = match index {
          RegionLocation::Arg(idx) => {
            self.body.local_decls[idx.into()].source_info.span
          }
          RegionLocation::Return => {
            self.body.local_decls[0usize.into()].source_info.span
          }
        };
        self.highlight_if_we_cannot_match_hir_ty(fr, arg_ty, span, counter)
      })
  }

  /// Generate a synthetic region named `'N`, where `N` is the next value of the counter. Then,
  /// increment the counter.
  ///
  /// This is _not_ idempotent. Call `give_region_a_name` when possible.
  pub(crate) fn synthesize_region_name(&self) -> Symbol {
    let c = self.next_region_name.replace_with(|counter| *counter + 1);
    Symbol::intern(&format!("'{c:?}"))
  }

  /// "Supposed to" -->
  /// Converts a region inference variable into a `ty::Region` that
  /// we can use for error reporting. If `r` is universally bound,
  /// then we use the name that we have on record for it. If `r` is
  /// existentially bound, then we check its inferred value and try
  /// to find a good name from that. Returns `None` if we can't find
  /// one (e.g., this is just some random part of the CFG).
  pub(super) fn to_error_region(
    &self,
    r: RegionVid,
  ) -> Option<ty::Region<'tcx>> {
    self
      .body
      .regions_in_args()
      .chain(self.body.regions_in_return())
      .find(|fr| fr.to_region_vid() == r)

    // self.to_error_region_vid(r).and_then(|r| self.regioncx.region_definition(r).external_name)
  }

  // /// Maps from an internal MIR region vid to something that we can
  // /// report to the user. In some cases, the region vids will map
  // /// directly to lifetimes that the user has a name for (e.g.,
  // /// `'static`). But frequently they will not, in which case we
  // /// have to find some way to identify the lifetime to the user. To
  // /// that end, this function takes a "diagnostic" so that it can
  // /// create auxiliary notes as needed.
  // ///
  // /// The names are memoized, so this is both cheap to recompute and idempotent.
  // ///
  // /// Example (function arguments):
  // ///
  // /// Suppose we are trying to give a name to the lifetime of the
  // /// reference `x`:
  // ///
  // /// ```ignore (pseudo-rust)
  // /// fn foo(x: &u32) { .. }
  // /// ```
  // ///
  // /// This function would create a label like this:
  // ///
  // /// ```text
  // ///  | fn foo(x: &u32) { .. }
  // ///           ------- fully elaborated type of `x` is `&'1 u32`
  // /// ```
  // ///
  // /// and then return the name `'1` for us to use.
  // pub(crate) fn give_region_a_name(&self, fr: RegionVid) -> Option<RegionName> {
  //   log::debug!(
  //     "give_region_a_name(fr={:?}, counter={:?})",
  //     fr,
  //     self.next_region_name.try_borrow().unwrap()
  //   );

  //   // assert!(self.regioncx.universal_regions().is_universal_region(fr));

  //   if let Some(value) = self.region_names.try_borrow_mut().unwrap().get(&fr) {
  //     return Some(value.clone());
  //   }

  //   let value = self
  //     .give_name_from_error_region(fr)
  //     .or_else(|| self.give_name_if_anonymous_region_appears_in_arguments(fr))
  //     .or_else(|| self.give_name_if_anonymous_region_appears_in_upvars(fr))
  //     .or_else(|| self.give_name_if_anonymous_region_appears_in_output(fr))
  //     .or_else(|| self.give_name_if_anonymous_region_appears_in_yield_ty(fr))
  //     .or_else(|| {
  //       self.give_name_if_anonymous_region_appears_in_impl_signature(fr)
  //     })
  //     .or_else(|| {
  //       self
  //         .give_name_if_anonymous_region_appears_in_arg_position_impl_trait(fr)
  //     });

  //   if let Some(value) = &value {
  //     self
  //       .region_names
  //       .try_borrow_mut()
  //       .unwrap()
  //       .insert(fr, value.clone());
  //   }

  //   log::debug!("give_region_a_name: gave name {:?}", value);
  //   value
  // }

  /// Checks for the case where `fr` maps to something that the
  /// *user* has a name for. In that case, we'll be able to map
  /// `fr` to a `Region<'tcx>`, and that region will be one of
  /// named variants.
  pub fn give_name_from_error_region(
    &self,
    fr: RegionVid,
  ) -> Option<RegionName> {
    let error_region = self.to_error_region(fr)?;

    let tcx = self.tcx;

    log::debug!("give_region_a_name: error_region = {:?}", error_region);
    match *error_region {
      ty::ReEarlyBound(ebr) => ebr.has_name().then(|| {
        let span = tcx.hir().span_if_local(ebr.def_id).unwrap_or(DUMMY_SP);
        RegionName {
          name: ebr.name,
          source: RegionNameSource::NamedEarlyBoundRegion(span),
        }
      }),

      ty::ReStatic => Some(RegionName {
        name: kw::StaticLifetime,
        source: RegionNameSource::Static,
      }),

      ty::ReFree(free_region) => match free_region.bound_region {
        ty::BoundRegionKind::BrNamed(region_def_id, name) => {
          // Get the span to point to, even if we don't use the name.
          let span = tcx.hir().span_if_local(region_def_id).unwrap_or(DUMMY_SP);
          log::debug!(
            "bound region named: {:?}, is_named: {:?}",
            name,
            free_region.bound_region.is_named()
          );

          if free_region.bound_region.is_named() {
            // A named region that is actually named.
            Some(RegionName {
              name,
              source: RegionNameSource::NamedFreeRegion(span),
            })
          } else if let hir::IsAsync::Async =
            tcx.asyncness(self.mir_hir_id().owner)
          {
            // If we spuriously thought that the region is named, we should let the
            // system generate a true name for error messages. Currently this can
            // happen if we have an elided name in an async fn for example: the
            // compiler will generate a region named `'_`, but reporting such a name is
            // not actually useful, so we synthesize a name for it instead.
            let name = self.synthesize_region_name();
            Some(RegionName {
              name,
              source: RegionNameSource::AnonRegionFromAsyncFn(span),
            })
          } else {
            None
          }
        }

        // ty::BoundRegionKind::BrEnv => {
        //     let def_ty = self.regioncx.universal_regions().defining_ty;

        //     let DefiningTy::Closure(_, substs) = def_ty else {
        //                   // Can't have BrEnv in functions, constants or generators.
        //                   bug!("BrEnv outside of closure.");
        //               };
        //     let hir::ExprKind::Closure(&hir::Closure { fn_decl_span, .. })
        //                   = tcx.hir().expect_expr(self.mir_hir_id()).kind
        //               else {
        //                   bug!("Closure is not defined by a closure expr");
        //               };
        //     let region_name = self.synthesize_region_name();

        //     let closure_kind_ty = substs.as_closure().kind_ty();
        //     let note = match closure_kind_ty.to_opt_closure_kind() {
        //       Some(ty::ClosureKind::Fn) => {
        //         "closure implements `Fn`, so references to captured variables \
        //                        can't escape the closure"
        //       }
        //       Some(ty::ClosureKind::FnMut) => {
        //         "closure implements `FnMut`, so references to captured variables \
        //                        can't escape the closure"
        //       }
        //       Some(ty::ClosureKind::FnOnce) => {
        //         bug!("BrEnv in a `FnOnce` closure");
        //       }
        //       None => bug!("Closure kind not inferred in borrow check"),
        //     };

        //     Some(RegionName {
        //       name: region_name,
        //       source: RegionNameSource::SynthesizedFreeEnvRegion(
        //         fn_decl_span,
        //         note,
        //       ),
        //     })
        //   }
        //   ty::BoundRegionKind::BrAnon(..) => None,
        _ => None,
      },

      // ty::ReLateBound(..)
      // | ty::ReVar(..)
      // | ty::RePlaceholder(..)
      // | ty::ReErased
      // | ty::ReError(_) => None,
      //
      // FIXME: these other cases we will not be able to handle for now!
      _ => None,
    }
  }

  // /// Finds an argument that contains `fr` and label it with a fully
  // /// elaborated type, returning something like `'1`. Result looks
  // /// like:
  // ///
  // /// ```text
  // ///  | fn foo(x: &u32) { .. }
  // ///           ------- fully elaborated type of `x` is `&'1 u32`
  // /// ```
  // fn give_name_if_anonymous_region_appears_in_arguments(
  //   &self,
  //   fr: RegionVid,
  // ) -> Option<RegionName> {
  //   let implicit_inputs = self
  //     .regioncx
  //     .universal_regions()
  //     .defining_ty
  //     .implicit_inputs();
  //   let argument_index =
  //     self.regioncx.get_argument_index_for_region(self.tcx, fr)?;

  //   let arg_ty = self.regioncx.universal_regions().unnormalized_input_tys
  //     [implicit_inputs + argument_index];
  //   let (_, span) = self.regioncx.get_argument_name_and_span_for_region(
  //     &self.body,
  //     &self.local_names,
  //     argument_index,
  //   );

  //   let highlight = self
  //     .get_argument_hir_ty_for_highlighting(argument_index)
  //     .and_then(|arg_hir_ty| {
  //       self.highlight_if_we_can_match_hir_ty(fr, arg_ty, arg_hir_ty)
  //     })
  //     .unwrap_or_else(|| {
  //       // `highlight_if_we_cannot_match_hir_ty` needs to know the number we will give to
  //       // the anonymous region. If it succeeds, the `synthesize_region_name` call below
  //       // will increment the counter, "reserving" the number we just used.
  //       let counter = *self.next_region_name.try_borrow().unwrap();
  //       self.highlight_if_we_cannot_match_hir_ty(fr, arg_ty, span, counter)
  //     });

  //   Some(RegionName {
  //     name: self.synthesize_region_name(),
  //     source: RegionNameSource::AnonRegionFromArgument(highlight),
  //   })
  // }

  fn get_hir_ty_for_highlighting(
    &self,
    index: RegionLocation,
  ) -> Option<&hir::Ty<'tcx>> {
    log::debug!("finding hir type for argument {index:?}");
    let fn_decl = self.tcx.hir().fn_decl_by_hir_id(self.mir_hir_id())?;
    let hir_ty: &hir::Ty<'_> = match index {
      RegionLocation::Arg(idx) => fn_decl.inputs.get(idx)?,
      RegionLocation::Return => match fn_decl.output {
        hir::FnRetTy::DefaultReturn(..) => {
          unreachable!("region specified in default return type")
        }
        hir::FnRetTy::Return(ty) => ty,
      },
    };

    match hir_ty.kind {
      // This indicates a variable with no type annotation, like
      // `|x|`... in that case, we can't highlight the type but
      // must highlight the variable.
      // NOTE(eddyb) this is handled in/by the sole caller
      // (`give_name_if_anonymous_region_appears_in_arguments`).
      hir::TyKind::Infer => None,

      _ => Some(hir_ty),
    }
  }

  /// Attempts to highlight the specific part of a type in an argument
  /// that has no type annotation.
  /// For example, we might produce an annotation like this:
  ///
  /// ```text
  ///  |     foo(|a, b| b)
  ///  |          -  -
  ///  |          |  |
  ///  |          |  has type `&'1 u32`
  ///  |          has type `&'2 u32`
  /// ```
  fn highlight_if_we_cannot_match_hir_ty(
    &self,
    needle_fr: RegionVid,
    _ty: Ty<'tcx>,
    span: Span,
    counter: usize,
  ) -> RegionNameHighlight {
    let mut highlight = RegionHighlightMode::new(self.tcx);
    highlight.highlighting_region_vid(needle_fr, counter);

    let type_name = String::from("<anon type>");
    // let type_name = self
    //   .extract_inference_diagnostics_data(ty.into(), Some(highlight))
    //   .name;

    log::debug!(
      "highlight_if_we_cannot_match_hir_ty: type_name={:?} needle_fr={:?}",
      type_name,
      needle_fr
    );
    if type_name.contains(&format!("'{counter}")) {
      // Only add a label if we can confirm that a region was labelled.
      RegionNameHighlight::CannotMatchHirTy(span, type_name)
    } else {
      RegionNameHighlight::Occluded(span, type_name)
    }
  }

  /// Attempts to highlight the specific part of a type annotation
  /// that contains the anonymous reference we want to give a name
  /// to. For example, we might produce an annotation like this:
  ///
  /// ```text
  ///  | fn a<T>(items: &[T]) -> Box<dyn Iterator<Item = &T>> {
  ///  |                - let's call the lifetime of this reference `'1`
  /// ```
  ///
  /// the way this works is that we match up `ty`, which is
  /// a `Ty<'tcx>` (the internal form of the type) with
  /// `hir_ty`, a `hir::Ty` (the syntax of the type
  /// annotation). We are descending through the types stepwise,
  /// looking in to find the region `needle_fr` in the internal
  /// type. Once we find that, we can use the span of the `hir::Ty`
  /// to add the highlight.
  ///
  /// This is a somewhat imperfect process, so along the way we also
  /// keep track of the **closest** type we've found. If we fail to
  /// find the exact `&` or `'_` to highlight, then we may fall back
  /// to highlighting that closest type instead.
  fn highlight_if_we_can_match_hir_ty(
    &self,
    needle_fr: RegionVid,
    ty: Ty<'tcx>,
    hir_ty: &hir::Ty<'_>,
  ) -> Option<RegionNameHighlight> {
    let search_stack: &mut Vec<(Ty<'tcx>, &hir::Ty<'_>)> =
      &mut vec![(ty, hir_ty)];

    while let Some((ty, hir_ty)) = search_stack.pop() {
      log::debug!("SEARCHING WITH\nTY:{ty:#?}\nHIRTY:{hir_ty:#?}");
      match (ty.kind(), &hir_ty.kind) {
        // Check if the `ty` is `&'X ..` where `'X`
        // is the region we are looking for -- if so, and we have a `&T`
        // on the RHS, then we want to highlight the `&` like so:
        //
        //     &
        //     - let's call the lifetime of this reference `'1`
        (
          ty::Ref(region, referent_ty, _),
          hir::TyKind::Rptr(_lifetime, referent_hir_ty),
        ) => {
          if region.to_region_vid() == needle_fr {
            // Just grab the first character, the `&`.
            let source_map = self.tcx.sess.source_map();
            let ampersand_span = source_map.start_point(hir_ty.span);

            return Some(RegionNameHighlight::MatchedHirTy(ampersand_span));
          }

          // Otherwise, let's descend into the referent types.
          search_stack.push((*referent_ty, referent_hir_ty.ty));
        }

        // Match up something like `Foo<'1>`
        (
          ty::Adt(_adt_def, substs),
          hir::TyKind::Path(hir::QPath::Resolved(None, path)),
        ) => {
          match path.res {
            // Type parameters of the type alias have no reason to
            // be the same as those of the ADT.
            // FIXME: We should be able to do something similar to
            // match_adt_and_segment in this case.
            Res::Def(DefKind::TyAlias, _) => (),
            _ => {
              if let Some(last_segment) = path.segments.last() {
                if let Some(highlight) = self.match_adt_and_segment(
                  substs,
                  needle_fr,
                  last_segment,
                  search_stack,
                ) {
                  return Some(highlight);
                }
              }
            }
          }
        }

        // The following cases don't have lifetimes, so we
        // just worry about trying to match up the rustc type
        // with the HIR types:
        (&ty::Tuple(elem_tys), hir::TyKind::Tup(elem_hir_tys)) => {
          search_stack.extend(iter::zip(elem_tys, *elem_hir_tys));
        }

        (ty::Slice(elem_ty), hir::TyKind::Slice(elem_hir_ty))
        | (ty::Array(elem_ty, _), hir::TyKind::Array(elem_hir_ty, _)) => {
          search_stack.push((*elem_ty, elem_hir_ty));
        }

        (ty::RawPtr(mut_ty), hir::TyKind::Ptr(mut_hir_ty)) => {
          search_stack.push((mut_ty.ty, mut_hir_ty.ty));
        }

        _ => {
          // FIXME there are other cases that we could trace
        }
      }
    }

    None
  }

  /// We've found an enum/struct/union type with the substitutions
  /// `substs` and -- in the HIR -- a path type with the final
  /// segment `last_segment`. Try to find a `'_` to highlight in
  /// the generic args (or, if not, to produce new zipped pairs of
  /// types+hir to search through).
  fn match_adt_and_segment<'hir>(
    &self,
    substs: SubstsRef<'tcx>,
    needle_fr: RegionVid,
    last_segment: &'hir hir::PathSegment<'hir>,
    search_stack: &mut Vec<(Ty<'tcx>, &'hir hir::Ty<'hir>)>,
  ) -> Option<RegionNameHighlight> {
    // Did the user give explicit arguments? (e.g., `Foo<..>`)
    let args = last_segment.args.as_ref()?;
    let lifetime = self.try_match_adt_and_generic_args(
      substs,
      needle_fr,
      args,
      search_stack,
    )?;
    if lifetime.is_anonymous() {
      None
    } else {
      Some(RegionNameHighlight::MatchedAdtAndSegment(
        lifetime.ident.span,
      ))
    }
  }

  /// We've found an enum/struct/union type with the substitutions
  /// `substs` and -- in the HIR -- a path with the generic
  /// arguments `args`. If `needle_fr` appears in the args, return
  /// the `hir::Lifetime` that corresponds to it. If not, push onto
  /// `search_stack` the types+hir to search through.
  fn try_match_adt_and_generic_args<'hir>(
    &self,
    substs: SubstsRef<'tcx>,
    needle_fr: RegionVid,
    args: &'hir hir::GenericArgs<'hir>,
    search_stack: &mut Vec<(Ty<'tcx>, &'hir hir::Ty<'hir>)>,
  ) -> Option<&'hir hir::Lifetime> {
    for (kind, hir_arg) in iter::zip(substs, args.args) {
      match (kind.unpack(), hir_arg) {
        (GenericArgKind::Lifetime(r), hir::GenericArg::Lifetime(lt)) => {
          if r.to_region_vid() == needle_fr {
            return Some(lt);
          }
        }

        (GenericArgKind::Type(ty), hir::GenericArg::Type(hir_ty)) => {
          search_stack.push((ty, hir_ty));
        }

        (GenericArgKind::Const(_ct), hir::GenericArg::Const(_hir_ct)) => {
          // Lifetimes cannot be found in consts, so we don't need
          // to search anything here.
        }

        (
          GenericArgKind::Lifetime(_)
          | GenericArgKind::Type(_)
          | GenericArgKind::Const(_),
          _,
        ) => {
          // HIR lowering sometimes doesn't catch this in erroneous
          // programs, so we need to use delay_span_bug here. See #82126.
          self.tcx.sess.delay_span_bug(
            hir_arg.span(),
            &format!(
              "unmatched subst and hir arg: found {kind:?} vs {hir_arg:?}"
            ),
          );
        }
      }
    }

    None
  }

  // /// Finds a closure upvar that contains `fr` and label it with a
  // /// fully elaborated type, returning something like `'1`. Result
  // /// looks like:
  // ///
  // /// ```text
  // ///  | let x = Some(&22);
  // ///        - fully elaborated type of `x` is `Option<&'1 u32>`
  // /// ```
  // fn give_name_if_anonymous_region_appears_in_upvars(
  //   &self,
  //   fr: RegionVid,
  // ) -> Option<RegionName> {
  //   let upvar_index = self.regioncx.get_upvar_index_for_region(self.tcx, fr)?;
  //   let (upvar_name, upvar_span) = self
  //     .regioncx
  //     .get_upvar_name_and_span_for_region(self.tcx, &self.upvars, upvar_index);
  //   let region_name = self.synthesize_region_name();

  //   Some(RegionName {
  //     name: region_name,
  //     source: RegionNameSource::AnonRegionFromUpvar(upvar_span, upvar_name),
  //   })
  // }

  // /// Checks for arguments appearing in the (closure) return type. It
  // /// must be a closure since, in a free fn, such an argument would
  // /// have to either also appear in an argument (if using elision)
  // /// or be early bound (named, not in argument).
  // fn give_name_if_anonymous_region_appears_in_output(
  //   &self,
  //   fr: RegionVid,
  // ) -> Option<RegionName> {
  //   let tcx = self.tcx;
  //   let hir = tcx.hir();

  //   let return_ty = self.regioncx.universal_regions().unnormalized_output_ty;
  //   log::debug!(
  //     "give_name_if_anonymous_region_appears_in_output: return_ty = {:?}",
  //     return_ty
  //   );
  //   if !tcx.any_free_region_meets(&return_ty, |r| r.to_region_vid() == fr) {
  //     return None;
  //   }

  //   let mir_hir_id = self.mir_hir_id();

  //   let (return_span, mir_description, hir_ty) = match hir.get(mir_hir_id) {
  //     hir::Node::Expr(hir::Expr {
  //       kind:
  //         hir::ExprKind::Closure(&hir::Closure {
  //           fn_decl,
  //           body,
  //           fn_decl_span,
  //           ..
  //         }),
  //       ..
  //     }) => {
  //       let (mut span, mut hir_ty) = match fn_decl.output {
  //         hir::FnRetTy::DefaultReturn(_) => {
  //           (tcx.sess.source_map().end_point(fn_decl_span), None)
  //         }
  //         hir::FnRetTy::Return(hir_ty) => (fn_decl.output.span(), Some(hir_ty)),
  //       };
  //       let mir_description = match hir.body(body).generator_kind {
  //         Some(hir::GeneratorKind::Async(gen)) => match gen {
  //           hir::AsyncGeneratorKind::Block => " of async block",
  //           hir::AsyncGeneratorKind::Closure => " of async closure",
  //           hir::AsyncGeneratorKind::Fn => {
  //             let parent_item =
  //               hir.get_by_def_id(hir.get_parent_item(mir_hir_id).def_id);
  //             let output = &parent_item
  //               .fn_decl()
  //               .expect("generator lowered from async fn should be in fn")
  //               .output;
  //             span = output.span();
  //             if let hir::FnRetTy::Return(ret) = output {
  //               hir_ty = Some(self.get_future_inner_return_ty(*ret));
  //             }
  //             " of async function"
  //           }
  //         },
  //         Some(hir::GeneratorKind::Gen) => " of generator",
  //         None => " of closure",
  //       };
  //       (span, mir_description, hir_ty)
  //     }
  //     node => match node.fn_decl() {
  //       Some(fn_decl) => {
  //         let hir_ty = match fn_decl.output {
  //           hir::FnRetTy::DefaultReturn(_) => None,
  //           hir::FnRetTy::Return(ty) => Some(ty),
  //         };
  //         (fn_decl.output.span(), "", hir_ty)
  //       }
  //       None => (self.body.span, "", None),
  //     },
  //   };

  //   let highlight = hir_ty
  //     .and_then(|hir_ty| {
  //       self.highlight_if_we_can_match_hir_ty(fr, return_ty, hir_ty)
  //     })
  //     .unwrap_or_else(|| {
  //       // `highlight_if_we_cannot_match_hir_ty` needs to know the number we will give to
  //       // the anonymous region. If it succeeds, the `synthesize_region_name` call below
  //       // will increment the counter, "reserving" the number we just used.
  //       let counter = *self.next_region_name.try_borrow().unwrap();
  //       self.highlight_if_we_cannot_match_hir_ty(
  //         fr,
  //         return_ty,
  //         return_span,
  //         counter,
  //       )
  //     });

  //   Some(RegionName {
  //     name: self.synthesize_region_name(),
  //     source: RegionNameSource::AnonRegionFromOutput(
  //       highlight,
  //       mir_description,
  //     ),
  //   })
  // }

  // /// From the [`hir::Ty`] of an async function's lowered return type,
  // /// retrieve the `hir::Ty` representing the type the user originally wrote.
  // ///
  // /// e.g. given the function:
  // ///
  // /// ```
  // /// async fn foo() -> i32 { 2 }
  // /// ```
  // ///
  // /// this function, given the lowered return type of `foo`, an [`OpaqueDef`] that implements `Future<Output=i32>`,
  // /// returns the `i32`.
  // ///
  // /// [`OpaqueDef`]: hir::TyKind::OpaqueDef
  // fn get_future_inner_return_ty(
  //   &self,
  //   hir_ty: &'tcx hir::Ty<'tcx>,
  // ) -> &'tcx hir::Ty<'tcx> {
  //   let hir = self.tcx.hir();

  //   let hir::TyKind::OpaqueDef(id, _, _) = hir_ty.kind else {
  //           span_bug!(
  //               hir_ty.span,
  //               "lowered return type of async fn is not OpaqueDef: {:?}",
  //               hir_ty
  //           );
  //       };
  //   let opaque_ty = hir.item(id);
  //   if let hir::ItemKind::OpaqueTy(hir::OpaqueTy {
  //     bounds:
  //       [hir::GenericBound::LangItemTrait(
  //         hir::LangItem::Future,
  //         _,
  //         _,
  //         hir::GenericArgs {
  //           bindings:
  //             [hir::TypeBinding {
  //               ident:
  //                 Ident {
  //                   name: sym::Output, ..
  //                 },
  //               kind:
  //                 hir::TypeBindingKind::Equality {
  //                   term: hir::Term::Ty(ty),
  //                 },
  //               ..
  //             }],
  //           ..
  //         },
  //       )],
  //     ..
  //   }) = opaque_ty.kind
  //   {
  //     ty
  //   } else {
  //     span_bug!(
  //               hir_ty.span,
  //               "bounds from lowered return type of async fn did not match expected format: {opaque_ty:?}",
  //           );
  //   }
  // }

  // fn give_name_if_anonymous_region_appears_in_yield_ty(
  //   &self,
  //   fr: RegionVid,
  // ) -> Option<RegionName> {
  //   // Note: generators from `async fn` yield `()`, so we don't have to
  //   // worry about them here.
  //   let yield_ty = self.regioncx.universal_regions().yield_ty?;
  //   log::debug!(
  //     "give_name_if_anonymous_region_appears_in_yield_ty: yield_ty = {:?}",
  //     yield_ty
  //   );

  //   let tcx = self.tcx;

  //   if !tcx.any_free_region_meets(&yield_ty, |r| r.to_region_vid() == fr) {
  //     return None;
  //   }

  //   let mut highlight = RegionHighlightMode::new(tcx);
  //   highlight.highlighting_region_vid(
  //     fr,
  //     *self.next_region_name.try_borrow().unwrap(),
  //   );
  //   let type_name = self
  //     .extract_inference_diagnostics_data(yield_ty.into(), Some(highlight))
  //     .name;

  //   let yield_span = match tcx.hir().get(self.mir_hir_id()) {
  //     hir::Node::Expr(hir::Expr {
  //       kind: hir::ExprKind::Closure(&hir::Closure { fn_decl_span, .. }),
  //       ..
  //     }) => tcx.sess.source_map().end_point(fn_decl_span),
  //     _ => self.body.span,
  //   };

  //   log::debug!(
  //     "give_name_if_anonymous_region_appears_in_yield_ty: \
  //            type_name = {:?}, yield_span = {:?}",
  //     yield_span,
  //     type_name,
  //   );

  //   Some(RegionName {
  //     name: self.synthesize_region_name(),
  //     source: RegionNameSource::AnonRegionFromYieldTy(yield_span, type_name),
  //   })
  // }

  // fn give_name_if_anonymous_region_appears_in_impl_signature(
  //   &self,
  //   fr: RegionVid,
  // ) -> Option<RegionName> {
  //   let ty::ReEarlyBound(region) = *self.to_error_region(fr)? else {
  //           return None;
  //       };
  //   if region.has_name() {
  //     return None;
  //   };

  //   let tcx = self.tcx;
  //   let region_parent = tcx.parent(region.def_id);
  //   let DefKind::Impl { .. } = tcx.def_kind(region_parent) else {
  //           return None;
  //       };

  //   let found = tcx.any_free_region_meets(
  //     &tcx.type_of(region_parent).subst_identity(),
  //     |r| *r == ty::ReEarlyBound(region),
  //   );

  //   Some(RegionName {
  //     name: self.synthesize_region_name(),
  //     source: RegionNameSource::AnonRegionFromImplSignature(
  //       tcx.def_span(region.def_id),
  //       // FIXME(compiler-errors): Does this ever actually show up
  //       // anywhere other than the self type? I couldn't create an
  //       // example of a `'_` in the impl's trait being referenceable.
  //       if found { "self type" } else { "header" },
  //     ),
  //   })
  // }

  // fn give_name_if_anonymous_region_appears_in_arg_position_impl_trait(
  //   &self,
  //   fr: RegionVid,
  // ) -> Option<RegionName> {
  //   let ty::ReEarlyBound(region) = *self.to_error_region(fr)? else {
  //           return None;
  //       };
  //   if region.has_name() {
  //     return None;
  //   };

  //   let predicates = self
  //     .tcx
  //     .predicates_of(self.body.source.def_id())
  //     .instantiate_identity(self.tcx)
  //     .predicates;

  //   if let Some(upvar_index) = self
  //     .regioncx
  //     .universal_regions()
  //     .defining_ty
  //     .upvar_tys()
  //     .position(|ty| self.any_param_predicate_mentions(&predicates, ty, region))
  //   {
  //     let (upvar_name, upvar_span) =
  //       self.regioncx.get_upvar_name_and_span_for_region(
  //         self.tcx,
  //         &self.upvars,
  //         upvar_index,
  //       );
  //     let region_name = self.synthesize_region_name();

  //     Some(RegionName {
  //       name: region_name,
  //       source: RegionNameSource::AnonRegionFromUpvar(upvar_span, upvar_name),
  //     })
  //   } else if let Some(arg_index) = self
  //     .regioncx
  //     .universal_regions()
  //     .unnormalized_input_tys
  //     .iter()
  //     .position(|ty| {
  //       self.any_param_predicate_mentions(&predicates, *ty, region)
  //     })
  //   {
  //     let (arg_name, arg_span) =
  //       self.regioncx.get_argument_name_and_span_for_region(
  //         self.body,
  //         &self.local_names,
  //         arg_index,
  //       );
  //     let region_name = self.synthesize_region_name();

  //     Some(RegionName {
  //       name: region_name,
  //       source: RegionNameSource::AnonRegionFromArgument(
  //         RegionNameHighlight::CannotMatchHirTy(
  //           arg_span,
  //           arg_name?.to_string(),
  //         ),
  //       ),
  //     })
  //   } else {
  //     None
  //   }
  // }

  // fn any_param_predicate_mentions(
  //   &self,
  //   predicates: &[ty::Predicate<'tcx>],
  //   ty: Ty<'tcx>,
  //   region: ty::EarlyBoundRegion,
  // ) -> bool {
  //   let tcx = self.tcx;
  //   ty.walk().any(|arg| {
  //           if let ty::GenericArgKind::Type(ty) = arg.unpack()
  //               && let ty::Param(_) = ty.kind()
  //           {
  //               predicates.iter().any(|pred| {
  //                   match pred.kind().skip_binder() {
  //                       ty::PredicateKind::Clause(ty::Clause::Trait(data)) if data.self_ty() == ty => {}
  //                       ty::PredicateKind::Clause(ty::Clause::Projection(data)) if data.projection_ty.self_ty() == ty => {}
  //                       _ => return false,
  //                   }
  //                   tcx.any_free_region_meets(pred, |r| {
  //                       *r == ty::ReEarlyBound(region)
  //                   })
  //               })
  //           } else {
  //               false
  //           }
  //       })
  // }
}
