use anyhow::Result;
use aquascope::{
    analysis::{self, PermissionsInfo},
    Range,
};
use flowistry::{
    mir::{
        borrowck_facts::{self, get_body_with_borrowck_facts, NO_SIMPLIFY},
        utils::SpanExt,
    },
    source_map::{self, find_bodies},
};
use rustc_hir::BodyId;
use rustc_middle::ty::TyCtxt;
use serde::Serialize;
use ts_rs::TS;

use crate::plugin::AquascopeResult;

#[derive(Debug, Clone, Serialize, TS)]
#[ts(export)]
pub struct PermissionsOutput(Vec<PermissionsInfo>);

impl super::plugin::Join for PermissionsOutput {
    fn join(self, other: Self) -> Self {
        PermissionsOutput(self.0.join(other.0))
    }
}

// struct Callbacks {
//     filename: String,
//     output: Vec<Vec<PermissionsInfo>>,
// }

// impl rustc_driver::Callbacks for Callbacks {
//     fn config(&mut self, config: &mut rustc_interface::Config) {
//         NO_SIMPLIFY.store(true, std::sync::atomic::Ordering::SeqCst);
//         config.override_queries = Some(borrowck_facts::override_queries);
//     }

//     fn after_parsing<'tcx>(
//         &mut self,
//         _compiler: &rustc_interface::interface::Compiler,
//         queries: &'tcx rustc_interface::Queries<'tcx>,
//     ) -> rustc_driver::Compilation {
//         queries.global_ctxt().unwrap().take().enter(|tcx| {
//             find_bodies(tcx).into_iter().for_each(|(_, body_id)| {});
//         });

//         log::debug!("Returning ...");

//         rustc_driver::Compilation::Stop
//     }
// }

pub fn permissions(tcx: TyCtxt, body_id: BodyId) -> Result<PermissionsOutput> {
    let def_id = tcx.hir().body_owner_def_id(body_id);
    let body_with_facts = get_body_with_borrowck_facts(tcx, def_id);
    let permissions_ctxt = &analysis::compute_permissions(tcx, body_id, body_with_facts);
    let source_map = tcx.sess.source_map();
    let call_infos = analysis::pair_permissions_to_calls(permissions_ctxt, |span| {
        source_map::Range::from_span(span, source_map)
            .ok()
            .unwrap_or_default()
            .into()
    });

    log::debug!("Analysis returning {:?}", call_infos);

    Ok(PermissionsOutput(call_infos))
}

// pub fn permissions(
//   args: &[String],
//   filename: String,
// ) -> AquascopeResult<PermissionsOutput> {
//   let mut callbacks = Callbacks {
//     filename,
//     output: Vec::default(),
//   };
//   crate::plugin::run_with_callbacks(args, &mut callbacks)?;
//   Ok(PermissionsOutput(
//     callbacks.output.into_iter().flatten().collect(),
//   ))
// }
