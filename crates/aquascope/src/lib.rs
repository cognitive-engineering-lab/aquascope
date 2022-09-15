#![feature(
  rustc_private,
  box_patterns,
  associated_type_defaults,
  min_specialization,
  type_alias_impl_trait,
  generic_associated_types,
  crate_visibility_modifier,
  trait_alias,
  let_else
)]
#![allow(clippy::needless_lifetimes)]

extern crate either;
extern crate polonius_engine;
extern crate rustc_borrowck;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_graphviz;
extern crate rustc_hir;
extern crate rustc_hir_pretty;
extern crate rustc_index;
extern crate rustc_infer;
extern crate rustc_interface;
extern crate rustc_macros;
extern crate rustc_middle;
extern crate rustc_mir_dataflow;
extern crate rustc_mir_transform;
extern crate rustc_serialize;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;
extern crate rustc_trait_selection;
extern crate smallvec;

pub mod analysis;
pub mod cached;
pub mod indexed;
pub mod mir;
pub mod source_map;

// use rustc_plugin::{RustcPlugin, RustcPluginArgs};

// pub mod mir;

// use crate::mir::borrowck_facts;

// pub struct AquascopePlugin;
// impl RustcPlugin for AquascopePlugin {
//     type Args = ();

//     fn bin_name() -> String {
//         "aquascope-driver".to_owned()
//     }

//     fn args(&self, _target_dir: &rustc_plugin::Utf8Path) -> RustcPluginArgs<Self::Args> {
//         RustcPluginArgs {
//             args: (),
//             file: None,
//             flags: None,
//         }
//     }

//     fn run(
//         self,
//         compiler_args: Vec<String>,
//         _plugin_args: Self::Args,
//     ) -> rustc_interface::interface::Result<()> {
//         rustc_driver::RunCompiler::new(&compiler_args, &mut Callbacks).run()
//     }
// }

// pub struct Callbacks;
// impl rustc_driver::Callbacks for Callbacks {
//     // In this callback we override the mir_borrowck query.
//     fn config(&mut self, config: &mut rustc_interface::Config) {
//         assert!(config.override_queries.is_none());
//         config.override_queries = Some(borrowck_facts::override_queries);
//     }

//     fn after_parsing<'tcx>(
//         &mut self,
//         compiler: &rustc_interface::interface::Compiler,
//         queries: &'tcx rustc_interface::Queries<'tcx>,
//     ) -> rustc_driver::Compilation {
//         println!("I finished parsing!");
//         log::debug!("I finished parsing!");

//         println!("-- Here are some Compiler diagnostics --");
//         match compiler.input() {
//             rustc_session::config::Input::File(path_buf) => {
//                 println!("Input::File:");
//                 println!("\tPath Buffer: {}", path_buf.display())
//             }
//             rustc_session::config::Input::Str { name: n, input: i } => {
//                 println!("Input::Str:");
//                 println!("\t Str.Name: {:?}", n);
//                 println!("\t Str.input: {}", i);
//             }
//         }

//         queries.global_ctxt().unwrap().take().enter(|tcx| {
//             for id in tcx.hir().items() {
//                 let hir = tcx.hir();
//                 let item = hir.item(id);
//                 match item.kind {
//                     rustc_hir::ItemKind::Static(_, _, _) | rustc_hir::ItemKind::Fn(_, _, _) => {
//                         let name = item.ident;
//                         let ty = tcx.type_of(hir.local_def_id(item.hir_id()));
//                         println!("{name:?}:\t{ty:?}")
//                     }
//                     _ => (),
//                 }
//             }
//         });

//         rustc_driver::Compilation::Continue
//     }
// }
