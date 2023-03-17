use std::{
  env,
  process::{exit, Command},
  time::Instant,
};

use aquascope::{
  analysis::{
    self,
    permissions::ENABLE_FLOW_PERMISSIONS,
    stepper::{PermIncludeMode, INCLUDE_MODE},
    AquascopeError, AquascopeResult,
  },
  errors::{initialize_error_tracking, track_body_diagnostics},
  Range,
};
use clap::{Parser, Subcommand};
use flowistry::{
  mir::borrowck_facts::{self, NO_SIMPLIFY},
  source_map::find_bodies,
};
use fluid_let::fluid_set;
use rustc_hir::BodyId;
use rustc_interface::interface::Result as RustcResult;
use rustc_middle::ty::TyCtxt;
use rustc_plugin::{RustcPlugin, RustcPluginArgs, Utf8Path};
use serde::{self, Deserialize, Serialize};

const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Debug, Parser, Serialize, Deserialize)]
#[clap(version = VERSION)]
pub struct AquascopePluginArgs {
  #[clap(long)]
  should_fail: bool,

  #[clap(subcommand)]
  command: AquascopeCommand,
}

#[derive(Debug, Subcommand, Serialize, Deserialize)]
enum AquascopeCommand {
  Permissions {
    #[clap(long)]
    steps_include_mode: Option<PermIncludeMode>,

    #[clap(long)]
    show_flows: bool,

    #[clap(last = true)]
    flags: Vec<String>,
  },

  Interpreter {
    #[clap(last = true)]
    flags: Vec<String>,
  },

  Preload,
  RustcVersion,
}

pub struct AquascopePlugin;
impl RustcPlugin for AquascopePlugin {
  type Args = AquascopePluginArgs;

  fn version() -> &'static str {
    "0.0.0"
  }

  fn bin_name() -> String {
    "aquascope-driver".into()
  }

  fn args(
    &self,
    target_dir: &Utf8Path,
  ) -> RustcPluginArgs<AquascopePluginArgs> {
    let args = AquascopePluginArgs::parse_from(env::args().skip(1));

    log::debug!("Provided PluginArgs {args:?}");

    let cargo_path =
      env::var("CARGO_PATH").unwrap_or_else(|_| "cargo".to_string());

    use AquascopeCommand::*;
    match &args.command {
      Preload => {
        let mut cmd = Command::new(cargo_path);
        // Note: this command must share certain parameters with rustc_plugin so Cargo will not recompute
        // dependencies when actually running the driver, e.g. RUSTFLAGS.
        cmd
          .args(["check", "--all", "--all-features", "--target-dir"])
          .arg(target_dir)
          .env("RUSTFLAGS", "-Awarnings");
        let exit_status = cmd.status().expect("could not run cargo");
        exit(exit_status.code().unwrap_or(-1));
      }
      RustcVersion => {
        let commit_hash =
          rustc_interface::util::rustc_version_str().unwrap_or("unknown");
        println!("{commit_hash}");
        exit(0);
      }
      _ => {}
    };

    let flags = match &args.command {
      Permissions { flags, .. } => flags,
      Interpreter { flags, .. } => flags,
      _ => unreachable!(),
    };

    RustcPluginArgs {
      flags: Some(flags.clone()),
      file: None,
      args,
    }
  }

  fn run(
    self,
    compiler_args: Vec<String>,
    plugin_args: AquascopePluginArgs,
  ) -> RustcResult<()> {
    use AquascopeCommand::*;
    match plugin_args.command {
      Permissions {
        steps_include_mode,
        show_flows,
        ..
      } => {
        let steps_include_mode =
          steps_include_mode.unwrap_or(PermIncludeMode::Changes);
        let mut callbacks = AquascopeCallbacks {
          analysis: Some(permissions_analyze_body),
          output: Vec::default(),
          should_fail: plugin_args.should_fail,
          steps_include_mode,
          show_flows,
          rustc_start: Instant::now(),
        };
        log::info!("Starting rustc analysis...");
        let _ = run_with_callbacks(&compiler_args, &mut callbacks);
        postprocess(callbacks.output)
      }
      Interpreter { .. } => {
        let mut callbacks = aquascope::interpreter::InterpretCallbacks::new(
          plugin_args.should_fail,
        );
        let _ = run_with_callbacks(&compiler_args, &mut callbacks);
        postprocess(
          callbacks
            .result
            .unwrap()
            .map_err(|e| AquascopeError::AnalysisError { msg: e.to_string() }),
        )
      }
      _ => unreachable!(),
    }
  }
}

fn permissions_analyze_body(
  tcx: TyCtxt,
  id: BodyId,
) -> AquascopeResult<analysis::AnalysisOutput> {
  analysis::AquascopeAnalysis::run(tcx, id)
}

fn postprocess<T: Serialize>(result: T) -> RustcResult<()> {
  println!("{}", serde_json::to_string(&result).unwrap());
  Ok(())
}

pub fn run_with_callbacks(
  args: &[String],
  callbacks: &mut (dyn rustc_driver::Callbacks + Send),
) -> AquascopeResult<()> {
  let mut args = args.to_vec();
  args.extend(
    "-Z identify-regions -Z mir-opt-level=0 -Z track-diagnostics=yes -Z maximal-hir-to-mir-coverage -A warnings"
      .split(' ')
      .map(|s| s.to_owned()),
  );

  log::debug!("Running command with callbacks: {args:?}");

  let compiler = rustc_driver::RunCompiler::new(&args, callbacks);

  log::debug!("building compiler ...");

  compiler.run().map_err(|_| AquascopeError::BuildError {
    range: Range::default(),
  })
}

pub trait AquascopeAnalysis: Sized + Send + Sync {
  type Output: Serialize + Send + Sync;
  fn analyze(
    &mut self,
    tcx: TyCtxt,
    id: BodyId,
  ) -> AquascopeResult<Self::Output>;
}

impl<F, O> AquascopeAnalysis for F
where
  F: for<'tcx> Fn<(TyCtxt<'tcx>, BodyId), Output = AquascopeResult<O>>
    + Send
    + Sync,
  O: Serialize + Send + Sync,
{
  type Output = O;
  fn analyze(
    &mut self,
    tcx: TyCtxt,
    id: BodyId,
  ) -> AquascopeResult<Self::Output> {
    (self)(tcx, id)
  }
}

#[allow(dead_code)]
struct AquascopeCallbacks<A: AquascopeAnalysis> {
  analysis: Option<A>,
  output: Vec<AquascopeResult<A::Output>>,
  should_fail: bool,
  steps_include_mode: PermIncludeMode,
  show_flows: bool,
  rustc_start: Instant,
}

impl<A: AquascopeAnalysis> rustc_driver::Callbacks for AquascopeCallbacks<A> {
  fn config(&mut self, config: &mut rustc_interface::Config) {
    NO_SIMPLIFY.store(true, std::sync::atomic::Ordering::SeqCst);
    config.override_queries = Some(borrowck_facts::override_queries);
  }

  fn after_parsing<'tcx>(
    &mut self,
    _compiler: &rustc_interface::interface::Compiler,
    queries: &'tcx rustc_interface::Queries<'tcx>,
  ) -> rustc_driver::Compilation {
    // Setting up error tracking happens here. Within rustc callbacks
    // seem to be set up *after* `config` is called.
    initialize_error_tracking();

    fluid_set!(INCLUDE_MODE, self.steps_include_mode);
    fluid_set!(ENABLE_FLOW_PERMISSIONS, self.show_flows);

    let _start = Instant::now();

    queries.global_ctxt().unwrap().take().enter(|tcx| {
      let mut analysis = self.analysis.take().unwrap();
      find_bodies(tcx).into_iter().for_each(|(_, body_id)| {
        // Track diagnostics for the analysis of the current body
        let def_id = tcx.hir().body_owner_def_id(body_id);
        track_body_diagnostics(def_id);
        self.output.push(analysis.analyze(tcx, body_id));
      });
    });

    log::debug!("Callback analysis took {:?}", self.rustc_start.elapsed());

    rustc_driver::Compilation::Stop
  }
}
