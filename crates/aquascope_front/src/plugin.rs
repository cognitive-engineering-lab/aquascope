use std::{
  borrow::Cow,
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
  errors::{
    initialize_error_tracking, silent::silent_session, track_body_diagnostics,
  },
};
use clap::{Parser, Subcommand};
use fluid_let::fluid_set;
use rustc_hir::BodyId;
use rustc_interface::interface::Result as RustcResult;
use rustc_middle::ty::TyCtxt;
use rustc_plugin::{CrateFilter, RustcPlugin, RustcPluginArgs, Utf8Path};
use rustc_utils::{mir::borrowck_facts, source_map::find_bodies::find_bodies};
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
  },

  Interpreter,

  Preload,
  RustcVersion,
}

pub struct AquascopePlugin;
impl RustcPlugin for AquascopePlugin {
  type Args = AquascopePluginArgs;

  fn version(&self) -> Cow<'static, str> {
    env!("CARGO_PKG_VERSION").into()
  }

  fn driver_name(&self) -> Cow<'static, str> {
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

    RustcPluginArgs {
      filter: CrateFilter::OnlyWorkspace,
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
            .map_err(|_| AquascopeError::BuildError { range: None }),
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

  rustc_driver::catch_fatal_errors(move || {
    compiler.run();
  })
  .map_err(|_| AquascopeError::BuildError { range: None })
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
    config.psess_created = Some(silent_session());
    config.override_queries = Some(borrowck_facts::override_queries);
  }

  fn after_expansion(
    &mut self,
    _compiler: &rustc_interface::interface::Compiler,
    tcx: TyCtxt<'_>,
  ) -> rustc_driver::Compilation {
    // Setting up error tracking happens here. Within rustc callbacks
    // seem to be set up *after* `config` is called.
    initialize_error_tracking();

    fluid_set!(INCLUDE_MODE, self.steps_include_mode);
    fluid_set!(ENABLE_FLOW_PERMISSIONS, self.show_flows);

    let _start = Instant::now();

    let mut analysis = self.analysis.take().unwrap();
    find_bodies(tcx).into_iter().for_each(|(_, body_id)| {
      // Track diagnostics for the analysis of the current body
      let def_id = tcx.hir().body_owner_def_id(body_id);
      track_body_diagnostics(def_id);
      self.output.push(analysis.analyze(tcx, body_id));
    });

    log::debug!("Callback analysis took {:?}", self.rustc_start.elapsed());

    rustc_driver::Compilation::Stop
  }
}
