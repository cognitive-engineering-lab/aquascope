use std::{collections::HashMap, fs, io, panic, path::Path, process::Command};

use anyhow::{bail, Context, Result};
use flowistry::{
  indexed::impls::LocationOrArg,
  mir::{
    borrowck_facts::{self, NO_SIMPLIFY},
    utils::{BodyExt, OperandExt},
  },
  source_map::{self, GraphemeIndices, Range, Spanner, ToSpan},
};
use itertools::Itertools;
use rustc_borrowck::BodyWithBorrowckFacts;
use rustc_hir::{BodyId, ItemKind};
use rustc_middle::{
  mir::{Rvalue, StatementKind},
  ty::TyCtxt,
};
use rustc_span::{source_map::FileLoader, BytePos};

use crate::{
  analysis::{self, permissions::Permissions, stepper::PermissionsDataDiff},
  errors,
  interpreter::{self, MStep},
};

struct StringLoader(String);
impl FileLoader for StringLoader {
  fn file_exists(&self, _: &Path) -> bool {
    true
  }
  fn read_file(&self, _: &Path) -> io::Result<String> {
    Ok(self.0.clone())
  }
}

lazy_static::lazy_static! {
  static ref SYSROOT: String = {
    let rustc_output = Command::new("rustc")
      .args(["--print", "sysroot"])
      .output()
      .unwrap()
      .stdout;
    String::from_utf8(rustc_output).unwrap().trim().to_owned()
  };
}

impl From<&str> for Permissions {
  fn from(s: &str) -> Permissions {
    let l = s.to_lowercase();
    Permissions {
      read: l.contains('r'),
      write: l.contains('w'),
      // we keep 'd' for backwards compatibility from a
      // time when the front-end permissions showd 'D' for drop.
      drop: l.contains('d') || l.contains('o'),
    }
  }
}

// Intermediate step that maps a start-end position,
// to a place string and corresponding permissions.
type PermMap = HashMap<(usize, usize), (String, Permissions)>;

fn split_test_source(
  source: impl AsRef<str>,
  delimeters: (&'static str, &'static str),
) -> Result<(String, PermMap)> {
  let source = source.as_ref();
  let mut source_idx = 0;
  let mut out = Vec::default();
  let mut stack = Vec::default();
  let bytes = source.bytes().collect::<Vec<_>>();

  let mut perm_map = HashMap::default();

  let (open, close) = delimeters;

  // Make this a macro so I can change it later.
  macro_rules! check_delim {
    ($token:expr) => {
      source_idx + $token.len() <= bytes.len()
        && $token.as_bytes() == &bytes[source_idx .. source_idx + $token.len()]
    };
  }

  // The current assumption is that annotations are of the form `(VAR PERMS)`, in this scenario
  // `()` are the delimeters and there is a VAR and expected PERMS separated by a space.
  while source_idx < bytes.len() {
    if check_delim!(open) {
      source_idx += open.len();
      stack.push(source_idx);

      let start_range = out.len();

      while source_idx < bytes.len() && !check_delim!(close) {
        source_idx += 1;
      }

      if !check_delim!(close) || stack.is_empty() {
        bail!("Unmatched opening delimeter {:?}", stack);
      }

      let start_idx = stack.pop().unwrap();
      let use_with_perms =
        std::str::from_utf8(&bytes[start_idx .. source_idx])?;

      let (var, perms_str) =
        use_with_perms.split_whitespace().next_tuple().unwrap();

      // Need to push the variable back into the output.
      var.as_bytes().iter().for_each(|b| out.push(*b));

      let end_range = out.len();

      let perms = perms_str.into();

      perm_map.insert((start_range, end_range), (var.to_string(), perms));
      source_idx += close.len();
    } else if check_delim!(close) {
      bail!(
        "Closing delimeter without matching open {:?}",
        &bytes[.. source_idx]
      );
    } else {
      out.push(bytes[source_idx]);
      source_idx += 1;
    }
  }

  let clean = String::from_utf8(out)?;

  Ok((clean, perm_map))
}

fn parse_test_source(
  src: &str,
  delimeters: (&'static str, &'static str),
) -> Result<(String, HashMap<Range, Permissions>)> {
  let (clean, interim_map) = split_test_source(src, delimeters)?;

  let indices = GraphemeIndices::new(&clean);

  let map = interim_map
    .into_iter()
    .map(|((s, e), (_var_str, perms))| {
      (Range::from_byte_range(s, e, "dummy.rs", &indices), perms)
    })
    .collect::<HashMap<_, _>>();

  Ok((clean, map))
}

pub fn load_test_from_file(path: &Path) -> Result<String> {
  log::info!(
    "Loading test from {}",
    path.file_name().unwrap().to_string_lossy()
  );
  let c = fs::read(path)
    .with_context(|| format!("failed to load test from {path:?}"))?;
  String::from_utf8(c)
    .with_context(|| format!("UTF8 parse error in file: {path:?}"))
}

pub fn test_refinements_in_file(path: &Path) {
  let inner = || -> Result<()> {
    let input = load_test_from_file(path)?;

    let (clean_input, expected_permissions) =
      parse_test_source(&input, ("`[", "]`"))?;

    let mut expected_permissions = expected_permissions;

    compile_bodies(clean_input, |tcx, body_id, body_with_facts| {
      let ctxt = analysis::compute_permissions(tcx, body_id, body_with_facts);
      let spanner = Spanner::new(tcx, body_id, &body_with_facts.body);
      let source_map = tcx.sess.source_map();

      expected_permissions.retain(|range, expected_perms| {
        let span = range.to_span(tcx).unwrap();
        let places = spanner.span_to_places(span);
        let source_file = source_map.lookup_source_file(span.lo());
        let source_line = source_map.lookup_line(span.lo()).unwrap().line;
        let line_str = source_file.get_line(source_line).unwrap();
        let source_line = source_line + 1;

        log::debug!("Spanned places {span:?} {expected_perms:?}: {:?}", places);

        // HACK: revisit this because it is most certainly based in
        // a fragile assumption.
        let mir_spanner = if places.is_empty() {
          // If no places were found for this span then ignore it
          // for now and see if it matches in a different body.
          return true;
        } else {
          places.first().unwrap()
        };
        let loc = match mir_spanner.locations[0] {
          LocationOrArg::Location(l) => l,
          _ => unreachable!("not a location"),
        };

        // FIXME: this code is to catch any false assumptions I'm making
        // about the structure of the generated MIR and the Flowistry Spanner.
        let stmt = ctxt.body_with_facts.body.stmt_at(loc).left().unwrap();
        let place = match &stmt.kind {
          StatementKind::Assign(box (lhs, rvalue)) => {
            let exp = ctxt.place_to_path(&mir_spanner.place);
            let act = ctxt.place_to_path(lhs);
            assert_eq!(exp, act);

            match rvalue {
              Rvalue::Ref(_, _, place) => *place,
              Rvalue::Use(op) => op.to_place().unwrap(),
              _ => unimplemented!(),
            }
          }
          _ => unreachable!("not a move"),
        };

        let path = ctxt.place_to_path(&place);
        let point = ctxt.location_to_point(loc);
        let computed_perms =
          ctxt.permissions_data_at_point(path, point).permissions;

        if *expected_perms != computed_perms {
          panic!(
            "\n\n\x1b[31mExpected {expected_perms:?} \
               but got {computed_perms:?} permissions\n  \
               \x1b[33m\
               for {place:?} in {stmt:?}\n  \
               on line {source_line}: {line_str}\n  \
               \x1b[0m\n\n"
          );
        }

        log::debug!("successful test!");

        false
      });
    });

    if !expected_permissions.is_empty() {
      panic!("Not all ranges tested! {expected_permissions:#?}");
    }

    Ok(())
  };

  inner().unwrap()
}

pub fn test_steps_in_file(
  path: &Path,
  assert_snap: impl Fn(String, Vec<(usize, Vec<(String, PermissionsDataDiff)>)>)
    + Send
    + Sync
    + Copy,
) {
  let inner = || -> Result<()> {
    let source = load_test_from_file(path)?;
    compile_bodies(source, move |tcx, body_id, body_with_facts| {
      let ctxt = &analysis::compute_permissions(tcx, body_id, body_with_facts);
      // Required to give the snapshot a more specific internal name.
      let owner = ctxt.tcx.hir().body_owner(ctxt.body_id);
      let tag = ctxt
        .tcx
        .hir()
        .opt_name(owner)
        .map(|n| String::from(n.as_str()))
        .unwrap_or_else(|| String::from("<anon body>"));

      let source_map = tcx.sess.source_map();
      let body_steps = analysis::compute_permission_steps(ctxt, |span| {
        source_map::Range::from_span(span, source_map)
          .ok()
          .unwrap_or_default()
          .into()
      });

      // NOTE: we normalize the permission steps to be
      // - usize: the line number of the corresponding statement.
      // - String: the the path (place) of the permissions.
      // - PermsDiff: obviously the actual permission diffs.
      let normalized = body_steps
        .into_iter()
        .map(|pss| {
          let bp = BytePos(pss.location.byte_end as u32);
          let line_num = source_map.lookup_line(bp).unwrap().line;
          let inner_info = pss.state;
          (line_num, inner_info)
        })
        .collect::<Vec<_>>();

      assert_snap(tag, normalized);
    });

    Ok(())
  };

  inner().unwrap()
}

pub fn test_interpreter_in_file(
  path: &Path,
  run_insta: impl Fn(String, Vec<MStep<crate::Range>>) + Sync,
) {
  let main = || -> Result<()> {
    let input = load_test_from_file(path)?;
    let args = format!(
      "--crate-type bin --sysroot {}",
      aquascope_workspace_utils::miri_sysroot()?.display()
    );
    compile(input, &args, |tcx| {
      let name = path.file_name().unwrap().to_string_lossy().to_string();
      let result = interpreter::interpret(tcx).unwrap();
      run_insta(name, result);
    });
    Ok(())
  };
  main().unwrap();
}

pub fn run_in_dir(
  dir: impl AsRef<Path>,
  test_fn: impl Fn(&Path) + std::panic::RefUnwindSafe,
) {
  let main = || -> Result<()> {
    let test_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
      .join("tests")
      .join(dir.as_ref());
    let tests = fs::read_dir(test_dir)?;
    let mut failed = false;
    let mut passed = 0;
    let mut total = 0;
    for test in tests {
      let path = test?.path();
      let test_name = path.file_name().unwrap().to_str().unwrap();
      let res = panic::catch_unwind(|| test_fn(&path));

      if let Err(e) = res {
        failed = true;
        eprintln!("\n\n\x1b[31m{test_name}\x1b[0m\n\t{e:?}\n\n");
      } else {
        passed += 1;
      }
      total += 1;
    }

    log::info!(
      "\n\n{:?}: {} / {} succeeded\n\n",
      dir.as_ref(),
      passed,
      total
    );

    if failed {
      panic!("some tests failed");
    }

    Ok(())
  };

  main().unwrap();
}

pub fn compile_bodies(
  input: impl Into<String>,
  mut callback: impl for<'tcx> FnMut(TyCtxt<'tcx>, BodyId, &BodyWithBorrowckFacts<'tcx>)
    + Send
    + std::marker::Sync,
) {
  compile(
    input,
    &format!("--crate-type lib --sysroot {}", &*SYSROOT),
    |tcx| {
      let hir = tcx.hir();
      hir
        .items()
        .filter_map(|id| match hir.item(id).kind {
          ItemKind::Fn(_, _, body) => Some(body),
          _ => None,
        })
        .for_each(|body_id| {
          let def_id = tcx.hir().body_owner_def_id(body_id);
          errors::track_body_diagnostics(def_id);
          let body_with_facts =
            borrowck_facts::get_body_with_borrowck_facts(tcx, def_id);

          log::debug!("{}", body_with_facts.body.to_string(tcx).unwrap());

          callback(tcx, body_id, body_with_facts);
        })
    },
  )
}

#[allow(unused_must_use)]
pub fn compile(
  input: impl Into<String>,
  args: &str,
  callback: impl FnOnce(TyCtxt<'_>) + Send,
) {
  let mut callbacks = TestCallbacks {
    callback: Some(callback),
  };
  let args = format!(
    "rustc dummy.rs --edition=2021 -Z identify-regions -Z mir-opt-level=0 -Z track-diagnostics=yes -Z maximal-hir-to-mir-coverage --allow warnings {args}",
  );
  let args = args.split(' ').map(|s| s.to_string()).collect::<Vec<_>>();

  // Explicitly ignore the unused return value. Many test cases are intended
  // to fail compilation, but the analysis results should still be sound.
  rustc_driver::catch_fatal_errors(|| {
    let mut compiler = rustc_driver::RunCompiler::new(&args, &mut callbacks);
    compiler.set_file_loader(Some(Box::new(StringLoader(input.into()))));
    compiler.run()
  });
}

struct TestCallbacks<Cb> {
  callback: Option<Cb>,
}

impl<Cb> rustc_driver::Callbacks for TestCallbacks<Cb>
where
  Cb: FnOnce(TyCtxt<'_>),
{
  fn config(&mut self, config: &mut rustc_interface::Config) {
    use std::{io::sink, sync::atomic::Ordering};

    use rustc_error_messages::fallback_fluent_bundle;
    use rustc_errors::{
      emitter::EmitterWriter, Handler, DEFAULT_LOCALE_RESOURCES,
    };
    config.parse_sess_created = Some(Box::new(|sess| {
      // Create a new emitter writer which consumes *silently* all
      // errors. There most certainly is a *better* way to do this,
      // if you, the reader, know what that is, please open an issue :)
      //
      // I decided to not use the SilentEmitter, because it still emitted
      // something, so it wasn't completely silent.
      let fallback_bundle =
        fallback_fluent_bundle(DEFAULT_LOCALE_RESOURCES, false);
      let emitter = EmitterWriter::new(
        Box::new(sink()),
        None,
        None,
        fallback_bundle,
        false,
        false,
        false,
        None,
        false,
        false,
      );
      let handler = Handler::with_emitter(false, None, Box::new(emitter));
      sess.span_diagnostic = handler;
    }));

    NO_SIMPLIFY.store(true, Ordering::SeqCst);
    config.override_queries = Some(borrowck_facts::override_queries);
  }

  fn after_parsing<'tcx>(
    &mut self,
    _compiler: &rustc_interface::interface::Compiler,
    queries: &'tcx rustc_interface::Queries<'tcx>,
  ) -> rustc_driver::Compilation {
    errors::initialize_error_tracking();
    queries.global_ctxt().unwrap().take().enter(|tcx| {
      let callback = self.callback.take().unwrap();
      callback(tcx);
    });
    rustc_driver::Compilation::Stop
  }
}
