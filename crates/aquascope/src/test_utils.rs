use std::{collections::HashMap, fs, io, panic, path::Path, process::Command};

use anyhow::{bail, Result};
use flowistry::{
  indexed::impls::LocationOrArg,
  mir::{
    borrowck_facts::{self, NO_SIMPLIFY},
    utils::{BodyExt, OperandExt},
  },
  source_map::{find_bodies, GraphemeIndices, Range, Spanner, ToSpan},
};
use itertools::Itertools;
use rustc_borrowck::BodyWithBorrowckFacts;
use rustc_hir::{BodyId, ItemKind};
use rustc_middle::{
  mir::{Rvalue, StatementKind},
  ty::TyCtxt,
};
use rustc_span::source_map::FileLoader;

use crate::analysis::{self, permissions::Permissions};

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
      drop: l.contains('d'),
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

pub fn test_file(path: &Path) {
  let inner = || -> Result<()> {
    log::info!("Testing {}", path.file_name().unwrap().to_string_lossy());
    let input = String::from_utf8(fs::read(path)?)?;

    let (clean_input, expected_permissions) =
      parse_test_source(&input, ("`[", "]`"))?;

    compile_bodies(clean_input, move |tcx, body_id, body_with_facts| {
      let ctxt = analysis::compute_permissions(tcx, body_id, body_with_facts);
      let spanner = Spanner::new(tcx, body_id, &body_with_facts.body);

      expected_permissions
        .iter()
        .for_each(|(range, expected_perms)| {
          let span = range.to_span(tcx).unwrap();
          let places = spanner.span_to_places(span);

          log::debug!(
            "Spanned places {span:?} {expected_perms:?}: {:?}",
            places
          );

          // HACK: revisit this because it is most certainly based in
          // a fragile assumption.
          let mir_spanner = places.first().unwrap();
          let loc = match mir_spanner.locations[0] {
            LocationOrArg::Location(l) => l,
            _ => unreachable!("not a location"),
          };

          // FIXME: this code is to catch any false assumptions I'm making
          // about the structure of the generated MIR and the Flowistry Spanner.
          let stmt = ctxt.body_with_facts.body.stmt_at(loc).left().unwrap();
          let path = match &stmt.kind {
            StatementKind::Assign(box (lhs, rvalue)) => {
              let exp = ctxt.place_to_path(&mir_spanner.place);
              let act = ctxt.place_to_path(lhs);
              assert_eq!(exp, act);

              let rplace = match rvalue {
                Rvalue::Ref(_, _, place) => *place,
                Rvalue::Use(op) => op.to_place().unwrap(),
                _ => unimplemented!(),
              };

              ctxt.place_to_path(&rplace)
            }
            _ => unreachable!("not a move"),
          };

          let point = ctxt.location_to_point(loc);
          let computed_perms = ctxt.permissions_at_point(path, point);

          if *expected_perms != computed_perms {
            panic!(
              "\n\n\x1b[31mExpected {expected_perms:?} \
               but got {computed_perms:?} permissions\n\t\
               \x1b[33m{stmt:?}\
               \x1b[0m\n\n"
            );
          }
        });
    });

    Ok(())
  };

  inner().unwrap()
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
  callback: impl for<'tcx> Fn(TyCtxt<'tcx>, BodyId, &BodyWithBorrowckFacts<'tcx>)
    + Send
    + std::marker::Sync,
) {
  compile(input, |tcx| {
    let hir = tcx.hir();
    hir
      .items()
      .filter_map(|id| match hir.item(id).kind {
        ItemKind::Fn(_, _, body) => Some(body),
        _ => None,
      })
      .for_each(|body_id| {
        let def_id = tcx.hir().body_owner_def_id(body_id);
        let body_with_facts =
          borrowck_facts::get_body_with_borrowck_facts(tcx, def_id);

        log::debug!("{}", body_with_facts.body.to_string(tcx).unwrap());

        callback(tcx, body_id, body_with_facts);
      })
  })
}

pub fn compile(
  input: impl Into<String>,
  callback: impl FnOnce(TyCtxt<'_>) + Send,
) {
  let mut callbacks = TestCallbacks {
    callback: Some(callback),
  };
  let args = format!(
    "rustc dummy.rs --crate-type lib --edition=2021 -Z identify-regions -Z mir-opt-level=0 -Z maximal-hir-to-mir-coverage --allow warnings --sysroot {}",
    &*SYSROOT
  );
  let args = args.split(' ').map(|s| s.to_string()).collect::<Vec<_>>();

  rustc_driver::catch_fatal_errors(|| {
    let mut compiler = rustc_driver::RunCompiler::new(&args, &mut callbacks);
    compiler.set_file_loader(Some(Box::new(StringLoader(input.into()))));
    // compiler.set_emitter(None); // XXX: we want to suppress rustc errors but I'm not sure why this doesn't work.
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
    NO_SIMPLIFY.store(true, std::sync::atomic::Ordering::SeqCst);
    config.override_queries = Some(borrowck_facts::override_queries);
  }

  fn after_parsing<'tcx>(
    &mut self,
    _compiler: &rustc_interface::interface::Compiler,
    queries: &'tcx rustc_interface::Queries<'tcx>,
  ) -> rustc_driver::Compilation {
    queries.global_ctxt().unwrap().take().enter(|tcx| {
      let callback = self.callback.take().unwrap();
      callback(tcx);
    });
    rustc_driver::Compilation::Stop
  }
}
