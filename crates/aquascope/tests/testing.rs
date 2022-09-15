#![feature(rustc_private)]

extern crate rustc_borrowck;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_span;

use std::{fs, io, panic, path::Path, process::Command};

use anyhow::{anyhow, bail, Context, Result};
use aquascope::{
  analysis::{self},
  mir::borrowck_facts,
  source_map::{
    find_enclosing_bodies, GraphemeIndices, Range, Spanner, ToSpan,
  },
};
use rustc_borrowck::consumers::BodyWithBorrowckFacts;
use rustc_data_structures::fx::{FxHashMap as HashMap, FxHashSet as HashSet};
use rustc_hir::{BodyId, ItemKind};
use rustc_interface::interface;
use rustc_middle::{
  mir::{Location, Place},
  ty::TyCtxt,
};
use rustc_span::{source_map::FileLoader, BytePos, Span, SyntaxContext};
use unicode_segmentation::UnicodeSegmentation;

lazy_static::lazy_static! {
  static ref SYSROOT: String = String::from_utf8(
    Command::new("rustc")
      .args(&["--print", "sysroot"])
      .output()
      .unwrap()
      .stdout
  )
  .unwrap()
  .trim()
  .to_owned();
}

struct StringLoader(String);
impl FileLoader for StringLoader {
  fn file_exists(&self, _: &Path) -> bool {
    true
  }
  fn read_file(&self, _: &Path) -> io::Result<String> {
    Ok(self.0.clone())
  }
}

struct TestCallbacks<Cb> {
  callback: Option<Cb>,
}

impl<Cb> rustc_driver::Callbacks for TestCallbacks<Cb>
where
  Cb: FnOnce(TyCtxt<'_>),
{
  fn config(&mut self, config: &mut rustc_interface::Config) {
    config.override_queries = Some(borrowck_facts::override_queries);
  }

  fn after_parsing<'tcx>(
    &mut self,
    _compiler: &interface::Compiler,
    queries: &'tcx rustc_interface::Queries<'tcx>,
  ) -> rustc_driver::Compilation {
    queries.global_ctxt().unwrap().take().enter(|tcx| {
      let callback = self.callback.take().unwrap();
      callback(tcx);
    });
    rustc_driver::Compilation::Stop
  }
}

pub type RangeMap = HashMap<&'static str, Vec<(usize, usize)>>;

pub fn parse_ranges(
  src: impl AsRef<str>,
  delimiters: impl AsRef<[(&'static str, &'static str)]>,
) -> Result<(String, RangeMap)> {
  let src = src.as_ref();
  let delimiters = delimiters.as_ref();

  let mut in_idx = 0;
  let mut out_idx = 0;
  let mut buf = Vec::new();
  let bytes = src.bytes().collect::<Vec<_>>();
  let mut stack = vec![];

  let (opens, closes): (Vec<_>, Vec<_>) = delimiters.iter().copied().unzip();
  let mut ranges: HashMap<_, Vec<_>> = HashMap::default();

  macro_rules! check_token {
    ($tokens:expr) => {
      $tokens
        .iter()
        .find(|t| {
          in_idx + t.len() <= bytes.len()
            && t.as_bytes() == &bytes[in_idx .. in_idx + t.len()]
        })
        .map(|t| *t)
    };
  }

  while in_idx < bytes.len() {
    if let Some(open) = check_token!(opens) {
      stack.push((out_idx, open));
      in_idx += open.len();
      continue;
    }

    if let Some(close) = check_token!(closes) {
      let (start, delim) = stack
        .pop()
        .with_context(|| anyhow!("Missing open delimiter for \"{close}\""))?;
      ranges.entry(delim).or_default().push((start, out_idx));
      in_idx += close.len();
      continue;
    }

    buf.push(bytes[in_idx]);
    in_idx += 1;
    out_idx += 1;
  }

  if stack.len() > 0 {
    bail!("Unclosed delimiters: {stack:?}");
  }

  let prog_clean = String::from_utf8(buf)?;
  Ok((prog_clean, ranges))
}

pub fn make_span((lo, hi): (usize, usize)) -> Span {
  Span::new(
    BytePos(lo as u32),
    BytePos(hi as u32),
    SyntaxContext::root(),
    None,
  )
}

// pub fn color_ranges(prog: &str, all_ranges: Vec<(&str, &HashSet<Range>)>) -> String {
//     let mut new_tokens = all_ranges
//         .iter()
//         .flat_map(|(_, ranges)| {
//             ranges.iter().flat_map(|range| {
//                 let contained = all_ranges.iter().any(|(_, ranges)| {
//                     ranges.iter().any(|other| {
//                         range != other
//                             && other.byte_start <= range.byte_end
//                             && range.byte_end < other.byte_end
//                     })
//                 });
//                 let end_marker = if contained { "]" } else { "\x1B[0m]" };
//                 [
//                     ("[\x1B[31m", range.byte_start),
//                     (end_marker, range.byte_end),
//                 ]
//             })
//         })
//         .collect::<Vec<_>>();
//     new_tokens.sort_by_key(|(_, i)| -(*i as isize));

//     let mut output = prog.to_owned();
//     for (s, i) in new_tokens {
//         output.insert_str(i, s);
//     }

//     return output;
// }

// fn fmt_ranges(prog: &str, s: &HashSet<Range>) -> String {
//     textwrap::indent(&color_ranges(prog, vec![("", s)]), "  ")
// }

// pub fn compare_ranges(expected: HashSet<Range>, actual: HashSet<Range>, prog: &str) {
//     let missing = &expected - &actual;
//     let extra = &actual - &expected;

//     let check = |s: HashSet<Range>, message: &str| {
//         if s.len() > 0 {
//             println!("Expected ranges:\n{}", fmt_ranges(prog, &expected));
//             println!("Actual ranges:\n{}", fmt_ranges(prog, &actual));
//             panic!("{message} ranges:\n{}", fmt_ranges(prog, &s));
//         }
//     };

//     check(missing, "Analysis did NOT have EXPECTED");
//     check(extra, "Actual DID have UNEXPECTED");
// }

// pub fn bless(path: &Path, contents: String, actual: HashSet<Range>) -> Result<()> {
//     let mut delims = actual
//         .into_iter()
//         .flat_map(|range| [("`[", range.char_start), ("]`", range.char_end)])
//         .collect::<Vec<_>>();
//     delims.sort_by_key(|(_, i)| *i);

//     let mut output = String::new();
//     for (i, g) in contents.graphemes(true).enumerate() {
//         while delims.len() > 0 && delims[0].1 == i {
//             let (delim, _) = delims.remove(0);
//             output.push_str(delim);
//         }
//         output.push_str(g);
//     }

//     fs::write(path.with_extension("txt.expected"), output)?;

//     Ok(())
// }

fn parse_range_map(
  src: &str,
  delims: Vec<(&'static str, &'static str)>,
) -> Result<(String, HashMap<&'static str, Vec<Range>>)> {
  let (clean, parsed_ranges) = parse_ranges(src, delims)?;
  let indices = GraphemeIndices::new(&clean);
  let map = parsed_ranges
    .into_iter()
    .map(|(k, vs)| {
      (
        k,
        vs.into_iter()
          .map(|(byte_start, byte_end)| {
            Range::from_byte_range(byte_start, byte_end, "dummy.rs", &indices)
          })
          .collect::<Vec<_>>(),
      )
    })
    .collect::<HashMap<_, _>>();
  Ok((clean, map))
}

pub fn compile_enriched_body(
  input: impl Into<String>,
  target: impl ToSpan,
  callback: impl for<'tcx> FnOnce(TyCtxt<'tcx>, BodyId, &BodyWithBorrowckFacts<'tcx>)
    + Send,
) {
  compile(input, |tcx| {
    let body_id = find_enclosing_bodies(tcx, target.to_span(tcx).unwrap())
      .next()
      .unwrap();
    let def_id = tcx.hir().body_owner_def_id(body_id);
    let body_with_facts =
      borrowck_facts::get_body_with_borrowck_facts(tcx, def_id);
    callback(tcx, body_id, body_with_facts);
  })
}

pub fn compile_body(
  input: impl Into<String>,
  callback: impl for<'tcx> FnOnce(TyCtxt<'tcx>, BodyId, &BodyWithBorrowckFacts<'tcx>)
    + Send,
) {
  compile(input, |tcx| {
    let hir = tcx.hir();
    let body_id = hir
      .items()
      .filter_map(|id| match hir.item(id).kind {
        ItemKind::Fn(_, _, body) => Some(body),
        _ => None,
      })
      .next()
      .unwrap();

    let def_id = tcx.hir().body_owner_def_id(body_id);
    let body_with_facts =
      borrowck_facts::get_body_with_borrowck_facts(tcx, def_id);

    log::debug!("{:?}", body_with_facts.body);

    callback(tcx, body_id, body_with_facts);
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
    "rustc dummy.rs --crate-type lib --edition=2021 -Z identify-regions -Z mir-opt-level=0 --allow warnings --sysroot {}",
    &*SYSROOT
  );
  let args = args.split(' ').map(|s| s.to_string()).collect::<Vec<_>>();

  rustc_driver::catch_fatal_errors(|| {
    let mut compiler = rustc_driver::RunCompiler::new(&args, &mut callbacks);
    compiler.set_file_loader(Some(Box::new(StringLoader(input.into()))));
    compiler.run()
  })
  .unwrap()
  .unwrap();
}

fn test_command_output(
  path: &Path,
  expected: Option<&Path>,
  output_fn: impl for<'a, 'hir, 'tcx> Fn(
      analysis::ScopeResults<'a, 'tcx>,
      Spanner<'hir, 'tcx>,
      Span,
    ) -> Vec<Span>
    + Send
    + Sync,
) {
  let inner = move || -> Result<()> {
    log::info!("Testing {}", path.file_name().unwrap().to_string_lossy());

    let annotated_input = String::from_utf8(fs::read(path)?)?;
    let (input, ranges) =
      parse_range_map(&annotated_input, vec![("`(", ")`")])?;
    let target = ranges["`("][0].clone();

    compile_enriched_body(
      input.clone(),
      target.clone(),
      move |tcx, body_id, body_with_facts| {
        let header = input.lines().next().unwrap();
        if header.starts_with("/*!") {
          log::warn!("Testing modes not supported, mode ignored...");
        }

        let target = target.to_span(tcx).unwrap();
        let result = analysis::compute_context(tcx, body_id, body_with_facts);
        let spanner = Spanner::new(tcx, body_id, &body_with_facts.body);

        let actual = output_fn(result, spanner, target);

        log::debug!("FINISHED!");
        log::debug!("{:?}", actual);

        todo!();
        // TODO
        // let hir = tcx.hir();
        // let item = hir.item(body_id);
        // let name = item.ident;
        // let ty = tcx.type_of(hir.local_def + id(item.hir_id()));
        // log::debug!("{name:?}:\t{ty:?}");
      },
    );

    Ok(())
  };

  inner().unwrap();
}

fn run_tests(
  dir: impl AsRef<Path>,
  test_fn: impl Fn(&Path, Option<&Path>) + std::panic::RefUnwindSafe,
) {
  let main = || -> Result<()> {
    let test_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
      .join("tests")
      .join(dir.as_ref());
    let tests = fs::read_dir(test_dir)?;
    let mut failed = false;
    for test in tests {
      let test = test?.path();
      if test.extension().unwrap() == "expect" {
        continue;
      }
      let test_name = test.file_name().unwrap().to_str().unwrap();

      // if let Some(only) = ONLY {
      //     if !test_name.contains(only) {
      //         continue;
      //     }
      // }

      // let expected_path = test.with_extension("expect");
      // let expected = expected_path.as_ref();

      // FIXME TODO currently I don't care about testing specific results.
      let result = panic::catch_unwind(|| test_fn(&test, None));
      // if let Err(e) = result {
      //     failed = true;
      //     eprintln!("\n\n{test_name}:\n{e:?}\n\n");
      // }
    }

    if failed {
      panic!("Tests failed.")
    }

    Ok(())
  };

  main().unwrap();
}

fn outline_lifetimes(dir: &str) {
  run_tests(dir, |input_path, expected_path| {
    test_command_output(
      input_path,
      expected_path,
      |results, spanner, target| {
        let places = spanner.span_to_places(target);
        let targets: Vec<Vec<(Place<'_>, Location)>> = places
          .iter()
          .map(|mir_span| {
            mir_span
              .locations
              .iter()
              .map(|location| (mir_span.place, *location))
              .collect::<Vec<_>>()
          })
          .collect();
        log::debug!("targets={targets:#?}");

        // XXX! we need to return a vector of spans. For the 'outline_spans
        // function this should the lifetimes in the body for which the
        // provided target is *live*.

        todo!();
      },
    );
  });
}

#[cfg(test)]
mod test {
  use super::*;

  fn setup() {
    let _ = env_logger::builder().is_test(true).try_init();
  }

  #[test]
  fn test_lifetimes() {
    setup();
    outline_lifetimes("lifetimes");
  }
}
