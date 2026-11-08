#![feature(rustc_private)]

use aquascope::{analysis::AquascopeAnalysis, test_utils};

// The stepper cannot yet segment a branch that leaves the body early when
// another branch follows it (#157). The analysis of such a body should still
// succeed with its permission boundaries, rather than fail as a whole.
#[test_log::test]
fn early_return_keeps_boundaries() {
  let code = r#"
fn parse(mut args: impl Iterator<Item = String>) -> Result<(String, String), String> {
  let query = match args.next() {
    Some(arg) => arg,
    None => return Err("Missing query".into()),
  };
  let path = match args.next() {
    Some(arg) => arg,
    None => return Err("Missing path".into()),
  };
  Ok((query, path))
}

fn both(v: Vec<i32>) -> Option<i32> {
  let mut it = v.into_iter();
  let x = it.next()?;
  let y = it.next()?;
  Some(x + y)
}
"#;

  test_utils::compile_normal(code, |tcx| {
    test_utils::for_each_body(tcx, |body_id, _| {
      let output = match AquascopeAnalysis::run(tcx, body_id, false) {
        Ok(output) => output,
        Err(e) => panic!("the analysis failed: {e:?}"),
      };
      assert!(!output.boundaries.is_empty(), "no boundaries");
    });
  });
}
