#![feature(rustc_private)]

use aquascope::{
  analysis::{AquascopeAnalysis, AquascopeError},
  test_utils,
};
use rustc_utils::test_utils::DUMMY_FILE_NAME;

// Each result names the file holding its body, and so does a build error, so
// that a caller analysing a crate of several files can place them.
#[test_log::test]
fn results_and_build_errors_name_their_file() {
  let code = r#"
fn builds() -> i32 {
  let x = 1;
  x
}

fn does_not_build() -> i32 {
  let x: i32 = "not a number";
  x
}
"#;

  let mut counts = (0, 0);
  test_utils::compile_normal(code, |tcx| {
    test_utils::for_each_body(tcx, |body_id, _| {
      match AquascopeAnalysis::run(tcx, body_id, false) {
        Ok(output) => {
          assert_eq!(output.file_path, DUMMY_FILE_NAME);
          counts.0 += 1;
        }
        Err(AquascopeError::BuildError { file_path, .. }) => {
          assert_eq!(file_path.as_deref(), Some(DUMMY_FILE_NAME));
          counts.1 += 1;
        }
        Err(e) => panic!("unexpected error: {e:?}"),
      }
    });
  });
  assert_eq!(counts, (1, 1), "one body built and one did not");
}
