#![feature(rustc_private)]

use aquascope::test_utils;

#[test_log::test]
fn permissions() {
  test_utils::run_in_dir("refinement", |path| {
    test_utils::test_refinements_in_file(path);
  });
}
