#![feature(rustc_private)]

use aquascope::test_utils;

#[test_log::test]
fn permissions() {
  test_utils::run_in_dir("refinement", |path| {
    if path.ends_with("mut_ref_0.test") {
      log::warn!("skipping mut_ref_0, FIXME");
      return;
    }

    test_utils::test_refinements_in_file(path);
  });
}
