use aquascope::test_utils;

fn permissions(dir: &str) {
  test_utils::run_in_dir(dir, |path| {
    test_utils::test_file(path);
  });
}

#[test]
#[test_log::test]
fn test_path_permissions() {
  permissions("refinement");
}
