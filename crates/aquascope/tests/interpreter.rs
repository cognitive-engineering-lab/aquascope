use aquascope::test_utils;

#[test]
#[test_log::test]
fn interpreter() {
  test_utils::run_in_dir("interpreter", |path| {
    test_utils::test_interpreter_in_file(path, |name, result| {
      insta::with_settings!({
        description => &name,
        omit_expression => true,
      }, {
        insta::assert_yaml_snapshot!(name, result);
      });
    });
  });
}
