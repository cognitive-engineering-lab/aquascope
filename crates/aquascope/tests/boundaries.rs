use aquascope::test_utils;

#[test]
#[test_log::test]
fn boundaries() {
  test_utils::run_in_dir("boundaries", |path| {
    let filename = path.file_name().unwrap().to_string_lossy();
    test_utils::test_boundaries_in_file(path, |tag, mut state| {
      let f = filename.clone();
      let name = format!("{tag}@{f}");
      // Sort the boundaries by location
      state.sort_unstable_by_key(|b| b.location);
      insta::with_settings!({
        description => &name,
        omit_expression => true,
      }, {
        insta::assert_yaml_snapshot!(name, state);
      })
    });
  });
}
