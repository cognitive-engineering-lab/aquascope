#![feature(rustc_private)]

use aquascope::test_utils;

#[test_log::test]
fn stepper() {
  test_utils::run_in_dir("steps", |path| {
    let filename = path.file_name().unwrap().to_string_lossy();
    test_utils::test_steps_in_file(path, |tag, state| {
      let f = filename.clone();
      let name = format!("{tag}@{f}");

      // Sort the nested arrays by Place string
      let mut state = state
        .into_iter()
        .map(|(s, mut vec)| {
          vec.sort_unstable_by_key(|v| v.0.clone());
          (s, vec)
        })
        .collect::<Vec<_>>();

      // Sort the outer array by line number
      state.sort_unstable_by_key(|v| v.0);

      insta::with_settings!({
        description => &name,
        omit_expression => true,
      }, {
        insta::assert_yaml_snapshot!(name, state);
      })
    });
  });
}
