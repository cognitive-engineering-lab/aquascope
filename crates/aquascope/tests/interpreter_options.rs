#![feature(rustc_private)]

use aquascope::{
  interpreter::{InterpretOptions, MTrace},
  test_utils,
};
use rustc_utils::source_map::range::CharRange;
use serde_json::{Value, json};

/// The last value the trace shows for local `name` of the innermost frame.
fn last_local(trace: &MTrace<CharRange>, name: &str) -> Value {
  let trace = serde_json::to_value(trace).unwrap();
  trace["steps"]
    .as_array()
    .unwrap()
    .iter()
    .rev()
    .filter_map(|step| {
      let frame = step["stack"]["frames"].as_array()?.last()?;
      let local = frame["locals"]
        .as_array()?
        .iter()
        .find(|local| local["name"] == name)?;
      Some(local["value"].clone())
    })
    .next()
    .unwrap_or_else(|| panic!("no local {name} in any step"))
}

// The program sees the crate's name and then its arguments, as with Cargo.
#[test_log::test]
fn the_program_receives_its_arguments() {
  let code = r#"
fn main() {
  let n = std::env::args().count();
  let first = std::env::args().nth(1).unwrap().len();
}
"#;
  let options = InterpretOptions {
    program_args: vec!["frog".into()],
    ..Default::default()
  };
  let trace = test_utils::interpret_with(code, &options).unwrap();
  assert_eq!(last_local(&trace, "n"), json!({"type": "Uint", "value": 2}));
  assert_eq!(
    last_local(&trace, "first"),
    json!({"type": "Uint", "value": 4})
  );
}

// Isolation stops a program that touches the file system, unless lifted.
#[test_log::test]
fn isolation_can_be_lifted() {
  let code = r#"
fn main() {
  let found = std::path::Path::new(".").exists();
}
"#;
  let isolated = test_utils::interpret_with(code, &InterpretOptions::default());
  let message = format!("{:#}", isolated.unwrap_err());
  assert!(message.contains("isolation"), "{message}");

  let options = InterpretOptions {
    disable_isolation: true,
    ..Default::default()
  };
  let trace = test_utils::interpret_with(code, &options).unwrap();
  assert_eq!(
    last_local(&trace, "found"),
    json!({"type": "Bool", "value": true})
  );
}
