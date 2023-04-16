//! Parser for annotations within an Aquascope code block body.

use std::{collections::HashMap, hash::Hash};

use anyhow::Result;
use itertools::Itertools;
use regex::Regex;
use serde::Serialize;
use ts_rs::TS;

#[derive(PartialEq, Eq, Debug, TS, Serialize, Clone, Copy)]
#[ts(export)]
pub struct CharPos(usize);

#[derive(PartialEq, Eq, Debug, TS, Serialize, Hash, Clone, Copy)]
#[ts(export)]
pub struct LinePos(usize);

#[derive(PartialEq, Eq, Debug, TS, Serialize, Clone)]
#[ts(export)]
#[serde(tag = "type", content = "value")]
pub enum PathMatcher {
  Literal(String),
  Regex(String),
}

#[derive(PartialEq, Eq, Debug, TS, Serialize, Default, Clone)]
#[ts(export)]
pub struct StepperAnnotations {
  focused_lines: Vec<LinePos>,
  focused_paths: HashMap<LinePos, Vec<PathMatcher>>,
}

#[derive(PartialEq, Eq, Debug, TS, Serialize, Default, Clone)]
#[ts(export)]
pub struct BoundariesAnnotations {
  focused_lines: Vec<LinePos>,
}

#[derive(PartialEq, Eq, Debug, TS, Serialize, Default, Clone)]
#[ts(export)]
pub struct InterpAnnotations {
  state_locations: Vec<CharPos>,
}

#[derive(PartialEq, Eq, Debug, Default, TS, Serialize, Clone)]
#[ts(export)]
pub struct AquascopeAnnotations {
  hidden_lines: Vec<LinePos>,
  interp: InterpAnnotations,
  stepper: StepperAnnotations,
  boundaries: BoundariesAnnotations,
}

#[allow(clippy::derived_hash_with_manual_eq)]
impl Hash for AquascopeAnnotations {
  fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {
    // HashMaps aren't hashable, and we can ignore annotations when hashing
    // anyway since they don't change the result of an aquascope computation.
  }
}

pub fn parse_annotations(code: &str) -> Result<(String, AquascopeAnnotations)> {
  let marker_interp = ("`[", "]`", "interp");
  let marker_stepper = ("`(", ")`", "stepper");
  let marker_boundaries = ("`{", "}`", "boundaries");

  let pattern = Itertools::intersperse(
    [marker_interp, marker_stepper, marker_boundaries]
      .into_iter()
      .map(|(open, close, name)| {
        format!(
          "{}(?P<{name}>[^{}]*){}",
          regex::escape(open),
          regex::escape(&close[.. 1]),
          regex::escape(close)
        )
      }),
    "|".into(),
  )
  .collect::<String>();
  let re = Regex::new(&pattern)?;

  let mut annots = AquascopeAnnotations::default();
  let mut idx = 0;
  let mut output_lines = Vec::new();
  for (line_idx, mut line) in code.lines().enumerate() {
    let line_pos = LinePos(line_idx + 1);
    let mut fragments = Vec::new();
    macro_rules! add_fragment {
      ($s:expr) => {
        fragments.push($s);
        idx += $s.len();
      };
    }
    if let Some(suffix) = line.strip_prefix('#') {
      annots.hidden_lines.push(line_pos);
      add_fragment!(suffix);
    } else if let Some(suffix) = line.strip_prefix("\\#") {
      add_fragment!("#");
      add_fragment!(suffix);
    } else {
      while let Some(cap) = re.captures(line) {
        let matched = cap.get(0).unwrap();
        add_fragment!(&line[0 .. matched.start()]);

        let match_str = matched.as_str();
        let match_type = if match_str.starts_with(marker_interp.0) {
          "interp"
        } else if match_str.starts_with(marker_stepper.0) {
          "stepper"
        } else {
          "boundaries"
        };

        let interior = cap.name(match_type).unwrap();
        let mut config = HashMap::new();
        for s in interior.as_str().split(',').filter(|s| *s != "") {
          match s.split_once(':') {
            Some((s1, s2)) => config.insert(s1, s2),
            None => config.insert(s, ""),
          };
        }

        match match_type {
          "interp" => annots.interp.state_locations.push(CharPos(idx)),
          "stepper" => {
            if config.contains_key("focus") {
              annots.stepper.focused_lines.push(line_pos);
            }
            let mut add_matcher = |matcher: PathMatcher| {
              annots
                .stepper
                .focused_paths
                .entry(line_pos)
                .or_default()
                .push(matcher)
            };
            if let Some(paths) = config.get("paths") {
              add_matcher(PathMatcher::Literal(paths.to_string()));
            }
            if let Some(rpaths) = config.get("rxpaths") {
              add_matcher(PathMatcher::Regex(rpaths.to_string()));
            }
          }
          "boundaries" => {
            annots.boundaries.focused_lines.push(line_pos);
          }
          _ => unreachable!(),
        }

        line = &line[matched.end() ..];
      }
      add_fragment!(line);
    }
    idx += 1; // for \n
    output_lines.push(fragments);
  }

  let output = Itertools::intersperse(output_lines.into_iter(), vec!["\n"])
    .flatten()
    .collect::<String>();
  Ok((output, annots))
}

#[test]
fn test_parse_annotations() {
  let input = r#"#fn main() {
let x = 1;`(focus,paths:x,rxpaths:y)`
`[]`let y = 2;`{}`
#}"#;
  let (cleaned, annot) = parse_annotations(input).unwrap();
  assert_eq!(
    cleaned,
    r#"fn main() {
let x = 1;
let y = 2;
}"#
  );
  assert_eq!(annot, AquascopeAnnotations {
    hidden_lines: vec![LinePos(1), LinePos(4)],
    interp: InterpAnnotations {
      state_locations: vec![CharPos(23)],
    },
    stepper: StepperAnnotations {
      focused_lines: vec![LinePos(2)],
      focused_paths: maplit::hashmap! {
        LinePos(2) => vec![PathMatcher::Literal("x".into()), PathMatcher::Regex("y".into())]
      }
    },
    boundaries: BoundariesAnnotations {
      focused_lines: vec![LinePos(3)]
    }
  });
}
