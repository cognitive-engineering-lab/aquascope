//! Parser for Aquascope code blocks within Markdown.

use std::{hash::Hash, ops::Range};

use nom::{
  branch::alt,
  bytes::complete::{tag, take_until},
  character::complete::{anychar, char, none_of},
  combinator::map,
  multi::many0,
  sequence::{preceded, separated_pair, tuple},
  IResult,
};
use nom_locate::LocatedSpan;

use crate::annotations::AquascopeAnnotations;

#[derive(PartialEq, Hash, Debug, Clone)]
pub struct AquascopeBlock {
  pub operation: String,
  pub config: Vec<(String, String)>,
  pub code: String,
  pub annotations: AquascopeAnnotations,
}

impl AquascopeBlock {
  fn parse(i: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, Self> {
    fn parse_sym(i: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, String> {
      let (i, v) = many0(none_of(",=\n"))(i)?;
      Ok((i, v.into_iter().collect::<String>()))
    }

    let mut parser = tuple((
      tag("```aquascope"),
      preceded(char(','), parse_sym),
      many0(preceded(
        char(','),
        alt((
          separated_pair(parse_sym, char('='), parse_sym),
          map(parse_sym, |s| (s, String::from("true"))),
        )),
      )),
      take_until("```"),
      tag("```"),
    ));
    let (i, (_, operation, config, code, _)) = parser(i)?;
    let code = code.fragment().trim();
    let (code, annotations) =
      crate::annotations::parse_annotations(code).unwrap();
    Ok((i, AquascopeBlock {
      operation,
      config,
      code,
      annotations,
    }))
  }

  pub fn parse_all(content: &str) -> Vec<(Range<usize>, Self)> {
    let mut content = LocatedSpan::new(content);
    let mut to_process = Vec::new();
    loop {
      if let Ok((next, block)) = AquascopeBlock::parse(content) {
        let range = content.location_offset() .. next.location_offset();
        to_process.push((range, block));
        content = next;
      } else {
        match anychar::<_, nom::error::Error<LocatedSpan<&str>>>(content) {
          Ok((next, _)) => {
            content = next;
          }
          Err(_) => break,
        }
      }
    }

    to_process
  }
}

#[test]
fn test_parse_block() {
  let inp = r#"```aquascope,interpreter,foo=bar,baz
content!
```"#;
  let s = |s: &str| s.to_string();
  let blocks = AquascopeBlock::parse_all(inp);
  assert_eq!(blocks, vec![(0 .. inp.len(), AquascopeBlock {
    operation: s("interpreter"),
    config: vec![(s("foo"), s("bar")), (s("baz"), s("true"))],
    code: s("content!"),
    annotations: Default::default()
  })]);
}
