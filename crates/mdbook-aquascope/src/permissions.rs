//! Parses inline permissions outside of an Aquascope block.

use anyhow::{bail, Result};
use regex::Regex;

const VALID_PERMISSIONS: &[&str] = &["read", "write", "own"];

pub type Replacement = (std::ops::Range<usize>, String);

pub fn parse_perms(
  content: &'_ str,
) -> impl Iterator<Item = Result<Replacement>> + '_ {
  lazy_static::lazy_static! {
      static ref RE: Regex = Regex::new(r"@Perm(\[[^\]]*\])?\{([^}]*)\}").unwrap();
  }

  RE.captures_iter(content)
    .map(move |cap| {
      let range = cap.get(0).unwrap().range();
      let perm = cap.get(2).unwrap().as_str();
      if !VALID_PERMISSIONS.contains(&perm) {
          bail!("Invalid permission: {perm}");
      }

      let letter = perm.chars().next().unwrap().to_ascii_uppercase();
      let perm_html = format!(r#"<span class="perm {perm}">{letter}</span>"#);
      let html = match cap.get(1) {
        Some(options) => match options.as_str() {
          "[gained]" => format!(
            r#"<span><span class="perm-diff-add">+</span>{perm_html}</span>"#
          ),
          "[lost]" => format!(
            r#"<span class="perm-diff-sub-container"><div class="perm-diff-sub"></div>{perm_html}</span>"#
          ),
          "[missing]" => format!(r#"<span class="perm missing {perm}">{letter}</span>"#),
          _ => unimplemented!(),
        },
        None => perm_html,
      };
      Ok((range, html))
    })
}

#[cfg(test)]
mod test {
  use super::*;
  fn parse(s: &str) -> Vec<Replacement> {
    parse_perms(s).collect::<Result<Vec<_>>>().unwrap()
  }

  #[test]
  fn test_parse_perms() {
    let s = "Hello @Perm{read} world";
    assert_eq!(parse(s), vec![
      ((6 .. 17, String::from(r#"<span class="perm read">R</span>"#)))
    ]);

    let s = "Hello @Perm[lost]{read} world";
    assert_eq!(parse(s), vec![
      ((
        6 .. 23,
        String::from(
          r#"<span class="perm-diff-sub-container"><div class="perm-diff-sub"></div><span class="perm read">R</span></span>"#
        )
      ))
    ]);
  }

  #[test]
  #[should_panic]
  fn test_parse_perms_fail() {
    let s = "@Perm{not-a-perm}";
    parse(s);
  }
}
