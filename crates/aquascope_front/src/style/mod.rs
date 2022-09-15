//! Basic syntax highlighting functionality.
//!
//! This module uses librustc_ast's lexer to provide token-based highlighting for
//! the Aquascope frontend.
//!
//! This source code was modified from rustdoc::html::highlight
//! https://doc.rust-lang.org/stable/nightly-rustc/src/rustdoc/html/highlight.rs.html#1-805
//!
//! Use the `fontify_source` to highlight some rust code.

use std::{borrow::Cow, io::BufRead, path::Path};

use getopts::Options;
use syntect::{
  dumps::{dump_to_file, from_dump_file},
  easy::HighlightFile,
  highlighting::{Style, Theme, ThemeSet},
  parsing::SyntaxSet,
  util::as_24_bit_terminal_escaped,
};

fn load_theme(tm_file: &str, enable_caching: bool) -> Theme {
  let tm_path = Path::new(tm_file);

  if enable_caching {
    let tm_cache = tm_path.with_extension("tmdump");

    if tm_cache.exists() {
      from_dump_file(tm_cache).unwrap()
    } else {
      let theme = ThemeSet::get_theme(tm_path).unwrap();
      dump_to_file(&theme, tm_cache).unwrap();
      theme
    }
  } else {
    ThemeSet::get_theme(tm_path).unwrap()
  }
}

// TODO include Ranges in the output so each token can be mapped to the
// original source file contents.
pub fn stylize_source(
  filename: String,
  args: Vec<String>,
) -> Vec<Vec<(Style, String)>> {
  let ss = SyntaxSet::load_defaults_newlines();
  let ts = ThemeSet::load_defaults();
  let theme_file: String = "base16-ocean.light".to_string();

  let theme = ts
    .themes
    .get(&theme_file)
    .map(Cow::Borrowed)
    .unwrap_or_else(|| {
      Cow::Owned(load_theme(
        &theme_file,
        false, // matches.opt_present("cache-theme")
      ))
    });

  let mut highlighter = HighlightFile::new(filename, &ss, &theme).unwrap();

  highlighter
    .reader
    .lines()
    .map(|line| {
      let regions: Vec<(Style, String)> = highlighter
        .highlight_lines
        .highlight_line(&line.unwrap(), &ss)
        .unwrap()
        .iter()
        .map(|(style, s)| (*style, String::from(*s)))
        .collect();
      regions
    })
    .collect()
}
