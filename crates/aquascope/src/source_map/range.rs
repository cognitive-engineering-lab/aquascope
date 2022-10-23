//! Data structure for sharing [spans][Span] outside rustc.

use std::{default::Default, fs, path::Path};

use anyhow::{bail, Context, Result};
use rustc_data_structures::{fx::FxHashMap as HashMap, sync::Lrc};
use rustc_hir::{
  intravisit::{self, Visitor},
  BodyId,
};
use rustc_middle::ty::TyCtxt;
use rustc_span::{
  source_map::SourceMap, BytePos, FileName, RealFileName, SourceFile, Span,
};
use serde::Serialize;
use unicode_segmentation::UnicodeSegmentation;

use crate::cached::Cache;

pub struct GraphemeIndices {
  byte_to_char: HashMap<usize, usize>,
  char_to_byte: Vec<usize>,
}

impl GraphemeIndices {
  pub fn new(s: &str) -> Self {
    let mut char_to_byte = Vec::new();
    let mut idx = 0;
    for g in s.graphemes(true) {
      char_to_byte.push(idx);
      idx += g.as_bytes().len();
    }
    let byte_to_char = char_to_byte
      .iter()
      .enumerate()
      .map(|(idx, byte)| (*byte, idx))
      .chain([(char_to_byte.last().unwrap_or(&0) + 1, char_to_byte.len())])
      .collect::<HashMap<_, _>>();
    GraphemeIndices {
      byte_to_char,
      char_to_byte,
    }
  }

  pub fn from_path(path: impl AsRef<Path>) -> Result<Self> {
    let bytes = fs::read(path)?;
    let s = std::str::from_utf8(&bytes)?;
    Ok(Self::new(s))
  }

  pub fn byte_to_char(&self, byte: usize) -> usize {
    *self.byte_to_char.get(&byte).unwrap_or_else(|| {
      panic!(
        "Could not find byte {byte:?} in bytes {:?}",
        self.char_to_byte
      )
    })
  }

  pub fn char_to_byte(&self, chr: usize) -> usize {
    self.char_to_byte[chr]
  }
}

thread_local! {
  static GRAPHEME_INDICES: Cache<String, GraphemeIndices> = Cache::default();
  static SOURCE_FILES: Cache<String, Option<FileName>> = Cache::default();
}

fn qpath_to_span(tcx: TyCtxt, qpath: String) -> Result<Span> {
  struct Finder<'tcx> {
    tcx: TyCtxt<'tcx>,
    qpath: String,
    span: Option<Span>,
  }

  impl<'tcx> Visitor<'tcx> for Finder<'tcx> {
    fn visit_nested_body(&mut self, id: BodyId) {
      intravisit::walk_body(self, self.tcx.hir().body(id));

      let local_def_id = self.tcx.hir().body_owner_def_id(id);
      let function_path = self
        .tcx
        .def_path(local_def_id.to_def_id())
        .to_string_no_crate_verbose();
      if function_path[2 ..] == self.qpath {
        self.span = Some(self.tcx.hir().span(id.hir_id));
      }
    }
  }

  let mut finder = Finder {
    tcx,
    qpath,
    span: None,
  };
  tcx.hir().visit_all_item_likes_in_crate(&mut finder);
  finder
    .span
    .with_context(|| format!("No function with qpath {}", finder.qpath))
}

#[derive(Serialize, Debug, Clone, Hash, PartialEq, Eq, Default)]
pub struct Range {
  pub char_start: usize,
  pub char_end: usize,
  pub byte_start: usize,
  pub byte_end: usize,
  pub filename: String,
}

impl Range {
  pub fn substr(&self, s: &str) -> String {
    s[self.byte_start .. self.byte_end].to_string()
  }

  pub fn from_char_range(
    char_start: usize,
    char_end: usize,
    filename: &str,
    file: &GraphemeIndices,
  ) -> Self {
    let byte_start = file.char_to_byte(char_start);
    let byte_end = file.char_to_byte(char_end);
    Range {
      char_start,
      char_end,
      byte_start,
      byte_end,
      filename: filename.to_string(),
    }
  }

  pub fn from_byte_range(
    byte_start: usize,
    byte_end: usize,
    filename: &str,
    file: &GraphemeIndices,
  ) -> Self {
    let char_start = file.byte_to_char(byte_start);
    let char_end = file.byte_to_char(byte_end);
    Range {
      byte_start,
      byte_end,
      char_start,
      char_end,
      filename: filename.to_string(),
    }
  }

  pub fn from_span(span: Span, source_map: &SourceMap) -> Result<Self> {
    log::trace!("Converting to range: {span:?}");
    let file = source_map.lookup_source_file(span.lo());
    let filename = match &file.name {
      FileName::Real(RealFileName::LocalPath(filename)) => {
        filename.to_string_lossy().into_owned()
      }
      filename => bail!("Range::from_span doesn't support {filename:?}"),
    };

    assert!(
      source_map.ensure_source_file_source_present(file.clone()),
      "Could not load source for file: {:?}",
      file.name
    );
    let external = file.external_src.borrow();
    let src = file
      .src
      .as_ref()
      .unwrap_or_else(|| external.get_source().as_ref().unwrap());

    let byte_start = source_map.lookup_byte_offset(span.lo()).pos.0 as usize;
    let byte_end = source_map.lookup_byte_offset(span.hi()).pos.0 as usize;

    GRAPHEME_INDICES.with(|grapheme_indices| {
      let indices =
        grapheme_indices.get(filename.clone(), |_| GraphemeIndices::new(src));
      Ok(Self::from_byte_range(
        byte_start, byte_end, &filename, indices,
      ))
    })
  }

  pub fn source_file<'a>(
    &self,
    source_map: &'a SourceMap,
  ) -> Result<Lrc<SourceFile>> {
    let files = source_map.files();
    SOURCE_FILES.with(|source_files| {
      let filename = source_files.get(self.filename.clone(), |filename| {
        let filename = Path::new(&filename);
        let filename = filename
          .canonicalize()
          .unwrap_or_else(|_| filename.to_path_buf());
        files
          .iter()
          .map(|file| &file.name)
          .find(|name| match &name {
            // rustc seems to store relative paths to files in the workspace, so if filename is absolute,
            // we can compare them using Path::ends_with
            FileName::Real(RealFileName::LocalPath(other)) => {
              let canonical = other.canonicalize();
              let other = canonical.as_ref().unwrap_or(other);
              filename.ends_with(other)
            }
            _ => false,
          })
          .cloned()
      });
      let filename = filename.as_ref().with_context(|| {
        format!(
          "Could not find SourceFile for path: {}. Available SourceFiles were: [{}]",
          self.filename,
          files
            .iter()
            .filter_map(|file| match &file.name {
              FileName::Real(RealFileName::LocalPath(other)) =>
                Some(format!("{}", other.display())),
              _ => None,
            })
            .collect::<Vec<_>>()
            .join(", ")
        )
      })?;

      Ok(source_map.get_source_file(filename).unwrap())
    })
  }
}

pub trait ToSpan: Send + Sync {
  fn to_span(&self, tcx: TyCtxt) -> Result<Span>;
}

impl ToSpan for Range {
  fn to_span(&self, tcx: TyCtxt) -> Result<Span> {
    let source_map = tcx.sess.source_map();
    let source_file = self.source_file(source_map)?;
    let offset = source_file.start_pos;

    Ok(Span::with_root_ctxt(
      offset + BytePos(self.byte_start as u32),
      offset + BytePos(self.byte_end as u32),
    ))
  }
}

pub enum FunctionIdentifier {
  Qpath(String),
  Range(Range),
}

impl ToSpan for FunctionIdentifier {
  fn to_span(&self, tcx: TyCtxt) -> Result<Span> {
    match self {
      FunctionIdentifier::Qpath(qpath) => qpath_to_span(tcx, qpath.clone()),
      FunctionIdentifier::Range(range) => range.to_span(tcx),
    }
  }
}
