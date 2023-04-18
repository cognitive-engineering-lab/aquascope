use rustc_utils::source_map::{
  filename::FilenameIndex,
  range::{CharPos, CharRange},
};
use ts_rs::TS;

fn main() -> anyhow::Result<()> {
  FilenameIndex::export()?;
  CharPos::export()?;
  CharRange::export()?;

  Ok(())
}
