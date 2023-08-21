use std::{fs, io::ErrorKind, path::Path};

use anyhow::{Error, Result};

const SRC_DIR: &str = "../../frontend/packages/aquascope-embed/dist/";
const DST_DIR: &str = "./js";

fn main() -> Result<()> {
  println!("cargo:rerun-if-changed={SRC_DIR}");

  let dst_dir = Path::new(DST_DIR);
  fs::create_dir_all(dst_dir)?;

  let src_entries = match fs::read_dir(SRC_DIR) {
    Ok(src_entries) => src_entries,
    Err(err) => match err.kind() {
      ErrorKind::NotFound => return Ok(()),
      _ => return Err(Error::new(err)),
    },
  };

  for entry in src_entries {
    let path = entry?.path();
    fs::copy(&path, dst_dir.join(path.file_name().unwrap()))?;
  }

  Ok(())
}
