use anyhow::Result;

const SRC_DIR: &str = "../../frontend/packages/aquascope-embed/dist/";
const DST_DIR: &str = "./js";

fn main() -> Result<()> {
  mdbook_preprocessor_utils::copy_assets(SRC_DIR, DST_DIR)
}
