#![allow(clippy::comparison_to_empty)]

use std::path::Path;

use anyhow::Result;
use mdbook_aquascope::AquascopePreprocessor;
use mdbook_preprocessor_utils::{
  mdbook::preprocess::PreprocessorContext, Asset, SimplePreprocessor,
};

mdbook_preprocessor_utils::asset_generator!("../dist/");

const FRONTEND_ASSETS: [Asset; 2] =
  [make_asset!("lib.js"), make_asset!("lib.css")];

struct AquascopePreprocessorWrapper(AquascopePreprocessor);

impl SimplePreprocessor for AquascopePreprocessorWrapper {
  fn name() -> &'static str {
    "aquascope"
  }

  fn build(_ctx: &PreprocessorContext) -> Result<Self> {
    Ok(AquascopePreprocessorWrapper(AquascopePreprocessor::new()?))
  }

  fn replacements(
    &self,
    _chapter_dir: &Path,
    content: &str,
  ) -> Result<Vec<(std::ops::Range<usize>, String)>> {
    self.0.replacements(content)
  }

  fn linked_assets(&self) -> Vec<Asset> {
    FRONTEND_ASSETS.to_vec()
  }

  fn all_assets(&self) -> Vec<Asset> {
    self.linked_assets()
  }

  fn finish(mut self) {
    self.0.save_cache();
  }
}

fn main() {
  mdbook_preprocessor_utils::main::<AquascopePreprocessorWrapper>()
}

#[cfg(test)]
mod test {
  use std::fs;

  use anyhow::Result;
  use mdbook_aquascope::CACHE_PATH;
  use mdbook_preprocessor_utils::testing::MdbookTestHarness;

  use super::AquascopePreprocessorWrapper;

  #[test]
  fn cache_test() -> Result<()> {
    let harness = MdbookTestHarness::new()?;
    let mk_contents = |x: &str| {
      format!(
        r#"
```aquascope,interpreter
fn main() {{
  let x = {x};
}}
```
    "#
      )
    };
    let chapter_path = harness.root().join("src/chapter_1.md");
    fs::write(&chapter_path, mk_contents("0"))?;

    let _book =
      harness.compile::<AquascopePreprocessorWrapper>(serde_json::json!({}))?;

    // After running the first compile, a cache should exist
    let cache_path = harness.root().join(CACHE_PATH);
    let cache_contents = fs::read(&cache_path)?;

    // After a second compile, the cache should be unchanged
    let _book =
      harness.compile::<AquascopePreprocessorWrapper>(serde_json::json!({}))?;
    assert_eq!(fs::read(&cache_path)?, cache_contents);

    // After modifying the chapter and recompiling, the cache should be changed
    fs::write(&chapter_path, mk_contents("1"))?;
    let _book =
      harness.compile::<AquascopePreprocessorWrapper>(serde_json::json!({}))?;
    assert_ne!(fs::read(&cache_path)?, cache_contents);

    Ok(())
  }
}
