//! Disk cache for persisting computed Aquascope results.

use std::{
  collections::{hash_map::DefaultHasher, HashMap},
  fs::{File, OpenOptions},
  hash::{Hash, Hasher},
  io::{Seek, Write},
  marker::PhantomData,
};

use anyhow::Result;
use flate2::{read::GzDecoder, write::GzEncoder, Compression};
use serde::{de::DeserializeOwned, Serialize};

pub struct Cache<K, V> {
  cache_file: File,
  cache: HashMap<u64, V>,
  dirty: bool,
  _key: PhantomData<K>,
}

pub const CACHE_PATH: &str = ".aquascope-cache";

fn hash<K: Hash>(k: &K) -> u64 {
  let mut hasher = DefaultHasher::new();
  k.hash(&mut hasher);
  hasher.finish()
}

impl<K: Hash, V: Serialize + DeserializeOwned> Cache<K, V> {
  pub fn load() -> Result<Self> {
    let cache_file = OpenOptions::new()
      .read(true)
      .write(true)
      .create(true)
      .truncate(true)
      .open(CACHE_PATH)?;
    let cache = if cache_file.metadata()?.len() == 0 {
      HashMap::new()
    } else {
      let mut decoder = GzDecoder::new(&cache_file);
      serde_json::from_reader(&mut decoder).unwrap_or_else(|e| {
        eprintln!("Warning: failed to read Aquascope cache with error {e}");
        HashMap::new()
      })
    };
    Ok(Cache {
      cache,
      cache_file,
      dirty: false,
      _key: PhantomData,
    })
  }

  pub fn get(&self, key: &K) -> Option<&V> {
    self.cache.get(&hash(key))
  }

  pub fn set(&mut self, key: K, value: V) {
    self.cache.insert(hash(&key), value);
    self.dirty = true;
  }

  pub fn save(&mut self) -> Result<()> {
    if self.dirty {
      self.cache_file.set_len(0)?;
      self.cache_file.rewind()?;
      let mut encoder =
        GzEncoder::new(&mut self.cache_file, Compression::best());
      serde_json::to_writer(&mut encoder, &self.cache)?;
      encoder.finish()?;
      self.cache_file.flush()?;
    }

    Ok(())
  }
}
