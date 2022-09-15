//! Data structure for memoizing computations.

use std::{cell::RefCell, hash::Hash, mem, pin::Pin};

use rustc_data_structures::fx::FxHashMap as HashMap;

pub struct Cache<In, Out>(RefCell<HashMap<In, Pin<Box<Out>>>>);

impl<In, Out> Cache<In, Out>
where
  In: Hash + Eq + Clone,
  Out: Unpin,
{
  pub fn get<'a>(
    &'a self,
    key: In,
    compute: impl FnOnce(In) -> Out,
  ) -> &'a Out {
    if !self.0.borrow().contains_key(&key) {
      let out = Pin::new(Box::new(compute(key.clone())));
      self.0.borrow_mut().insert(key.clone(), out);
    }

    let cache = self.0.borrow();
    let entry = cache.get(&key).unwrap();

    // SAFETY: because the entry is pinned, it cannot move and this pointer will
    // only be invalidated if Cache is dropped. The returned reference has a lifetime
    // equal to Cache, so Cache cannot be dropped before this reference goes out of scope.
    unsafe { mem::transmute::<&'_ Out, &'a Out>(&**entry) }
  }
}

impl<In, Out> Default for Cache<In, Out> {
  fn default() -> Self {
    Cache(RefCell::new(HashMap::default()))
  }
}

pub struct CopyCache<In, Out>(RefCell<HashMap<In, Out>>);

impl<In, Out> CopyCache<In, Out>
where
  In: Hash + Eq + Clone,
  Out: Copy,
{
  pub fn get<'a>(&'a self, key: In, compute: impl FnOnce(In) -> Out) -> Out {
    let mut cache = self.0.borrow_mut();
    *cache
      .entry(key.clone())
      .or_insert_with(move || compute(key))
  }
}

impl<In, Out> Default for CopyCache<In, Out> {
  fn default() -> Self {
    CopyCache(RefCell::new(HashMap::default()))
  }
}

#[test]
fn test_cached() {
  let cache: Cache<usize, usize> = Cache::default();
  let x = cache.get(0, |_| 0);
  let y = cache.get(1, |_| 1);
  let z = cache.get(0, |_| 2);
  assert_eq!(*x, 0);
  assert_eq!(*y, 1);
  assert_eq!(*z, 0);
  assert!(std::ptr::eq(x, z));
}
