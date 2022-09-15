//! APIs for efficiently representing values with an fixed-size domain.

use std::{
  fmt,
  hash::Hash,
  ops::{Deref, DerefMut},
  rc::Rc,
};

use rustc_data_structures::fx::FxHashMap as HashMap;
use rustc_index::{
  bit_set::BitSet,
  vec::{Idx, IndexVec},
};
use rustc_mir_dataflow::{fmt::DebugWithContext, JoinSemiLattice};

pub mod impls;

pub trait IndexedValue: Eq + Hash + Clone + fmt::Debug {
  type Index: Idx + ToIndex<Self>;
  type Domain: IndexedDomain<Index = Self::Index, Value = Self> =
    DefaultDomain<Self::Index, Self>;
}

/// Converts an element to the index representing that element.
///
/// Useful for enabling a more flexible API where either an element or its
/// index can be given as input. By convention, `ToIndex` should be implemented
/// for an index, see [`to_index_impl`].
pub trait ToIndex<T: IndexedValue> {
  fn to_index(&self, domain: &T::Domain) -> T::Index;
}

impl<T: IndexedValue> ToIndex<T> for T {
  fn to_index(&self, domain: &T::Domain) -> T::Index {
    domain.index(self)
  }
}

impl<T: IndexedValue> ToIndex<T> for &T {
  fn to_index(&self, domain: &T::Domain) -> T::Index {
    domain.index(self)
  }
}

/// Implements [`ToIndex`] for the index of an indexed type.
// Can't make this a blanket impl b/c it conflicts with the blanket impl above :(
#[macro_export]
macro_rules! to_index_impl {
  ($t:ty) => {
    impl ToIndex<$t> for <$t as IndexedValue>::Index {
      fn to_index(
        &self,
        _domain: &<$t as IndexedValue>::Domain,
      ) -> <$t as IndexedValue>::Index {
        *self
      }
    }
  };
}

/// Represents a fixed size domain of elements where each element has an index.
///
/// Enables the bidirectional mapping from element to index and vice-versa.
pub trait IndexedDomain {
  type Value: IndexedValue;
  type Index: Idx = <Self::Value as IndexedValue>::Index;
  fn value(&self, index: Self::Index) -> &Self::Value;
  fn index(&self, value: &Self::Value) -> Self::Index;
  fn contains(&self, value: &Self::Value) -> bool;
  fn as_vec(&self) -> &IndexVec<Self::Index, Self::Value>;
  fn size(&self) -> usize {
    self.as_vec().len()
  }

  fn as_set(
    domain: &Rc<<Self::Value as IndexedValue>::Domain>,
  ) -> IndexSet<Self::Value> {
    let mut set = IndexSetImpl::new_empty(domain.size());
    set.insert_all();
    IndexSet {
      set: OwnedSet(set),
      domain: domain.clone(),
    }
  }
}

#[derive(Clone)]
pub struct DefaultDomain<I: Idx, T> {
  index_to_value: IndexVec<I, T>,
  value_to_index: HashMap<T, I>,
}

impl<I: Idx, T: IndexedValue> DefaultDomain<I, T> {
  pub fn new(domain: Vec<T>) -> Self {
    let index_to_value = IndexVec::from_raw(domain);
    let value_to_index = index_to_value
      .iter_enumerated()
      .map(|(idx, t)| (t.clone(), idx))
      .collect();
    DefaultDomain {
      index_to_value,
      value_to_index,
    }
  }
}

impl<I: Idx, T: IndexedValue> IndexedDomain for DefaultDomain<I, T> {
  type Index = I;
  type Value = T;

  fn value(&self, index: I) -> &T {
    self.index_to_value.get(index).unwrap()
  }

  fn index(&self, value: &T) -> I {
    *self
      .value_to_index
      .get(value)
      .unwrap_or_else(|| panic!("No index for value: {value:?}"))
  }

  fn contains(&self, value: &T) -> bool {
    self.value_to_index.contains_key(value)
  }

  fn as_vec(&self) -> &IndexVec<Self::Index, Self::Value> {
    &self.index_to_value
  }
}

pub type IndexSetImpl<T> = BitSet<T>;

#[derive(Clone)]
pub struct OwnedSet<T: IndexedValue>(IndexSetImpl<T::Index>);
#[derive(Clone, Copy)]
pub struct RefSet<'a, T: IndexedValue>(&'a IndexSetImpl<T::Index>);
pub struct MutSet<'a, T: IndexedValue>(&'a mut IndexSetImpl<T::Index>);

impl<T: IndexedValue> Deref for OwnedSet<T> {
  type Target = IndexSetImpl<T::Index>;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl<T: IndexedValue> DerefMut for OwnedSet<T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

impl<T: IndexedValue> Deref for RefSet<'_, T> {
  type Target = IndexSetImpl<T::Index>;

  fn deref(&self) -> &Self::Target {
    self.0
  }
}

impl<T: IndexedValue> Deref for MutSet<'_, T> {
  type Target = IndexSetImpl<T::Index>;

  fn deref(&self) -> &Self::Target {
    self.0
  }
}

impl<T: IndexedValue> DerefMut for MutSet<'_, T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    self.0
  }
}

pub trait ToSet<T: IndexedValue>:
  Deref<Target = IndexSetImpl<T::Index>>
{
}
pub trait ToSetMut<T: IndexedValue>:
  DerefMut<Target = IndexSetImpl<T::Index>>
{
}

impl<S: Deref<Target = IndexSetImpl<T::Index>>, T: IndexedValue> ToSet<T>
  for S
{
}
impl<S: DerefMut<Target = IndexSetImpl<T::Index>>, T: IndexedValue> ToSetMut<T>
  for S
{
}

/// High-level wrapper around rustc's bitset (specifically [`IndexSetImpl`]).
///
/// This data structure ties a bitset to its [`IndexedDomain`], enabling inputs to be
/// represented either as values or the index of that value in the domain (see [`ToIndex`]).
///
/// To deal with references to bitsets, e.g. as returned by [`IndexMatrix::row_set`],
/// `IndexSet` has an additional type parameter for whether it contains an [`OwnedSet`]
/// or a [`RefSet`] / [`MutSet`].
pub struct IndexSet<T: IndexedValue, S = OwnedSet<T>> {
  set: S,
  domain: Rc<T::Domain>,
}

impl<T: IndexedValue> IndexSet<T, OwnedSet<T>> {
  pub fn new(domain: &Rc<T::Domain>) -> Self {
    IndexSet {
      set: OwnedSet(IndexSetImpl::new_empty(domain.as_vec().len())),
      domain: domain.clone(),
    }
  }
}

impl<T, S> IndexSet<T, S>
where
  T: IndexedValue,
  S: ToSet<T>,
{
  pub fn to_owned(&self) -> IndexSet<T, OwnedSet<T>> {
    IndexSet {
      set: OwnedSet(self.set.clone()),
      domain: self.domain.clone(),
    }
  }

  pub fn as_ref(&self) -> IndexSet<T, RefSet<T>> {
    IndexSet {
      set: RefSet(&*self.set),
      domain: self.domain.clone(),
    }
  }

  pub fn indices(&self) -> impl Iterator<Item = T::Index> + '_ {
    self.set.iter()
  }

  pub fn iter(&self) -> impl Iterator<Item = &T> + '_ {
    self.set.iter().map(move |index| self.domain.value(index))
  }

  pub fn iter_enumerated(&self) -> impl Iterator<Item = (T::Index, &T)> + '_ {
    self
      .set
      .iter()
      .map(move |index| (index, self.domain.value(index)))
  }

  pub fn contains(&self, index: impl ToIndex<T>) -> bool {
    let elem = index.to_index(&self.domain);
    self.set.contains(elem)
  }

  pub fn len(&self) -> usize {
    self.set.count()
  }

  pub fn is_empty(&self) -> bool {
    self.len() == 0
  }

  pub fn is_superset<S2: ToSet<T>>(&self, other: &IndexSet<T, S2>) -> bool {
    self.set.superset(&*other.set)
  }
}

impl<T: IndexedValue, S: ToSetMut<T>> IndexSet<T, S> {
  pub fn insert(&mut self, elt: impl ToIndex<T>) {
    let elt = elt.to_index(&self.domain);
    self.set.insert(elt);
  }

  pub fn union<S2: ToSet<T>>(&mut self, other: &IndexSet<T, S2>) -> bool {
    self.set.union(&*other.set)
  }

  pub fn subtract<S2: ToSet<T>>(&mut self, other: &IndexSet<T, S2>) -> bool {
    self.set.subtract(&*other.set)
  }

  pub fn intersect<S2: ToSet<T>>(&mut self, other: &IndexSet<T, S2>) -> bool {
    self.set.intersect(&*other.set)
  }

  pub fn insert_all(&mut self) {
    self.set.insert_all();
  }
}

impl<T: IndexedValue, S: ToSet<T>> PartialEq for IndexSet<T, S> {
  fn eq(&self, other: &Self) -> bool {
    *self.set == *other.set
  }
}

impl<T: IndexedValue, S: ToSet<T>> Eq for IndexSet<T, S> {}

impl<T: IndexedValue, S: ToSetMut<T>> JoinSemiLattice for IndexSet<T, S> {
  fn join(&mut self, other: &Self) -> bool {
    self.union(other)
  }
}

impl<T: IndexedValue> Clone for IndexSet<T> {
  fn clone(&self) -> Self {
    IndexSet {
      set: self.set.clone(),
      domain: self.domain.clone(),
    }
  }

  fn clone_from(&mut self, source: &Self) {
    self.set.clone_from(&source.set);
    self.domain = source.domain.clone();
  }
}

impl<T: IndexedValue + fmt::Debug, S: ToSet<T>> fmt::Debug for IndexSet<T, S> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{{")?;
    let n = self.len();
    for (i, elt) in self.iter().enumerate() {
      write!(f, "{elt:?}")?;
      if i < n - 1 {
        write!(f, ", ")?;
        if f.alternate() {
          write!(f, "\n  ")?;
        }
      }
    }

    write!(f, "}}")
  }
}

struct Escape<T>(T);
impl<T: fmt::Debug> fmt::Display for Escape<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    cfg_if::cfg_if! {
      if #[cfg(feature = "debug")] {
        write!(f, "{}", html_escape::encode_text(&format!("{:?}", self.0)))
      } else {
        write!(f, "{:?}", self.0)
      }
    }
  }
}

impl<T: IndexedValue + fmt::Debug, S: ToSet<T>, C> DebugWithContext<C>
  for IndexSet<T, S>
where
  T::Index: ToIndex<T>,
{
  fn fmt_with(&self, _ctxt: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let elts = self.iter().collect::<Vec<_>>();
    // elts.sort();
    write!(f, "{}", Escape(elts))
  }

  fn fmt_diff_with(
    &self,
    old: &Self,
    ctxt: &C,
    f: &mut fmt::Formatter<'_>,
  ) -> fmt::Result {
    if self == old {
      return Ok(());
    }

    let added = self
      .indices()
      .filter(|idx| !old.contains(*idx))
      .collect_indices(&self.domain);
    let removed = old
      .indices()
      .filter(|idx| !self.contains(*idx))
      .collect_indices(&self.domain);

    if added.len() > 0 {
      write!(f, "\u{001f}+")?;
      added.fmt_with(ctxt, f)?;
    }

    if removed.len() > 0 {
      write!(f, "\u{001f}-")?;
      removed.fmt_with(ctxt, f)?;
    }

    Ok(())
  }
}
pub trait IndexSetIteratorExt<T: IndexedValue> {
  fn collect_indices(self, domain: &Rc<T::Domain>) -> IndexSet<T>;
}

impl<T, S, Iter> IndexSetIteratorExt<T> for Iter
where
  T: IndexedValue,
  Iter: Iterator<Item = S>,
  S: ToIndex<T>,
{
  fn collect_indices(self, domain: &Rc<T::Domain>) -> IndexSet<T> {
    let mut set = IndexSet::new(domain);
    for s in self {
      set.insert(s);
    }
    set
  }
}

pub struct IndexMatrix<R, C: IndexedValue> {
  matrix: HashMap<R, IndexSetImpl<C::Index>>,
  empty_set: IndexSetImpl<C::Index>,
  pub col_domain: Rc<C::Domain>,
}

pub trait RowBounds = Hash + PartialEq + Eq + Copy;

impl<R: RowBounds, C: IndexedValue> IndexMatrix<R, C> {
  pub fn new(col_domain: &Rc<C::Domain>) -> Self {
    IndexMatrix {
      matrix: HashMap::default(),
      empty_set: IndexSetImpl::new_empty(col_domain.size()),
      col_domain: col_domain.clone(),
    }
  }

  fn ensure_row(&mut self, row: R) -> &mut IndexSetImpl<C::Index> {
    self
      .matrix
      .entry(row)
      .or_insert_with(|| IndexSetImpl::new_empty(self.col_domain.size()))
  }

  pub fn insert(&mut self, row: R, col: impl ToIndex<C>) -> bool {
    let col = col.to_index(&self.col_domain);
    self.ensure_row(row).insert(col)
  }

  pub fn union_into_row<S2>(&mut self, into: R, from: &IndexSet<C, S2>) -> bool
  where
    S2: ToSet<C>,
  {
    self.ensure_row(into).union(&*from.set)
  }

  pub fn union_rows(&mut self, from: R, to: R) -> bool {
    if from == to {
      return false;
    }

    self.ensure_row(from);
    self.ensure_row(to);

    let from = self.row_set(from);
    let to = self.row_set(to);
    // SAFETY: `from` != `to` therefore this is a disjoint mutable borrow
    let mut to =
      unsafe { std::mem::transmute::<_, IndexSet<C, MutSet<C>>>(to) };
    to.union(&from)
  }

  pub fn row<'a>(&'a self, row: R) -> impl Iterator<Item = &'a C> + 'a {
    self.matrix.get(&row).into_iter().flat_map(move |set| {
      set.iter().map(move |idx| self.col_domain.value(idx))
    })
  }

  // This use to return Option<...> for the empty case, but in my experience it's usually fine to return an empty set
  // and reduces the amount of error handling at the user's end.
  pub fn row_set<'a>(&'a self, row: R) -> IndexSet<C, RefSet<'a, C>> {
    let set = self.matrix.get(&row).unwrap_or(&self.empty_set);
    IndexSet {
      set: RefSet(set),
      domain: self.col_domain.clone(),
    }
  }

  pub fn rows<'a>(
    &'a self,
  ) -> impl Iterator<Item = (R, IndexSet<C, RefSet<'a, C>>)> + 'a {
    self.matrix.iter().map(move |(row, col)| {
      (*row, IndexSet {
        set: RefSet(col),
        domain: self.col_domain.clone(),
      })
    })
  }

  pub fn clear_row(&mut self, row: R) {
    self.matrix.remove(&row);
  }
}

impl<R: RowBounds, C: IndexedValue> PartialEq for IndexMatrix<R, C> {
  fn eq(&self, other: &Self) -> bool {
    self.matrix.len() == other.matrix.len()
      && self
        .matrix
        .iter()
        .all(|(row, col)| match other.matrix.get(row) {
          Some(other_col) => col == other_col,
          None => false,
        })
  }
}

impl<R: RowBounds, C: IndexedValue> Eq for IndexMatrix<R, C> {}

impl<R: RowBounds, C: IndexedValue> JoinSemiLattice for IndexMatrix<R, C> {
  fn join(&mut self, other: &Self) -> bool {
    let mut changed = false;
    for (row, col) in other.matrix.iter() {
      changed |= self.ensure_row(*row).union(col);
    }
    return changed;
  }
}

impl<R: RowBounds, C: IndexedValue> Clone for IndexMatrix<R, C> {
  fn clone(&self) -> Self {
    Self {
      matrix: self.matrix.clone(),
      empty_set: self.empty_set.clone(),
      col_domain: self.col_domain.clone(),
    }
  }

  fn clone_from(&mut self, source: &Self) {
    for col in self.matrix.values_mut() {
      col.clear();
    }

    for (row, col) in source.matrix.iter() {
      self.ensure_row(*row).clone_from(col);
    }

    self.empty_set = source.empty_set.clone();
    self.col_domain = source.col_domain.clone();
  }
}

impl<R: RowBounds + fmt::Debug, C: IndexedValue + fmt::Debug> fmt::Debug
  for IndexMatrix<R, C>
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{{")?;

    for row in self.matrix.keys() {
      let n = self.matrix.get(row).map(|set| set.count()).unwrap_or(0);
      if n == 0 {
        continue;
      }

      write!(f, "  {row:?}: {:?},", self.matrix.get(row).unwrap())?;
    }

    write!(f, "}}")
  }
}

impl<R: RowBounds + fmt::Debug, C: IndexedValue + fmt::Debug, Ctx>
  DebugWithContext<Ctx> for IndexMatrix<R, C>
{
  fn fmt_with(&self, ctxt: &Ctx, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{{")?;

    for row in self.matrix.keys() {
      let row_set = self.row_set(*row);
      if row_set.len() == 0 {
        continue;
      }

      write!(f, "  {}: ", Escape(*row))?;
      row_set.fmt_with(ctxt, f)?;
      write!(f, "]<br align=\"left\" />")?;
    }

    write!(f, "}}<br align=\"left\" />")
  }

  fn fmt_diff_with(
    &self,
    old: &Self,
    ctxt: &Ctx,
    f: &mut fmt::Formatter<'_>,
  ) -> fmt::Result {
    if self == old {
      return Ok(());
    }

    for (row, set) in self.rows() {
      let old_set = old.row_set(row);

      if old_set == set {
        continue;
      }

      write!(f, "{}: ", Escape(row))?;
      set.fmt_diff_with(&old_set, ctxt, f)?;
      writeln!(f)?;
    }

    Ok(())
  }
}
