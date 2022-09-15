use intervaltree::IntervalTree;
use rustc_span::{source_map::Spanned, BytePos, SpanData};

pub struct SpanTree<T> {
  tree: IntervalTree<BytePos, (SpanData, T)>,
  len: usize,
}

impl<T> SpanTree<T> {
  pub fn new(spans: impl IntoIterator<Item = Spanned<T>>) -> Self {
    let tree = spans
      .into_iter()
      .map(|spanned| {
        let data = spanned.span.data();
        (data.lo .. data.hi, (data, spanned.node))
      })
      .collect::<IntervalTree<_, _>>();
    let len = tree.iter().count();
    SpanTree { tree, len }
  }

  pub fn len(&self) -> usize {
    self.len
  }

  pub fn iter<'a>(&'a self) -> impl Iterator<Item = &'a T> + 'a {
    self.tree.iter().map(|el| &el.value.1)
  }

  pub fn overlapping<'a>(
    &'a self,
    query: SpanData,
  ) -> impl Iterator<Item = &'a (SpanData, T)> + 'a {
    self.tree.query(query.lo .. query.hi).map(|el| &el.value)
  }
}

#[cfg(test)]
mod test {
  use rustc_span::SyntaxContext;

  use super::*;

  #[test]
  fn span_tree_test() {
    rustc_span::create_default_session_if_not_set_then(|_| {
      let mk_span = |lo, hi| SpanData {
        lo: BytePos(lo),
        hi: BytePos(hi),
        ctxt: SyntaxContext::root(),
        parent: None,
      };
      let mk = |node, lo, hi| Spanned {
        span: mk_span(lo, hi).span(),
        node,
      };

      let input = [mk("a", 0, 1), mk("b", 2, 3), mk("c", 0, 5)];
      let tree = SpanTree::new(input);

      let query = |lo, hi| {
        let mut result = tree
          .overlapping(mk_span(lo, hi))
          .map(|(_, t)| t)
          .copied()
          .collect::<Vec<_>>();
        result.sort();
        result
      };

      assert_eq!(query(0, 2), ["a", "c"]);
      assert_eq!(query(0, 3), ["a", "b", "c"]);
      assert_eq!(query(2, 3), ["b", "c"]);
      assert_eq!(query(6, 8), [] as [&str; 0]);
    });
  }
}
