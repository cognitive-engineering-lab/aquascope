use hir::LoopSource;
use log::trace;
use rustc_hir::{
  self as hir,
  intravisit::{self, Visitor as HirVisitor},
  Expr, ExprKind, MatchSource, Node, Param, Stmt,
};
use rustc_span::{BytePos, Span, SpanData};

use super::Spanner;
use crate::mir::utils::SpanExt;

// Collect all the spans for children beneath the visited node.
// For example, when visiting "if true { 1 } else { 2 }" then we
// should collect: "true" "1" "2"
struct ChildExprSpans {
  spans: Vec<Span>,
  item_span: Span,
}
impl<'hir> HirVisitor<'hir> for ChildExprSpans {
  fn visit_expr(&mut self, ex: &hir::Expr) {
    match ex.kind {
      // Don't take the span for the whole block, since we want to leave
      // curly braces to be associated with the outer statement
      ExprKind::Block(..) => {
        intravisit::walk_expr(self, ex);
      }
      // The HIR span for a for-loop desugared to a match is *smaller*
      // than the span of its children. So we have to explicitly recurse
      // into the match arm instead of just taking the span for the match.
      // See `forloop_some_relevant` for where this matters.
      ExprKind::Match(_, arms, MatchSource::ForLoopDesugar) => {
        for arm in arms {
          intravisit::walk_arm(self, arm);
        }
      }
      _ => {
        if let Some(span) = ex.span.as_local(self.item_span) {
          self.spans.push(span);
        }
      }
    }
  }

  fn visit_arm(&mut self, arm: &hir::Arm) {
    // We want the arm condition to be included in the outer span for the match,
    // so we only visit the arm body here.
    self.visit_expr(arm.body);
  }

  fn visit_stmt(&mut self, stmt: &hir::Stmt) {
    if let Some(span) = stmt.span.as_local(self.item_span) {
      self.spans.push(span);
    }
  }
}

#[derive(Clone, Copy)]
pub enum EnclosingHirSpans {
  OuterOnly,
  Full,
  None,
}

#[derive(Clone, Debug)]
pub struct HirSpannedNode<'hir> {
  pub full: SpanData,
  pub outer: Vec<Span>,
  pub node: hir::Node<'hir>,
}

impl HirSpannedNode<'_> {
  pub fn get_spans(&self, span_type: EnclosingHirSpans) -> Vec<Span> {
    match span_type {
      EnclosingHirSpans::OuterOnly => self.outer.clone(),
      EnclosingHirSpans::Full => vec![self.full.span()],
      EnclosingHirSpans::None => Vec::new(),
    }
  }
}

pub struct HirSpanCollector<'a, 'hir, 'tcx>(pub &'a mut Spanner<'hir, 'tcx>);

macro_rules! try_span {
  ($self:expr, $span:expr) => {
    match $span.as_local($self.0.item_span) {
      Some(span) if !$self.0.invalid_span(span) => span,
      _ => {
        return;
      }
    }
  };
}

fn expr_to_string(ex: &Expr) -> String {
  rustc_hir_pretty::to_string(rustc_hir_pretty::NO_ANN, |s| s.print_expr(ex))
}

impl<'hir> HirVisitor<'hir> for HirSpanCollector<'_, 'hir, '_> {
  fn visit_expr(&mut self, expr: &'hir hir::Expr<'hir>) {
    intravisit::walk_expr(self, expr);

    let span = try_span!(self, expr.span);

    let inner_spans = match expr.kind {
      ExprKind::Loop(_, _, loop_source, header) => match loop_source {
        LoopSource::ForLoop | LoopSource::While => {
          vec![expr.span.trim_start(header).unwrap_or(expr.span)]
        }
        LoopSource::Loop => {
          vec![expr.span.with_lo(expr.span.lo() + BytePos(4))]
        }
      },
      ExprKind::Break(..) => {
        return;
      }
      _ => {
        let mut visitor = ChildExprSpans {
          spans: Vec::new(),
          item_span: self.0.item_span,
        };
        intravisit::walk_expr(&mut visitor, expr);
        visitor.spans
      }
    };

    let mut outer_spans = span.subtract(inner_spans.clone());

    // In an expression `match e { .. }` the span of `e` is only stored in a `FakeRead`,
    // so we have to ensure that the span of the HIR match includes the matched expression.
    if let ExprKind::Match(matched, _, _) = expr.kind {
      outer_spans.push(matched.span);
    }

    trace!(
      "Expr:\n{}\nhas span: {:?}\nand inner spans: {:?}\nand outer spans: {:?}",
      expr_to_string(expr),
      span,
      inner_spans,
      outer_spans
    );

    if outer_spans.is_empty() {
      return;
    }

    self.0.hir_spans.push(HirSpannedNode {
      full: span.data(),
      outer: outer_spans,
      node: Node::Expr(expr),
    });
  }

  fn visit_stmt(&mut self, stmt: &'hir Stmt<'hir>) {
    intravisit::walk_stmt(self, stmt);

    let span = try_span!(self, stmt.span);

    let mut visitor = ChildExprSpans {
      spans: Vec::new(),
      item_span: self.0.item_span,
    };
    intravisit::walk_stmt(&mut visitor, stmt);
    let outer_spans = span.subtract(visitor.spans);

    self.0.hir_spans.push(HirSpannedNode {
      full: span.data(),
      outer: outer_spans,
      node: Node::Stmt(stmt),
    });
  }

  fn visit_param(&mut self, param: &'hir Param<'hir>) {
    intravisit::walk_param(self, param);

    let span = match param.span.as_local(self.0.item_span) {
      Some(span) if !self.0.invalid_span(span) => span,
      _ => {
        return;
      }
    };

    // TODO: more precise outer spans
    self.0.hir_spans.push(HirSpannedNode {
      full: span.data(),
      outer: vec![span],
      node: Node::Param(param),
    });
  }
}
