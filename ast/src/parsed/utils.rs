use std::ops::ControlFlow;

use super::{visitor::ExpressionVisitor, Expression};

/// Visits `expr` and all of its sub-expressions and returns true if `f` returns true on any of them.
pub fn expr_any<T, Ref>(
    expr: &Expression<T, Ref>,
    mut f: impl FnMut(&Expression<T, Ref>) -> bool,
) -> bool {
    expr.pre_visit_expressions_return(&mut |e| {
        if f(e) {
            ControlFlow::Break(())
        } else {
            ControlFlow::Continue(())
        }
    })
    .is_break()
}
