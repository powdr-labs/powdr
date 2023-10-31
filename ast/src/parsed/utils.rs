use std::ops::ControlFlow;

use super::visitor::ExpressionVisitable;

/// Visits `expr` and all of its sub-expressions and returns true if `f` returns true on any of them.
pub fn expr_any<E>(expr: &E, mut f: impl FnMut(&E) -> bool) -> bool
where
    E: ExpressionVisitable<E>,
{
    expr.pre_visit_expressions_return(&mut |e| {
        if f(e) {
            ControlFlow::Break(())
        } else {
            ControlFlow::Continue(())
        }
    })
    .is_break()
}
