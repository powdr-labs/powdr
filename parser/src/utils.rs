use std::{iter::once, ops::ControlFlow};

use crate::ast::Expression;

/// Visits `expr` and all of its sub-expressions and returns true if `f` returns true on any of them.
pub fn expr_any<T>(expr: &Expression<T>, mut f: impl FnMut(&Expression<T>) -> bool) -> bool {
    previsit_expression(expr, &mut |e| {
        if f(e) {
            ControlFlow::Break(())
        } else {
            ControlFlow::Continue(())
        }
    })
    .is_break()
}

/// Traverses the expression tree and calls `f` in pre-order.
pub fn previsit_expression<'a, T, F, B>(e: &'a Expression<T>, f: &mut F) -> ControlFlow<B>
where
    F: FnMut(&'a Expression<T>) -> ControlFlow<B>,
{
    f(e)?;

    match e {
        Expression::PolynomialReference(_)
        | Expression::Constant(_)
        | Expression::PublicReference(_)
        | Expression::Number(_)
        | Expression::String(_)
        | Expression::FreeInput(_) => {}
        Expression::BinaryOperation(left, _, right) => {
            previsit_expression(left, f)?;
            previsit_expression(right, f)?;
        }
        Expression::UnaryOperation(_, e) => previsit_expression(e, f)?,
        Expression::Tuple(items) | Expression::FunctionCall(_, items) => items
            .iter()
            .try_for_each(|item| previsit_expression(item, f))?,
        Expression::MatchExpression(scrutinee, arms) => {
            once(scrutinee.as_ref())
                .chain(arms.iter().map(|(_n, e)| e))
                .try_for_each(move |item| previsit_expression(item, f))?;
        }
    };
    ControlFlow::Continue(())
}
