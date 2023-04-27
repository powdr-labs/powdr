use std::{iter::once, ops::ControlFlow};

use crate::Expression;

/// Visits `expr` and all of its sub-expressions and returns true if `f` returns true on any of them.
pub fn expr_any(expr: &Expression, mut f: impl FnMut(&Expression) -> bool) -> bool {
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
pub fn previsit_expression<'a, F, B>(e: &'a Expression, f: &mut F) -> ControlFlow<B>
where
    F: FnMut(&'a Expression) -> ControlFlow<B>,
{
    f(e)?;

    match e {
        Expression::PolynomialReference(_)
        | Expression::Constant(_)
        | Expression::LocalVariableReference(_)
        | Expression::PublicReference(_)
        | Expression::Number(_)
        | Expression::String(_) => {}
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

/// Traverses the expression tree and calls `f` in pre-order.
pub fn previsit_expression_mut<F, B>(e: &mut Expression, f: &mut F) -> ControlFlow<B>
where
    F: FnMut(&mut Expression) -> ControlFlow<B>,
{
    f(e)?;

    match e {
        Expression::PolynomialReference(_)
        | Expression::Constant(_)
        | Expression::LocalVariableReference(_)
        | Expression::PublicReference(_)
        | Expression::Number(_)
        | Expression::String(_) => {}
        Expression::BinaryOperation(left, _, right) => {
            previsit_expression_mut(left, f)?;
            previsit_expression_mut(right, f)?;
        }
        Expression::UnaryOperation(_, e) => previsit_expression_mut(e.as_mut(), f)?,
        Expression::Tuple(items) | Expression::FunctionCall(_, items) => items
            .iter_mut()
            .try_for_each(|item| previsit_expression_mut(item, f))?,
        Expression::MatchExpression(scrutinee, arms) => {
            once(scrutinee.as_mut())
                .chain(arms.iter_mut().map(|(_n, e)| e))
                .try_for_each(move |item| previsit_expression_mut(item, f))?;
        }
    };
    ControlFlow::Continue(())
}
