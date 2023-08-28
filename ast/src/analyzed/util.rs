use std::{iter::once, ops::ControlFlow};

use super::{Analyzed, Expression, FunctionValueDefinition, Identity};

/// Calls `f` on each expression in the pil file and then descends into the
/// (potentially modified) expression.
pub fn previsit_expressions_in_pil_file_mut<T, F, B>(
    pil_file: &mut Analyzed<T>,
    f: &mut F,
) -> ControlFlow<B>
where
    F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
{
    pil_file
        .definitions
        .values_mut()
        .try_for_each(|(_poly, definition)| match definition {
            Some(FunctionValueDefinition::Mapping(e)) | Some(FunctionValueDefinition::Query(e)) => {
                previsit_expression_mut(e, f)
            }
            Some(FunctionValueDefinition::Array(elements)) => elements
                .iter_mut()
                .flat_map(|e| e.pattern.iter_mut())
                .try_for_each(|e| previsit_expression_mut(e, f)),
            None => ControlFlow::Continue(()),
        })?;

    pil_file
        .identities
        .iter_mut()
        .try_for_each(|i| previsit_expressions_in_identity_mut(i, f))
}

pub fn postvisit_expressions_in_identity_mut<T, F, B>(
    i: &mut Identity<T>,
    f: &mut F,
) -> ControlFlow<B>
where
    F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
{
    i.left
        .selector
        .as_mut()
        .into_iter()
        .chain(i.right.selector.as_mut())
        .try_for_each(move |item| postvisit_expression_mut(item, f))
}

/// Calls `f` on each expression in the pil file and then descends into the
/// (potentially modified) expression.
pub fn postvisit_expressions_in_pil_file_mut<T, F, B>(
    pil_file: &mut Analyzed<T>,
    f: &mut F,
) -> ControlFlow<B>
where
    F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
{
    pil_file
        .definitions
        .values_mut()
        .try_for_each(|(_poly, definition)| match definition {
            Some(FunctionValueDefinition::Mapping(e)) | Some(FunctionValueDefinition::Query(e)) => {
                postvisit_expression_mut(e, f)
            }
            Some(FunctionValueDefinition::Array(elements)) => elements
                .iter_mut()
                .flat_map(|e| e.pattern.iter_mut())
                .try_for_each(|e| postvisit_expression_mut(e, f)),
            None => ControlFlow::Continue(()),
        })?;

    pil_file
        .identities
        .iter_mut()
        .try_for_each(|i| postvisit_expressions_in_identity_mut(i, f))
}

pub fn previsit_expressions_in_identity_mut<T, F, B>(
    i: &mut Identity<T>,
    f: &mut F,
) -> ControlFlow<B>
where
    F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
{
    i.left
        .selector
        .as_mut()
        .into_iter()
        .chain(i.left.expressions.iter_mut())
        .chain(i.right.selector.as_mut())
        .chain(i.right.expressions.iter_mut())
        .try_for_each(move |item| previsit_expression_mut(item, f))
}

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
        | Expression::LocalVariableReference(_)
        | Expression::PublicReference(_)
        | Expression::Number(_)
        | Expression::String(_) => {}
        Expression::BinaryOperation(left, _, right) => {
            previsit_expression(left, f)?;
            previsit_expression(right, f)?;
        }
        Expression::UnaryOperation(_, e) => previsit_expression(e, f)?,
        Expression::SumOfProducts(_, _) => todo!(),
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
pub fn previsit_expression_mut<T, F, B>(e: &mut Expression<T>, f: &mut F) -> ControlFlow<B>
where
    F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
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
        Expression::SumOfProducts(_, _) => todo!(),
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

/// Traverses the expression tree and calls `f` in post-order.
pub fn postvisit_expression_mut<T, F, B>(e: &mut Expression<T>, f: &mut F) -> ControlFlow<B>
where
    F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
{
    match e {
        Expression::PolynomialReference(_)
        | Expression::Constant(_)
        | Expression::LocalVariableReference(_)
        | Expression::PublicReference(_)
        | Expression::Number(_)
        | Expression::String(_) => {}
        Expression::BinaryOperation(left, _, right) => {
            postvisit_expression_mut(left, f)?;
            postvisit_expression_mut(right, f)?;
        }
        Expression::UnaryOperation(_, e) => postvisit_expression_mut(e.as_mut(), f)?,
        Expression::SumOfProducts(_, _) => todo!(),
        Expression::Tuple(items) | Expression::FunctionCall(_, items) => items
            .iter_mut()
            .try_for_each(|item| postvisit_expression_mut(item, f))?,
        Expression::MatchExpression(scrutinee, arms) => {
            once(scrutinee.as_mut())
                .chain(arms.iter_mut().map(|(_n, e)| e))
                .try_for_each(|item| postvisit_expression_mut(item, f))?;
        }
    };
    f(e)
}
