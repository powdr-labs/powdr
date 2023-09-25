use std::{iter::once, ops::ControlFlow};

use super::{
    ArrayExpression, Expression, FunctionCall, FunctionDefinition, LambdaExpression, MatchArm,
    PilStatement,
};

/// Visits `expr` and all of its sub-expressions and returns true if `f` returns true on any of them.
pub fn expr_any<T, Ref>(
    expr: &Expression<T, Ref>,
    mut f: impl FnMut(&Expression<T, Ref>) -> bool,
) -> bool {
    previsit_expression(expr, &mut |e| {
        if f(e) {
            ControlFlow::Break(())
        } else {
            ControlFlow::Continue(())
        }
    })
    .is_break()
}

/// Traverses the expression trees of the statement and calls `f` in post-order.
/// Does not enter macro definitions.
pub fn postvisit_expression_in_statement_mut<T, F, B>(
    statement: &mut PilStatement<T>,
    f: &mut F,
) -> ControlFlow<B>
where
    F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
{
    match statement {
        PilStatement::FunctionCall(_, _, arguments) => arguments
            .iter_mut()
            .try_for_each(|e| postvisit_expression_mut(e, f)),
        PilStatement::PlookupIdentity(_, left, right)
        | PilStatement::PermutationIdentity(_, left, right) => left
            .selector
            .iter_mut()
            .chain(left.expressions.iter_mut())
            .chain(right.selector.iter_mut())
            .chain(right.expressions.iter_mut())
            .try_for_each(|e| postvisit_expression_mut(e, f)),
        PilStatement::ConnectIdentity(_start, left, right) => left
            .iter_mut()
            .chain(right.iter_mut())
            .try_for_each(|e| postvisit_expression_mut(e, f)),

        PilStatement::Namespace(_, _, e)
        | PilStatement::PolynomialDefinition(_, _, e)
        | PilStatement::PolynomialIdentity(_, e)
        | PilStatement::PublicDeclaration(_, _, _, e)
        | PilStatement::ConstantDefinition(_, _, e)
        | PilStatement::LetStatement(_, _, Some(e)) => postvisit_expression_mut(e, f),

        PilStatement::PolynomialConstantDefinition(_, _, fundef)
        | PilStatement::PolynomialCommitDeclaration(_, _, Some(fundef)) => match fundef {
            FunctionDefinition::Query(_, e) | FunctionDefinition::Mapping(_, e) => {
                postvisit_expression_mut(e, f)
            }
            FunctionDefinition::Array(ae) => postvisit_expression_in_array_expression_mut(ae, f),
            FunctionDefinition::Expression(e) => postvisit_expression_mut(e, f),
        },
        PilStatement::PolynomialCommitDeclaration(_, _, None)
        | PilStatement::Include(_, _)
        | PilStatement::PolynomialConstantDeclaration(_, _)
        | PilStatement::MacroDefinition(_, _, _, _, _)
        | PilStatement::LetStatement(_, _, None) => ControlFlow::Continue(()),
    }
}

fn postvisit_expression_in_array_expression_mut<T, F, B>(
    ae: &mut ArrayExpression<T>,
    f: &mut F,
) -> ControlFlow<B>
where
    F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
{
    match ae {
        ArrayExpression::Value(expressions) | ArrayExpression::RepeatedValue(expressions) => {
            expressions
                .iter_mut()
                .try_for_each(|e| postvisit_expression_mut(e, f))
        }
        ArrayExpression::Concat(a1, a2) => [a1, a2]
            .iter_mut()
            .try_for_each(|e| postvisit_expression_in_array_expression_mut(e, f)),
    }
}

/// Traverses the expression tree and calls `f` in pre-order.
pub fn previsit_expression<'a, T, Ref, F, B>(e: &'a Expression<T, Ref>, f: &mut F) -> ControlFlow<B>
where
    F: FnMut(&'a Expression<T, Ref>) -> ControlFlow<B>,
{
    f(e)?;

    match e {
        Expression::Reference(_)
        | Expression::Constant(_)
        | Expression::PublicReference(_)
        | Expression::Number(_)
        | Expression::String(_) => {}
        Expression::BinaryOperation(left, _, right) => {
            previsit_expression(left, f)?;
            previsit_expression(right, f)?;
        }
        Expression::FreeInput(e)
        | Expression::UnaryOperation(_, e)
        | Expression::LambdaExpression(LambdaExpression { params: _, body: e }) => {
            previsit_expression(e, f)?
        }
        Expression::Tuple(items)
        | Expression::FunctionCall(FunctionCall {
            id: _,
            arguments: items,
        }) => items
            .iter()
            .try_for_each(|item| previsit_expression(item, f))?,
        Expression::MatchExpression(scrutinee, arms) => {
            once(scrutinee.as_ref())
                .chain(arms.iter().map(|MatchArm { pattern: _, value }| value))
                .try_for_each(move |item| previsit_expression(item, f))?;
        }
    };
    ControlFlow::Continue(())
}

/// Traverses the expression tree and calls `f` in pre-order.
pub fn previsit_expression_mut<T, Ref, F, B>(
    e: &mut Expression<T, Ref>,
    f: &mut F,
) -> ControlFlow<B>
where
    F: FnMut(&mut Expression<T, Ref>) -> ControlFlow<B>,
{
    f(e)?;

    match e {
        Expression::Reference(_)
        | Expression::Constant(_)
        | Expression::PublicReference(_)
        | Expression::Number(_)
        | Expression::String(_) => {}
        Expression::BinaryOperation(left, _, right) => {
            previsit_expression_mut(left, f)?;
            previsit_expression_mut(right, f)?;
        }
        Expression::FreeInput(e)
        | Expression::UnaryOperation(_, e)
        | Expression::LambdaExpression(LambdaExpression { params: _, body: e }) => {
            previsit_expression_mut(e.as_mut(), f)?
        }
        Expression::Tuple(items)
        | Expression::FunctionCall(FunctionCall {
            arguments: items, ..
        }) => items
            .iter_mut()
            .try_for_each(|item| previsit_expression_mut(item, f))?,
        Expression::MatchExpression(scrutinee, arms) => {
            once(scrutinee.as_mut())
                .chain(arms.iter_mut().map(|MatchArm { pattern: _, value }| value))
                .try_for_each(move |item| previsit_expression_mut(item, f))?;
        }
    };
    ControlFlow::Continue(())
}

/// Traverses the expression tree and calls `f` in post-order.
pub fn postvisit_expression_mut<T, Ref, F, B>(
    e: &mut Expression<T, Ref>,
    f: &mut F,
) -> ControlFlow<B>
where
    F: FnMut(&mut Expression<T, Ref>) -> ControlFlow<B>,
{
    match e {
        Expression::Reference(_)
        | Expression::Constant(_)
        | Expression::PublicReference(_)
        | Expression::Number(_)
        | Expression::String(_) => {}
        Expression::BinaryOperation(left, _, right) => {
            postvisit_expression_mut(left, f)?;
            postvisit_expression_mut(right, f)?;
        }
        Expression::FreeInput(e)
        | Expression::UnaryOperation(_, e)
        | Expression::LambdaExpression(LambdaExpression { params: _, body: e }) => {
            postvisit_expression_mut(e.as_mut(), f)?
        }
        Expression::Tuple(items)
        | Expression::FunctionCall(FunctionCall {
            arguments: items, ..
        }) => items
            .iter_mut()
            .try_for_each(|item| postvisit_expression_mut(item, f))?,
        Expression::MatchExpression(scrutinee, arms) => {
            once(scrutinee.as_mut())
                .chain(arms.iter_mut().map(|MatchArm { pattern: _, value }| value))
                .try_for_each(|item| postvisit_expression_mut(item, f))?;
        }
    };
    f(e)
}
