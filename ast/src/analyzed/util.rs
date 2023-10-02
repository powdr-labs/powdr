use std::ops::ControlFlow;

use crate::parsed::utils::{
    postvisit_expression_mut, previsit_expression, previsit_expression_mut,
};

use super::{Analyzed, Expression, FunctionValueDefinition, Identity, SelectedExpressions};

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
            Some(FunctionValueDefinition::Expression(e)) => previsit_expression_mut(e, f),
            None => ControlFlow::Continue(()),
        })?;

    pil_file
        .identities
        .iter_mut()
        .try_for_each(|i| previsit_expressions_in_identity_mut(i, f))
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
            Some(FunctionValueDefinition::Expression(e)) => postvisit_expression_mut(e, f),
            None => ControlFlow::Continue(()),
        })?;

    pil_file
        .identities
        .iter_mut()
        .try_for_each(|i| postvisit_expressions_in_identity_mut(i, f))
}

pub fn previsit_expressions_in_identity<T, F, B>(i: &Identity<T>, f: &mut F) -> ControlFlow<B>
where
    F: FnMut(&Expression<T>) -> ControlFlow<B>,
{
    [&i.left, &i.right]
        .iter()
        .try_for_each(move |item| previsit_expressions_in_selected_expressions(item, f))
}

pub fn previsit_expressions_in_identity_mut<T, F, B>(
    i: &mut Identity<T>,
    f: &mut F,
) -> ControlFlow<B>
where
    F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
{
    [&mut i.left, &mut i.right]
        .iter_mut()
        .try_for_each(move |item| previsit_expressions_in_selected_expressions_mut(item, f))
}

pub fn postvisit_expressions_in_identity_mut<T, F, B>(
    i: &mut Identity<T>,
    f: &mut F,
) -> ControlFlow<B>
where
    F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
{
    [&mut i.left, &mut i.right]
        .iter_mut()
        .try_for_each(move |item| postvisit_expressions_in_selected_expressions_mut(item, f))
}

pub fn previsit_expressions_in_selected_expressions<T, F, B>(
    s: &SelectedExpressions<T>,
    f: &mut F,
) -> ControlFlow<B>
where
    F: FnMut(&Expression<T>) -> ControlFlow<B>,
{
    s.selector
        .as_ref()
        .into_iter()
        .chain(s.expressions.iter())
        .try_for_each(move |item| previsit_expression(item, f))
}

pub fn previsit_expressions_in_selected_expressions_mut<T, F, B>(
    s: &mut SelectedExpressions<T>,
    f: &mut F,
) -> ControlFlow<B>
where
    F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
{
    s.selector
        .as_mut()
        .into_iter()
        .chain(s.expressions.iter_mut())
        .try_for_each(move |item| previsit_expression_mut(item, f))
}

pub fn postvisit_expressions_in_selected_expressions_mut<T, F, B>(
    s: &mut SelectedExpressions<T>,
    f: &mut F,
) -> ControlFlow<B>
where
    F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
{
    s.selector
        .as_mut()
        .into_iter()
        .chain(s.expressions.iter_mut())
        .try_for_each(move |item| postvisit_expression_mut(item, f))
}
