use powdr_constraint_solver::{
    grouped_expression::GroupedExpression,
};
use powdr_expression::{AlgebraicUnaryOperation, AlgebraicUnaryOperator};
use powdr_number::{ExpressionConvertible, FieldElement};

use crate::expression::{AlgebraicExpression, AlgebraicReference};

/// Turns an algebraic expression into a grouped expression,
/// assuming all [`AlgebraicReference`]s are unknown variables.
pub fn algebraic_to_quadratic_symbolic_expression<T: FieldElement>(
    expr: &AlgebraicExpression<T>,
) -> GroupedExpression<T, AlgebraicReference> {
    expr.to_expression(
        &|n| GroupedExpression::from_number(*n),
        &|reference| GroupedExpression::from_unknown_variable(reference.clone()),
    )
}

/// Turns a grouped expression back into an algebraic expression.
/// Tries to simplify the expression wrt negation and constant factors
/// to aid human readability.
pub fn quadratic_symbolic_expression_to_algebraic<T: FieldElement>(
    expr: &GroupedExpression<T, AlgebraicReference>,
) -> AlgebraicExpression<T> {
    // Turn the expression into a list of to-be-summed items and try to
    // simplify on the way.
    let (quadratic, linear, constant) = expr.components();
    let items = quadratic
        .iter()
        .map(|(l, r)| {
            let l = quadratic_symbolic_expression_to_algebraic(l);
            let (l, l_negated) = extract_negation_if_possible(l);
            let r = quadratic_symbolic_expression_to_algebraic(r);
            let (r, r_negated) = extract_negation_if_possible(r);
            if l_negated == r_negated {
                l * r
            } else {
                -(l * r)
            }
        })
        .chain(linear.map(|(v, c)| {
            if c.is_one() {
                return AlgebraicExpression::Reference(v.clone());
            } else if (*c).neg().is_one() {
                return -AlgebraicExpression::Reference(v.clone());
            }
            let (c, negated) = extract_negation_if_possible(field_element_to_algebraic(c));
            if negated {
                -(c * AlgebraicExpression::Reference(v.clone()))
            } else {
                c * AlgebraicExpression::Reference(v.clone())
            }
        }))
        .chain((!constant.is_zero()).then(|| field_element_to_algebraic(constant)));

    // Now order the items by negated and non-negated.
    let mut positive = vec![];
    let mut negated = vec![];
    for item in items {
        let (item, item_negated) = extract_negation_if_possible(item);
        if item_negated {
            negated.push(item);
        } else {
            positive.push(item);
        }
    }
    let positive = positive.into_iter().reduce(|acc, item| acc + item);
    let negated = negated.into_iter().reduce(|acc, item| acc + item);
    match (positive, negated) {
        (Some(positive), Some(negated)) => positive - negated,
        (Some(positive), None) => positive,
        (None, Some(negated)) => -negated,
        (None, None) => AlgebraicExpression::from(T::zero()),
    }
}

fn field_element_to_algebraic<T: FieldElement>(
    e: &T,
) -> AlgebraicExpression<T> {
    if e.is_in_lower_half() {
        AlgebraicExpression::from(*e)
    } else {
        -AlgebraicExpression::from(-*e)
    }
}

/// If `e` is negated, returns the expression without negation and `true`,
/// otherwise returns the un-modified expression and `false`.
fn extract_negation_if_possible<T>(e: AlgebraicExpression<T>) -> (AlgebraicExpression<T>, bool) {
    match e {
        AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation {
            op: AlgebraicUnaryOperator::Minus,
            expr,
        }) => (*expr, true),
        _ => (e, false),
    }
}
