use powdr_constraint_solver::{
    grouped_expression::{GroupedExpression, GroupedExpressionComponent},
    runtime_constant::RuntimeConstant,
};
use powdr_expression::{AlgebraicExpression, AlgebraicUnaryOperation, AlgebraicUnaryOperator};
use powdr_number::{ExpressionConvertible, FieldElement};

/// Turns an algebraic expression into a grouped expression,
/// assuming all [`AlgebraicReference`]s are unknown variables.
pub fn algebraic_to_grouped_expression<T, V>(
    expr: &AlgebraicExpression<T, V>,
) -> GroupedExpression<T, V>
where
    T: FieldElement,
    V: Ord + Clone,
{
    expr.to_expression(&|n| GroupedExpression::from_number(*n), &|reference| {
        GroupedExpression::from_unknown_variable(reference.clone())
    })
}

/// Turns a grouped expression back into an algebraic expression.
/// Tries to simplify the expression wrt negation and constant factors
/// to aid human readability.
pub fn grouped_expression_to_algebraic<T, V>(
    expr: GroupedExpression<T, V>,
) -> powdr_expression::AlgebraicExpression<T, V>
where
    T: FieldElement,
    V: Ord + Clone,
{
    // Turn the expression into a list of to-be-summed items and try to
    // simplify on the way.
    let items = expr.into_summands().filter_map(|c| match c {
        GroupedExpressionComponent::Quadratic(l, r) => {
            let l = grouped_expression_to_algebraic(l);
            let (l, l_negated) = extract_negation_if_possible(l);
            let r = grouped_expression_to_algebraic(r);
            let (r, r_negated) = extract_negation_if_possible(r);
            Some(if l_negated == r_negated {
                l * r
            } else {
                -(l * r)
            })
        }
        GroupedExpressionComponent::Linear(v, c) => Some(if c.is_one() {
            AlgebraicExpression::Reference(v.clone())
        } else if (-c).is_one() {
            -AlgebraicExpression::Reference(v.clone())
        } else if c.is_in_lower_half() {
            AlgebraicExpression::from(c) * AlgebraicExpression::Reference(v.clone())
        } else {
            -(AlgebraicExpression::from(-c) * AlgebraicExpression::Reference(v.clone()))
        }),
        GroupedExpressionComponent::Constant(constant) => {
            (!constant.is_known_zero()).then(|| field_element_to_algebraic_expression(constant))
        }
    });

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

fn field_element_to_algebraic_expression<T: FieldElement, V>(v: T) -> AlgebraicExpression<T, V> {
    if v.is_in_lower_half() {
        AlgebraicExpression::from(v)
    } else {
        -AlgebraicExpression::from(-v)
    }
}

/// If `e` is negated, returns the expression without negation and `true`,
/// otherwise returns the un-modified expression and `false`.
fn extract_negation_if_possible<T, V>(
    e: AlgebraicExpression<T, V>,
) -> (AlgebraicExpression<T, V>, bool) {
    match e {
        AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation {
            op: AlgebraicUnaryOperator::Minus,
            expr,
        }) => (*expr, true),
        _ => (e, false),
    }
}
