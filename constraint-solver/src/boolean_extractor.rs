use std::hash::Hash;

use crate::quadratic_symbolic_expression::QuadraticSymbolicExpression;
use powdr_number::FieldElement;

/// Tries to simplify a quadratic constraint by transforming it into an affine
/// constraint that makes use of a new boolean variable.
///
/// @param constraint The quadratic constraint to transform.
/// @param var_dispenser A function that returns a new variable that is assumed to be boolean-constrained.
/// It will only be called if the transformation is performed.
pub fn extract_boolean<T: FieldElement, V: Ord + Clone + Hash + Eq>(
    constraint: &QuadraticSymbolicExpression<T, V>,
    mut var_dispenser: impl FnMut() -> V,
) -> Option<QuadraticSymbolicExpression<T, V>> {
    let (left, right) = constraint.try_as_single_product()?;
    // `constr = 0` is equivalent to `left * right = 0`
    let offset = left - right;
    // We only do the transformation if `offset` is known, because
    // otherwise the constraint stays quadratic.
    offset.try_to_known()?;
    // `offset + right = left`
    // `constr = 0` is equivalent to `right * (right + offset) = 0`

    let z = var_dispenser();

    // We return `right + z * offset == 0`, which is equivalent to the original constraint.
    Some(right + &(QuadraticSymbolicExpression::from_unknown_variable(z) * offset))
}

#[cfg(test)]
mod tests {
    use crate::test_utils::{constant, var};

    use super::*;

    #[test]
    fn test_extract_boolean() {
        let mut var_dispenser = || "z";
        let expr = (var("a") + var("b")) * (var("a") + var("b") + constant(10));
        let result = extract_boolean(&expr, &mut var_dispenser);
        assert!(result.is_some());
        let result = result.unwrap();
        assert_eq!(result.to_string(), "a + b + -10 * z + 10");
    }
}
