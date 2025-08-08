use std::hash::Hash;

use crate::{grouped_expression::GroupedExpression, runtime_constant::RuntimeConstant};

/// Tries to simplify a quadratic constraint by transforming it into an affine
/// constraint that makes use of a new boolean variable.
/// NOTE: The boolean constraint is not part of the output.
///
/// For example `(a + b) * (a + b + 10) = 0` can be transformed into
/// `a + b + z * 10 = 0`, where `z` is a new boolean variable.
///
/// @param constraint The quadratic constraint to transform.
/// @param var_dispenser A function that returns a new variable that is assumed to be boolean-constrained.
/// It will only be called if the transformation is performed.
pub fn try_extract_boolean<T: RuntimeConstant, V: Ord + Clone + Hash + Eq>(
    constraint: &GroupedExpression<T, V>,
    mut var_dispenser: impl FnMut() -> V,
) -> Option<GroupedExpression<T, V>> {
    let (left, right) = constraint.try_as_single_product()?;
    // We want to check if `left` and `right` differ by a constant offset.
    // Since multiplying the whole constraint by a non-zero constant does
    // not change the constraint, we also transform `left` by a constant
    // (non-zero) factor.
    // So we are looking for a offset `c` and a non-zero constant factor `f`
    // such that `f * left = right + c`.
    // Then we can write the original constraint `left * right = 0` as
    // `(right + c) * right = 0` (we can just ignore `f`).
    // This is in turn equivalent to `right + z * c = 0`, where `z` is
    // a new boolean variable.

    // For example, if the constraint was `(2 * a + 2 * b) * (a + b + 10) = 0`, we would
    // set `factor = 1 / 2`, such that `left * factor - right` is a constant.

    // First, try to find a good factor so that `left` and `right`
    // likely cancel out except for a constant. As a good guess,
    // we try to match the coefficient of the first variable.
    let factor = match (left.components().1.next(), right.components().1.next()) {
        (Some((left_var, left_coeff)), Some((right_var, right_coeff))) if left_var == right_var => {
            right_coeff.field_div(left_coeff)
        }
        _ => T::one(),
    };

    // `constr = 0` is equivalent to `left * right = 0`
    let offset = &(left.clone() * &factor) - right;
    // We only do the transformation if `offset` is known, because
    // otherwise the constraint stays quadratic.
    let offset = offset.try_to_known()?;
    // We know that `offset + right = left` and thus
    // `constr = 0` is equivalent to `right * (right + offset) = 0`
    // which is equivalent to `right + z * offset = 0` for a new
    // boolean variable `z`.

    if offset.is_zero() {
        // In this special case, we do not need a new variable.
        Some(right.clone())
    } else if (right.clone() * -T::one().field_div(offset))
        .try_to_simple_unknown()
        .is_some()
    {
        // In this case we don't gain anything because the new variable `z` will just
        // be equivalent to the single variable in `right`.
        return None;
    } else {
        let z = var_dispenser();

        // We return `right + z * offset == 0`, which is equivalent to the original constraint.
        Some(right + &(GroupedExpression::from_unknown_variable(z) * offset))
    }
}

#[cfg(test)]
mod tests {
    use crate::test_utils::{constant, var};

    use super::*;

    #[test]
    fn test_extract_boolean() {
        let mut var_dispenser = || "z";
        let expr = (var("a") + var("b")) * (var("a") + var("b") + constant(10));
        let result = try_extract_boolean(&expr, &mut var_dispenser);
        assert!(result.is_some());
        let result = result.unwrap();
        assert_eq!(result.to_string(), "a + b - 10 * z + 10");
    }

    #[test]
    fn test_extract_boolean_square() {
        let mut var_dispenser = || "z";
        let expr = (var("a") + var("b")) * (var("a") + var("b"));
        let result = try_extract_boolean(&expr, &mut var_dispenser);
        assert!(result.is_some());
        let result = result.unwrap();
        assert_eq!(result.to_string(), "a + b");
    }

    #[test]
    fn test_extract_boolean_useless() {
        let mut var_dispenser = || "z";
        let expr = (var("a") - constant(1)) * (var("a"));
        let result = try_extract_boolean(&expr, &mut var_dispenser);
        assert!(result.is_none());

        let expr = (constant(2) * var("a") - constant(2)) * (constant(2) * var("a"));
        let result = try_extract_boolean(&expr, &mut var_dispenser);
        assert!(result.is_none());
    }
}
