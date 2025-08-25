use std::{cmp::min, collections::HashMap, hash::Hash};

use itertools::Itertools;
use powdr_number::{FieldElement, LargeInt};

use crate::{
    constraint_system::AlgebraicConstraint,
    grouped_expression::GroupedExpression,
    indexed_constraint_system::apply_substitutions_to_expressions,
    runtime_constant::{RuntimeConstant, Substitutable},
    solver::VariableAssignment,
};

pub struct BooleanExtractor<T, V> {
    /// If (expr, Some(z)) is in the map, it means that
    /// we have transformed a constraint `left * right = 0` into
    /// `right + z * offset = 0`, where `z` is a new boolean variable
    /// and `expr = -right / offset = z`.
    ///
    /// If (expr, None) is in the map, it means that
    /// we have transformed a constraint `right * right = 0` into
    /// `right = 0`, which is a special case where we do not need
    /// a new boolean variable.
    substitutions: HashMap<GroupedExpression<T, V>, Option<V>>,
}

impl<T, V> Default for BooleanExtractor<T, V> {
    fn default() -> Self {
        Self {
            substitutions: HashMap::new(),
        }
    }
}

pub struct BooleanExtractionValue<T, V> {
    pub constraint: AlgebraicConstraint<GroupedExpression<T, V>>,
    pub new_unconstrained_boolean_variable: Option<V>,
}

impl<T: RuntimeConstant + Hash, V: Ord + Clone + Hash + Eq> BooleanExtractor<T, V> {
    /// Tries to simplify a quadratic constraint by transforming it into an affine
    /// constraint that makes use of a new boolean variable.
    /// NOTE: The boolean constraint is not part of the output.
    ///
    /// Returns the new constraint and the new variable if required.
    ///
    /// If the same simplification has been performed before, it will
    /// return None (in particular, it will not request a new variable).
    ///
    /// For example `(a + b) * (a + b + 10) = 0` can be transformed into
    /// `a + b + z * 10 = 0`, where `z` is a new boolean variable.
    ///
    /// @param constraint The quadratic constraint to transform.
    /// @param var_dispenser A function that returns a new variable that is assumed to be boolean-constrained.
    /// It will only be called if the transformation is performed.
    pub fn try_extract_boolean(
        &mut self,
        constraint: &AlgebraicConstraint<GroupedExpression<T, V>>,
        mut var_dispenser: impl FnMut() -> V,
    ) -> Option<BooleanExtractionValue<T, V>> {
        let (left, right) = constraint.expression.try_as_single_product()?;
        // We want to check if `left` and `right` differ by a constant offset.
        // Since multiplying the whole constraint by a non-zero constant does
        // not change the constraint, we also transform `left` by a constant
        // (non-zero) factor.
        // So we are looking for an offset `c` and a non-zero constant factor `f`
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
            (Some((left_var, left_coeff)), Some((right_var, right_coeff)))
                if left_var == right_var =>
            {
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
            if self.substitutions.contains_key(right) {
                None
            } else {
                self.substitutions.insert(right.clone(), None);
                Some(BooleanExtractionValue {
                    constraint: AlgebraicConstraint::assert_zero(right.clone()),
                    new_unconstrained_boolean_variable: None,
                })
            }
        } else {
            // We can substitute the initial constraint using a new boolean variable `z`
            // either by
            // `0 = right + z * offset`
            // or by
            // `0 = right + (1 - z) * offset = right + offset - z * offset`,
            // which is equivalent to
            // `0 = -right - offset + z * offset`.
            // We use the one that has a smaller constant offset in the resulting expression.
            let expr = [
                right.clone(),
                -right - GroupedExpression::from_runtime_constant(offset.clone()),
            ]
            .into_iter()
            .min_by_key(|e| {
                // Return the abs of the constant offset, or None on larger fields.
                e.components().2.try_to_number().and_then(try_to_abs_u64)
            })
            .unwrap();

            let key = -&expr * T::one().field_div(offset);
            if self.substitutions.contains_key(&key) {
                // We have already performed this transformation before.
                return None;
            }

            if key.try_to_simple_unknown().is_some() {
                // In this case we don't gain anything because the new variable `z` will just
                // be equivalent to the single variable in `right`.
                None
            } else {
                let z = var_dispenser();

                self.substitutions.insert(key, Some(z.clone()));

                // We return `expr + z * offset == 0`, which is equivalent to the original constraint.
                Some(BooleanExtractionValue {
                    constraint: AlgebraicConstraint::assert_zero(
                        expr + (GroupedExpression::from_unknown_variable(z.clone()) * offset),
                    ),
                    new_unconstrained_boolean_variable: Some(z),
                })
            }
        }
    }
}

fn try_to_abs_u64<T: FieldElement>(x: T) -> Option<u64> {
    let modulus = T::modulus().try_into_u64()?;
    let x = x.to_integer().try_into_u64()?;
    Some(min(x, modulus - x))
}

impl<T: RuntimeConstant + Substitutable<V> + Hash, V: Clone + Eq + Ord + Hash>
    BooleanExtractor<T, V>
{
    /// Applies the assignments to the stored substitutions.
    pub fn apply_assignments(&mut self, assignments: &[VariableAssignment<T, V>]) {
        if assignments.is_empty() {
            return;
        }
        let (exprs, vars): (Vec<_>, Vec<_>) = self.substitutions.drain().unzip();
        let exprs = apply_substitutions_to_expressions(exprs, assignments.iter().cloned());
        self.substitutions = exprs.into_iter().zip_eq(vars).collect();
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
        let mut extractor: BooleanExtractor<_, _> = Default::default();
        let result = extractor
            .try_extract_boolean(&expr.into(), &mut var_dispenser)
            .unwrap();
        assert_eq!(result.constraint.to_string(), "-(a + b + 10 * z) = 0");
        assert_eq!(result.new_unconstrained_boolean_variable, Some("z"));
    }

    #[test]
    fn test_extract_boolean_square() {
        let mut var_dispenser = || "z";
        let expr = (var("a") + var("b")) * (var("a") + var("b"));
        let mut extractor: BooleanExtractor<_, _> = Default::default();
        let result = extractor
            .try_extract_boolean(&expr.into(), &mut var_dispenser)
            .unwrap();
        assert_eq!(result.constraint.to_string(), "a + b = 0");
        assert_eq!(result.new_unconstrained_boolean_variable, None);
    }

    #[test]
    fn test_extract_boolean_useless() {
        let mut var_dispenser = || "z";
        let expr = (var("a") - constant(1)) * (var("a"));
        let mut extractor: BooleanExtractor<_, _> = Default::default();
        let result = extractor.try_extract_boolean(&expr.into(), &mut var_dispenser);
        assert!(result.is_none());

        let expr = (constant(2) * var("a") - constant(2)) * (constant(2) * var("a"));
        let result = extractor.try_extract_boolean(&expr.into(), &mut var_dispenser);
        assert!(result.is_none());
    }

    #[test]
    fn do_not_extract_twice() {
        let mut var_dispenser = || "z";
        let expr = (var("a") + var("b")) * (var("a") + var("b") + constant(10));
        let mut extractor: BooleanExtractor<_, _> = Default::default();
        let result = extractor
            .try_extract_boolean(&expr.clone().into(), &mut var_dispenser)
            .unwrap();
        assert_eq!(result.constraint.to_string(), "-(a + b + 10 * z) = 0");
        assert_eq!(result.new_unconstrained_boolean_variable, Some("z"));

        assert!(extractor
            .try_extract_boolean(&expr.into(), &mut var_dispenser)
            .is_none());

        // left and right swapped
        assert!(extractor
            .try_extract_boolean(
                &(var("a") + var("b") + constant(10) * (var("a") + var("b"))).into(),
                &mut var_dispenser
            )
            .is_none());

        let expr2 = (constant(2) * (var("a") + var("b"))) * (var("a") + var("b") + constant(10));
        assert!(extractor
            .try_extract_boolean(&expr2.into(), &mut var_dispenser)
            .is_none());

        let expr3 = (var("a") + var("b")) * (constant(2) * (var("a") + var("b") + constant(10)));
        assert!(extractor
            .try_extract_boolean(&expr3.into(), &mut var_dispenser)
            .is_none());

        // This is different because the effective constant is different.
        let expr4 = (var("a") + var("b")) * (constant(2) * (var("a") + var("b") + constant(20)));
        assert_eq!(
            extractor
                .try_extract_boolean(&expr4.into(), &mut var_dispenser)
                .unwrap()
                .constraint
                .to_string(),
            "-(2 * a + 2 * b + 40 * z) = 0"
        );
    }

    #[test]
    fn do_not_extract_squares_twice() {
        let mut var_dispenser = || "z";
        let expr = (var("a") + var("b")) * (var("a") + var("b"));
        let mut extractor: BooleanExtractor<_, _> = Default::default();
        let result = extractor
            .try_extract_boolean(&expr.clone().into(), &mut var_dispenser)
            .unwrap();

        assert_eq!(result.constraint.to_string(), "a + b = 0");
        assert_eq!(result.new_unconstrained_boolean_variable, None);

        let result = extractor.try_extract_boolean(&expr.into(), &mut var_dispenser);
        assert!(result.is_none());
    }

    #[test]
    fn apply_assignments() {
        let mut counter = 0;
        let vars = (0..10).map(|i| format!("z_{i}")).collect_vec();
        let mut var_dispenser = || {
            counter += 1;
            vars[counter - 1].as_str()
        };
        let expr =
            (var("a") + var("b") + var("k")) * (var("a") + var("b") + var("k") - constant(2));
        let mut extractor: BooleanExtractor<_, _> = Default::default();
        let result = extractor
            .try_extract_boolean(&expr.into(), &mut var_dispenser)
            .unwrap();
        assert_eq!(result.constraint.to_string(), "-(a + b + k - 2 * z_0) = 0");
        assert_eq!(result.new_unconstrained_boolean_variable, Some("z_0"));

        extractor.apply_assignments(&[("k", -constant(9))]);
        let expr2 =
            (var("a") + var("b") - constant(9)) * (var("a") + var("b") - constant(9) - constant(2));

        let result = extractor.try_extract_boolean(&expr2.into(), &mut var_dispenser);
        assert!(result.is_none());
    }
}
