use std::fmt::Display;

use itertools::Itertools;
use num_traits::Zero;
use powdr_number::FieldElement;

use crate::{
    grouped_expression::{GroupedExpression, RangeConstraintProvider},
    runtime_constant::RuntimeConstant,
};

/// Tries to split the given constraint into a list of equivalent constraints.
/// This is the case for example if the variables in this expression can
/// be split into different bit areas.
pub fn try_split_constraint<T: RuntimeConstant + Display, V: Clone + Ord + Display>(
    constraint: &GroupedExpression<T, V>,
    range_constraints: &impl RangeConstraintProvider<T::FieldType, V>,
) -> Option<Vec<GroupedExpression<T, V>>> {
    // Group the linear part by absolute coefficients.
    let (components, constant) = components_grouped_by_absolute_coefficient(constraint)?;
    if components.len() < 2 {
        return None;
    }
    let mut constant = -constant;

    // Now try to split out each one in turn.
    let mut parts = vec![];
    for index in 0..components.len() {
        println!(
            "Components: {} = {constant}",
            components
                .iter()
                .map(|(c, e)| format!("{c} * ({e})"))
                .join(" + ")
        );

        let (candidate_coeff, candidate) = &components[index];
        // println!("Trying to split out {candidate_coeff} * ({candidate})");
        let rest = components
            .iter()
            .enumerate()
            .filter(|(i, component)| *i != index && component.0 != Zero::zero())
            .map(|(_, (coeff, expr))| (*coeff / *candidate_coeff, expr.clone()))
            .map(|(coeff, expr)| {
                if !coeff.is_in_lower_half() {
                    (-coeff, -expr)
                } else {
                    (coeff, expr)
                }
            })
            .collect_vec();
        if rest.is_empty() {
            // We are done anyway.
            continue;
        }
        // The original constraint is equivalent to `candidate + rest = constant`.
        // Now we try to extract the smallest coefficient in rest.
        let smallest_coeff = rest.iter().map(|(coeff, _)| *coeff).min().unwrap();
        assert_ne!(smallest_coeff, 0.into());
        assert!(smallest_coeff.is_in_lower_half());

        let rest: GroupedExpression<_, _> = rest
            .into_iter()
            .map(|(coeff, expr)| expr * &T::from(coeff / smallest_coeff))
            .sum();

        let candidate_rc = candidate.range_constraint(range_constraints);
        println!("Trying candidate {candidate} [rc: {candidate_rc}] with coeff {candidate_coeff} and rest {rest} (smallest coeff: {smallest_coeff})");
        // TODO do we need to compute the full range constraint of the complete expression?
        // TODO what about `constant`?
        if candidate_rc.is_unconstrained()
            || rest
                .range_constraint(range_constraints)
                .multiple(smallest_coeff)
                .is_unconstrained()
        {
            println!(" -> Cannot split out {candidate} because its rc {candidate_rc} is not tight enough");
            for var in candidate.referenced_unknown_variables() {
                println!("    {var} has rc {}", range_constraints.get(var));
            }
            continue;
        }
        // The original constraint is equivalent to `candidate + smallest_coeff * rest = constant`
        // and the constraint can equivalently be evaluated in the integers.
        // We now apply `x -> x % smallest_coeff` to the whole constraint.
        // If it was true before, it will be true afterwards.
        // So we get `candidate % smallest_coeff = constant % smallest_coeff`.
        // Now the only remaining task is to check that this new constraint has a unique solution
        // that does not require the use of the `%` operator.

        if let Some(solution) = candidate_rc
            // TODO what if the field div here is not a division without remainder in the integers?
            .has_unique_modular_solution(constant.field_div(candidate_coeff), smallest_coeff)
        {
            // TODO do we need to modify constant in some way?

            // candidate % smallest_coeff == constant only if candidate = solution.
            // Add `candidate = solution` to the parts
            parts.push(candidate - &GroupedExpression::from_number(solution));
            // println!("Split out {}", parts.last().unwrap());
            // println!(
            //     "Adjusting constant from {constant} to {}",
            //     constant - solution * *candidate_coeff
            // );
            constant -= solution * *candidate_coeff;
            // Substitute `candidate = solution` in our expression
            // by replacing the component by zero and subtracting
            // the solution from the constant.
            components[index] = (Zero::zero(), Zero::zero());
            // TODO correct?
            //constant = constant.field_div(&smallest_coeff);
        }
    }
    if parts.is_empty() {
        None
    } else {
        // We found some independent parts, add the remaining components to the parts
        // and return them.
        let remaining = components
            .into_iter()
            .filter(|(coeff, _)| *coeff != 0.into())
            .collect_vec();
        let constant = GroupedExpression::from_number(constant);
        parts.push(match remaining.as_slice() {
            [(coeff, expr)] => expr - &(constant * T::one().field_div(&T::from(*coeff))), // if there is only one component, we normalize
            _ => {
                remaining
                    .into_iter()
                    .map(|(coeff, expr)| expr * T::from(coeff))
                    .sum::<GroupedExpression<_, _>>()
                    - constant
            }
        });
        Some(parts)
    }
}

/// Turns an affine constraint `constraint = 0` into a list of expressions grouped by absolute coefficients
/// and an offset. The list is sorted by the coefficient.
/// If it returns `Some(([(c1, e1), ..., (cn, en)], o))`, then
/// `c1 * e1 + ... + cn * en + o = constraint`.
fn components_grouped_by_absolute_coefficient<
    T: RuntimeConstant + Display,
    V: Clone + Ord + Display,
>(
    constraint: &GroupedExpression<T, V>,
) -> Option<(Vec<(T::FieldType, GroupedExpression<T, V>)>, T::FieldType)> {
    let (quadratic, linear, constant) = constraint.components();
    if !quadratic.is_empty() {
        // We cannot split quadratic constraints.
        return None;
    }
    let constant = constant.try_to_number()?;

    // Group the linear part by absolute coefficients.
    let components = linear
        .map(|(var, coeff)| Some((coeff.try_to_number()?, var.clone())))
        .collect::<Option<Vec<_>>>()?
        .into_iter()
        .map(|(coeff, var)| {
            if coeff.is_in_lower_half() {
                (coeff, GroupedExpression::from_unknown_variable(var))
            } else {
                (-coeff, -GroupedExpression::from_unknown_variable(var))
            }
        })
        .into_grouping_map()
        .sum()
        .into_iter()
        .filter(|(_, expr)| !expr.is_zero())
        .sorted_by_key(|(c, _)| c.to_integer())
        .collect_vec();

    Some((components, constant))
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use itertools::Itertools;

    use crate::{
        grouped_expression::GroupedExpression,
        range_constraint::RangeConstraint,
        solver::constraint_splitter::try_split_constraint,
        test_utils::{constant, var},
    };

    #[test]
    fn split_simple() {
        let four_bit_rc = RangeConstraint::from_mask(0xfu32);
        let rcs = [
            ("x", four_bit_rc.clone()),
            ("y", four_bit_rc.clone()),
            ("a", four_bit_rc.clone()),
            ("b", four_bit_rc.clone()),
        ]
        .into_iter()
        .collect::<HashMap<_, _>>();
        let expr = var("x") + var("y") * constant(255) - var("a") + var("b") * constant(255);
        let items = try_split_constraint(&expr, &rcs).unwrap().iter().join("\n");
        assert_eq!(
            items,
            "-(a - x)
b + y"
        );
    }

    #[test]
    fn split_multiple() {
        let four_bit_rc = RangeConstraint::from_mask(0xfu32);
        let rcs = [
            ("x", four_bit_rc.clone()),
            ("y", four_bit_rc.clone()),
            ("a", four_bit_rc.clone()),
            ("b", four_bit_rc.clone()),
            ("r", four_bit_rc.clone()),
            ("s", four_bit_rc.clone()),
            ("w", four_bit_rc.clone()),
        ]
        .into_iter()
        .collect::<HashMap<_, _>>();
        let expr = var("x") + var("y") * constant(50) - var("a") + var("b") * constant(50)
            - var("r") * constant(6000)
            + var("s") * constant(6000)
            + var("w") * constant(1200000);
        let items = try_split_constraint(&expr, &rcs).unwrap().iter().join("\n");
        assert_eq!(
            items,
            "-(a - x)
b + y
-(r - s)
w"
        );
    }

    #[test]
    fn split_bit_decomp_bug() {
        let lin = GroupedExpression::from_unknown_variable("lin");
        let result = GroupedExpression::from_unknown_variable("result");
        let constr = lin.clone() - constant(4) * result.clone() - constant(4);
        let range_constraints = HashMap::from([
            ("lin", RangeConstraint::from_mask(0x8u32)),
            ("result", RangeConstraint::from_mask(0x1u32)),
        ]);
        let result = try_split_constraint(&constr, &range_constraints).unwrap();
    }

    #[test]
    fn split_seqz() {
        // From the seqz instruction:
        // (b__3_0 - b_msb_f_0) * (b_msb_f_0 + 256 - b__3_0) = 0
        // After boolean extraction:
        // b__3_0 - b_msb_f_0 + 256 * x = 0;
        // or:
        // b__3_0 - b_msb_f_0 + 256 * (1 - x) = 0;

        let byte_rc = RangeConstraint::from_mask(0xffu32);
        let bit_rc = RangeConstraint::from_mask(0x1u32);
        let rcs = [
            ("b__3_0", byte_rc.clone()),
            ("b_msb_f_0", byte_rc.clone()),
            ("x", bit_rc.clone()),
        ]
        .into_iter()
        .collect::<HashMap<_, _>>();
        let expr1 = var("b__3_0") - var("b_msb_f_0") + constant(256) * var("x");
        let items = try_split_constraint(&expr1, &rcs)
            .unwrap()
            .iter()
            .join("\n");
        assert_eq!(
            items,
            "b__3_0 - b_msb_f_0
x"
        );
        let expr2 = var("b__3_0") - var("b_msb_f_0") + constant(256) * (var("x") - constant(1));
        let items = try_split_constraint(&expr2, &rcs)
            .unwrap()
            .iter()
            .join("\n");
        assert_eq!(
            items,
            "b__3_0 - b_msb_f_0
x - 1"
        );
    }

    #[test]
    fn split_multiple_with_const() {
        let four_bit_rc = RangeConstraint::from_mask(0xfu32);
        let rcs = [
            ("x", four_bit_rc.clone()),
            ("y", four_bit_rc.clone()),
            ("a", four_bit_rc.clone()),
            ("b", four_bit_rc.clone()),
            ("r", four_bit_rc.clone()),
            ("s", four_bit_rc.clone()),
            ("w", four_bit_rc.clone()),
        ]
        .into_iter()
        .collect::<HashMap<_, _>>();
        let expr = var("x") + var("y") * constant(64)
            - var("a")
            - var("b") * constant(64)
            - var("r") * constant(65536)
            + var("s") * constant(65536)
            + var("w") * constant(0x1000000)
            - constant(5 * 0x1000000 - 6 + 64 - 5 * 65536);

        let items = try_split_constraint(&expr, &rcs).unwrap().iter().join("\n");
        assert_eq!(
            items,
            "-(a - x - 6)
-(b - y + 1)
-(r - s - 5)
w - 5"
        );
    }

    #[test]
    fn split_limb_decomposition() {
        let four_bit_rc = RangeConstraint::from_mask(0xfu32);
        let rcs = [
            ("l0", four_bit_rc.clone()),
            ("l1", four_bit_rc.clone()),
            ("l2", four_bit_rc.clone()),
            ("l3", four_bit_rc.clone()),
        ]
        .into_iter()
        .collect::<HashMap<_, _>>();
        let expr = var("l0")
            + var("l1") * constant(0x10)
            + var("l2") * constant(0x100)
            + var("l3") * constant(0x1000)
            - constant(0x1234);

        let items = try_split_constraint(&expr, &rcs).unwrap().iter().join("\n");
        assert_eq!(
            items,
            "l0 - 4
l1 - 3
l2 - 2
l3 - 1"
        );
    }
}
