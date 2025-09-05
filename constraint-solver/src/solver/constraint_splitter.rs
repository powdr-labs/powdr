use std::{
    fmt::Display,
    ops::{Add, Div},
};

use itertools::Itertools;
use num_traits::Zero;
use powdr_number::{FieldElement, LargeInt};

use crate::{
    constraint_system::AlgebraicConstraint,
    grouped_expression::{GroupedExpression, RangeConstraintProvider},
    range_constraint::RangeConstraint,
    runtime_constant::RuntimeConstant,
};

/// Tries to split the given algebraic constraint into a list of equivalent
/// algebraic constraints.
/// This is the case for example if the variables in this expression can
/// be split into different bit areas.
///
/// The core idea (which is applied multiple times) is as follows:
///
/// Suppose we have the constraint `x + k * y + c = 0` with `x` and `y` being
/// variables (or expressions containing variables) and `k` and `c` are constants.
/// Furthermore, the range constraints of `x` and `y` are such that no wrapping
/// occurs in the operations, i.e. the constraint is equivalent to the same
/// constraint in the natural numbers.
///
/// Then the same constraint is also true modulo `k`, where we get
/// `x % k + c % k = 0`. If this equation has a unique solution `s` in the range
/// constraints for `x`, we get a new constraint `x - s = 0`. We can subtract
/// that constraint from the original to get `k * y + c - s = 0` and iterate.
pub fn try_split_constraint<T: RuntimeConstant + Display, V: Clone + Ord + Display>(
    constraint: &AlgebraicConstraint<&GroupedExpression<T, V>>,
    range_constraints: &impl RangeConstraintProvider<T::FieldType, V>,
) -> Option<Vec<AlgebraicConstraint<GroupedExpression<T, V>>>> {
    let (quadratic, linear, constant) = constraint.expression.components();
    if !quadratic.is_empty() {
        // We cannot split quadratic constraints.
        return None;
    }
    if linear
        .clone()
        .any(|(var, _)| range_constraints.get(var).is_unconstrained())
    {
        // If any variable is unconstrained, we cannot split.
        return None;
    }

    let mut constant = constant.try_to_number()?;

    // Turn the linear part into components ("coefficient * expression"),
    // and combine components with the same coefficient, ending up with
    // components of the form "coefficient * (var1 + var2 - var3)".
    let mut components = group_components_by_coefficients(
        linear
            .map(|(var, coeff)| Component::try_from((var, coeff)).ok())
            .collect::<Option<Vec<_>>>()?,
    )
    .collect_vec();
    if components.len() < 2 {
        return None;
    }

    // The original constraint is equivalent to `sum of components + constant = 0`

    // Now try to split out each component in turn, modifying `components`
    // and `constant` for every successful split.
    let mut extracted_parts = vec![];
    for index in 0..components.len() {
        let candidate = &components[index];
        let rest = components
            .iter()
            .enumerate()
            // Filter out the candidate itself and all zero components
            // because we set split components to zero when we extract them.
            .filter(|(i, component)| *i != index && !component.is_zero())
            .map(|(_, comp)| (comp.clone() / candidate.coeff).normalize())
            .collect_vec();
        if rest.is_empty() {
            // Nothing to split, we are done.
            break;
        }

        // The original constraint is equivalent to
        // `candidate.expr + rest + constant / candidate.coeff = 0`.

        // The idea is to find some `k` such that the equation has the form
        // `expr + k * rest' + constant' = 0` and it is equivalent to
        // the same expression in the natural numbers. Then we apply `x -> x % k` to the whole equation
        // to obtain `expr % k + constant' % k = 0`. Finally, we check if it has a unique solution.

        // We start by finding a good `k`. It is likely wo work better if the factor exists
        // in all components of `rest`, so the GCD of the coefficients of the components would
        // be best, but we just try the smallest coefficient.
        let smallest_coeff_in_rest = rest.iter().map(|comp| comp.coeff).min().unwrap();
        assert_ne!(smallest_coeff_in_rest, 0.into());
        assert!(smallest_coeff_in_rest.is_in_lower_half());

        // Try to find the unique value for `candidate.expr` in this equation.
        if let Some(solution) = find_solution(
            &candidate.expr,
            smallest_coeff_in_rest,
            rest.into_iter()
                .map(|comp| GroupedExpression::from(comp / smallest_coeff_in_rest))
                .sum(),
            constant / candidate.coeff,
            range_constraints,
        ) {
            // We now know that `candidate.expr = solution`, so we add it to the extracted parts.
            extracted_parts.push(AlgebraicConstraint::assert_eq(
                candidate.expr.clone(),
                GroupedExpression::from_number(solution),
            ));
            // We remove the candidate (`candidate.coeff * candidate.expr`) from the expression.
            // To balance this out, we add `candidate.coeff * candidate.expr = candidate.coeff * solution`
            // to the constant.
            constant += solution * candidate.coeff;
            components[index] = Zero::zero();
        }
    }
    if extracted_parts.is_empty() {
        None
    } else {
        // We found some independent parts, add the remaining components to the parts
        // and return them.
        extracted_parts.push(recombine_components(components, constant));
        Some(extracted_parts)
    }
}

/// Groups a sequence of components (thought of as a sum) by coefficients
/// so that its sum does not change.
/// Before grouping, the components are normalized such that the coefficient is always
/// in the lower half of the field (and the expression might be negated to compensate).
/// The list is sorted by the coefficient.
fn group_components_by_coefficients<T: RuntimeConstant + Display, V: Clone + Ord + Display>(
    components: impl IntoIterator<Item = Component<T, V>>,
) -> impl Iterator<Item = Component<T, V>> {
    components
        .into_iter()
        .map(|c| c.normalize())
        .into_grouping_map_by(|c| c.coeff)
        .sum()
        .into_iter()
        .filter(|(_, expr)| !expr.is_zero())
        .map(|(_, comp)| comp)
        .sorted_by_key(|comp| comp.coeff.to_integer())
}

/// If this returns `Some(x)`, then `x` is the only valid value for `expr` in the equation
/// `expr + coefficient * rest + constant = 0`.
/// It does not make assumptions about its inputs.
/// We try to translate the equation to an equation in the natural numbers
/// and try to find a unique solution.
fn find_solution<T: RuntimeConstant + Display, V: Clone + Ord + Display>(
    expr: &GroupedExpression<T, V>,
    coefficient: T::FieldType,
    rest: GroupedExpression<T, V>,
    constant: T::FieldType,
    range_constraints: &impl RangeConstraintProvider<T::FieldType, V>,
) -> Option<T::FieldType> {
    let expr_rc = expr.range_constraint(range_constraints);
    let rest_rc = rest.range_constraint(range_constraints);

    let unconstrained_range_width = RangeConstraint::<T::FieldType>::unconstrained().range_width();
    if expr_rc.range_width() == unconstrained_range_width
        || rest_rc.range_width() == unconstrained_range_width
    {
        // We probably cannot translate this into the natural numbers.
        return None;
    }

    // Both range constraints have a "gap'. We shift the gap such that the
    // lower bounds for both `expr` and `rest` are zero.
    if expr_rc.range().0 != 0.into() {
        let shift = expr_rc.range().0;
        return find_solution(
            &(expr - &GroupedExpression::from_number(shift)),
            coefficient,
            rest,
            constant + shift,
            range_constraints,
        )
        .map(|s| s + shift);
    } else if rest_rc.range().0 != 0.into() {
        return find_solution(
            expr,
            coefficient,
            rest - GroupedExpression::from_number(rest_rc.range().0),
            constant + coefficient * rest_rc.range().0,
            range_constraints,
        );
    }

    // rc(expr): [0, max_expr]
    // rc(rest): [0, max_rest]
    // If max_expr + k * max_rest < P, then we can translate the equation to the natural numbers:
    // expr + k * rest = (-constant) % modulus

    let max_expr = expr_rc.range().1;
    let max_rest = rest_rc.range().1;

    // Evaluate `expr + smallest_coeff * rest` for the largest possible value
    // and see if it wraps around in the field.
    if max_expr.to_arbitrary_integer()
        + coefficient.to_arbitrary_integer() * max_rest.to_arbitrary_integer()
        >= T::FieldType::modulus().to_arbitrary_integer()
    {
        return None;
    }
    // It does not wrap around, so we know that the equation can be translated to the
    // natural numbers:
    // expr + smallest_coeff * rest = (-constant) % modulus

    // Next, we apply `x -> x % smallest_coeff` to both sides of the equation to get
    // expr % smallest_coeff = ((-constant) % modulus) % smallest_coeff
    // Note that at this point, we only get an implication, not an equivalence,
    // but if the range constraints of `expr` only allow a unique solution,
    // it holds unconditionally.

    if max_expr.to_integer() >= coefficient.to_integer() + coefficient.to_integer() {
        // In this case, there are always at least two solutions (ignoring masks and other
        // constraints).
        return None;
    }

    // TODO this only works for fields that fit 64 bits, but that is probably fine for now.
    let rhs = T::FieldType::from(
        (-constant).to_integer().try_into_u64().unwrap()
            % coefficient.to_integer().try_into_u64().unwrap(),
    );

    // Now we try `rhs`, `rhs + smallest_coeff`, `rhs + 2 * smallest_coeff`, ...
    // But because of the check above, we can stop at `2 * smallest_coeff`.
    (0..=2)
        .map(|i| rhs + T::FieldType::from(i) * coefficient)
        .filter(|candidate| expr_rc.allows_value(*candidate))
        .exactly_one()
        .ok()
}

/// Turns the remaining components and constant into a single constraint,
/// i.e. returns an algebraic constraint that is equivalent to
/// `sum of components + constant = 0`.
fn recombine_components<T: RuntimeConstant + Display, V: Clone + Ord + Display>(
    components: Vec<Component<T, V>>,
    constant: T::FieldType,
) -> AlgebraicConstraint<GroupedExpression<T, V>> {
    let remaining = components
        .into_iter()
        .filter(|comp| !comp.is_zero())
        .collect_vec();
    AlgebraicConstraint::assert_zero(match remaining.as_slice() {
        [Component { coeff, expr }] => {
            // if there is only one component, we normalize
            expr + &GroupedExpression::from_number(constant / *coeff)
        }
        _ => {
            remaining
                .into_iter()
                .map(|comp| comp.into())
                .sum::<GroupedExpression<_, _>>()
                + GroupedExpression::from_number(constant)
        }
    })
}

/// A component of a constraint. Equivalent to the expression `coeff * expr`.
#[derive(Clone)]
struct Component<T: RuntimeConstant, V> {
    coeff: T::FieldType,
    expr: GroupedExpression<T, V>,
}

impl<T: RuntimeConstant + Display, V: Clone + Ord + Display> Display for Component<T, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} * ({})", self.coeff, self.expr)
    }
}

impl<'a, T: RuntimeConstant, V: Ord + Clone + Eq> TryFrom<(&'a V, &'a T)> for Component<T, V> {
    type Error = ();
    fn try_from((var, coeff): (&'a V, &'a T)) -> Result<Self, ()> {
        let coeff = coeff.try_to_number().ok_or(())?;
        let expr = GroupedExpression::from_unknown_variable(var.clone());
        Ok(Self { coeff, expr })
    }
}

impl<T: RuntimeConstant, V: Ord + Clone + Eq> Component<T, V> {
    /// Normalize the component such that the coefficient is positive.
    fn normalize(self) -> Self {
        if self.coeff.is_in_lower_half() {
            self
        } else {
            Self {
                coeff: -self.coeff,
                expr: -self.expr,
            }
        }
    }
}

impl<T: RuntimeConstant, V: Ord + Clone + Eq> Add for Component<T, V> {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        assert!(self.coeff == other.coeff);
        Self {
            coeff: self.coeff,
            expr: self.expr + other.expr,
        }
    }
}

impl<T: RuntimeConstant, V: Ord + Clone + Eq> Div<T::FieldType> for Component<T, V> {
    type Output = Self;

    fn div(self, rhs: T::FieldType) -> Self {
        assert!(!rhs.is_zero());
        Self {
            coeff: self.coeff / rhs,
            expr: self.expr,
        }
    }
}

impl<T: RuntimeConstant, V: Ord + Clone + Eq> From<Component<T, V>> for GroupedExpression<T, V> {
    fn from(comp: Component<T, V>) -> Self {
        comp.expr * T::from(comp.coeff)
    }
}

impl<T: RuntimeConstant, V: Clone + Ord> Zero for Component<T, V> {
    fn zero() -> Self {
        Self {
            coeff: T::FieldType::zero(),
            expr: GroupedExpression::zero(),
        }
    }

    fn is_zero(&self) -> bool {
        self.coeff.is_zero() || self.expr.is_zero()
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use expect_test::expect;
    use itertools::Itertools;
    use powdr_number::BabyBearField;

    use super::*;
    use crate::{
        range_constraint::RangeConstraint,
        test_utils::{constant, var},
    };

    fn try_split<T: RuntimeConstant + Display, V: Clone + Ord + Display>(
        expr: GroupedExpression<T, V>,
        rcs: &impl RangeConstraintProvider<T::FieldType, V>,
    ) -> Option<Vec<AlgebraicConstraint<GroupedExpression<T, V>>>> {
        try_split_constraint(&AlgebraicConstraint::assert_zero(&expr), rcs)
    }

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
        let items = try_split(expr, &rcs).unwrap().iter().join(", ");

        expect!["-(a - x) = 0, b + y = 0"].assert_eq(&items);
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
        let items = try_split(expr, &rcs).unwrap().iter().join("\n");
        assert_eq!(
            items,
            "-(a - x) = 0
b + y = 0
-(r - s) = 0
w = 0"
        );
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
        let items = try_split(expr1, &rcs).unwrap().iter().join("\n");
        assert_eq!(
            items,
            "b__3_0 - b_msb_f_0 = 0
x = 0"
        );
        let expr2 = var("b__3_0") - var("b_msb_f_0") + constant(256) * (var("x") - constant(1));
        let items = try_split(expr2, &rcs).unwrap().iter().join("\n");
        assert_eq!(
            items,
            "b__3_0 - b_msb_f_0 = 0
x - 1 = 0"
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

        let items = try_split(expr, &rcs).unwrap().iter().join("\n");
        assert_eq!(
            items,
            "-(a - x - 6) = 0
-(b - y + 1) = 0
-(r - s - 5) = 0
w - 5 = 0"
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

        let items = try_split(expr, &rcs).unwrap().iter().join("\n");
        assert_eq!(
            items,
            "l0 - 4 = 0
l1 - 3 = 0
l2 - 2 = 0
l3 - 1 = 0"
        );
    }

    #[test]
    fn negated_and_unnegated() {
        // 7864320 * a__0_12 - bool_113 + 314572801
        // a__0_12 + 256 * bool_113 - 216
        let byte_rc = RangeConstraint::from_mask(0xffu32);
        let bit_rc = RangeConstraint::from_mask(0x1u32);
        let rcs = [("a__0_12", byte_rc.clone()), ("bool_113", bit_rc.clone())]
            .into_iter()
            .collect::<HashMap<_, _>>();
        let expr1: GroupedExpression<BabyBearField, _> =
            -(GroupedExpression::from_unknown_variable("a__0_12")
                * GroupedExpression::from_number(BabyBearField::from(7864320))
                - GroupedExpression::from_unknown_variable("bool_113")
                + GroupedExpression::from_number(BabyBearField::from(314572801)));

        // Split `expr1` and `-expr1`, the result should be equivalent.
        let first = try_split(expr1.clone(), &rcs)
            .unwrap()
            .into_iter()
            .join(", ");
        expect!["bool_113 = 0, -(a__0_12 - 216) = 0"].assert_eq(&first);
        let expr2 = -expr1;
        let second = try_split(expr2, &rcs).unwrap().into_iter().join(", ");
        expect!["-(bool_113) = 0, a__0_12 - 216 = 0"].assert_eq(&second);
    }

    #[test]
    fn wrapping_1() {
        // -(c__1_3) + 256 * (30720 * c__0_3 - c__2_3) = 1226833928
        let byte_rc = RangeConstraint::from_mask(0xffu32);
        let rcs = [
            ("c__0_3", byte_rc.clone()),
            ("c__1_3", byte_rc.clone()),
            ("c__2_3", byte_rc.clone()),
        ]
        .into_iter()
        .collect::<HashMap<_, _>>();
        let expr: GroupedExpression<BabyBearField, _> =
            -GroupedExpression::from_unknown_variable("c__1_3")
                + GroupedExpression::from_number(BabyBearField::from(256))
                    * (GroupedExpression::from_number(BabyBearField::from(30720))
                        * GroupedExpression::from_unknown_variable("c__0_3")
                        - GroupedExpression::from_unknown_variable("c__2_3"))
                - GroupedExpression::from_number(BabyBearField::from(1226833928));
        let result = try_split(expr.clone(), &rcs).unwrap().iter().join(", ");
        expect!["-(c__1_3 - 248) = 0, c__0_3 - 157 = 0, -(c__2_3 - 30719) = 0"].assert_eq(&result);

        let mut expr = expr;
        expr.substitute_by_known(&"c__0_3", &BabyBearField::from(157));
        expr.substitute_by_known(&"c__1_3", &BabyBearField::from(248));
        expr.substitute_by_known(&"c__2_3", &BabyBearField::from(30719));
        assert!(expr.is_zero());
    }

    #[test]
    fn wrapping_2() {
        // bool_17 + 943718400 * (a__0_0) = 1069547521
        let bit_rc = RangeConstraint::from_mask(0x1u32);
        let rcs = [("bool_17", bit_rc.clone()), ("a__0_0", bit_rc.clone())]
            .into_iter()
            .collect::<HashMap<_, _>>();
        let expr: GroupedExpression<BabyBearField, _> =
            GroupedExpression::from_unknown_variable("bool_17")
                + GroupedExpression::from_number(BabyBearField::from(943718400))
                    * GroupedExpression::from_unknown_variable("a__0_0")
                - GroupedExpression::from_number(BabyBearField::from(1069547521));
        let result = try_split(expr.clone(), &rcs).unwrap().iter().join(", ");
        expect!["bool_17 = 0, a__0_0 + 1 = 0"].assert_eq(&result);
    }

    #[test]
    fn split_at_boundary() {
        let bit_rc = RangeConstraint::from_mask(0x1u32);
        let limb_rc = RangeConstraint::from_mask(0x7fffu32);
        let rcs = [
            ("bool_103", bit_rc.clone()),
            ("to_pc_least_sig_bit_4", bit_rc.clone()),
            ("to_pc_limbs__0_4", limb_rc.clone()),
        ]
        .into_iter()
        .collect::<HashMap<_, _>>();
        let expr: GroupedExpression<BabyBearField, _> =
            GroupedExpression::from_unknown_variable("bool_103")
                + GroupedExpression::from_number(BabyBearField::from(30720))
                    * (GroupedExpression::from_unknown_variable("to_pc_least_sig_bit_4")
                        + GroupedExpression::from_number(BabyBearField::from(2))
                            * GroupedExpression::from_unknown_variable("to_pc_limbs__0_4"))
                - GroupedExpression::from_number(BabyBearField::from(30720 * 123 + 1));
        let items = try_split(expr, &rcs).unwrap().iter().join(", ");
        assert_eq!(
            items,
            "bool_103 - 1 = 0, to_pc_least_sig_bit_4 - 1 = 0, to_pc_limbs__0_4 - 61 = 0"
        );
    }

    #[test]
    fn bit_decomposition_bug() {
        // This tests against a bug that was present in the old bit
        // decomposition algorithm.
        let lin = var("lin");
        let result = var("result");
        let constr = lin.clone() - constant(4) * result.clone() - constant(4);
        let range_constraints = HashMap::from([
            ("lin", RangeConstraint::from_mask(0x8u32)),
            ("result", RangeConstraint::from_mask(0x1u32)),
        ]);
        // We try to solve `lin - 4 * result = 4` and the problem is
        // that we cannot assign `lin = 4 & mask` for some mask, since
        // it needs to be assigned `8`.
        assert!(try_split(constr, &range_constraints).is_none());
    }

    #[test]
    fn split_fail_overlapping() {
        let four_bit_rc = RangeConstraint::from_mask(0xfu32);
        let rcs = [("x", four_bit_rc.clone()), ("y", four_bit_rc.clone())]
            .into_iter()
            .collect::<HashMap<_, _>>();
        // The RC of x is not tight enough
        let expr = var("x") + var("y") * constant(2);
        assert!(try_split(expr, &rcs).is_none());
    }

    #[test]
    fn split_fail_not_unique() {
        let four_bit_rc = RangeConstraint::from_mask(0xfu32);
        let rcs = [
            ("x", four_bit_rc.clone()),
            ("y", four_bit_rc.clone()),
            ("z", four_bit_rc.clone()),
        ]
        .into_iter()
        .collect::<HashMap<_, _>>();
        // There are multiple ways to solve the modulo equation.
        let expr = (var("x") - var("y")) + constant(16) * var("z") - constant(1);
        assert!(try_split(expr, &rcs).is_none());

        // If we adjust the constant, it works.
        let expr = (var("x") - var("y")) + constant(16) * var("z") - constant(0);
        let result = try_split(expr.clone(), &rcs).unwrap().iter().join(", ");
        expect!["x - y = 0, z = 0"].assert_eq(&result);
    }
}
