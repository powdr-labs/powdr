use std::{
    fmt::Display,
    ops::{Add, Div},
};

use itertools::Itertools;
use num_traits::Zero;
use powdr_number::FieldElement;

use crate::{
    constraint_system::AlgebraicConstraint,
    grouped_expression::{GroupedExpression, RangeConstraintProvider},
    runtime_constant::RuntimeConstant,
};

/// Tries to split the given constraint into a list of equivalent constraints.
/// This is the case for example if the variables in this expression can
/// be split into different bit areas.
pub fn try_split_constraint<T: RuntimeConstant + Display, V: Clone + Ord + Display>(
    constraint: &AlgebraicConstraint<GroupedExpression<T, V>>,
    range_constraints: &impl RangeConstraintProvider<T::FieldType, V>,
) -> Option<Vec<AlgebraicConstraint<GroupedExpression<T, V>>>> {
    let (quadratic, linear, constant) = constraint.expression.components();
    if !quadratic.is_empty() {
        // We cannot split quadratic constraints.
        return None;
    }
    let mut constant = constant.try_to_number()?;

    // Group the linear part by absolute coefficients.
    let mut components = group_components_by_absolute_coefficients(
        linear
            .map(|(var, coeff)| Component::try_from((var, coeff)).ok())
            .collect::<Option<Vec<_>>>()?,
    )
    .collect_vec();
    if components.len() < 2 {
        return None;
    }

    // The original constraint is equivalent to `sum of components + constant`

    // Now try to split out each component in turn.
    let mut extracted_parts = vec![];
    for index in 0..components.len() {
        let candidate = &components[index];
        let rest = components
            .iter()
            .enumerate()
            .filter(|(i, component)| *i != index && !component.is_zero())
            .map(|(_, comp)| (comp.clone() / candidate.coeff).normalize())
            .collect_vec();
        // The original constraint is equivalent to
        // `candidate.expr + rest + constant / candidate.coeff = 0`.
        // Try to find the unique value for `candidate.expr` in this equation.
        if let Some(solution) = find_solution(
            &candidate.expr,
            rest,
            constant / candidate.coeff,
            range_constraints,
        ) {
            // We now know that `candidate.expr = solution`, so we add it to the extracted parts.
            extracted_parts.push(AlgebraicConstraint::assert_eq(
                candidate.expr.clone(),
                GroupedExpression::from_number(solution),
            ));
            // Add `solution * candidate.coeff - candidate` (which is zero) to our expression
            // by replacing the component by zero and adding `solution * candidate.coeff` to the
            // constant.
            constant += solution * candidate.coeff;
            components[index] = Zero::zero();
        }
    }
    if extracted_parts.is_empty() {
        None
    } else {
        // We found some independent parts, add the remaining components to the parts
        // and return them.
        extracted_parts.push(recombine_rest(components, constant));
        Some(extracted_parts)
    }
}

/// Groups a sequence of components (thought of as a sum) by absolute coefficients
/// so that its sum does not change.
/// The list is sorted by the coefficient.
fn group_components_by_absolute_coefficients<
    T: RuntimeConstant + Display,
    V: Clone + Ord + Display,
>(
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
/// `expr + rest + constant = 0`.
fn find_solution<T: RuntimeConstant + Display, V: Clone + Ord + Display>(
    expr: &GroupedExpression<T, V>,
    rest: Vec<Component<T, V>>,
    constant: T::FieldType,
    range_constraints: &impl RangeConstraintProvider<T::FieldType, V>,
) -> Option<T::FieldType> {
    if rest.is_empty() {
        // We are done anyway.
        return None;
    }

    // We try to find some `k` such that the equation has the form
    // `expr + k * rest + constant = 0` and it is equivalent to
    // the same expression in the integers. Then we apply `x -> x % k` to the whole equation
    // to obtain `expr % k + constant % k = 0` and then we hope that this has a unique solution.

    // GCD would be best, but we just try the samllest coefficient in `rest`.
    let smallest_coeff = rest.iter().map(|comp| comp.coeff).min().unwrap();
    assert_ne!(smallest_coeff, 0.into());
    assert!(smallest_coeff.is_in_lower_half());

    let rest: GroupedExpression<_, _> = rest
        .into_iter()
        .map(|comp| GroupedExpression::from(comp / smallest_coeff))
        .sum();

    let candidate_rc = expr.range_constraint(range_constraints);

    if candidate_rc.is_unconstrained()
        || rest
            .range_constraint(range_constraints)
            .multiple(smallest_coeff)
            .is_unconstrained()
    {
        return None;
    }
    candidate_rc.has_unique_modular_solution(-constant, smallest_coeff)
}

fn recombine_rest<T: RuntimeConstant + Display, V: Clone + Ord + Display>(
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
                expr: -self.expr.clone(),
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
        println!(
            "Trying to split: {}\n{}",
            expr,
            expr.clone() * T::one().field_div(&T::from_u64(30720))
        );
        try_split_constraint(&AlgebraicConstraint::assert_zero(expr), rcs).map(|c| {
            println!(
                "Split into:\n{}",
                c.iter().map(|c| c.to_string()).join("\n")
            );
            c
        })
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
        let items = try_split(expr, &rcs).unwrap().iter().join("\n");
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
        let items = try_split(expr, &rcs).unwrap().iter().join("\n");
        assert_eq!(
            items,
            "-(a - x)
b + y
-(r - s)
w"
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
            "b__3_0 - b_msb_f_0
x"
        );
        let expr2 = var("b__3_0") - var("b_msb_f_0") + constant(256) * (var("x") - constant(1));
        let items = try_split(expr2, &rcs).unwrap().iter().join("\n");
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

        let items = try_split(expr, &rcs).unwrap().iter().join("\n");
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

        let items = try_split(expr, &rcs).unwrap().iter().join("\n");
        assert_eq!(
            items,
            "l0 - 4
l1 - 3
l2 - 2
l3 - 1"
        );
    }

    #[test]
    fn correct_split() {
        // 7864320 * a__0_12 - bool_113 + 314572801
        // a__0_12 + 256 * bool_113 - 216
        let byte_rc = RangeConstraint::from_mask(0xffu32);
        let bit_rc = RangeConstraint::from_mask(0x1u32);
        let rcs = [("a__0_12", byte_rc.clone()), ("bool_113", bit_rc.clone())]
            .into_iter()
            .collect::<HashMap<_, _>>();
        let expr1: GroupedExpression<BabyBearField, _> =
            GroupedExpression::from_unknown_variable("a__0_12")
                * GroupedExpression::from_number(BabyBearField::from(7864320))
                - GroupedExpression::from_unknown_variable("bool_113")
                + GroupedExpression::from_number(BabyBearField::from(314572801));
        assert!(try_split(expr1, &rcs).is_some());
        let expr2: GroupedExpression<BabyBearField, _> =
            GroupedExpression::from_unknown_variable("a__0_12")
                + GroupedExpression::from_unknown_variable("bool_113")
                    * GroupedExpression::from_number(BabyBearField::from(256))
                - GroupedExpression::from_number(BabyBearField::from(216));
        assert!(try_split(expr2, &rcs).is_some());
    }

    #[test]
    fn invalid_split() {
        // 30720 * a__0_12 + 7864320 * a__1_12 - bool_114 - 6635520
        // a__0_12 + 256 * a__1_12 + 65536 * bool_114 - 216
        let byte_rc = RangeConstraint::from_mask(0xffu32);
        let bit_rc = RangeConstraint::from_mask(0x1u32);
        let rcs = [
            ("a__0_12", byte_rc.clone()),
            ("a__1_12", byte_rc),
            ("bool_114", bit_rc),
        ]
        .into_iter()
        .collect::<HashMap<_, _>>();
        let expr: GroupedExpression<BabyBearField, _> =
            GroupedExpression::from_unknown_variable("a__0_12")
                * GroupedExpression::from_number(BabyBearField::from(30720))
                + GroupedExpression::from_unknown_variable("a__1_12")
                    * GroupedExpression::from_number(BabyBearField::from(7864320))
                - GroupedExpression::from_unknown_variable("bool_114")
                - GroupedExpression::from_number(BabyBearField::from(6635520));
        assert!(try_split(expr, &rcs).is_some());
    }
}
