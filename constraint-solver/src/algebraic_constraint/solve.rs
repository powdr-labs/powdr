use std::{collections::HashSet, fmt::Display, hash::Hash};

use itertools::Itertools;
use num_traits::Zero;
use powdr_number::{log2_exact, ExpressionConvertible, FieldElement, LargeInt};

use crate::{
    algebraic_constraint::AlgebraicConstraint,
    effect::{Assertion, BitDecomposition, BitDecompositionComponent, Condition, Effect},
    grouped_expression::{GroupedExpression, RangeConstraintProvider},
    range_constraint::RangeConstraint,
    runtime_constant::RuntimeConstant,
};

#[derive(Default)]
pub struct ProcessResult<T: RuntimeConstant, V> {
    pub effects: Vec<Effect<T, V>>,
    pub complete: bool,
}

impl<T: RuntimeConstant, V> ProcessResult<T, V> {
    pub fn empty() -> Self {
        Self {
            effects: vec![],
            complete: false,
        }
    }
    pub fn complete(effects: Vec<Effect<T, V>>) -> Self {
        Self {
            effects,
            complete: true,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    /// The range constraints of the parts do not cover the full constant sum.
    ConflictingRangeConstraints,
    /// An equality constraint evaluates to a known-nonzero value.
    ConstraintUnsatisfiable(String),
}

impl<T: RuntimeConstant + Display, V: Ord + Clone + Eq + Hash + Display>
    AlgebraicConstraint<&GroupedExpression<T, V>>
{
    /// Solves the constraint for `variable`. This is only possible if
    /// `variable` does not appear in the quadratic component and
    /// has a coefficient which is known to be not zero.
    ///
    /// Returns the resulting solved grouped expression.
    pub fn try_solve_for(&self, variable: &V) -> Option<GroupedExpression<T, V>> {
        let expression = self.expression;

        if expression
            .quadratic
            .iter()
            .flat_map(|(l, r)| [l, r])
            .flat_map(|c| c.referenced_unknown_variables())
            .contains(variable)
        {
            // The variable is in the quadratic component, we cannot solve for it.
            return None;
        }
        if !expression.linear.get(variable)?.is_known_nonzero() {
            return None;
        }
        let mut result = self.expression.clone();
        let coefficient = result.linear.remove(variable)?;
        Some(result * (-T::one().field_div(&coefficient)))
    }

    /// Algebraically transforms the constraint such that `self = 0` is equivalent
    /// to `expr = result` and returns `result`.
    ///
    /// Returns `None` if it cannot solve (this happens for example if self is quadratic).
    /// Panics if `expr` is quadratic.
    pub fn try_solve_for_expr(
        &self,
        expr: &GroupedExpression<T, V>,
    ) -> Option<GroupedExpression<T, V>> {
        let expression = self.expression;

        assert!(
            expr.is_affine(),
            "Tried to solve for quadratic expression {expr}"
        );
        if expression.is_quadratic() {
            return None;
        }

        // Find a normalization factor by iterating over the variables.
        let normalization_factor = expr
            .referenced_unknown_variables()
            .find_map(|var| {
                let coeff = expression.coefficient_of_variable(var)?;
                // We can only divide if we know the coefficient is non-zero.
                if coeff.is_known_nonzero() {
                    Some(expr.coefficient_of_variable(var).unwrap().field_div(coeff))
                } else {
                    None
                }
            })
            .unwrap_or(T::one());
        let result = expr - &(self.expression.clone() * normalization_factor);

        // Check that the operations removed all variables in `expr` from `self`.
        if !expr
            .referenced_unknown_variables()
            .collect::<HashSet<_>>()
            .is_disjoint(
                &result
                    .referenced_unknown_variables()
                    .collect::<HashSet<_>>(),
            )
        {
            // The variables did not fully cancel out
            return None;
        }
        Some(result)
    }
}

impl<
        T: RuntimeConstant + Display + ExpressionConvertible<<T as RuntimeConstant>::FieldType, V>,
        V: Ord + Clone + Eq + Hash + Display,
    > AlgebraicConstraint<&GroupedExpression<T, V>>
{
    /// Solves the equation `self = 0` and returns how to compute the solution.
    /// The solution can contain assignments to multiple variables.
    /// If no way to solve the equation (and no way to derive new range
    /// constraints) has been found, but it still contains
    /// unknown variables, returns an empty, incomplete result.
    /// If the equation is known to be unsolvable, returns an error.
    pub fn solve(
        &self,
        range_constraints: &impl RangeConstraintProvider<T::FieldType, V>,
    ) -> Result<ProcessResult<T, V>, Error> {
        let expression = self.expression;

        if !expression
            .range_constraint(range_constraints)
            .allows_value(Zero::zero())
        {
            return Err(Error::ConstraintUnsatisfiable(self.to_string()));
        }

        Ok(if expression.is_quadratic() {
            self.solve_quadratic(range_constraints)?
        } else if let Some(k) = expression.try_to_known() {
            if k.is_known_nonzero() {
                return Err(Error::ConstraintUnsatisfiable(self.to_string()));
            } else {
                // TODO we could still process more information
                // and reach "unsatisfiable" here.
                ProcessResult::complete(vec![])
            }
        } else {
            self.solve_affine(range_constraints)?
        })
    }

    fn solve_affine(
        &self,
        range_constraints: &impl RangeConstraintProvider<T::FieldType, V>,
    ) -> Result<ProcessResult<T, V>, Error> {
        let expression = self.expression;

        Ok(if expression.linear.len() == 1 {
            let (var, coeff) = expression.linear.iter().next().unwrap();
            // Solve "coeff * X + self.constant = 0" by division.
            assert!(
                !coeff.is_known_zero(),
                "Zero coefficient has not been removed: {self}"
            );
            if coeff.is_known_nonzero() {
                // In this case, we can always compute a solution.
                let value = expression.constant.field_div(&-coeff.clone());
                ProcessResult::complete(vec![assignment_if_satisfies_range_constraints(
                    var.clone(),
                    value,
                    range_constraints,
                )?])
            } else if expression.constant.is_known_nonzero() {
                // If the offset is not zero, then the coefficient must be non-zero,
                // otherwise the constraint is violated.
                let value = expression.constant.field_div(&-coeff.clone());
                ProcessResult::complete(vec![
                    Assertion::assert_is_nonzero(coeff.clone()),
                    assignment_if_satisfies_range_constraints(
                        var.clone(),
                        value,
                        range_constraints,
                    )?,
                ])
            } else {
                // If this case, we could have an equation of the form
                // 0 * X = 0, which is valid and generates no information about X.
                ProcessResult::empty()
            }
        } else {
            // Solve expression of the form `a * X + b * Y + ... + self.constant = 0`
            let r = self.solve_bit_decomposition(range_constraints)?;

            if r.complete {
                r
            } else {
                ProcessResult {
                    effects: self.transfer_constraints(range_constraints),
                    complete: false,
                }
            }
        })
    }

    /// Tries to solve a bit-decomposition equation.
    /// Assumptions:
    /// - The constraint is linear
    fn solve_bit_decomposition(
        &self,
        range_constraints: &impl RangeConstraintProvider<T::FieldType, V>,
    ) -> Result<ProcessResult<T, V>, Error> {
        let expression = self.expression;

        assert!(!expression.is_quadratic());
        // All the coefficients need to be known numbers and the
        // variables need to be range-constrained.
        let constrained_coefficients = expression
            .linear
            .iter()
            .map(|(var, coeff)| {
                let coeff = coeff.try_to_number()?;
                let rc = range_constraints.get(var);
                let is_negative = !coeff.is_in_lower_half();
                let coeff_abs = if is_negative { -coeff } else { coeff };
                // We could work with non-powers of two, but it would require
                // division instead of shifts.
                let exponent = log2_exact(coeff_abs.to_arbitrary_integer())?;
                // We negate here because we are solving
                // c_1 * x_1 + c_2 * x_2 + ... + offset = 0,
                // instead of
                // c_1 * x_1 + c_2 * x_2 + ... = offset.
                Some((var.clone(), rc, !is_negative, coeff_abs, exponent))
            })
            .collect::<Option<Vec<_>>>();
        let Some(constrained_coefficients) = constrained_coefficients else {
            return Ok(ProcessResult::empty());
        };

        // If the offset is a known number, we gradually remove the
        // components from this number.
        let mut offset = expression.constant.try_to_number();
        let mut concrete_assignments = vec![];

        let any_negative = constrained_coefficients
            .iter()
            .any(|(_, _, is_negative, _, _)| *is_negative);

        // Check if they are mutually exclusive and compute assignments.
        let mut covered_bits: <T::FieldType as FieldElement>::Integer = 0.into();
        let mut components: Vec<BitDecompositionComponent<T::FieldType, V>> = vec![];
        for (variable, constraint, is_negative, coeff_abs, exponent) in constrained_coefficients
            .into_iter()
            .sorted_by_key(|(_, _, _, _, exponent)| *exponent)
        {
            let bit_mask = *constraint.multiple(coeff_abs).mask();
            if !(bit_mask & covered_bits).is_zero() {
                // Overlapping range constraints.
                return Ok(ProcessResult::empty());
            } else {
                covered_bits |= bit_mask;
            }

            // If the offset is a known number, we create concrete assignments and modify the offset.
            // if it is not known, we return a BitDecomposition effect.
            if let Some(offset) = &mut offset {
                let mut component = if is_negative { -*offset } else { *offset }.to_integer();
                if component > (T::FieldType::modulus() - 1.into()) >> 1 {
                    // Convert a signed finite field element into two's complement.
                    // a regular subtraction would underflow, so we do this.
                    // We add the difference between negative numbers in the field
                    // and negative numbers in two's complement.
                    component += <T::FieldType as FieldElement>::Integer::MAX
                        - T::FieldType::modulus()
                        + 1.into();
                };
                component &= bit_mask;
                if component >= T::FieldType::modulus() {
                    // If the component does not fit the field, the bit mask is not
                    // tight good enough.
                    return Ok(ProcessResult::empty());
                }
                concrete_assignments.push(
                    // We're not using assignment_if_satisfies_range_constraints here, because we
                    // might still exit early. The error case is handled below.
                    Effect::Assignment(
                        variable.clone(),
                        T::FieldType::from(component >> exponent).into(),
                    ),
                );
                if is_negative {
                    *offset += T::FieldType::from(component);
                } else {
                    *offset -= T::FieldType::from(component);
                }
            } else {
                components.push(BitDecompositionComponent {
                    variable,
                    is_negative,
                    exponent: exponent as u64,
                    bit_mask,
                });
            }
        }

        if covered_bits >= T::FieldType::modulus() {
            return Ok(ProcessResult::empty());
        }

        if let Some(offset) = offset {
            if offset != 0.into() {
                if any_negative {
                    // In case we have negative coefficients, the algorithm
                    // does not always find the correct assignment.
                    return Ok(ProcessResult::empty());
                } else {
                    return Err(Error::ConstraintUnsatisfiable(self.to_string()));
                }
            }
            assert_eq!(concrete_assignments.len(), expression.linear.len());
            Ok(ProcessResult::complete(concrete_assignments))
        } else {
            Ok(ProcessResult::complete(vec![Effect::BitDecomposition(
                BitDecomposition {
                    value: expression.constant.clone(),
                    components,
                },
            )]))
        }
    }

    /// Extract the range constraints from the expression.
    /// Assumptions:
    /// - The expression is linear
    fn transfer_constraints(
        &self,
        range_constraints: &impl RangeConstraintProvider<T::FieldType, V>,
    ) -> Vec<Effect<T, V>> {
        let expression = self.expression;
        // Solve for each of the variables in the linear component and
        // compute the range constraints.
        assert!(!expression.is_quadratic());
        expression
            .linear
            .iter()
            .filter_map(|(var, _)| {
                let rc = self.try_solve_for(var)?.range_constraint(range_constraints);
                Some((var, rc))
            })
            .filter(|(_, constraint)| !constraint.is_unconstrained())
            .map(|(var, constraint)| Effect::RangeConstraint(var.clone(), constraint))
            .collect()
    }

    fn solve_quadratic(
        &self,
        range_constraints: &impl RangeConstraintProvider<T::FieldType, V>,
    ) -> Result<ProcessResult<T, V>, Error> {
        let expression = self.expression;
        let Some((left, right)) = expression.try_as_single_product() else {
            return Ok(ProcessResult::empty());
        };
        // Now we have `left * right = 0`, i.e. one (or both) of them has to be zero.
        let (left_solution, right_solution) = match (
            AlgebraicConstraint::assert_zero(left).solve(range_constraints),
            AlgebraicConstraint::assert_zero(right).solve(range_constraints),
        ) {
            // If one of them is always unsatisfiable, it is equivalent to just solving the other one for zero.
            (Err(_), o) | (o, Err(_)) => {
                return o;
            }
            (Ok(left), Ok(right)) => (left, right),
        };

        if let Some(result) =
            combine_to_conditional_assignment(&left_solution, &right_solution, range_constraints)
        {
            return Ok(result);
        }

        // Now at least combine new range constraints on the same variable.
        // TODO: This will correctly find a bit range constraint on
        // `(X - 1) * X = 0`, but it fails to detect the case of
        // `X * X - X`.
        // This could be fixed by finding a canonical form for the quadratic
        // expression, and normalizing the constants.
        Ok(combine_range_constraints(&left_solution, &right_solution))
    }
}

/// Tries to combine two process results from alternative branches into a
/// conditional assignment.
fn combine_to_conditional_assignment<
    T: RuntimeConstant + ExpressionConvertible<<T as RuntimeConstant>::FieldType, V>,
    V: Ord + Clone + Eq + Display,
>(
    left: &ProcessResult<T, V>,
    right: &ProcessResult<T, V>,
    range_constraints: &impl RangeConstraintProvider<T::FieldType, V>,
) -> Option<ProcessResult<T, V>> {
    let [Effect::Assignment(first_var, first_assignment)] = left.effects.as_slice() else {
        return None;
    };
    let [Effect::Assignment(second_var, second_assignment)] = right.effects.as_slice() else {
        return None;
    };

    if first_var != second_var {
        return None;
    }

    // At this point, we have two assignments to the same variable, i.e.
    // "`X = A` or `X = B`". If the two alternatives can never be satisfied at
    // the same time (i.e. the "or" is exclusive), we can turn this into a
    // conditional assignment.

    let diff = first_assignment.clone() + -second_assignment.clone();
    let diff: GroupedExpression<T::FieldType, V> = diff.try_to_expression(
        &|n| GroupedExpression::from_number(*n),
        &|v| GroupedExpression::from_unknown_variable(v.clone()),
        &|e| e.try_to_number(),
    )?;

    let diff = diff.try_to_known()?.try_to_number()?;
    // `diff = A - B` is a compile-time known number, i.e. `A = B + diff`.
    // Now if `rc + diff` is disjoint from `rc`, it means
    // that if the value that `A` evaluates to falls into the allowed range for `X`,
    // then `B = A + diff` is not a possible value for `X` and vice-versa.
    // This means the two alternatives are disjoint and we can use a conditional assignment.
    let rc = range_constraints.get(first_var);
    if !rc
        .combine_sum(&RangeConstraint::from_value(diff))
        .is_disjoint(&rc)
    {
        return None;
    }

    Some(ProcessResult {
        effects: vec![Effect::ConditionalAssignment {
            variable: first_var.clone(),
            condition: Condition {
                value: first_assignment.clone(),
                condition: rc,
            },
            in_range_value: first_assignment.clone(),
            out_of_range_value: second_assignment.clone(),
        }],
        complete: left.complete && right.complete,
    })
}

/// Turns an effect into a range constraint on a variable.
fn effect_to_range_constraint<T: RuntimeConstant, V: Ord + Clone + Eq>(
    effect: &Effect<T, V>,
) -> Option<(V, RangeConstraint<T::FieldType>)> {
    match effect {
        Effect::RangeConstraint(var, rc) => Some((var.clone(), rc.clone())),
        Effect::Assignment(var, value) => Some((var.clone(), value.range_constraint())),
        _ => None,
    }
}

/// Tries to combine range constraint results from two alternative branches.
/// In some cases, if both branches produce a complete range constraint for the same variable,
/// and those range constraints can be combined without loss, the result is complete as well.
fn combine_range_constraints<T: RuntimeConstant, V: Ord + Clone + Eq + Hash + Display>(
    left: &ProcessResult<T, V>,
    right: &ProcessResult<T, V>,
) -> ProcessResult<T, V> {
    let left_constraints = left
        .effects
        .iter()
        .filter_map(|e| effect_to_range_constraint(e))
        .into_grouping_map()
        .reduce(|rc1, _, rc2| rc1.conjunction(&rc2));
    let right_constraints = right
        .effects
        .iter()
        .filter_map(|e| effect_to_range_constraint(e))
        .into_grouping_map()
        .reduce(|rc1, _, rc2| rc1.conjunction(&rc2));

    let effects = left_constraints
        .iter()
        .filter_map(|(v, rc1)| {
            let rc2 = right_constraints.get(v)?;
            let rc = rc1.disjunction(rc2);
            // This does not capture all cases where the disjunction does not lose information,
            // but we want this to be an indicator of whether we can remove the original
            // constraint, and thus we want it to only hit the "single value" case.
            let complete = rc1.try_to_single_value().is_some()
                && rc2.try_to_single_value().is_some()
                && rc.size_estimate() <= 2.into();
            Some((v, rc, complete))
        })
        .collect_vec();
    // The completeness is tricky, but if there is just a single left effect
    // and a single right effect and the final range constraint is complete,
    // it means that both branches have a concrete assignment for the variable
    // and thus the range constraint is exactly what the original constraint captures.
    let complete = left.effects.len() == 1
        && right.effects.len() == 1
        && effects.len() == 1
        && effects.iter().all(|(_, _, complete)| *complete);
    ProcessResult {
        effects: effects
            .into_iter()
            .map(|(v, rc, _)| Effect::RangeConstraint(v.clone(), rc))
            .collect(),
        complete,
    }
}

fn assignment_if_satisfies_range_constraints<T: RuntimeConstant, V: Ord + Clone + Eq>(
    var: V,
    value: T,
    range_constraints: &impl RangeConstraintProvider<T::FieldType, V>,
) -> Result<Effect<T, V>, Error> {
    let rc = range_constraints.get(&var);
    if rc.is_disjoint(&value.range_constraint()) {
        return Err(Error::ConflictingRangeConstraints);
    }
    Ok(Effect::Assignment(var, value))
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        grouped_expression::NoRangeConstraints,
        symbolic_expression::SymbolicExpression,
        test_utils::{constant, var},
    };

    use super::*;
    use powdr_number::GoldilocksField;

    use pretty_assertions::assert_eq;

    type Qse = GroupedExpression<SymbolicExpression<GoldilocksField, &'static str>, &'static str>;

    #[test]
    fn test_mul() {
        let x = Qse::from_unknown_variable("X");
        let y = Qse::from_unknown_variable("Y");
        let a = Qse::from_known_symbol("A", RangeConstraint::default());
        let t = x * y + a;
        assert_eq!(t.to_string(), "(X) * (Y) + A");
    }

    #[test]
    fn test_add() {
        let x = Qse::from_unknown_variable("X");
        let y = Qse::from_unknown_variable("Y");
        let a = Qse::from_unknown_variable("A");
        let b = Qse::from_known_symbol("B", RangeConstraint::default());
        let t: Qse = x * y - a + b;
        assert_eq!(t.to_string(), "(X) * (Y) - A + B");
        assert_eq!(
            (t.clone() + t).to_string(),
            "(X) * (Y) + (X) * (Y) - 2 * A + (B + B)"
        );
    }

    #[test]
    fn test_mul_by_known() {
        let x = Qse::from_unknown_variable("X");
        let y = Qse::from_unknown_variable("Y");
        let a = Qse::from_known_symbol("A", RangeConstraint::default());
        let b = Qse::from_known_symbol("B", RangeConstraint::default());
        let t: Qse = (x * y + a) * b;
        assert_eq!(t.to_string(), "(B * X) * (Y) + (A * B)");
    }

    #[test]
    fn test_mul_by_zero() {
        let x = Qse::from_unknown_variable("X");
        let y = Qse::from_unknown_variable("Y");
        let a = Qse::from_known_symbol("A", RangeConstraint::default());
        let zero = Qse::zero();
        let t: Qse = x * y + a;
        assert_eq!(t.to_string(), "(X) * (Y) + A");
        assert_eq!((t.clone() * zero).to_string(), "0");
    }

    #[test]
    fn test_apply_update() {
        let x = Qse::from_unknown_variable("X");
        let y = Qse::from_unknown_variable("Y");
        let a = Qse::from_known_symbol("A", RangeConstraint::default());
        let b = Qse::from_known_symbol("B", RangeConstraint::default());
        let mut t: Qse = (x * y + a) * b;
        assert_eq!(t.to_string(), "(B * X) * (Y) + (A * B)");
        t.substitute_by_known(
            &"B",
            &SymbolicExpression::from_symbol("B", RangeConstraint::from_value(7.into())),
        );
        assert!(t.is_quadratic());
        assert_eq!(t.to_string(), "(7 * X) * (Y) + (A * 7)");
        t.substitute_by_known(
            &"X",
            &SymbolicExpression::from_symbol("X", RangeConstraint::from_range(1.into(), 2.into())),
        );
        assert!(!t.is_quadratic());
        assert_eq!(t.to_string(), "(7 * X) * Y + (A * 7)");
        t.substitute_by_known(
            &"Y",
            &SymbolicExpression::from_symbol("Y", RangeConstraint::from_value(3.into())),
        );
        assert!(t.try_to_known().is_some());
        assert_eq!(t.to_string(), "((A * 7) + ((7 * X) * 3))");
    }

    #[test]
    fn test_apply_update_inner_zero() {
        let x = Qse::from_unknown_variable("X");
        let y = Qse::from_unknown_variable("Y");
        let a = Qse::from_known_symbol("A", RangeConstraint::default());
        let b = Qse::from_known_symbol("B", RangeConstraint::default());
        let mut t: Qse = (x * a + y) * b;
        assert_eq!(t.to_string(), "(A * B) * X + B * Y");
        t.substitute_by_known(
            &"B",
            &SymbolicExpression::from_symbol("B", RangeConstraint::from_value(7.into())),
        );
        assert_eq!(t.to_string(), "(A * 7) * X + 7 * Y");
        t.substitute_by_known(
            &"A",
            &SymbolicExpression::from_symbol("A", RangeConstraint::from_value(0.into())),
        );
        assert_eq!(t.to_string(), "7 * Y");
    }

    #[test]
    fn substitute_known() {
        let x = Qse::from_unknown_variable("X");
        let y = Qse::from_unknown_variable("Y");
        let a = Qse::from_known_symbol("A", RangeConstraint::default());
        let b = Qse::from_known_symbol("B", RangeConstraint::default());
        let mut t: Qse = (x * a + y) * b.clone() + b;
        assert_eq!(t.to_string(), "(A * B) * X + B * Y + B");
        // We substitute B by an expression containing B on purpose.
        t.substitute_by_known(
            &"B",
            &(SymbolicExpression::from_symbol("B", Default::default())
                + SymbolicExpression::from(GoldilocksField::from(1))),
        );
        assert_eq!(t.to_string(), "(A * (B + 1)) * X + (B + 1) * Y + (B + 1)");
        t.substitute_by_known(
            &"B",
            &SymbolicExpression::from_symbol("B", RangeConstraint::from_value(10.into())),
        );
        assert_eq!(t.to_string(), "(A * 11) * X + 11 * Y + 11");
    }

    impl<T: FieldElement> RangeConstraintProvider<T, &'static str>
        for HashMap<&'static str, RangeConstraint<T>>
    {
        fn get(&self, var: &&'static str) -> RangeConstraint<T> {
            self.get(var).cloned().unwrap_or_default()
        }
    }

    #[test]
    fn unsolvable() {
        let r = AlgebraicConstraint::assert_zero(&Qse::from_number(GoldilocksField::from(10)))
            .solve(&NoRangeConstraints);
        assert!(r.is_err());
    }

    #[test]
    fn unsolvable_with_vars() {
        let x = &Qse::from_known_symbol("X", Default::default());
        let y = &Qse::from_known_symbol("Y", Default::default());
        let mut constr = x + y - constant(10);
        // We cannot solve it, but we can also not learn anything new from it.
        let result = AlgebraicConstraint::assert_zero(&constr)
            .solve(&NoRangeConstraints)
            .unwrap();
        assert!(result.complete && result.effects.is_empty());
        // But if we know the values, we can be sure there is a conflict.
        assert!(AlgebraicConstraint::assert_zero(&constant(10))
            .solve(&NoRangeConstraints)
            .is_err());

        // The same with range constraints that disallow zero.
        constr.substitute_by_known(
            &"X",
            &SymbolicExpression::from_symbol("X", RangeConstraint::from_value(5.into())),
        );
        constr.substitute_by_known(
            &"Y",
            &SymbolicExpression::from_symbol(
                "Y",
                RangeConstraint::from_range(100.into(), 102.into()),
            ),
        );
        assert!(AlgebraicConstraint::assert_zero(&constant(10))
            .solve(&NoRangeConstraints)
            .is_err());
    }

    #[test]
    fn solvable_without_vars() {
        let constr = constant(0);
        let result = AlgebraicConstraint::assert_zero(&constr)
            .solve(&NoRangeConstraints)
            .unwrap();
        assert!(result.complete && result.effects.is_empty());
    }

    #[test]
    fn solve_simple_eq() {
        let y = Qse::from_known_symbol("y", Default::default());
        let x = Qse::from_unknown_variable("X");
        // 2 * X + 7 * y - 10 = 0
        let two = constant(2);
        let seven = constant(7);
        let ten = constant(10);
        let constr = two * x + seven * y - ten;
        let result = AlgebraicConstraint::assert_zero(&constr)
            .solve(&NoRangeConstraints)
            .unwrap();
        assert!(result.complete);
        assert_eq!(result.effects.len(), 1);
        let Effect::Assignment(var, expr) = &result.effects[0] else {
            panic!("Expected assignment");
        };
        assert_eq!(var.to_string(), "X");
        assert_eq!(expr.to_string(), "(((7 * y) + -10) / -2)");
    }

    #[test]
    fn solve_div_by_range_constrained_var() {
        let y = Qse::from_known_symbol("y", Default::default());
        let z = Qse::from_known_symbol("z", Default::default());
        let x = Qse::from_unknown_variable("X");
        // z * X + 7 * y - 10 = 0
        let seven = constant(7);
        let ten = constant(10);
        let mut constr = z * x + seven * y - ten.clone();
        // If we do not range-constrain z, we cannot solve since we don't know if it might be zero.
        let result = AlgebraicConstraint::assert_zero(&constr)
            .solve(&NoRangeConstraints)
            .unwrap();
        assert!(!result.complete && result.effects.is_empty());
        let z_rc = RangeConstraint::from_range(1.into(), 2.into());
        let range_constraints: HashMap<&'static str, RangeConstraint<GoldilocksField>> =
            HashMap::from([("z", z_rc.clone())]);
        // Just solving without applying the update to the known symbolic expressions
        // does not help either. Note that the argument `&range_constraints` to
        // `solve()` is only used for unknown variables and not for known variables.
        // For the latter to take effect, we need to call `apply_update`.
        let result = AlgebraicConstraint::assert_zero(&constr)
            .solve(&range_constraints)
            .unwrap();
        assert!(!result.complete && result.effects.is_empty());
        constr.substitute_by_known(&"z", &SymbolicExpression::from_symbol("z", z_rc.clone()));
        // Now it should work.
        let result = AlgebraicConstraint::assert_zero(&constr)
            .solve(&range_constraints)
            .unwrap();
        assert!(result.complete);
        let effects = result.effects;
        let Effect::Assignment(var, expr) = &effects[0] else {
            panic!("Expected assignment");
        };
        assert_eq!(var.to_string(), "X");
        assert_eq!(expr.to_string(), "(((7 * y) + -10) / -z)");
    }

    #[test]
    fn solve_bit_decomposition() {
        let rc = RangeConstraint::from_mask(0xffu32);
        // First try without range constrain on a, but on b and c.
        let a = Qse::from_unknown_variable("a");
        let b = Qse::from_unknown_variable("b");
        let c = Qse::from_unknown_variable("c");
        let z = Qse::from_known_symbol("Z", Default::default());
        // a * 0x100 - b * 0x10000 + c * 0x1000000 + 10 + Z = 0
        let ten = constant(10);
        let constr: Qse = a * constant(0x100) - b * constant(0x10000)
            + c * constant(0x1000000)
            + ten.clone()
            + z.clone();
        // Without range constraints on a, this is not solvable.
        let mut range_constraints = HashMap::from([("b", rc.clone()), ("c", rc.clone())]);
        let result = AlgebraicConstraint::assert_zero(&constr)
            .solve(&range_constraints)
            .unwrap();
        assert!(!result.complete && result.effects.is_empty());
        // Now add the range constraint on a, it should be solvable.
        range_constraints.insert("a", rc.clone());
        let result = AlgebraicConstraint::assert_zero(&constr)
            .solve(&range_constraints)
            .unwrap();
        assert!(result.complete);

        let [effect] = &result.effects[..] else {
            panic!();
        };
        let Effect::BitDecomposition(BitDecomposition { value, components }) = effect else {
            panic!();
        };
        assert_eq!(format!("{value}"), "(10 + Z)");
        let formatted = components
            .iter()
            .map(|c| {
                format!(
                    "{} = (({value} & 0x{:0x}) >> {}){};\n",
                    c.variable,
                    c.bit_mask,
                    c.exponent,
                    if c.is_negative { " [negative]" } else { "" }
                )
            })
            .join("");

        assert_eq!(
            formatted,
            "\
a = (((10 + Z) & 0xff00) >> 8) [negative];
b = (((10 + Z) & 0xff0000) >> 16);
c = (((10 + Z) & 0xff000000) >> 24) [negative];
"
        );
    }

    #[test]
    fn bit_decomposition_bug() {
        let lin = Qse::from_unknown_variable("lin");
        let result = Qse::from_unknown_variable("result");
        let constr = lin.clone() - constant(4) * result.clone() - constant(4);
        let range_constraints = HashMap::from([
            ("lin", RangeConstraint::from_mask(0x8u32)),
            ("result", RangeConstraint::from_mask(0x1u32)),
        ]);
        // We try to solve `lin - 4 * result = 4` and the problem is
        // that we cannot assign `lin = 4 & mask` for some mask, since
        // it needs to be assigned `8`.
        let result = AlgebraicConstraint::assert_zero(&constr)
            .solve(&range_constraints)
            .unwrap();
        assert!(!result.complete);
        // The algorithm has a bug, so we exect no bit decomposition.
        let has_bit_decomp = result
            .effects
            .iter()
            .filter(|e| matches!(e, Effect::BitDecomposition(_)))
            .count()
            != 0;
        assert!(!has_bit_decomp);
    }

    #[test]
    fn solve_constraint_transfer() {
        let rc = RangeConstraint::from_mask(0xffu32);
        let a = Qse::from_unknown_variable("a");
        let b = Qse::from_unknown_variable("b");
        let c = Qse::from_unknown_variable("c");
        let z = Qse::from_unknown_variable("Z");
        let range_constraints =
            HashMap::from([("a", rc.clone()), ("b", rc.clone()), ("c", rc.clone())]);
        // a * 0x100 + b * 0x10000 + c * 0x1000000 + 10 - Z = 0
        let ten = constant(10);
        let constr =
            a * constant(0x100) + b * constant(0x10000) + c * constant(0x1000000) + ten.clone()
                - z.clone();
        let result = AlgebraicConstraint::assert_zero(&constr)
            .solve(&range_constraints)
            .unwrap();
        assert!(!result.complete);
        let effects = result
            .effects
            .into_iter()
            .map(|effect| match effect {
                Effect::RangeConstraint(v, rc) => format!("{v}: {rc};\n"),
                _ => panic!(),
            })
            .format("")
            .to_string();
        // It appears twice because we solve the positive and the negated equation.
        // Note that the negated version has a different bit mask.
        assert_eq!(
            effects,
            "Z: [10, 4294967050] & 0xffffff0a;
"
        );
    }

    #[test]
    fn solve_quadratic() {
        let rc = RangeConstraint::from_mask(0xffu32);
        let a = Qse::from_unknown_variable("a");
        let b = Qse::from_known_symbol("b", rc.clone());
        let range_constraints = HashMap::from([("a", rc.clone()), ("b", rc.clone())]);
        let ten = constant(10);
        let two_pow8 = constant(0x100);
        let constr = (a.clone() - b.clone() + two_pow8 - ten.clone()) * (a - b - ten);
        let result = AlgebraicConstraint::assert_zero(&constr)
            .solve(&range_constraints)
            .unwrap();
        assert!(result.complete);
        let effects = result
            .effects
            .into_iter()
            .map(|effect| match effect {
                Effect::ConditionalAssignment {
                    variable,
                    condition: Condition { value, condition },
                    in_range_value,
                    out_of_range_value,
                } => {
                    format!("{variable} = if {value} in {condition} {{ {in_range_value} }} else {{ {out_of_range_value} }}\n")
                }
                _ => panic!(),
            })
            .format("")
            .to_string();
        assert_eq!(
            effects,
            "a = if ((b + -256) + 10) in [0, 255] & 0xff { ((b + -256) + 10) } else { (b + 10) }
"
        );

        // Do the same, but setting b to a concrete value (2).
        // The result should be an unconditional assignment to b + 10 = 12.
        let mut constr = constr;
        constr.substitute_by_known(&"b", &GoldilocksField::from(2).into());
        let result = AlgebraicConstraint::assert_zero(&constr)
            .solve(&range_constraints)
            .unwrap();
        assert!(result.complete);
        let [Effect::Assignment(var, expr)] = result.effects.as_slice() else {
            panic!("Expected 1 assignment");
        };
        assert_eq!(var, &"a");
        assert_eq!(expr.to_string(), "12");
    }

    fn unpack_range_constraint(
        process_result: &ProcessResult<
            SymbolicExpression<GoldilocksField, &'static str>,
            &'static str,
        >,
    ) -> (&'static str, RangeConstraint<GoldilocksField>) {
        let [effect] = &process_result.effects[..] else {
            panic!();
        };
        let Effect::RangeConstraint(var, rc) = effect else {
            panic!();
        };
        (var, rc.clone())
    }

    #[test]
    fn detect_bit_constraint() {
        let a = Qse::from_unknown_variable("a");
        let one = constant(1);
        let three = constant(3);
        let five = constant(5);

        // All these constraints should be equivalent to a bit constraint.
        let constraints = [
            a.clone() * (a.clone() - one.clone()),
            (a.clone() - one.clone()) * a.clone(),
            (three * a.clone()) * (five.clone() * a.clone() - five),
        ];

        for constraint in constraints {
            let result = AlgebraicConstraint::assert_zero(&constraint)
                .solve(&NoRangeConstraints)
                .unwrap();
            assert!(result.complete);
            let (var, rc) = unpack_range_constraint(&result);
            assert_eq!(var.to_string(), "a");
            assert_eq!(rc, RangeConstraint::from_mask(1u64));
        }
    }

    #[test]
    fn detect_complete_range_constraint() {
        let a = Qse::from_unknown_variable("a");
        let three = constant(3);
        let four = constant(4);

        // `a` can be 3 or 4, which is can be completely represented by
        // RangeConstraint::from_range(3, 4), so the identity should be
        // marked as complete.
        let constraint = (a.clone() - three) * (a - four);

        let result = AlgebraicConstraint::assert_zero(&constraint)
            .solve(&NoRangeConstraints)
            .unwrap();
        assert!(result.complete);
        let (var, rc) = unpack_range_constraint(&result);
        assert_eq!(var.to_string(), "a");
        assert_eq!(
            rc,
            RangeConstraint::from_range(GoldilocksField::from(3), GoldilocksField::from(4))
        );
    }

    #[test]
    fn detect_incomplete_range_constraint() {
        let a = Qse::from_unknown_variable("a");
        let three = constant(3);
        let five = constant(5);

        // `a` can be 3 or 5, so there is a range constraint
        // RangeConstraint::from_range(3, 5) on `a`.
        // However, the identity is not complete, because the
        // range constraint allows for a value of 4, so removing
        // the identity would loose information.
        let constraint = (a.clone() - three) * (a - five);

        let result = AlgebraicConstraint::assert_zero(&constraint)
            .solve(&NoRangeConstraints)
            .unwrap();
        assert!(!result.complete);
        let (var, rc) = unpack_range_constraint(&result);
        assert_eq!(var.to_string(), "a");
        assert_eq!(
            rc,
            RangeConstraint::from_range(GoldilocksField::from(3), GoldilocksField::from(5))
        );
    }

    #[test]
    fn test_substitute_by_unknown_basic_replacement() {
        let mut expr = var("a");
        let subst = var("x");

        expr.substitute_by_unknown(&"a", &subst);
        assert_eq!(expr.to_string(), "x");
    }

    #[test]
    fn test_substitute_by_unknown_linear_to_quadratic() {
        let mut expr = var("x");
        let subst = var("y") * var("z") + constant(3);
        expr.substitute_by_unknown(&"x", &subst);

        assert!(expr.is_quadratic());
        assert_eq!(expr.to_string(), "(y) * (z) + 3");
    }

    #[test]
    fn test_substitute_by_unknown_inside_quadratic() {
        let mut expr = var("x") * var("y");
        let subst = var("a") + constant(1);

        expr.substitute_by_unknown(&"x", &subst);
        assert!(expr.is_quadratic());
        assert_eq!(expr.to_string(), "(a + 1) * (y)");
    }

    #[test]
    fn test_substitute_by_unknown_linear() {
        let mut expr = var("x") + var("y");
        let subst = var("a") + var("b");

        expr.substitute_by_unknown(&"x", &subst);
        assert!(!expr.is_quadratic());
        assert_eq!(expr.linear.len(), 3);
        assert_eq!(expr.to_string(), "a + b + y");
    }

    #[test]
    fn test_complex_expression_multiple_substitution() {
        let mut expr = (var("x") * var("w")) + var("x") + constant(3) * var("y") + constant(5);
        assert_eq!(expr.to_string(), "(x) * (w) + x + 3 * y + 5");

        let subst = var("a") * var("b") + constant(1);

        expr.substitute_by_unknown(&"x", &subst);

        let (quadratic, linear_iter, constant) = expr.components();
        let linear: Vec<_> = linear_iter.collect();

        assert_eq!(
            expr.to_string(),
            "((a) * (b) + 1) * (w) + (a) * (b) + 3 * y + 6"
        );
        // Structural validation
        assert_eq!(quadratic.len(), 2);
        assert_eq!(quadratic[0].0.to_string(), "(a) * (b) + 1");
        assert_eq!(quadratic[0].0.quadratic[0].0.to_string(), "a");
        assert_eq!(quadratic[0].0.quadratic[0].1.to_string(), "b");
        assert!(quadratic[0].0.linear.is_empty());
        assert_eq!(
            quadratic[0].0.constant.try_to_number(),
            Some(GoldilocksField::from(1)),
        );
        assert_eq!(quadratic[0].1.to_string(), "w");
        assert_eq!(quadratic[1].0.to_string(), "a");
        assert_eq!(quadratic[1].1.to_string(), "b");
        assert_eq!(linear[0].0.to_string(), "y");
        assert_eq!(linear.len(), 1);
        assert_eq!(constant.try_to_number(), Some(GoldilocksField::from(6)),);
    }

    #[test]
    fn test_substitute_by_unknown_coeff_distribution() {
        let mut expr = constant(2) * var("a") + constant(7);
        assert_eq!(expr.to_string(), "2 * a + 7");

        let subst = var("x") * var("y");

        expr.substitute_by_unknown(&"a", &subst);

        let (quadratic, linear_iter, constant) = expr.components();
        let linear: Vec<_> = linear_iter.collect();

        assert_eq!(expr.to_string(), "(2 * x) * (y) + 7");

        assert_eq!(quadratic.len(), 1);
        assert_eq!(quadratic[0].0.to_string(), "2 * x");
        assert_eq!(quadratic[0].1.to_string(), "y");
        assert!(linear.is_empty());
        assert_eq!(constant.try_to_number(), Some(GoldilocksField::from(7)));
    }

    #[test]
    fn bool_plus_one_cant_be_zero() {
        let expr = var("a") + constant(1);
        let rc = RangeConstraint::from_mask(0x1u64);
        let range_constraints = HashMap::from([("a", rc.clone())]);
        assert!(AlgebraicConstraint::assert_zero(&expr)
            .solve(&range_constraints)
            .is_err());
    }

    #[test]
    fn solve_for() {
        let expr = var("w") + var("x") + constant(3) * var("y") + constant(5);
        let constr = AlgebraicConstraint::assert_zero(&expr);
        assert_eq!(expr.to_string(), "w + x + 3 * y + 5");
        assert_eq!(
            constr.try_solve_for(&"x").unwrap().to_string(),
            "-(w + 3 * y + 5)"
        );
        assert_eq!(
            constr.try_solve_for(&"y").unwrap().to_string(),
            "6148914689804861440 * w + 6148914689804861440 * x - 6148914689804861442"
        );
        assert!(constr.try_solve_for(&"t").is_none());
    }

    #[test]
    fn solve_for_expr() {
        let expr = var("w") + var("x") + constant(3) * var("y") + constant(5);
        let constr = AlgebraicConstraint::assert_zero(&expr);
        assert_eq!(expr.to_string(), "w + x + 3 * y + 5");
        assert_eq!(
            constr.try_solve_for_expr(&var("x")).unwrap().to_string(),
            "-(w + 3 * y + 5)"
        );
        assert_eq!(
            constr.try_solve_for_expr(&var("y")).unwrap().to_string(),
            "6148914689804861440 * w + 6148914689804861440 * x - 6148914689804861442"
        );
        assert_eq!(
            constr
                .try_solve_for_expr(&-(constant(3) * var("y")))
                .unwrap()
                .to_string(),
            "w + x + 5"
        );
        assert_eq!(
            constr
                .try_solve_for_expr(&-(constant(3) * var("y") + constant(2)))
                .unwrap()
                .to_string(),
            "w + x + 3"
        );
        assert_eq!(
            constr
                .try_solve_for_expr(&(var("x") + constant(3) * var("y") + constant(2)))
                .unwrap()
                .to_string(),
            "-(w + 3)"
        );
        // We cannot solve these because the constraint does not contain a linear multiple
        // of the expression.
        assert!(constr
            .try_solve_for_expr(&(var("x") + constant(2) * var("y")))
            .is_none());
        assert!(constr.try_solve_for_expr(&(var("x") + var("y"))).is_none());
        assert!(constr
            .try_solve_for_expr(&(constant(2) * var("x") + var("y")))
            .is_none());
    }

    #[test]
    fn solve_for_expr_normalization() {
        // Test normalization
        let t = SymbolicExpression::from_symbol("t", Default::default());
        let r = SymbolicExpression::from_symbol("r", Default::default());
        let expr = var("x") * r.clone() + var("y") * t;
        let constr = AlgebraicConstraint::assert_zero(&expr);
        assert_eq!(constr.to_string(), "r * x + t * y = 0");
        assert_eq!(
            constr
                .try_solve_for_expr(&(var("x") * r))
                .unwrap()
                .to_string(),
            "-t * y"
        );
    }

    #[test]
    fn combine_removing_zeros() {
        let a = var("x") * var("y") + var("z") * constant(3);
        let b = var("t") * var("u") + constant(5) + var("y") * var("x");
        assert_eq!(
            (a.clone() - b.clone()).to_string(),
            "-((t) * (u) - 3 * z + 5)"
        );
        assert_eq!((b - a).to_string(), "(t) * (u) - 3 * z + 5");
    }

    #[test]
    fn remove_quadratic_zeros_after_substitution() {
        let a = var("x") * var("r") + var("z") * constant(3);
        let b = var("t") * var("u") + constant(5) + var("y") * var("x");
        let mut t = b - a;
        // Cannot simplify yet, because the terms are different
        assert_eq!(
            t.to_string(),
            "(t) * (u) + (y) * (x) - (x) * (r) - 3 * z + 5"
        );
        t.substitute_by_unknown(&"r", &var("y"));
        // Now the first term in `a` is equal to the last in `b`.
        assert_eq!(t.to_string(), "(t) * (u) - 3 * z + 5");
    }

    #[test]
    fn to_factors() {
        let expr = (constant(3) * var("x"))
            * -var("y")
            * constant(3)
            * (constant(5) * var("z") + constant(5))
            * (constant(2) * var("t") + constant(4) * var("z"))
            * (var("t") * constant(2));
        assert_eq!(
            expr.to_string(),
            "-(((((9 * x) * (y)) * (5 * z + 5)) * (2 * t + 4 * z)) * (2 * t))"
        );
        let factors = expr.to_factors().into_iter().format(", ").to_string();
        assert_eq!(factors, "x, y, z + 1, t + 2 * z, t");
    }
}
