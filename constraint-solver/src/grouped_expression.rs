use std::{
    collections::{BTreeMap, HashSet},
    fmt::Display,
    hash::Hash,
    ops::{Add, AddAssign, Mul, MulAssign, Neg, Sub},
};

use crate::{
    effect::Condition,
    runtime_constant::{ReferencedSymbols, RuntimeConstant, Substitutable, VarTransformable},
};
use itertools::Itertools;
use num_traits::One;
use num_traits::Zero;
use powdr_number::{log2_exact, ExpressionConvertible, FieldElement, LargeInt};

use super::effect::{Assertion, BitDecomposition, BitDecompositionComponent, Effect};
use super::range_constraint::RangeConstraint;
use super::symbolic_expression::SymbolicExpression;

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

/// A symbolic expression in unknown variables of type `V` and (symbolically)
/// known terms, representing a sum of (super-)quadratic, linear and constant parts.
/// The quadratic terms are of the form `X * Y`, where `X` and `Y` are
/// `GroupedExpression`s that have at least one unknown.
/// The linear terms are of the form `a * X`, where `a` is a (symbolically) known
/// value and `X` is an unknown variable.
/// The constant term is a (symbolically) known value.
///
/// It also provides ways to quickly update the expression when the value of
/// an unknown variable gets known and provides functions to solve
/// (some kinds of) equations.
///
/// The name is derived from the fact that it groups linear terms by variable.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GroupedExpression<T, V> {
    /// Quadratic terms of the form `a * X * Y`, where `a` is a (symbolically)
    /// known value and `X` and `Y` are grouped expressions that
    /// have at least one unknown.
    quadratic: Vec<(Self, Self)>,
    /// Linear terms of the form `a * X`, where `a` is a (symbolically) known
    /// value and `X` is an unknown variable.
    linear: BTreeMap<V, T>,
    /// Constant term, a (symbolically) known value.
    constant: T,
}

impl<F: FieldElement, T: RuntimeConstant<FieldType = F>, V> GroupedExpression<T, V> {
    pub fn from_number(k: F) -> Self {
        Self {
            quadratic: Default::default(),
            linear: Default::default(),
            constant: T::from(k),
        }
    }
}

impl<T: RuntimeConstant, V: Clone + Ord + Eq> Zero for GroupedExpression<T, V> {
    fn zero() -> Self {
        Self {
            quadratic: Default::default(),
            linear: Default::default(),
            constant: T::zero(),
        }
    }

    fn is_zero(&self) -> bool {
        self.try_to_known().is_some_and(|k| k.is_known_zero())
    }
}

impl<T: RuntimeConstant, V: Clone + Ord + Eq> One for GroupedExpression<T, V> {
    fn one() -> Self {
        Self {
            quadratic: Default::default(),
            linear: Default::default(),
            constant: T::one(),
        }
    }

    fn is_one(&self) -> bool {
        self.try_to_known().is_some_and(|k| k.is_known_one())
    }
}

impl<F: FieldElement, V: Ord + Clone + Eq> GroupedExpression<SymbolicExpression<F, V>, V> {
    pub fn from_known_symbol(symbol: V, rc: RangeConstraint<F>) -> Self {
        Self::from_runtime_constant(SymbolicExpression::from_symbol(symbol, rc))
    }
}

impl<T: RuntimeConstant, V: Ord + Clone + Eq> GroupedExpression<T, V> {
    pub fn from_runtime_constant(constant: T) -> Self {
        Self {
            quadratic: Default::default(),
            linear: Default::default(),
            constant,
        }
    }

    pub fn from_unknown_variable(var: V) -> Self {
        Self {
            quadratic: Default::default(),
            linear: [(var.clone(), T::one())].into_iter().collect(),
            constant: T::zero(),
        }
    }

    /// If this expression does not contain unknown variables, returns the symbolic expression.
    pub fn try_to_known(&self) -> Option<&T> {
        if self.quadratic.is_empty() && self.linear.is_empty() {
            Some(&self.constant)
        } else {
            None
        }
    }

    /// Returns true if this expression does not contain any quadratic terms.
    pub fn is_affine(&self) -> bool {
        !self.is_quadratic()
    }

    /// If the expression is a known number, returns it.
    pub fn try_to_number(&self) -> Option<T::FieldType> {
        self.try_to_known()?.try_to_number()
    }

    /// If the expression is equal to `GroupedExpression::from_unknown_variable(v)`, returns `v`.
    pub fn try_to_simple_unknown(&self) -> Option<V> {
        if self.is_quadratic() || !self.constant.is_known_zero() {
            return None;
        }
        let Ok((var, coeff)) = self.linear.iter().exactly_one() else {
            return None;
        };
        if !coeff.is_known_one() {
            return None;
        }
        Some(var.clone())
    }

    /// Returns true if this expression contains at least one quadratic term.
    pub fn is_quadratic(&self) -> bool {
        !self.quadratic.is_empty()
    }

    /// Returns `(l, r)` if `self == l * r`.
    pub fn try_as_single_product(&self) -> Option<(&Self, &Self)> {
        if self.linear.is_empty() && self.constant.is_known_zero() {
            match self.quadratic.as_slice() {
                [(l, r)] => Some((l, r)),
                _ => None,
            }
        } else {
            None
        }
    }

    /// Returns the quadratic, linear and constant components of this expression.
    pub fn components(&self) -> (&[(Self, Self)], impl Iterator<Item = (&V, &T)>, &T) {
        (&self.quadratic, self.linear.iter(), &self.constant)
    }

    /// Returns the coefficient of the variable `variable` if this is an affine expression.
    /// Panics if the expression is quadratic.
    pub fn coefficient_of_variable(&self, var: &V) -> Option<&T> {
        assert!(!self.is_quadratic());
        self.linear.get(var)
    }

    /// Returns the range constraint of the full expression.
    pub fn range_constraint(
        &self,
        range_constraints: &impl RangeConstraintProvider<T::FieldType, V>,
    ) -> RangeConstraint<T::FieldType> {
        self.quadratic
            .iter()
            .map(|(l, r)| {
                l.range_constraint(range_constraints)
                    .combine_product(&r.range_constraint(range_constraints))
            })
            .chain(self.linear.iter().map(|(var, coeff)| {
                range_constraints
                    .get(var)
                    .combine_product(&coeff.range_constraint())
            }))
            .chain(std::iter::once(self.constant.range_constraint()))
            .reduce(|rc1, rc2| rc1.combine_sum(&rc2))
            .unwrap_or_else(|| RangeConstraint::from_value(0.into()))
    }
}

impl<T: RuntimeConstant + Substitutable<V>, V: Ord + Clone + Eq> GroupedExpression<T, V> {
    /// Substitute a variable by a symbolically known expression. The variable can be known or unknown.
    /// If it was already known, it will be substituted in the known expressions.
    pub fn substitute_by_known(&mut self, variable: &V, substitution: &T) {
        self.constant.substitute(variable, substitution);

        if self.linear.contains_key(variable) {
            // If the variable is a key in `linear`, it must be unknown
            // and thus can only occur there. Otherwise, it can be in
            // any symbolic expression.
            // We replace the variable by a symbolic expression, so it goes into the constant part.
            let coeff = self.linear.remove(variable).unwrap();
            self.constant += coeff * substitution.clone();
        } else {
            for coeff in self.linear.values_mut() {
                coeff.substitute(variable, substitution);
            }
            self.linear.retain(|_, f| !f.is_known_zero());
        }

        // TODO can we do that without moving everything?
        // In the end, the order does not matter much.

        let mut to_add = GroupedExpression::zero();
        self.quadratic.retain_mut(|(l, r)| {
            l.substitute_by_known(variable, substitution);
            r.substitute_by_known(variable, substitution);
            match (l.try_to_known(), r.try_to_known()) {
                (Some(l), Some(r)) => {
                    to_add += GroupedExpression::from_runtime_constant(l.clone() * r.clone());
                    false
                }
                (Some(l), None) => {
                    to_add += r.clone() * l;
                    false
                }
                (None, Some(r)) => {
                    to_add += l.clone() * r;
                    false
                }
                _ => true,
            }
        });
        if to_add.try_to_known().map(|ta| ta.is_known_zero()) != Some(true) {
            *self += to_add;
        }
    }

    /// Substitute an unknown variable by a GroupedExpression.
    ///
    /// Note this does NOT work properly if the variable is used inside a
    /// known SymbolicExpression.
    pub fn substitute_by_unknown(&mut self, variable: &V, substitution: &GroupedExpression<T, V>) {
        if !self.referenced_unknown_variables().any(|v| v == variable) {
            return;
        }

        let mut to_add = GroupedExpression::zero();
        for (var, coeff) in std::mem::take(&mut self.linear) {
            if var == *variable {
                to_add += substitution.clone() * coeff;
            } else {
                self.linear.insert(var, coeff);
            }
        }

        self.quadratic = std::mem::take(&mut self.quadratic)
            .into_iter()
            .filter_map(|(mut l, mut r)| {
                l.substitute_by_unknown(variable, substitution);
                r.substitute_by_unknown(variable, substitution);
                match (l.try_to_known(), r.try_to_known()) {
                    (Some(lval), Some(rval)) => {
                        to_add += Self::from_runtime_constant(lval.clone() * rval.clone());
                        None
                    }
                    (Some(lval), None) => {
                        to_add += r * lval;
                        None
                    }
                    (None, Some(rval)) => {
                        to_add += l * rval;
                        None
                    }
                    _ => Some((l, r)),
                }
            })
            .collect();

        *self += to_add;
    }
}

impl<T: ReferencedSymbols<V>, V> GroupedExpression<T, V> {
    /// Returns the set of referenced variables, both know and unknown. Might contain repetitions.
    pub fn referenced_variables(&self) -> Box<dyn Iterator<Item = &V> + '_> {
        let quadr = self
            .quadratic
            .iter()
            .flat_map(|(a, b)| a.referenced_variables().chain(b.referenced_variables()));

        let linear = self
            .linear
            .iter()
            .flat_map(|(var, coeff)| std::iter::once(var).chain(coeff.referenced_symbols()));
        let constant = self.constant.referenced_symbols();
        Box::new(quadr.chain(linear).chain(constant))
    }
}

impl<T, V> GroupedExpression<T, V> {
    /// Returns the referenced unknown variables. Might contain repetitions.
    pub fn referenced_unknown_variables(&self) -> Box<dyn Iterator<Item = &V> + '_> {
        let quadratic = self.quadratic.iter().flat_map(|(a, b)| {
            a.referenced_unknown_variables()
                .chain(b.referenced_unknown_variables())
        });
        Box::new(quadratic.chain(self.linear.keys()))
    }
}

impl<T: RuntimeConstant + VarTransformable<V1, V2>, V1: Ord + Clone, V2: Ord + Clone>
    VarTransformable<V1, V2> for GroupedExpression<T, V1>
{
    type Transformed = GroupedExpression<T::Transformed, V2>;

    fn transform_var_type(&self, var_transform: &mut impl FnMut(&V1) -> V2) -> Self::Transformed {
        GroupedExpression {
            quadratic: self
                .quadratic
                .iter()
                .map(|(l, r)| {
                    (
                        l.transform_var_type(var_transform),
                        r.transform_var_type(var_transform),
                    )
                })
                .collect(),
            linear: self
                .linear
                .iter()
                .map(|(var, coeff)| {
                    let new_var = var_transform(var);
                    (new_var, coeff.transform_var_type(var_transform))
                })
                .collect(),
            constant: self.constant.transform_var_type(var_transform),
        }
    }
}

pub trait RangeConstraintProvider<T: FieldElement, V> {
    fn get(&self, var: &V) -> RangeConstraint<T>;
}

#[derive(Clone, Copy)]
pub struct NoRangeConstraints;
impl<T: FieldElement, V> RangeConstraintProvider<T, V> for NoRangeConstraints {
    fn get(&self, _var: &V) -> RangeConstraint<T> {
        RangeConstraint::default()
    }
}

impl<
        T: RuntimeConstant + Display + ExpressionConvertible<<T as RuntimeConstant>::FieldType, V>,
        V: Ord + Clone + Eq + Hash + Display,
    > GroupedExpression<T, V>
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
        if !self
            .range_constraint(range_constraints)
            .allows_value(Zero::zero())
        {
            return Err(Error::ConstraintUnsatisfiable(self.to_string()));
        }

        Ok(if self.is_quadratic() {
            self.solve_quadratic(range_constraints)?
        } else if let Some(k) = self.try_to_known() {
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

    /// Solves the constraint for `variable`. This is only possible if
    /// `variable` does not appear in the quadratic component and
    /// has a coefficient which is known to be not zero.
    ///
    /// Returns the resulting solved grouped expression.
    pub fn try_solve_for(&self, variable: &V) -> Option<GroupedExpression<T, V>> {
        if self
            .quadratic
            .iter()
            .flat_map(|(l, r)| [l, r])
            .flat_map(|c| c.referenced_unknown_variables())
            .contains(variable)
        {
            // The variable is in the quadratic component, we cannot solve for it.
            return None;
        }
        if !self.linear.get(variable)?.is_known_nonzero() {
            return None;
        }
        let mut result = self.clone();
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
        assert!(
            expr.is_affine(),
            "Tried to solve for quadratic expression {expr}"
        );
        if self.is_quadratic() {
            return None;
        }

        // Find a normalization factor by iterating over the variables.
        let normalization_factor = expr
            .referenced_unknown_variables()
            .find_map(|var| {
                let coeff = self.coefficient_of_variable(var)?;
                // We can only divide if we know the coefficient is non-zero.
                if coeff.is_known_nonzero() {
                    Some(expr.coefficient_of_variable(var).unwrap().field_div(coeff))
                } else {
                    None
                }
            })
            .unwrap_or(T::one());
        let result = expr - &(self.clone() * normalization_factor);

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

    fn solve_affine(
        &self,
        range_constraints: &impl RangeConstraintProvider<T::FieldType, V>,
    ) -> Result<ProcessResult<T, V>, Error> {
        Ok(if self.linear.len() == 1 {
            let (var, coeff) = self.linear.iter().next().unwrap();
            // Solve "coeff * X + self.constant = 0" by division.
            assert!(
                !coeff.is_known_zero(),
                "Zero coefficient has not been removed: {self}"
            );
            if coeff.is_known_nonzero() {
                // In this case, we can always compute a solution.
                let value = self.constant.field_div(&-coeff.clone());
                ProcessResult::complete(vec![assignment_if_satisfies_range_constraints(
                    var.clone(),
                    value,
                    range_constraints,
                )?])
            } else if self.constant.is_known_nonzero() {
                // If the offset is not zero, then the coefficient must be non-zero,
                // otherwise the constraint is violated.
                let value = self.constant.field_div(&-coeff.clone());
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
    fn solve_bit_decomposition(
        &self,
        range_constraints: &impl RangeConstraintProvider<T::FieldType, V>,
    ) -> Result<ProcessResult<T, V>, Error> {
        assert!(!self.is_quadratic());
        // All the coefficients need to be known numbers and the
        // variables need to be range-constrained.
        let constrained_coefficients = self
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
        let mut offset = self.constant.try_to_number();
        let mut concrete_assignments = vec![];

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
                return Err(Error::ConstraintUnsatisfiable(self.to_string()));
            }
            assert_eq!(concrete_assignments.len(), self.linear.len());
            Ok(ProcessResult::complete(concrete_assignments))
        } else {
            Ok(ProcessResult::complete(vec![Effect::BitDecomposition(
                BitDecomposition {
                    value: self.constant.clone(),
                    components,
                },
            )]))
        }
    }

    fn transfer_constraints(
        &self,
        range_constraints: &impl RangeConstraintProvider<T::FieldType, V>,
    ) -> Vec<Effect<T, V>> {
        // Solve for each of the variables in the linear component and
        // compute the range constraints.
        assert!(!self.is_quadratic());
        self.linear
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
        let Some((left, right)) = self.try_as_single_product() else {
            return Ok(ProcessResult::empty());
        };
        // Now we have `left * right = 0`, i.e. one (or both) of them has to be zero.
        let (left_solution, right_solution) = match (
            left.solve(range_constraints),
            right.solve(range_constraints),
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
                && rc.range_width() <= 2.into();
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

impl<T: RuntimeConstant, V: Clone + Ord + Eq> Add for GroupedExpression<T, V> {
    type Output = GroupedExpression<T, V>;

    fn add(mut self, rhs: Self) -> Self {
        self += rhs;
        self
    }
}

impl<T: RuntimeConstant, V: Clone + Ord + Eq> Add for &GroupedExpression<T, V> {
    type Output = GroupedExpression<T, V>;

    fn add(self, rhs: Self) -> Self::Output {
        self.clone() + rhs.clone()
    }
}

impl<T: RuntimeConstant, V: Clone + Ord + Eq> AddAssign<GroupedExpression<T, V>>
    for GroupedExpression<T, V>
{
    fn add_assign(&mut self, rhs: Self) {
        self.quadratic.extend(rhs.quadratic);
        for (var, coeff) in rhs.linear {
            self.linear
                .entry(var.clone())
                .and_modify(|f| *f += coeff.clone())
                .or_insert_with(|| coeff);
        }
        self.constant += rhs.constant.clone();
        self.linear.retain(|_, f| !f.is_known_zero());
    }
}

impl<T: RuntimeConstant, V: Clone + Ord + Eq> Sub for &GroupedExpression<T, V> {
    type Output = GroupedExpression<T, V>;

    fn sub(self, rhs: Self) -> Self::Output {
        self + &-rhs
    }
}

impl<T: RuntimeConstant, V: Clone + Ord + Eq> Sub for GroupedExpression<T, V> {
    type Output = GroupedExpression<T, V>;

    fn sub(self, rhs: Self) -> Self::Output {
        &self - &rhs
    }
}

impl<T: RuntimeConstant, V: Clone + Ord> GroupedExpression<T, V> {
    fn negate(&mut self) {
        for (first, _) in &mut self.quadratic {
            first.negate()
        }
        for coeff in self.linear.values_mut() {
            *coeff = -coeff.clone();
        }
        self.constant = -self.constant.clone();
    }
}

impl<T: RuntimeConstant, V: Clone + Ord> Neg for GroupedExpression<T, V> {
    type Output = GroupedExpression<T, V>;

    fn neg(mut self) -> Self {
        self.negate();
        self
    }
}

impl<T: RuntimeConstant, V: Clone + Ord> Neg for &GroupedExpression<T, V> {
    type Output = GroupedExpression<T, V>;

    fn neg(self) -> Self::Output {
        -((*self).clone())
    }
}

/// Multiply by known symbolic expression.
impl<T: RuntimeConstant, V: Clone + Ord + Eq> Mul<&T> for GroupedExpression<T, V> {
    type Output = GroupedExpression<T, V>;

    fn mul(mut self, rhs: &T) -> Self {
        self *= rhs;
        self
    }
}

impl<T: RuntimeConstant, V: Clone + Ord + Eq> Mul<T> for GroupedExpression<T, V> {
    type Output = GroupedExpression<T, V>;

    fn mul(self, rhs: T) -> Self {
        self * &rhs
    }
}

impl<T: RuntimeConstant, V: Clone + Ord + Eq> MulAssign<&T> for GroupedExpression<T, V> {
    fn mul_assign(&mut self, rhs: &T) {
        if rhs.is_known_zero() {
            *self = Self::zero();
        } else {
            for (first, _) in &mut self.quadratic {
                *first *= rhs;
            }
            for coeff in self.linear.values_mut() {
                *coeff *= rhs.clone();
            }
            self.constant *= rhs.clone();
        }
    }
}

impl<T: RuntimeConstant, V: Clone + Ord + Eq> Mul for GroupedExpression<T, V> {
    type Output = GroupedExpression<T, V>;

    fn mul(self, rhs: GroupedExpression<T, V>) -> Self {
        if let Some(k) = rhs.try_to_known() {
            self * k
        } else if let Some(k) = self.try_to_known() {
            rhs * k
        } else {
            Self {
                quadratic: vec![(self, rhs)],
                linear: Default::default(),
                constant: T::zero(),
            }
        }
    }
}

impl<T: RuntimeConstant + Display, V: Clone + Ord + Display> Display for GroupedExpression<T, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (sign, s) = self.to_signed_string();
        if sign {
            write!(f, "-({s})")
        } else {
            write!(f, "{s}")
        }
    }
}

impl<T: RuntimeConstant + Display, V: Clone + Ord + Display> GroupedExpression<T, V> {
    fn to_signed_string(&self) -> (bool, String) {
        self.quadratic
            .iter()
            .map(|(a, b)| {
                let (a_sign, a) = a.to_signed_string();
                let (b_sign, b) = b.to_signed_string();
                (a_sign ^ b_sign, format!("({a}) * ({b})"))
            })
            .chain(
                self.linear
                    .iter()
                    .map(|(var, coeff)| match coeff.try_to_number() {
                        Some(k) if k == T::FieldType::one() => (false, format!("{var}")),
                        Some(k) if k == -T::FieldType::one() => (true, format!("{var}")),
                        _ => {
                            let (sign, coeff) = Self::symbolic_expression_to_signed_string(coeff);
                            (sign, format!("{coeff} * {var}"))
                        }
                    }),
            )
            .chain(match self.constant.try_to_number() {
                Some(k) if k == T::FieldType::zero() => None,
                _ => Some(Self::symbolic_expression_to_signed_string(&self.constant)),
            })
            .reduce(|(n1, p1), (n2, p2)| {
                (
                    n1,
                    if n1 == n2 {
                        format!("{p1} + {p2}")
                    } else {
                        format!("{p1} - {p2}")
                    },
                )
            })
            .unwrap_or((false, "0".to_string()))
    }

    fn symbolic_expression_to_signed_string(value: &T) -> (bool, String) {
        match value.try_to_number() {
            Some(k) => {
                if k.is_in_lower_half() {
                    (false, format!("{k}"))
                } else {
                    (true, format!("{}", -k))
                }
            }
            _ => (false, value.to_string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::test_utils::{constant, var};

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

    impl RangeConstraintProvider<GoldilocksField, &'static str>
        for HashMap<&'static str, RangeConstraint<GoldilocksField>>
    {
        fn get(&self, var: &&'static str) -> RangeConstraint<GoldilocksField> {
            self.get(var).cloned().unwrap_or_default()
        }
    }

    #[test]
    fn unsolvable() {
        let r = Qse::from_number(GoldilocksField::from(10)).solve(&NoRangeConstraints);
        assert!(r.is_err());
    }

    #[test]
    fn unsolvable_with_vars() {
        let x = &Qse::from_known_symbol("X", Default::default());
        let y = &Qse::from_known_symbol("Y", Default::default());
        let mut constr = x + y - constant(10);
        // We cannot solve it, but we can also not learn anything new from it.
        let result = constr.solve(&NoRangeConstraints).unwrap();
        assert!(result.complete && result.effects.is_empty());
        // But if we know the values, we can be sure there is a conflict.
        assert!(constant(10).solve(&NoRangeConstraints).is_err());

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
        assert!(constant(10).solve(&NoRangeConstraints).is_err());
    }

    #[test]
    fn solvable_without_vars() {
        let constr = constant(0);
        let result = constr.solve(&NoRangeConstraints).unwrap();
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
        let result = constr.solve(&NoRangeConstraints).unwrap();
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
        let result = constr.solve(&NoRangeConstraints).unwrap();
        assert!(!result.complete && result.effects.is_empty());
        let z_rc = RangeConstraint::from_range(1.into(), 2.into());
        let range_constraints: HashMap<&'static str, RangeConstraint<GoldilocksField>> =
            HashMap::from([("z", z_rc.clone())]);
        // Just solving without applying the update to the known symbolic expressions
        // does not help either. Note that the argument `&range_constraints` to
        // `solve()` is only used for unknown variables and not for known variables.
        // For the latter to take effect, we need to call `apply_update`.
        let result = constr.solve(&range_constraints).unwrap();
        assert!(!result.complete && result.effects.is_empty());
        constr.substitute_by_known(&"z", &SymbolicExpression::from_symbol("z", z_rc.clone()));
        // Now it should work.
        let result = constr.solve(&range_constraints).unwrap();
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
        let result = constr.solve(&range_constraints).unwrap();
        assert!(!result.complete && result.effects.is_empty());
        // Now add the range constraint on a, it should be solvable.
        range_constraints.insert("a", rc.clone());
        let result = constr.solve(&range_constraints).unwrap();
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
        let result = constr.solve(&range_constraints).unwrap();
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
        let result = constr.solve(&range_constraints).unwrap();
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
        let result = constr.solve(&range_constraints).unwrap();
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
            let result = constraint.solve(&NoRangeConstraints).unwrap();
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

        let result = constraint.solve(&NoRangeConstraints).unwrap();
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

        let result = constraint.solve(&NoRangeConstraints).unwrap();
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
        assert!(expr.solve(&range_constraints).is_err());
    }

    #[test]
    fn solve_for() {
        let expr = var("w") + var("x") + constant(3) * var("y") + constant(5);
        assert_eq!(expr.to_string(), "w + x + 3 * y + 5");
        assert_eq!(
            expr.try_solve_for(&"x").unwrap().to_string(),
            "-(w + 3 * y + 5)"
        );
        assert_eq!(
            expr.try_solve_for(&"y").unwrap().to_string(),
            "6148914689804861440 * w + 6148914689804861440 * x - 6148914689804861442"
        );
        assert!(expr.try_solve_for(&"t").is_none());
    }

    #[test]
    fn solve_for_expr() {
        let expr = var("w") + var("x") + constant(3) * var("y") + constant(5);
        assert_eq!(expr.to_string(), "w + x + 3 * y + 5");
        assert_eq!(
            expr.try_solve_for_expr(&var("x")).unwrap().to_string(),
            "-(w + 3 * y + 5)"
        );
        assert_eq!(
            expr.try_solve_for_expr(&var("y")).unwrap().to_string(),
            "6148914689804861440 * w + 6148914689804861440 * x - 6148914689804861442"
        );
        assert_eq!(
            expr.try_solve_for_expr(&-(constant(3) * var("y")))
                .unwrap()
                .to_string(),
            "w + x + 5"
        );
        assert_eq!(
            expr.try_solve_for_expr(&-(constant(3) * var("y") + constant(2)))
                .unwrap()
                .to_string(),
            "w + x + 3"
        );
        assert_eq!(
            expr.try_solve_for_expr(&(var("x") + constant(3) * var("y") + constant(2)))
                .unwrap()
                .to_string(),
            "-(w + 3)"
        );
        // We cannot solve these because the constraint does not contain a linear multiple
        // of the expression.
        assert!(expr
            .try_solve_for_expr(&(var("x") + constant(2) * var("y")))
            .is_none());
        assert!(expr.try_solve_for_expr(&(var("x") + var("y"))).is_none());
        assert!(expr
            .try_solve_for_expr(&(constant(2) * var("x") + var("y")))
            .is_none());
    }

    #[test]
    fn solve_for_expr_normalization() {
        // Test normalization
        let t = SymbolicExpression::from_symbol("t", Default::default());
        let r = SymbolicExpression::from_symbol("r", Default::default());
        let expr = var("x") * r.clone() + var("y") * t;
        assert_eq!(expr.to_string(), "r * x + t * y");
        assert_eq!(
            expr.try_solve_for_expr(&(var("x") * r))
                .unwrap()
                .to_string(),
            "-t * y"
        );
    }
}
