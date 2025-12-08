use std::{
    collections::{BTreeMap, HashSet},
    fmt::Display,
    hash::Hash,
    iter::{once, Sum},
    ops::{Add, AddAssign, Mul, MulAssign, Neg, Sub},
};

use crate::runtime_constant::{RuntimeConstant, Substitutable, VarTransformable};
use itertools::Itertools;
use num_traits::One;
use num_traits::Zero;
use powdr_number::FieldElement;

use super::range_constraint::RangeConstraint;
use super::symbolic_expression::SymbolicExpression;

/// Terms with more than `MAX_SUM_SIZE_FOR_QUADRATIC_ANALYSIS` quadratic terms
/// are not analyzed for pairs that sum to zero.
const MAX_SUM_SIZE_FOR_QUADRATIC_ANALYSIS: usize = 20;

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
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

/// A component of a grouped expression.
pub enum GroupedExpressionComponent<T, V> {
    /// A quadratic component `(c1, c1)`, representing `c1 * c2`.
    Quadratic(GroupedExpression<T, V>, GroupedExpression<T, V>),
    /// A linear component `(v, c)`, representing `c * v`.
    Linear(V, T),
    /// A constant component `c`.
    Constant(T),
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

    /// Returns `vec![f1, f2, ..., fn]` such that `self` is equivalent to
    /// `c * f1 * f2 * ... * fn` for some constant `c`.
    /// Tries to find as many factors as possible and also tries to normalize
    /// the factors as much as possible.
    pub fn to_factors(&self) -> Vec<Self> {
        let summands = self.quadratic.len()
            + self.linear.len()
            + if self.constant.is_known_zero() { 0 } else { 1 };
        if summands == 0 {
            vec![Self::zero()]
        } else if summands == 1 {
            if let [(l, r)] = self.quadratic.as_slice() {
                l.to_factors().into_iter().chain(r.to_factors()).collect()
            } else if let Some((var, _)) = self.linear.iter().next() {
                vec![Self::from_unknown_variable(var.clone())]
            } else {
                vec![]
            }
        } else {
            // Try to normalize
            let divide_by = if !self.constant.is_known_zero() {
                // If the constant is not zero, we divide by the constant.
                if self.constant.is_known_nonzero() {
                    self.constant.clone()
                } else {
                    T::one()
                }
            } else if !self.linear.is_empty() {
                // Otherwise, we divide by the coefficient of the smallest variable.
                self.linear.iter().next().unwrap().1.clone()
            } else {
                // This is a sum of quadratic expressions, we cannot really normalize this part.
                T::one()
            };
            vec![self.clone() * T::one().field_div(&divide_by)]
        }
    }

    /// Returns the linear components of this expression, i.e. summands that we were
    /// able to determine to be only a runtime constant times a single variable.
    /// If `is_affine()` returns true, this returns all summands except the constant offset.
    /// Otherwise, the variables returned here might also appear inside the higher order terms
    /// and this the dependency on these variables might be more complicated than just a
    /// runtime constant factor.
    pub fn linear_components(&self) -> impl DoubleEndedIterator<Item = (&V, &T)> + Clone {
        self.linear.iter()
    }

    /// Returns the constant offset in this expression.
    pub fn constant_offset(&self) -> &T {
        &self.constant
    }

    /// Returns a slice of the quadratic components of this expression.
    pub fn quadratic_components(&self) -> &[(Self, Self)] {
        &self.quadratic
    }

    /// Turns this expression into an iterator over its summands.
    pub fn into_summands(self) -> impl Iterator<Item = GroupedExpressionComponent<T, V>> {
        self.quadratic
            .into_iter()
            .map(|(l, r)| GroupedExpressionComponent::Quadratic(l, r))
            .chain(
                self.linear
                    .into_iter()
                    .map(|(v, c)| GroupedExpressionComponent::Linear(v, c)),
            )
            .chain(once(GroupedExpressionComponent::Constant(self.constant)))
    }

    /// Computes the degree of a GroupedExpression in the unknown variables.
    /// Note that it might overestimate the degree if the expression contains
    /// terms that cancel each other out, e.g. `a * (b + 1) - a * b - a`.
    /// Variables inside runtime constants are ignored.
    pub fn degree(&self) -> usize {
        self.quadratic
            .iter()
            .map(|(l, r)| l.degree() + r.degree())
            .chain((!self.linear.is_empty()).then_some(1))
            .max()
            .unwrap_or(0)
    }

    /// Computes the degree of a variable in this expression.
    /// Variables inside runtime constants are ignored.
    pub fn degree_of_variable(&self, var: &V) -> usize {
        let linear_degree = if self.linear.contains_key(var) { 1 } else { 0 };
        self.quadratic
            .iter()
            .map(|(l, r)| l.degree_of_variable(var) + r.degree_of_variable(var))
            .chain(once(linear_degree))
            .max()
            .unwrap()
    }

    /// Returns the coefficient of the variable `variable` in the affine part of this
    /// expression.
    /// If the expression is affine, this is the actual coefficient of the variable
    /// in the expression. Otherwise, the quadratic part of the expression could
    /// also contain the variable and thus the actual coefficient might be different
    /// (even zero).
    pub fn coefficient_of_variable_in_affine_part<'a>(&'a self, var: &V) -> Option<&'a T> {
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
                if l == r {
                    l.range_constraint(range_constraints).square()
                } else if let Some(f) = l.is_constant_multiple_of(r).and_then(|f| f.try_to_number())
                {
                    // l = f * r,
                    // thus l * r = f * r^2 = l^2 / f
                    let l_rc = l.range_constraint(range_constraints);
                    let r_rc = r.range_constraint(range_constraints);
                    r_rc.square()
                        .multiple(f)
                        .conjunction(&l_rc.square().multiple(T::FieldType::from(1) / f))
                } else {
                    l.range_constraint(range_constraints)
                        .combine_product(&r.range_constraint(range_constraints))
                }
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

    /// If this returns Some(x), then `self = x * other`.
    /// Only works if there is at least one linear term in both expressions
    pub fn is_constant_multiple_of(&self, other: &GroupedExpression<T, V>) -> Option<T> {
        let (v1, c1) = self.linear_components().next()?;
        let (ov1, oc1) = other.linear_components().next()?;
        if v1 != ov1 {
            return None;
        }
        let factor_candidate = c1.field_div(oc1);
        (*self == other.clone() * factor_candidate.clone()).then_some(factor_candidate)
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
        remove_quadratic_terms_adding_to_zero(&mut self.quadratic);

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
        remove_quadratic_terms_adding_to_zero(&mut self.quadratic);

        *self += to_add;
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

    fn try_transform_var_type(
        &self,
        var_transform: &mut impl FnMut(&V1) -> Option<V2>,
    ) -> Option<Self::Transformed> {
        Some(GroupedExpression {
            quadratic: self
                .quadratic
                .iter()
                .map(|(l, r)| {
                    Some((
                        l.try_transform_var_type(var_transform)?,
                        r.try_transform_var_type(var_transform)?,
                    ))
                })
                .collect::<Option<Vec<_>>>()?,
            linear: self
                .linear
                .iter()
                .map(|(var, coeff)| {
                    let new_var = var_transform(var)?;
                    Some((new_var, coeff.try_transform_var_type(var_transform)?))
                })
                .collect::<Option<BTreeMap<_, _>>>()?,
            constant: self.constant.try_transform_var_type(var_transform)?,
        })
    }
}

pub trait RangeConstraintProvider<T: FieldElement, V> {
    fn get(&self, var: &V) -> RangeConstraint<T>;
}

impl<R: RangeConstraintProvider<T, V>, T: FieldElement, V> RangeConstraintProvider<T, V> for &R {
    fn get(&self, var: &V) -> RangeConstraint<T> {
        R::get(self, var)
    }
}

#[derive(Clone, Copy)]
pub struct NoRangeConstraints;
impl<T: FieldElement, V> RangeConstraintProvider<T, V> for NoRangeConstraints {
    fn get(&self, _var: &V) -> RangeConstraint<T> {
        RangeConstraint::default()
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
        self.quadratic = combine_removing_zeros(std::mem::take(&mut self.quadratic), rhs.quadratic);
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

/// Returns the sum of these quadratic terms while removing terms that
/// cancel each other out.
fn combine_removing_zeros<E: PartialEq>(first: Vec<(E, E)>, mut second: Vec<(E, E)>) -> Vec<(E, E)>
where
    for<'a> &'a E: Neg<Output = E>,
{
    if first.len() + second.len() > MAX_SUM_SIZE_FOR_QUADRATIC_ANALYSIS {
        // If there are too many terms, we cannot do this efficiently.
        return first.into_iter().chain(second).collect();
    }

    let mut result = first
        .into_iter()
        .filter(|first| {
            // Try to find l1 * r1 inside `second`.
            if let Some((j, _)) = second
                .iter()
                .find_position(|second| quadratic_terms_add_to_zero(first, second))
            {
                // We found a match, so they cancel each other out, we remove both.
                second.remove(j);
                false
            } else {
                true
            }
        })
        .collect_vec();
    result.extend(second);
    result
}

/// Removes pairs of items from `terms` whose products add to zero.
fn remove_quadratic_terms_adding_to_zero<E: PartialEq>(terms: &mut Vec<(E, E)>)
where
    for<'a> &'a E: Neg<Output = E>,
{
    if terms.len() > MAX_SUM_SIZE_FOR_QUADRATIC_ANALYSIS {
        // If there are too many terms, we cannot do this efficiently.
        return;
    }

    let mut to_remove = HashSet::new();
    for ((i, first), (j, second)) in terms.iter().enumerate().tuple_combinations() {
        if to_remove.contains(&i) || to_remove.contains(&j) {
            // We already removed this term.
            continue;
        }
        if quadratic_terms_add_to_zero(first, second) {
            // We found a match, so they cancel each other out, we remove both.
            to_remove.insert(i);
            to_remove.insert(j);
        }
    }
    if !to_remove.is_empty() {
        *terms = terms
            .drain(..)
            .enumerate()
            .filter(|(i, _)| !to_remove.contains(i))
            .map(|(_, term)| term)
            .collect();
    }
}

/// Returns true if `first.0 * first.1 = -second.0 * second.1`,
/// but does not catch all cases.
fn quadratic_terms_add_to_zero<E: PartialEq>(first: &(E, E), second: &(E, E)) -> bool
where
    for<'a> &'a E: Neg<Output = E>,
{
    let (s0, s1) = second;
    // Check if `first.0 * first.1 == -(second.0 * second.1)`, but we can swap left and right
    // and we can put the negation either left or right.
    let n1 = (&-s0, s1);
    let n2 = (s0, &-s1);
    [n1, n2].contains(&(&first.0, &first.1)) || [n1, n2].contains(&(&first.1, &first.0))
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

impl<T: RuntimeConstant, V: Clone + Ord + Eq> Sum for GroupedExpression<T, V> {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Self::zero(), |mut acc, item| {
            acc += item;
            acc
        })
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

    use crate::{
        symbolic_expression::SymbolicExpression,
        test_utils::{constant, var},
    };

    use super::*;
    use expect_test::expect;
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
        assert_eq!(expr.linear_components().count(), 3);
        assert_eq!(expr.to_string(), "a + b + y");
    }

    #[test]
    fn test_complex_expression_multiple_substitution() {
        let mut expr = (var("x") * var("w")) + var("x") + constant(3) * var("y") + constant(5);
        assert_eq!(expr.to_string(), "(x) * (w) + x + 3 * y + 5");

        let subst = var("a") * var("b") + constant(1);

        expr.substitute_by_unknown(&"x", &subst);

        assert_eq!(
            expr.to_string(),
            "((a) * (b) + 1) * (w) + (a) * (b) + 3 * y + 6"
        );

        // Structural validation
        let [first_quadratic, second_quadratic] = expr
            .quadratic_components()
            .iter()
            .cloned()
            .collect_vec()
            .try_into()
            .unwrap();

        assert_eq!(first_quadratic.0.to_string(), "(a) * (b) + 1");
        let inner_quadratic = first_quadratic.0.quadratic_components();
        assert_eq!(inner_quadratic[0].0.to_string(), "a");
        assert_eq!(inner_quadratic[0].1.to_string(), "b");
        assert!(first_quadratic.0.linear_components().count() == 0);
        assert_eq!(
            first_quadratic.0.constant_offset().try_to_number(),
            Some(GoldilocksField::from(1)),
        );
        assert_eq!(first_quadratic.1.to_string(), "w");

        assert_eq!(second_quadratic.0.to_string(), "a");
        assert_eq!(second_quadratic.1.to_string(), "b");

        let [linear] = expr.linear_components().collect_vec().try_into().unwrap();
        assert_eq!(linear.0.to_string(), "y");
        assert_eq!(
            expr.constant_offset().try_to_number(),
            Some(GoldilocksField::from(6)),
        );
    }

    #[test]
    fn test_substitute_by_unknown_coeff_distribution() {
        let mut expr = constant(2) * var("a") + constant(7);
        assert_eq!(expr.to_string(), "2 * a + 7");

        let subst = var("x") * var("y");

        expr.substitute_by_unknown(&"a", &subst);

        assert_eq!(expr.to_string(), "(2 * x) * (y) + 7");

        let quadratic = expr.quadratic_components();
        assert_eq!(quadratic.len(), 1);
        assert_eq!(quadratic[0].0.to_string(), "2 * x");
        assert_eq!(quadratic[0].1.to_string(), "y");
        assert!(expr.linear_components().next().is_none());
        assert_eq!(
            expr.constant_offset().try_to_number(),
            Some(GoldilocksField::from(7))
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

    #[test]
    fn constant_multiple() {
        let expr1 = var("x") + var("y") + constant(3);
        let expr2 = constant(5) * var("x") + constant(5) * var("y") + constant(15);
        assert_eq!(
            expr2.is_constant_multiple_of(&expr1),
            Some(GoldilocksField::from(5).into())
        );
        assert_eq!(
            expr1.is_constant_multiple_of(&expr2),
            Some((GoldilocksField::from(1) / GoldilocksField::from(5)).into())
        );
        let expr3 = constant(4) * var("x") + constant(5) * var("y") + constant(15);
        assert_eq!(expr1.is_constant_multiple_of(&expr3), None);
    }

    #[test]
    fn rc_of_square() {
        let expr = (var("x") * var("x")) + constant(3);
        let rc1 = HashMap::from([("x", RangeConstraint::from_range(1.into(), 2.into()))]);
        expect!("[4, 7] & 0x7").assert_eq(&expr.range_constraint(&rc1).to_string());
        let rc2 = HashMap::from([(
            "x",
            RangeConstraint::from_range(-GoldilocksField::from(5), 3.into()),
        )]);
        expect!("[3, 28] & 0x1f").assert_eq(&expr.range_constraint(&rc2).to_string());
        let rc3 = HashMap::from([(
            "x",
            RangeConstraint::from_range(-GoldilocksField::from(3), 5.into()),
        )]);
        expect!("[3, 28] & 0x1f").assert_eq(&expr.range_constraint(&rc3).to_string());
    }
}
