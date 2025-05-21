use auto_enums::auto_enum;
use itertools::Itertools;
use std::hash::Hash;
use std::ops::Sub;
use std::ops::{AddAssign, MulAssign};
use std::{
    fmt::{self, Display, Formatter},
    iter,
    ops::{Add, Mul, Neg},
    sync::Arc,
};

use powdr_number::FieldElement;

use super::range_constraint::RangeConstraint;

/// A value that is known at run-time, defined through a complex expression
/// involving known cells or variables and compile-time constants.
/// Each of the sub-expressions can have its own range constraint.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolicExpression<T: FieldElement, S> {
    /// A concrete constant value known at compile time.
    Concrete(T),
    /// A symbolic value known at run-time, referencing a cell,
    /// an input, a local variable or whatever it is used for.
    Symbol(S, RangeConstraint<T>),
    BinaryOperation(Arc<Self>, BinaryOperator, Arc<Self>, RangeConstraint<T>),
    UnaryOperation(UnaryOperator, Arc<Self>, RangeConstraint<T>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    /// Finite field division.
    Div,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Neg,
}

impl<T: FieldElement, S> SymbolicExpression<T, S> {
    /// Returns all direct children of this expression.
    /// Does specifically not implement the `Children` trait, because it does not go
    /// well with recursive types.
    #[auto_enum(Iterator)]
    fn children(&self) -> impl Iterator<Item = &SymbolicExpression<T, S>> {
        match self {
            SymbolicExpression::BinaryOperation(lhs, _, rhs, _) => {
                [lhs.as_ref(), rhs.as_ref()].into_iter()
            }
            SymbolicExpression::UnaryOperation(_, expr, _) => iter::once(expr.as_ref()),
            SymbolicExpression::Concrete(_) | SymbolicExpression::Symbol(..) => iter::empty(),
        }
    }

    /// Returns an iterator over all direct and indirect children of this expression, including
    /// the expression itself.
    pub fn all_children(&self) -> Box<dyn Iterator<Item = &SymbolicExpression<T, S>> + '_> {
        Box::new(iter::once(self).chain(self.children().flat_map(|e| e.all_children())))
    }
}

impl<T: FieldElement, S> SymbolicExpression<T, S> {
    pub fn from_symbol(symbol: S, rc: RangeConstraint<T>) -> Self {
        if let Some(v) = rc.try_to_single_value() {
            SymbolicExpression::Concrete(v)
        } else {
            SymbolicExpression::Symbol(symbol, rc)
        }
    }

    pub fn is_known_zero(&self) -> bool {
        self.try_to_number().is_some_and(|n| n.is_zero())
    }

    pub fn is_known_one(&self) -> bool {
        self.try_to_number().is_some_and(|n| n.is_one())
    }

    pub fn is_known_minus_one(&self) -> bool {
        self.try_to_number().is_some_and(|n| n == -T::from(1))
    }

    pub fn is_known_nonzero(&self) -> bool {
        // Only checking range constraint is enough since if this is a known
        // fixed value, we will get a range constraint with just a single value.
        !self.range_constraint().allows_value(0.into())
    }

    pub fn range_constraint(&self) -> RangeConstraint<T> {
        match self {
            SymbolicExpression::Concrete(v) => RangeConstraint::from_value(*v),
            SymbolicExpression::Symbol(.., rc)
            | SymbolicExpression::BinaryOperation(.., rc)
            | SymbolicExpression::UnaryOperation(.., rc) => rc.clone(),
        }
    }

    pub fn try_to_number(&self) -> Option<T> {
        match self {
            SymbolicExpression::Concrete(n) => Some(*n),
            SymbolicExpression::Symbol(..)
            | SymbolicExpression::BinaryOperation(..)
            | SymbolicExpression::UnaryOperation(..) => None,
        }
    }
}

impl<T: FieldElement, S: Clone + Eq> SymbolicExpression<T, S> {
    /// Applies a variable substitution and returns a modified version if there was a change.
    pub fn compute_substitution(&self, variable: &S, substitution: &Self) -> Option<Self> {
        match self {
            SymbolicExpression::Concrete(_) => None,
            SymbolicExpression::Symbol(v, _) => (v == variable).then(|| substitution.clone()),
            SymbolicExpression::BinaryOperation(left, op, right, _) => {
                let (l, r) = match (
                    left.compute_substitution(variable, substitution),
                    right.compute_substitution(variable, substitution),
                ) {
                    (None, None) => return None,
                    (Some(l), None) => (l, (**right).clone()),
                    (None, Some(r)) => ((**left).clone(), r),
                    (Some(l), Some(r)) => (l, r),
                };
                match op {
                    BinaryOperator::Add => Some(l + r),
                    BinaryOperator::Sub => Some(l - r),
                    BinaryOperator::Mul => Some(l * r),
                    BinaryOperator::Div => Some(l.field_div(&r)),
                }
            }
            SymbolicExpression::UnaryOperation(op, inner, _) => {
                let inner = inner.compute_substitution(variable, substitution)?;
                match op {
                    UnaryOperator::Neg => Some(-inner),
                }
            }
        }
    }

    /// Applies a variable substitution in place.
    pub fn substitute(&mut self, variable: &S, substitution: &Self) {
        if let Some(updated) = self.compute_substitution(variable, substitution) {
            *self = updated;
        }
    }
}

impl<T: FieldElement, S1: Ord + Clone> SymbolicExpression<T, S1> {
    pub fn transform_var_type<S2: Ord + Clone>(
        &self,
        var_transform: &mut impl FnMut(&S1) -> S2,
    ) -> SymbolicExpression<T, S2> {
        match self {
            SymbolicExpression::Concrete(n) => SymbolicExpression::Concrete(*n),
            SymbolicExpression::Symbol(v, rc) => {
                SymbolicExpression::from_symbol(var_transform(v), rc.clone())
            }
            SymbolicExpression::BinaryOperation(lhs, op, rhs, rc) => {
                SymbolicExpression::BinaryOperation(
                    Arc::new(lhs.transform_var_type(var_transform)),
                    *op,
                    Arc::new(rhs.transform_var_type(var_transform)),
                    rc.clone(),
                )
            }
            SymbolicExpression::UnaryOperation(op, inner, rc) => {
                SymbolicExpression::UnaryOperation(
                    *op,
                    Arc::new(inner.transform_var_type(var_transform)),
                    rc.clone(),
                )
            }
        }
    }
}

impl<T: FieldElement, S: Hash + Eq> SymbolicExpression<T, S> {
    pub fn referenced_symbols(&self) -> impl Iterator<Item = &S> {
        self.all_children()
            .flat_map(|e| match e {
                SymbolicExpression::Symbol(s, _) => Some(s),
                _ => None,
            })
            .unique()
    }
}

/// Display for affine symbolic expressions, for informational purposes only.
impl<T: FieldElement, V: Display> Display for SymbolicExpression<T, V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            SymbolicExpression::Concrete(n) => {
                if n.is_in_lower_half() {
                    write!(f, "{n}")
                } else {
                    write!(f, "-{}", -*n)
                }
            }
            SymbolicExpression::Symbol(name, _) => write!(f, "{name}"),
            SymbolicExpression::BinaryOperation(lhs, op, rhs, _) => {
                write!(f, "({lhs} {op} {rhs})")
            }
            SymbolicExpression::UnaryOperation(op, expr, _) => write!(f, "{op}{expr}"),
        }
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOperator::Add => write!(f, "+"),
            BinaryOperator::Sub => write!(f, "-"),
            BinaryOperator::Mul => write!(f, "*"),
            BinaryOperator::Div => write!(f, "/"),
        }
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperator::Neg => write!(f, "-"),
        }
    }
}

impl<T: FieldElement, V> From<T> for SymbolicExpression<T, V> {
    fn from(n: T) -> Self {
        SymbolicExpression::Concrete(n)
    }
}

impl<T: FieldElement, V: Clone + PartialEq> Add for &SymbolicExpression<T, V> {
    type Output = SymbolicExpression<T, V>;

    fn add(self, rhs: Self) -> Self::Output {
        if self.is_known_zero() {
            return rhs.clone();
        }
        if rhs.is_known_zero() {
            return self.clone();
        }
        match (self, rhs) {
            (SymbolicExpression::Concrete(a), SymbolicExpression::Concrete(b)) => {
                SymbolicExpression::Concrete(*a + *b)
            }
            (SymbolicExpression::UnaryOperation(UnaryOperator::Neg, negated, _), other)
            | (other, SymbolicExpression::UnaryOperation(UnaryOperator::Neg, negated, _))
                if negated.as_ref() == other =>
            {
                T::from(0).into()
            }
            _ => SymbolicExpression::BinaryOperation(
                Arc::new(self.clone()),
                BinaryOperator::Add,
                Arc::new(rhs.clone()),
                self.range_constraint().combine_sum(&rhs.range_constraint()),
            ),
        }
    }
}

impl<T: FieldElement, V: Clone + PartialEq> Add for SymbolicExpression<T, V> {
    type Output = SymbolicExpression<T, V>;
    fn add(self, rhs: Self) -> Self::Output {
        &self + &rhs
    }
}

impl<T: FieldElement, V: Clone + PartialEq> AddAssign for SymbolicExpression<T, V> {
    fn add_assign(&mut self, rhs: Self) {
        *self = self.clone() + rhs;
    }
}

impl<T: FieldElement, V: Clone + PartialEq> Sub for &SymbolicExpression<T, V> {
    type Output = SymbolicExpression<T, V>;

    fn sub(self, rhs: Self) -> Self::Output {
        if self.is_known_zero() {
            return -rhs.clone();
        }
        if rhs.is_known_zero() {
            return self.clone();
        }
        match (self, rhs) {
            (SymbolicExpression::Concrete(a), SymbolicExpression::Concrete(b)) => {
                SymbolicExpression::Concrete(*a - *b)
            }
            (a, b) if a == b => T::from(0).into(),
            _ => SymbolicExpression::BinaryOperation(
                Arc::new(self.clone()),
                BinaryOperator::Sub,
                Arc::new(rhs.clone()),
                self.range_constraint()
                    .combine_sum(&rhs.range_constraint().neg()),
            ),
        }
    }
}

impl<T: FieldElement, V: Clone + PartialEq> Sub for SymbolicExpression<T, V> {
    type Output = SymbolicExpression<T, V>;
    fn sub(self, rhs: Self) -> Self::Output {
        &self - &rhs
    }
}

impl<T: FieldElement, V: Clone + PartialEq> Neg for &SymbolicExpression<T, V> {
    type Output = SymbolicExpression<T, V>;

    fn neg(self) -> Self::Output {
        match self {
            SymbolicExpression::Concrete(n) => SymbolicExpression::Concrete(-*n),
            SymbolicExpression::UnaryOperation(UnaryOperator::Neg, expr, _) => {
                expr.as_ref().clone()
            }
            SymbolicExpression::BinaryOperation(lhs, BinaryOperator::Add, rhs, _) => {
                -(**lhs).clone() + -(**rhs).clone()
            }
            SymbolicExpression::BinaryOperation(lhs, BinaryOperator::Sub, rhs, _) => {
                SymbolicExpression::BinaryOperation(
                    rhs.clone(),
                    BinaryOperator::Sub,
                    lhs.clone(),
                    self.range_constraint().multiple(-T::from(1)),
                )
            }
            SymbolicExpression::BinaryOperation(lhs, BinaryOperator::Mul, rhs, _)
                if matches!(**lhs, SymbolicExpression::Concrete(_)) =>
            {
                SymbolicExpression::BinaryOperation(
                    Arc::new(-(**lhs).clone()),
                    BinaryOperator::Mul,
                    rhs.clone(),
                    self.range_constraint().multiple(-T::from(1)),
                )
            }
            SymbolicExpression::BinaryOperation(lhs, BinaryOperator::Mul, rhs, _)
                if matches!(**rhs, SymbolicExpression::Concrete(_)) =>
            {
                SymbolicExpression::BinaryOperation(
                    lhs.clone(),
                    BinaryOperator::Mul,
                    Arc::new(-(**rhs).clone()),
                    self.range_constraint().multiple(-T::from(1)),
                )
            }
            _ => SymbolicExpression::UnaryOperation(
                UnaryOperator::Neg,
                Arc::new(self.clone()),
                self.range_constraint().multiple(-T::from(1)),
            ),
        }
    }
}

impl<T: FieldElement, V: Clone + PartialEq> Neg for SymbolicExpression<T, V> {
    type Output = SymbolicExpression<T, V>;
    fn neg(self) -> Self::Output {
        -&self
    }
}

impl<T: FieldElement, V: Clone + PartialEq> Mul for &SymbolicExpression<T, V> {
    type Output = SymbolicExpression<T, V>;

    fn mul(self, rhs: Self) -> Self::Output {
        if let (SymbolicExpression::Concrete(a), SymbolicExpression::Concrete(b)) = (self, rhs) {
            SymbolicExpression::Concrete(*a * *b)
        } else if self.is_known_zero() || rhs.is_known_zero() {
            SymbolicExpression::Concrete(T::from(0))
        } else if self.is_known_one() {
            rhs.clone()
        } else if rhs.is_known_one() {
            self.clone()
        } else if self.is_known_minus_one() {
            -rhs
        } else if rhs.is_known_minus_one() {
            -self
        } else {
            SymbolicExpression::BinaryOperation(
                Arc::new(self.clone()),
                BinaryOperator::Mul,
                Arc::new(rhs.clone()),
                self.range_constraint()
                    .combine_product(&rhs.range_constraint()),
            )
        }
    }
}

impl<T: FieldElement, V: Clone + PartialEq> Mul for SymbolicExpression<T, V> {
    type Output = SymbolicExpression<T, V>;
    fn mul(self, rhs: Self) -> Self {
        &self * &rhs
    }
}

impl<T: FieldElement, V: Clone + PartialEq> MulAssign for SymbolicExpression<T, V> {
    fn mul_assign(&mut self, rhs: Self) {
        *self = self.clone() * rhs;
    }
}

impl<T: FieldElement, V: Clone + PartialEq> SymbolicExpression<T, V> {
    /// Field element division.
    /// If you use this, you must ensure that the divisor is not zero.
    pub fn field_div(&self, rhs: &Self) -> Self {
        if let (SymbolicExpression::Concrete(a), SymbolicExpression::Concrete(b)) = (self, rhs) {
            assert!(b != &T::from(0));
            SymbolicExpression::Concrete(*a / *b)
        } else if self.is_known_zero() {
            SymbolicExpression::Concrete(T::from(0))
        } else if rhs.is_known_one() {
            self.clone()
        } else if rhs.is_known_minus_one() {
            -self
        } else {
            // TODO other simplifications like `-x / -y => x / y`, `-x / concrete => x / -concrete`, etc.
            SymbolicExpression::BinaryOperation(
                Arc::new(self.clone()),
                BinaryOperator::Div,
                Arc::new(rhs.clone()),
                Default::default(),
            )
        }
    }

    /// Returns the multiplicative inverse in the field.
    pub fn field_inverse(&self) -> Self {
        if let SymbolicExpression::Concrete(x) = self {
            assert!(x != &T::from(0));
            SymbolicExpression::Concrete(T::from(1) / *x)
        } else if let SymbolicExpression::BinaryOperation(x, BinaryOperator::Div, y, _) = self {
            SymbolicExpression::BinaryOperation(
                y.clone(),
                BinaryOperator::Div,
                x.clone(),
                Default::default(),
            )
        } else {
            SymbolicExpression::BinaryOperation(
                Arc::new(Self::from(T::from(1))),
                BinaryOperator::Div,
                Arc::new(self.clone()),
                Default::default(),
            )
        }
    }
}
