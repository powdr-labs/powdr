use std::{
    fmt::{self, Display, Formatter},
    ops::{Add, BitAnd, Mul, Neg},
    rc::Rc,
};

use num_traits::Zero;
use powdr_number::FieldElement;

use crate::witgen::range_constraints::RangeConstraint;

/// A value that is known at run-time, defined through a complex expression
/// involving known cells or variables and compile-time constants.
/// Each of the sub-expressions can have its own range constraint.
#[derive(Debug, Clone)]
pub enum SymbolicExpression<T: FieldElement, S> {
    /// A concrete constant value known at compile time.
    Concrete(T),
    /// A symbolic value known at run-time, referencing a cell,
    /// an input, a local variable or whatever it is used for.
    Symbol(S, Option<RangeConstraint<T>>),
    BinaryOperation(
        Rc<Self>,
        BinaryOperator,
        Rc<Self>,
        Option<RangeConstraint<T>>,
    ),
    UnaryOperation(UnaryOperator, Rc<Self>, Option<RangeConstraint<T>>),
    BitOperation(
        Rc<Self>,
        BitOperator,
        T::Integer,
        Option<RangeConstraint<T>>,
    ),
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    /// Finite field division.
    Div,
    /// Integer division, i.e. convert field elements to unsigned integer and divide.
    IntegerDiv,
}

#[derive(Debug, Clone)]
pub enum BitOperator {
    And,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Neg,
}

impl<T: FieldElement, S> SymbolicExpression<T, S> {
    pub fn from_symbol(symbol: S, rc: Option<RangeConstraint<T>>) -> Self {
        SymbolicExpression::Symbol(symbol, rc)
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
        if let Some(rc) = self.range_constraint() {
            !rc.allows_value(0.into())
        } else {
            // unknown
            false
        }
    }

    pub fn range_constraint(&self) -> Option<RangeConstraint<T>> {
        match self {
            SymbolicExpression::Concrete(v) => Some(RangeConstraint::from_value(*v)),
            SymbolicExpression::Symbol(.., rc)
            | SymbolicExpression::BinaryOperation(.., rc)
            | SymbolicExpression::UnaryOperation(.., rc)
            | SymbolicExpression::BitOperation(.., rc) => rc.clone(),
        }
    }

    pub fn try_to_number(&self) -> Option<T> {
        match self {
            SymbolicExpression::Concrete(n) => Some(*n),
            SymbolicExpression::Symbol(..)
            | SymbolicExpression::BinaryOperation(..)
            | SymbolicExpression::UnaryOperation(..)
            | SymbolicExpression::BitOperation(..) => None,
        }
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
            SymbolicExpression::BitOperation(expr, op, n, _) => {
                write!(f, "({expr} {op} {n})")
            }
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
            BinaryOperator::IntegerDiv => write!(f, "//"),
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

impl Display for BitOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            BitOperator::And => write!(f, "&"),
        }
    }
}

impl<T: FieldElement, V> From<T> for SymbolicExpression<T, V> {
    fn from(n: T) -> Self {
        SymbolicExpression::Concrete(n)
    }
}

impl<T: FieldElement, V: Clone> Add for &SymbolicExpression<T, V> {
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
            _ => SymbolicExpression::BinaryOperation(
                Rc::new(self.clone()),
                BinaryOperator::Add,
                Rc::new(rhs.clone()),
                self.range_constraint()
                    .zip(rhs.range_constraint())
                    .map(|(a, b)| a.combine_sum(&b)),
            ),
        }
    }
}

impl<T: FieldElement, V: Clone> Add for SymbolicExpression<T, V> {
    type Output = SymbolicExpression<T, V>;
    fn add(self, rhs: Self) -> Self::Output {
        &self + &rhs
    }
}

impl<T: FieldElement, V: Clone> Neg for &SymbolicExpression<T, V> {
    type Output = SymbolicExpression<T, V>;

    fn neg(self) -> Self::Output {
        match self {
            SymbolicExpression::Concrete(n) => SymbolicExpression::Concrete(-*n),
            SymbolicExpression::UnaryOperation(UnaryOperator::Neg, expr, _) => {
                expr.as_ref().clone()
            }
            _ => SymbolicExpression::UnaryOperation(
                UnaryOperator::Neg,
                Rc::new(self.clone()),
                self.range_constraint().map(|rc| rc.multiple(-T::from(1))),
            ),
        }
    }
}

impl<T: FieldElement, V: Clone> Neg for SymbolicExpression<T, V> {
    type Output = SymbolicExpression<T, V>;
    fn neg(self) -> Self::Output {
        -&self
    }
}

impl<T: FieldElement, V: Clone> Mul for &SymbolicExpression<T, V> {
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
                Rc::new(self.clone()),
                BinaryOperator::Mul,
                Rc::new(rhs.clone()),
                None,
            )
        }
    }
}

impl<T: FieldElement, V: Clone> Mul for SymbolicExpression<T, V> {
    type Output = SymbolicExpression<T, V>;
    fn mul(self, rhs: Self) -> Self {
        &self * &rhs
    }
}

impl<T: FieldElement, V: Clone> SymbolicExpression<T, V> {
    /// Field element division. See `integer_div` for integer division.
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
                Rc::new(self.clone()),
                BinaryOperator::Div,
                Rc::new(rhs.clone()),
                None,
            )
        }
    }

    /// Integer division, i.e. convert field elements to unsigned integer and divide.
    pub fn integer_div(&self, rhs: &Self) -> Self {
        if rhs.is_known_one() {
            self.clone()
        } else {
            SymbolicExpression::BinaryOperation(
                Rc::new(self.clone()),
                BinaryOperator::IntegerDiv,
                Rc::new(rhs.clone()),
                None,
            )
        }
    }
}

impl<T: FieldElement, V: Clone> BitAnd<T::Integer> for SymbolicExpression<T, V> {
    type Output = SymbolicExpression<T, V>;

    fn bitand(self, rhs: T::Integer) -> Self::Output {
        if let SymbolicExpression::Concrete(a) = self {
            SymbolicExpression::Concrete(T::from(a.to_integer() & rhs))
        } else if self.is_known_zero() || rhs.is_zero() {
            SymbolicExpression::Concrete(T::from(0))
        } else {
            let rc = Some(RangeConstraint::from_mask(
                if let Some(rc) = self.range_constraint() {
                    *rc.mask() & rhs
                } else {
                    rhs
                },
            ));
            SymbolicExpression::BitOperation(Rc::new(self), BitOperator::And, rhs, rc)
        }
    }
}
