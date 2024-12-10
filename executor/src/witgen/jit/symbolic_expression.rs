use std::{
    fmt::{self, Display, Formatter},
    ops::{Add, BitAnd, BitOr, Mul, Neg},
};

use powdr_number::FieldElement;

use crate::witgen::range_constraints::RangeConstraint;

/// A value that is known at run-time, defined through a complex expression
/// involving known cells or variables and compile-time constants.
/// Each of the sub-expressions can have its own range constraint.
#[derive(Debug, Clone)]
pub enum SymbolicExpression<T: FieldElement, V> {
    /// A concrete constant value known at compile time.
    Concrete(T),
    /// A symbolic value known at run-time, referencing either a cell or a local variable.
    Variable(V, Option<RangeConstraint<T>>),
    BinaryOperation(
        Box<Self>,
        BinaryOperator,
        Box<Self>,
        Option<RangeConstraint<T>>,
    ),
    UnaryOperation(UnaryOperator, Box<Self>, Option<RangeConstraint<T>>),
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
    BitAnd,
    BitOr,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Neg,
}

impl<T: FieldElement, V> SymbolicExpression<T, V> {
    pub fn from_var(name: V) -> Self {
        SymbolicExpression::Variable(name, None)
    }

    pub fn is_known_zero(&self) -> bool {
        self.try_to_number().map_or(false, |n| n.is_zero())
    }

    pub fn is_known_one(&self) -> bool {
        self.try_to_number().map_or(false, |n| n.is_one())
    }

    pub fn is_known_minus_one(&self) -> bool {
        self.try_to_number().map_or(false, |n| n == -T::from(1))
    }

    pub fn range_constraint(&self) -> Option<RangeConstraint<T>> {
        match self {
            SymbolicExpression::Concrete(v) => Some(RangeConstraint::from_value(*v)),
            SymbolicExpression::Variable(.., rc)
            | SymbolicExpression::BinaryOperation(.., rc)
            | SymbolicExpression::UnaryOperation(.., rc) => rc.clone(),
        }
    }

    pub fn try_to_number(&self) -> Option<T> {
        match self {
            SymbolicExpression::Concrete(n) => Some(*n),
            SymbolicExpression::Variable(..)
            | SymbolicExpression::BinaryOperation(..)
            | SymbolicExpression::UnaryOperation(..) => None,
        }
    }
}

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
            SymbolicExpression::Variable(name, _) => write!(f, "{name}"),
            SymbolicExpression::BinaryOperation(lhs, op, rhs, _) => {
                write!(f, "({lhs} {op} {rhs})")
            }
            SymbolicExpression::UnaryOperation(op, expr, _) => write!(f, "{op}({expr})"),
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
            BinaryOperator::BitAnd => write!(f, "&"),
            BinaryOperator::BitOr => write!(f, "|"),
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
                Box::new(self.clone()),
                BinaryOperator::Add,
                Box::new(rhs.clone()),
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
            SymbolicExpression::UnaryOperation(UnaryOperator::Neg, expr, _) => *expr.clone(),
            _ => SymbolicExpression::UnaryOperation(
                UnaryOperator::Neg,
                Box::new(self.clone()),
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
                Box::new(self.clone()),
                BinaryOperator::Mul,
                Box::new(rhs.clone()),
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
                Box::new(self.clone()),
                BinaryOperator::Div,
                Box::new(rhs.clone()),
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
                Box::new(self.clone()),
                BinaryOperator::IntegerDiv,
                Box::new(rhs.clone()),
                None,
            )
        }
    }
}

impl<T: FieldElement, V: Clone> BitAnd for &SymbolicExpression<T, V> {
    type Output = SymbolicExpression<T, V>;

    fn bitand(self, rhs: Self) -> Self::Output {
        if let (SymbolicExpression::Concrete(a), SymbolicExpression::Concrete(b)) = (self, rhs) {
            SymbolicExpression::Concrete(T::from(a.to_integer() & b.to_integer()))
        } else if self.is_known_zero() || rhs.is_known_zero() {
            SymbolicExpression::Concrete(T::from(0))
        } else {
            SymbolicExpression::BinaryOperation(
                Box::new(self.clone()),
                BinaryOperator::BitAnd,
                Box::new(rhs.clone()),
                self.range_constraint()
                    .zip(rhs.range_constraint())
                    .map(|(a, b)| a.conjunction(&b)),
            )
        }
    }
}

impl<T: FieldElement, V: Clone> BitAnd for SymbolicExpression<T, V> {
    type Output = SymbolicExpression<T, V>;

    fn bitand(self, rhs: Self) -> Self::Output {
        &self & &rhs
    }
}

impl<T: FieldElement, V: Clone> BitOr for &SymbolicExpression<T, V> {
    type Output = SymbolicExpression<T, V>;

    fn bitor(self, rhs: Self) -> Self::Output {
        if let (SymbolicExpression::Concrete(a), SymbolicExpression::Concrete(b)) = (self, rhs) {
            let v = a.to_integer() | b.to_integer();
            assert!(v <= T::modulus());
            SymbolicExpression::Concrete(T::from(v))
        } else {
            SymbolicExpression::BinaryOperation(
                Box::new(self.clone()),
                BinaryOperator::BitOr,
                Box::new(rhs.clone()),
                None,
            )
        }
    }
}

impl<T: FieldElement, V: Clone> BitOr for SymbolicExpression<T, V> {
    type Output = SymbolicExpression<T, V>;

    fn bitor(self, rhs: Self) -> Self::Output {
        &self | &rhs
    }
}
