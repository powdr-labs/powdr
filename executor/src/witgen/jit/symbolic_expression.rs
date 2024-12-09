use std::{
    fmt::{self, Display, Formatter},
    ops::{Add, BitAnd, Div, Mul, Neg},
};

use powdr_number::FieldElement;

use crate::witgen::range_constraints::RangeConstraint;

/// A value that is known at run-time, defined through a complex expression
/// involving known cells or variables and compile-time constants.
/// Each of the sub-expressions can have its own range constraint.
#[derive(Debug, Clone)]
pub enum SymbolicExpression<T: FieldElement> {
    /// A concrete constant value known at compile time.
    Concrete(T),
    /// A symbolic value known at run-time, referencing either a cell or a local variable.
    Variable(String, Option<RangeConstraint<T>>),
    BinaryOperation(
        Box<SymbolicExpression<T>>,
        BinaryOperator,
        Box<SymbolicExpression<T>>,
        Option<RangeConstraint<T>>,
    ),
    UnaryOperation(
        UnaryOperator,
        Box<SymbolicExpression<T>>,
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
    BitAnd,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Neg,
}

impl<T: FieldElement> SymbolicExpression<T> {
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

impl<T: FieldElement> Display for SymbolicExpression<T> {
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

impl<T: FieldElement> SymbolicExpression<T> {
    pub fn from_var(name: &str) -> Self {
        SymbolicExpression::Variable(name.to_string(), None)
    }
}

impl<T: FieldElement> From<T> for SymbolicExpression<T> {
    fn from(n: T) -> Self {
        SymbolicExpression::Concrete(n)
    }
}

impl<T: FieldElement> Add for &SymbolicExpression<T> {
    type Output = SymbolicExpression<T>;

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

impl<T: FieldElement> Neg for &SymbolicExpression<T> {
    type Output = SymbolicExpression<T>;

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

impl<T: FieldElement> Mul for &SymbolicExpression<T> {
    type Output = SymbolicExpression<T>;

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

impl<T: FieldElement> Div for &SymbolicExpression<T> {
    type Output = SymbolicExpression<T>;

    /// Field element division. See `integer_div` for integer division.
    fn div(self, rhs: Self) -> Self::Output {
        if let (SymbolicExpression::Concrete(a), SymbolicExpression::Concrete(b)) = (self, rhs) {
            assert!(b != &T::from(0));
            SymbolicExpression::Concrete(*a / *b)
        } else if self.is_known_zero() {
            // TODO should we still detect division by zero in this case?
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
}

impl<T: FieldElement> SymbolicExpression<T> {
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

impl<T: FieldElement> BitAnd for &SymbolicExpression<T> {
    type Output = SymbolicExpression<T>;

    fn bitand(self, rhs: Self) -> Self::Output {
        if let (SymbolicExpression::Concrete(a), SymbolicExpression::Concrete(b)) = (self, rhs) {
            SymbolicExpression::Concrete(T::from(a.to_integer() & b.to_integer()))
        } else {
            // TODO simplifications?
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
