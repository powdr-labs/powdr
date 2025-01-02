use std::{
    fmt::{self, Display, Formatter},
    ops::{Add, BitAnd, Mul, Neg},
    sync::Arc,
};

use num_traits::Zero;
use powdr_number::FieldElement;

use crate::witgen::range_constraints::RangeConstraint;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RPNExpressionElem<T: FieldElement, S> {
    Concrete(T),
    Symbol(S),
    BinaryOperation(BinaryOperator),
    UnaryOperation(UnaryOperator),
    BitOperation(BitOperator, T::Integer),
}

/// An expression in Reverse Polish Notation.
pub struct RPNExpression<T: FieldElement, S> {
    pub elems: Vec<RPNExpressionElem<T, S>>,
}

/// A value that is known at run-time, defined through a complex expression
/// involving known cells or variables and compile-time constants.
/// Each of the sub-expressions can have its own range constraint.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolicExpression<T: FieldElement, S> {
    /// A concrete constant value known at compile time.
    Concrete(T),
    /// A symbolic value known at run-time, referencing a cell,
    /// an input, a local variable or whatever it is used for.
    Symbol(S, Option<RangeConstraint<T>>),
    BinaryOperation(
        Arc<Self>,
        BinaryOperator,
        Arc<Self>,
        Option<RangeConstraint<T>>,
    ),
    UnaryOperation(UnaryOperator, Arc<Self>, Option<RangeConstraint<T>>),
    BitOperation(
        Arc<Self>,
        BitOperator,
        T::Integer,
        Option<RangeConstraint<T>>,
    ),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    /// Finite field division.
    Div,
    /// Integer division, i.e. convert field elements to unsigned integer and divide.
    IntegerDiv,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BitOperator {
    And,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
                Arc::new(self.clone()),
                BinaryOperator::Add,
                Arc::new(rhs.clone()),
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
                Arc::new(self.clone()),
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
                Arc::new(self.clone()),
                BinaryOperator::Mul,
                Arc::new(rhs.clone()),
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
                Arc::new(self.clone()),
                BinaryOperator::Div,
                Arc::new(rhs.clone()),
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
                Arc::new(self.clone()),
                BinaryOperator::IntegerDiv,
                Arc::new(rhs.clone()),
                None,
            )
        }
    }

    /// Transform the variables inside the expression using the given function
    pub fn map_variables<V2, F: FnMut(&V) -> V2>(&self, f: &mut F) -> SymbolicExpression<T, V2> {
        match self {
            SymbolicExpression::Concrete(n) => SymbolicExpression::Concrete(*n),
            SymbolicExpression::Symbol(s, rc) => SymbolicExpression::Symbol(f(s), rc.clone()),
            SymbolicExpression::BinaryOperation(lhs, op, rhs, rc) => {
                SymbolicExpression::BinaryOperation(
                    Arc::new(lhs.map_variables(f)),
                    op.clone(),
                    Arc::new(rhs.map_variables(f)),
                    rc.clone(),
                )
            }
            SymbolicExpression::UnaryOperation(op, expr, rc) => SymbolicExpression::UnaryOperation(
                op.clone(),
                Arc::new(expr.map_variables(f)),
                rc.clone(),
            ),
            SymbolicExpression::BitOperation(expr, op, n, rc) => SymbolicExpression::BitOperation(
                Arc::new(expr.map_variables(f)),
                op.clone(),
                *n,
                rc.clone(),
            ),
        }
    }

    /// Convert to an RPNExpression
    pub fn to_rpn(&self) -> RPNExpression<T, V> {
        let mut elems = Vec::new();
        self.to_rpn_inner(&mut elems);
        RPNExpression { elems }
    }

    fn to_rpn_inner(&self, elems: &mut Vec<RPNExpressionElem<T, V>>) {
        match self {
            SymbolicExpression::Concrete(n) => {
                elems.push(RPNExpressionElem::Concrete(*n));
            }
            SymbolicExpression::Symbol(s, _) => {
                elems.push(RPNExpressionElem::Symbol(s.clone()));
            }
            SymbolicExpression::BinaryOperation(lhs, op, rhs, _) => {
                lhs.to_rpn_inner(elems);
                rhs.to_rpn_inner(elems);
                elems.push(RPNExpressionElem::BinaryOperation(op.clone()));
            }
            SymbolicExpression::UnaryOperation(op, expr, _) => {
                expr.to_rpn_inner(elems);
                elems.push(RPNExpressionElem::UnaryOperation(op.clone()));
            }
            SymbolicExpression::BitOperation(expr, op, n, _) => {
                expr.to_rpn_inner(elems);
                elems.push(RPNExpressionElem::BitOperation(op.clone(), *n));
            }
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
            SymbolicExpression::BitOperation(Arc::new(self), BitOperator::And, rhs, rc)
        }
    }
}
