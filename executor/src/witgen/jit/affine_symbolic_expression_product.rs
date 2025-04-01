use std::{
    fmt::{self, Display, Formatter},
    ops::{Add, Mul, Neg},
};

use itertools::Itertools;
use powdr_number::FieldElement;

use crate::witgen::jit::effect::format_code;

use super::{
    affine_symbolic_expression::{AffineSymbolicExpression, Error, ProcessResult},
    effect::Effect,
    symbolic_expression::SymbolicExpression,
};

/// Represents the product of multiple `AffineSymbolicExpression`s.
/// Note that this really has to be a product at the outer layer, i.e.
/// you cannot add two `AffineSymbolicExpressionProduct`s, unless one of them
/// is a known constant.
#[derive(Debug, Clone)]
pub enum AffineSymbolicExpressionProduct<T: FieldElement, V> {
    Affine(AffineSymbolicExpression<T, V>),
    Product(Vec<AffineSymbolicExpression<T, V>>),
}

/// Display for informational purposes only.
impl<T: FieldElement, V: Display> Display for AffineSymbolicExpressionProduct<T, V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self {
            Self::Affine(expr) => write!(f, "{expr}"),
            Self::Product(factors) => write!(f, "({})", factors.iter().format(" * ")),
        }
    }
}

impl<T: FieldElement, V: Ord + Clone + Display, K: Into<AffineSymbolicExpression<T, V>>> From<K>
    for AffineSymbolicExpressionProduct<T, V>
{
    fn from(k: K) -> Self {
        Self::Affine(k.into())
    }
}

impl<T: FieldElement, V: Ord + Clone + Display> AffineSymbolicExpressionProduct<T, V> {
    /// If this expression does not contain unknown variables, returns the symbolic expression.
    pub fn try_to_known(&self) -> Option<&SymbolicExpression<T, V>> {
        if let Self::Affine(affine) = self {
            affine.try_to_known()
        } else {
            None
        }
    }

    /// Solves the equation `self = 0` and returns how to compute the solution.
    /// The solution can contain assignments to multiple variables.
    /// If no way to solve the equation (and no way to derive new range
    /// constraints) has been found, but it still contains
    /// unknown variables, returns an empty, incomplete result.
    /// If the equation is known to be unsolvable, returns an error.
    pub fn solve(&self) -> Result<ProcessResult<T, V>, Error> {
        if let AffineSymbolicExpressionProduct::Affine(expr) = self {
            return expr.solve();
        }
        let AffineSymbolicExpressionProduct::Product(factors) = self else {
            return Ok(ProcessResult::empty());
        };
        let [first, second] = factors.as_slice() else {
            return Ok(ProcessResult::empty());
        };
        let (var, first_assignment, second_assignment) = match (first.solve(), second.solve()) {
            // If both are conflicting, the product is conflicting as well.
            (e @ Err(_), Err(_)) => {
                return e;
            }
            (Ok(result_left), Ok(result_right))
                if result_left.complete && result_right.complete =>
            {
                if let (
                    [Effect::Assignment(first_var, first_assignment)],
                    [Effect::Assignment(second_var, second_assignment)],
                ) = (
                    &result_left.effects.as_slice(),
                    &result_right.effects.as_slice(),
                ) {
                    if first_var == second_var {
                        (
                            first_var.clone(),
                            first_assignment.clone(),
                            second_assignment.clone(),
                        )
                    } else {
                        return Ok(ProcessResult::empty());
                    }
                } else {
                    return Ok(ProcessResult::empty());
                }
            }
            _ => return Ok(ProcessResult::empty()),
        };
        // At this point, the constraint is equivalent to "`var = first_assignment or `var = second_assignment`".
        let difference = first_assignment.clone() + -second_assignment.clone();
        println!(
            "Difference: {difference}\nrc:{}",
            difference.range_constraint()
        );

        // TODO

        Ok(ProcessResult::empty())
    }

    pub fn try_add(self, rhs: Self) -> Option<Self> {
        match (self, rhs) {
            (Self::Affine(lhs), Self::Affine(rhs)) => Some((lhs + rhs.clone()).into()),
            (other, Self::Affine(affine)) | (Self::Affine(affine), other)
                if affine.try_to_known().map_or(false, |v| v.is_known_zero()) =>
            {
                Some(other)
            }
            _ => None,
        }
    }
}

impl<T: FieldElement, V: Clone + Ord> Neg for &AffineSymbolicExpressionProduct<T, V> {
    type Output = AffineSymbolicExpressionProduct<T, V>;

    fn neg(self) -> Self::Output {
        -self.clone()
    }
}

impl<T: FieldElement, V: Clone + Ord> Neg for AffineSymbolicExpressionProduct<T, V> {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            AffineSymbolicExpressionProduct::Affine(expr) => {
                AffineSymbolicExpressionProduct::Affine(-expr)
            }
            AffineSymbolicExpressionProduct::Product(mut factors) => {
                factors[0] = -factors[0].clone();
                AffineSymbolicExpressionProduct::Product(factors)
            }
        }
    }
}

impl<T: FieldElement, V: Clone + Ord + Display> Mul<AffineSymbolicExpressionProduct<T, V>>
    for AffineSymbolicExpressionProduct<T, V>
{
    type Output = Self;

    fn mul(self, rhs: AffineSymbolicExpressionProduct<T, V>) -> Self::Output {
        match (self, rhs) {
            (
                AffineSymbolicExpressionProduct::Affine(lhs),
                AffineSymbolicExpressionProduct::Affine(rhs),
            ) => {
                if let Some(product) = lhs.try_mul(&rhs) {
                    product.into()
                } else {
                    AffineSymbolicExpressionProduct::Product(vec![lhs, rhs])
                }
            }
            (
                AffineSymbolicExpressionProduct::Affine(affine),
                AffineSymbolicExpressionProduct::Product(mut factors),
            )
            | (
                AffineSymbolicExpressionProduct::Product(mut factors),
                AffineSymbolicExpressionProduct::Affine(affine),
            ) => {
                if let Some(v) = affine.try_to_known() {
                    if v.is_known_zero() {
                        return T::from(0).into();
                    } else {
                        let first = factors.first_mut().unwrap();
                        *first = std::mem::take(first) * v;
                    }
                } else {
                    factors.push(affine);
                }
                AffineSymbolicExpressionProduct::Product(factors)
            }
            (
                AffineSymbolicExpressionProduct::Product(mut factors1),
                AffineSymbolicExpressionProduct::Product(factors2),
            ) => {
                factors1.extend(factors2);
                AffineSymbolicExpressionProduct::Product(factors1)
            }
        }
    }
}
