use std::{
    fmt::{self, Display, Formatter},
    ops::{Add, Mul, Neg},
};

use itertools::Itertools;
use powdr_number::FieldElement;

use super::{
    affine_symbolic_expression::{AffineSymbolicExpression, Error, ProcessResult},
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
    //     pub fn from_known_symbol(symbol: V, rc: RangeConstraint<T>) -> Self {
    //         SymbolicExpression::from_symbol(symbol, rc).into()
    //     }
    //     pub fn from_unknown_variable(var: V, rc: RangeConstraint<T>) -> Self {
    //         AffineSymbolicExpression {
    //             coefficients: [(var.clone(), T::from(1).into())].into_iter().collect(),
    //             offset: SymbolicExpression::from(T::from(0)),
    //             range_constraints: [(var.clone(), rc)].into_iter().collect(),
    //         }
    //     }

    /// If this expression does not contain unknown variables, returns the symbolic expression.
    pub fn try_to_known(&self) -> Option<&SymbolicExpression<T, V>> {
        if let Self::Affine(affine) = self {
            affine.try_to_known()
        } else {
            None
        }
    }

    //     /// Returns the range constraint of the whole expression.
    //     /// This only works for simple expressions since all coefficients
    //     /// must be known numbers.
    //     pub fn range_constraint(&self) -> RangeConstraint<T> {
    //         self.coefficients
    //             .iter()
    //             .map(|(var, coeff)| {
    //                 let coeff = coeff.try_to_number()?;
    //                 let rc = self.range_constraints.get(var)?;
    //                 Some(rc.multiple(coeff))
    //             })
    //             .collect::<Option<Vec<_>>>()
    //             .and_then(|summands| {
    //                 summands
    //                     .into_iter()
    //                     .chain(std::iter::once(self.offset.range_constraint()))
    //                     .reduce(|c1, c2| c1.combine_sum(&c2))
    //             })
    //             .unwrap_or_default()
    //     }

    //     /// If this expression contains a single unknown variable, returns it.
    //     pub fn single_unknown_variable(&self) -> Option<&V> {
    //         if self.coefficients.len() == 1 {
    //             self.coefficients.keys().next()
    //         } else {
    //             None
    //         }
    //     }

    //     /// Tries to multiply this expression with another one.
    //     /// Returns `None` if the result would be quadratic, i.e.
    //     /// if both expressions contain unknown variables.
    //     pub fn try_mul(&self, other: &Self) -> Option<Self> {
    //         if let Some(multiplier) = other.try_to_known() {
    //             Some(self.clone() * multiplier)
    //         } else {
    //             self.try_to_known()
    //                 .map(|multiplier| other.clone() * multiplier)
    //         }
    //     }

    /// Solves the equation `self = 0` and returns how to compute the solution.
    /// The solution can contain assignments to multiple variables.
    /// If no way to solve the equation (and no way to derive new range
    /// constraints) has been found, but it still contains
    /// unknown variables, returns an empty, incomplete result.
    /// If the equation is known to be unsolvable, returns an error.
    pub fn solve(&self) -> Result<ProcessResult<T, V>, Error> {
        if let AffineSymbolicExpressionProduct::Affine(expr) = self {
            expr.solve()
        } else {
            todo!()
        }
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

// impl<T: FieldElement, V: Clone + Ord> Add for AffineSymbolicExpressionProduct<T, V> {
//     type Output = AffineSymbolicExpression<T, V>;

//     fn add(self, rhs: Self) -> Self::Output {
//         &self + &rhs
//     }
// }

// impl<T: FieldElement, V: Clone + Ord> Sub for &AffineSymbolicExpressionProduct<T, V> {
//     type Output = AffineSymbolicExpression<T, V>;

//     fn sub(self, rhs: Self) -> Self::Output {
//         self + &-rhs
//     }
// }

// impl<T: FieldElement, V: Clone + Ord> Sub for AffineSymbolicExpressionProduct<T, V> {
//     type Output = AffineSymbolicExpression<T, V>;

//     fn sub(self, rhs: Self) -> Self::Output {
//         &self - &rhs
//     }
// }

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
