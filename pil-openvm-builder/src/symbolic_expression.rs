use core::fmt::Debug;
use core::iter::{Product, Sum};
use core::ops::{Add, AddAssign, Mul, MulAssign, Neg, Sub, SubAssign};
use std::rc::Rc;

use openvm_circuit::openvm_stark_sdk::openvm_stark_backend::p3_field::{
    Field, FieldAlgebra, PackedField,
};
use p3_field::AbstractField;
//use p3_field::Field as P3Field;

use crate::symbolic_variable::SymbolicVariable;

/// An expression over `SymbolicVariable`s.
#[derive(Clone, Debug)]
pub enum SymbolicExpression<F: Field> {
    Variable(SymbolicVariable<F>),
    IsFirstRow,
    IsLastRow,
    IsTransition,
    Constant(F),
    Add {
        x: Rc<Self>,
        y: Rc<Self>,
        degree_multiple: usize,
    },
    Sub {
        x: Rc<Self>,
        y: Rc<Self>,
        degree_multiple: usize,
    },
    Neg {
        x: Rc<Self>,
        degree_multiple: usize,
    },
    Mul {
        x: Rc<Self>,
        y: Rc<Self>,
        degree_multiple: usize,
    },
}

impl<F: Field> SymbolicExpression<F> {
    /// Returns the multiple of `n` (the trace length) in this expression's degree.
    pub const fn degree_multiple(&self) -> usize {
        match self {
            SymbolicExpression::Variable(v) => v.degree_multiple(),
            SymbolicExpression::IsFirstRow => 1,
            SymbolicExpression::IsLastRow => 1,
            SymbolicExpression::IsTransition => 0,
            SymbolicExpression::Constant(_) => 0,
            SymbolicExpression::Add {
                degree_multiple, ..
            } => *degree_multiple,
            SymbolicExpression::Sub {
                degree_multiple, ..
            } => *degree_multiple,
            SymbolicExpression::Neg {
                degree_multiple, ..
            } => *degree_multiple,
            SymbolicExpression::Mul {
                degree_multiple, ..
            } => *degree_multiple,
        }
    }
}

impl<F: Field> Default for SymbolicExpression<F> {
    fn default() -> Self {
        Self::Constant(F::ZERO)
    }
}

impl<F: Field> From<F> for SymbolicExpression<F> {
    fn from(value: F) -> Self {
        Self::Constant(value)
    }
}

#[derive(Clone, Debug)]
pub struct PackedPowers<F, P: PackedField<Scalar = F>> {
    // base ** P::WIDTH
    pub multiplier: P,
    pub current: P,
}

impl<AF: AbstractField, P: PackedField<Scalar = AF>> Iterator for PackedPowers<AF, P> {
    type Item = P;

    fn next(&mut self) -> Option<P> {
        let result = self.current;
        self.current *= self.multiplier;
        Some(result)
    }
}

impl<F: Field> FieldAlgebra for SymbolicExpression<F> {
    type F = F;

    const ZERO: Self = Self::Constant(F::ZERO);

    const ONE: Self = Self::Constant(F::ONE);

    const TWO: Self = Self::Constant(F::TWO);

    const NEG_ONE: Self = Self::Constant(F::NEG_ONE);

    #[inline]
    fn from_f(f: Self::F) -> Self {
        f.into()
    }

    fn from_bool(b: bool) -> Self {
        Self::Constant(F::from_bool(b))
    }

    fn from_canonical_u8(n: u8) -> Self {
        Self::Constant(F::from_canonical_u8(n))
    }

    fn from_canonical_u16(n: u16) -> Self {
        Self::Constant(F::from_canonical_u16(n))
    }

    fn from_canonical_u32(n: u32) -> Self {
        Self::Constant(F::from_canonical_u32(n))
    }

    fn from_canonical_u64(n: u64) -> Self {
        Self::Constant(F::from_canonical_u64(n))
    }

    fn from_canonical_usize(n: usize) -> Self {
        Self::Constant(F::from_canonical_usize(n))
    }

    fn from_wrapped_u32(n: u32) -> Self {
        Self::Constant(F::from_wrapped_u32(n))
    }

    fn from_wrapped_u64(n: u64) -> Self {
        Self::Constant(F::from_wrapped_u64(n))
    }
}
// impl<F: Field> AbstractField for SymbolicExpression<F> {
//     type F = F;

//     fn zero() -> Self {
//         Self::Constant(F::ZERO)
//     }
//     fn one() -> Self {
//         Self::Constant(F::ONE)
//     }
//     fn two() -> Self {
//         Self::Constant(F::TWO)
//     }
//     fn neg_one() -> Self {
//         Self::Constant(F::NEG_ONE)
//     }

//     #[inline]
//     fn from_f(f: Self::F) -> Self {
//         f.into()
//     }

//     fn from_bool(b: bool) -> Self {
//         Self::Constant(F::from_bool(b))
//     }

//     fn from_canonical_u8(n: u8) -> Self {
//         Self::Constant(F::from_canonical_u8(n))
//     }

//     fn from_canonical_u16(n: u16) -> Self {
//         Self::Constant(F::from_canonical_u16(n))
//     }

//     fn from_canonical_u32(n: u32) -> Self {
//         Self::Constant(F::from_canonical_u32(n))
//     }

//     fn from_canonical_u64(n: u64) -> Self {
//         Self::Constant(F::from_canonical_u64(n))
//     }

//     fn from_canonical_usize(n: usize) -> Self {
//         Self::Constant(F::from_canonical_usize(n))
//     }

//     fn from_wrapped_u32(n: u32) -> Self {
//         Self::Constant(F::from_wrapped_u32(n))
//     }

//     fn from_wrapped_u64(n: u64) -> Self {
//         Self::Constant(F::from_wrapped_u64(n))
//     }

//     fn generator() -> Self {
//         Self::Constant(F::GENERATOR)
//     }
// }

impl<F: Field> Add for SymbolicExpression<F> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        let degree_multiple = self.degree_multiple().max(rhs.degree_multiple());
        Self::Add {
            x: Rc::new(self),
            y: Rc::new(rhs),
            degree_multiple,
        }
    }
}

impl<F: Field> Add<F> for SymbolicExpression<F> {
    type Output = Self;

    fn add(self, rhs: F) -> Self {
        self + Self::from(rhs)
    }
}

impl<F: Field> AddAssign for SymbolicExpression<F> {
    fn add_assign(&mut self, rhs: Self) {
        *self = self.clone() + rhs;
    }
}

impl<F: Field> AddAssign<F> for SymbolicExpression<F> {
    fn add_assign(&mut self, rhs: F) {
        *self += Self::from(rhs);
    }
}

impl<F: Field> Sum for SymbolicExpression<F> {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.reduce(|x, y| x + y).unwrap_or(Self::ZERO)
    }
}

impl<F: Field> Sum<F> for SymbolicExpression<F> {
    fn sum<I: Iterator<Item = F>>(iter: I) -> Self {
        iter.map(|x| Self::from(x)).sum()
    }
}

impl<F: Field> Sub for SymbolicExpression<F> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        let degree_multiple = self.degree_multiple().max(rhs.degree_multiple());
        Self::Sub {
            x: Rc::new(self),
            y: Rc::new(rhs),
            degree_multiple,
        }
    }
}

impl<F: Field> Sub<F> for SymbolicExpression<F> {
    type Output = Self;

    fn sub(self, rhs: F) -> Self {
        self - Self::from(rhs)
    }
}

impl<F: Field> SubAssign for SymbolicExpression<F> {
    fn sub_assign(&mut self, rhs: Self) {
        *self = self.clone() - rhs;
    }
}

impl<F: Field> SubAssign<F> for SymbolicExpression<F> {
    fn sub_assign(&mut self, rhs: F) {
        *self -= Self::from(rhs);
    }
}

impl<F: Field> Neg for SymbolicExpression<F> {
    type Output = Self;

    fn neg(self) -> Self {
        let degree_multiple = self.degree_multiple();
        Self::Neg {
            x: Rc::new(self),
            degree_multiple,
        }
    }
}

impl<F: Field> Mul for SymbolicExpression<F> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self {
        #[allow(clippy::suspicious_arithmetic_impl)]
        let degree_multiple = self.degree_multiple() + rhs.degree_multiple();
        Self::Mul {
            x: Rc::new(self),
            y: Rc::new(rhs),
            degree_multiple,
        }
    }
}

impl<F: Field> Mul<F> for SymbolicExpression<F> {
    type Output = Self;

    fn mul(self, rhs: F) -> Self {
        self * Self::from(rhs)
    }
}

impl<F: Field> MulAssign for SymbolicExpression<F> {
    fn mul_assign(&mut self, rhs: Self) {
        *self = self.clone() * rhs;
    }
}

impl<F: Field> MulAssign<F> for SymbolicExpression<F> {
    fn mul_assign(&mut self, rhs: F) {
        *self *= Self::from(rhs);
    }
}

impl<F: Field> Product for SymbolicExpression<F> {
    fn product<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.reduce(|x, y| x * y).unwrap_or(Self::ONE)
    }
}

impl<F: Field> Product<F> for SymbolicExpression<F> {
    fn product<I: Iterator<Item = F>>(iter: I) -> Self {
        iter.map(|x| Self::from(x)).product()
    }
}
