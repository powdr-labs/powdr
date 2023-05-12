use std::{fmt, ops::*};

use crate::{AbstractNumberType, DegreeType};

/// A fixed-width integer type
pub trait BigInt:
    From<u32>
    + BitAnd
    + BitOr
    + BitOrAssign
    + BitAndAssign
    + Copy
    + Not
    + Shl<u64>
    + Shr<u64>
    + BitXor
    + fmt::LowerHex
    + TryFrom<num_bigint::BigUint, Error = ()>
{
    fn to_arbitrary_integer(self) -> AbstractNumberType;
}

/// A field element
pub trait FieldElementTrait:
    Add
    + Sub
    + Mul
    + Div
    + Neg
    + fmt::Display
    + From<Self::Integer>
    + From<num_bigint::BigUint>
    + From<u32>
    + From<u64>
    + From<i32>
    + From<i64>
    + From<bool>
    + fmt::LowerHex
{
    /// The underlying fixed-width integer type
    type Integer: BigInt;

    fn to_degree(&self) -> DegreeType;

    fn to_integer(&self) -> Self::Integer;

    fn to_arbitrary_integer(&self) -> AbstractNumberType {
        self.to_integer().to_arbitrary_integer()
    }

    fn modulus() -> Self::Integer;

    fn pow(self, exponent: Self::Integer) -> Self;

    fn integer_div(self, other: Self) -> Self;

    fn to_bytes_le(&self) -> Vec<u8>;

    fn from_str(s: &str) -> Self;

    fn from_str_radix(s: &str, radix: u32) -> Result<Self, String>;
}

#[cfg(test)]
pub fn int_from_hex_str<T: FieldElementTrait>(s: &str) -> T::Integer {
    use num_traits::Num;
    T::Integer::try_from(AbstractNumberType::from_str_radix(s, 16).unwrap()).unwrap()
}
