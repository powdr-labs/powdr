use std::{fmt, hash::Hash, ops::*};

use crate::{AbstractNumberType, DegreeType};

/// A fixed-width integer type
pub trait BigInt:
    Copy
    + PartialEq
    + Eq
    + From<u32>
    + BitAnd<Output = Self>
    + BitOr<Output = Self>
    + BitOrAssign
    + BitAndAssign
    + fmt::Display
    + fmt::Debug
    + Copy
    + Not<Output = Self>
    + Shl<u64, Output = Self>
    + Shr<u64, Output = Self>
    + BitXor<Output = Self>
    + fmt::LowerHex
    + TryFrom<num_bigint::BigUint, Error = ()>
{
    fn to_arbitrary_integer(self) -> AbstractNumberType;
}

/// A field element
pub trait FieldElement:
    'static
    + Default
    + Copy
    + PartialEq
    + Eq
    + Send
    + Sync
    + PartialOrd
    + Ord
    + Hash
    + Add<Output = Self>
    + AddAssign
    + Sub<Output = Self>
    + Mul<Output = Self>
    + Div<Output = Self>
    + Neg<Output = Self>
    + fmt::Display
    + fmt::Debug
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

    fn integer_mod(self, other: Self) -> Self;

    fn to_bytes_le(&self) -> Vec<u8>;

    fn from_str(s: &str) -> Self;

    fn from_str_radix(s: &str, radix: u32) -> Result<Self, String>;
}

#[cfg(test)]
pub fn int_from_hex_str<T: FieldElement>(s: &str) -> T::Integer {
    use num_traits::Num;
    T::Integer::try_from(AbstractNumberType::from_str_radix(s, 16).unwrap()).unwrap()
}
