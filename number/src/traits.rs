use std::{fmt, hash::Hash, ops::*};

use num_traits::{One, Zero};

use crate::{AbstractNumberType, DegreeType};

/// A fixed-width integer type
pub trait BigInt:
    Copy
    + Send
    + Sync
    + PartialEq
    + Eq
    + PartialOrd
    + Ord
    + From<u64>
    + BitAnd<Output = Self>
    + BitOr<Output = Self>
    + BitOrAssign
    + BitAndAssign
    + AddAssign
    + Add<Output = Self>
    + fmt::Display
    + fmt::Debug
    + Copy
    + Not<Output = Self>
    + Shl<u64, Output = Self>
    + Shr<u64, Output = Self>
    + BitXor<Output = Self>
    + Zero
    + fmt::LowerHex
    + TryFrom<num_bigint::BigUint, Error = ()>
{
    /// Number of bits of this base type. Not to be confused with the number of bits
    /// of the field elements!
    const NUM_BITS: usize;
    fn to_arbitrary_integer(self) -> AbstractNumberType;
    /// Number of bits required to encode this particular number.
    fn num_bits(&self) -> u32;

    /// Returns the constant one.
    /// We are not implementing num_traits::One because it also requires multiplication.
    fn one() -> Self;

    /// Checks if the number is one.
    fn is_one(&self) -> bool;
}

/// A field element
pub trait FieldElement:
    'static
    + Sync
    + Send
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
    + Zero
    + One
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
    /// Number of bits required to represent elements of this field.
    const BITS: u32;

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

    fn from_bytes_le(bytes: &[u8]) -> Self;

    fn from_str(s: &str) -> Self;

    fn from_str_radix(s: &str, radix: u32) -> Result<Self, String>;

    /// Returns true if the value is in the "lower half" of the field,
    /// i.e. the value <= (modulus() - 1) / 2
    fn is_in_lower_half(&self) -> bool;
}

#[cfg(test)]
pub fn int_from_hex_str<T: FieldElement>(s: &str) -> T::Integer {
    use num_traits::Num;
    T::Integer::try_from(AbstractNumberType::from_str_radix(s, 16).unwrap()).unwrap()
}
