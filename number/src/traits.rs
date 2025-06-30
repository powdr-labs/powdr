use std::{
    fmt::{self, Display},
    hash::Hash,
    ops::*,
    str::FromStr,
};

use ibig::IBig;
use num_traits::{ConstOne, ConstZero, One, Zero};
use schemars::JsonSchema;
use serde::{de::DeserializeOwned, Deserialize, Serialize};

use crate::{BigUint, DegreeType};

/// A fixed-width integer type
pub trait LargeInt:
    Copy
    + Send
    + Sync
    + PartialEq
    + Eq
    + PartialOrd
    + Ord
    + Hash
    + From<u64>
    + BitAnd<Output = Self>
    + BitOr<Output = Self>
    + BitOrAssign
    + BitAndAssign
    + AddAssign
    + Add<Output = Self>
    + SubAssign
    + Sub<Output = Self>
    + fmt::Display
    + fmt::Debug
    + Copy
    + Not<Output = Self>
    + Shl<usize, Output = Self>
    + Shr<usize, Output = Self>
    + BitXor<Output = Self>
    + Zero
    + ConstZero
    + fmt::LowerHex
{
    /// The largest value of this type, i.e. 2**NUM_BITS - 1
    const MAX: Self;
    /// Number of bits of this base type. Not to be confused with the number of bits
    /// of the field elements!
    const NUM_BITS: usize;
    fn to_arbitrary_integer(self) -> BigUint;
    /// Number of bits required to encode this particular number.
    fn num_bits(&self) -> usize;

    /// Returns the constant one.
    /// We are not implementing num_traits::One because it also requires multiplication.
    fn one() -> Self;

    /// Checks if the number is one.
    fn is_one(&self) -> bool;

    /// Tries to convert to u64.
    ///
    /// Returns None if value is out of u64 range.
    fn try_into_u64(&self) -> Option<u64>;

    /// Tries to convert to u32.
    ///
    /// Returns None if value is out of u32 range.
    fn try_into_u32(&self) -> Option<u32>;

    /// Creates a LargeInt from a hex string.
    /// Panics on failure - intended for testing.
    fn from_hex(s: &str) -> Self;
}

pub enum FieldSize {
    /// Fields that fit a 29-Bit number, but not much more.
    Small,
    /// Fields that at least fit a product of two 32-Bit numbers
    /// (Goldilocks and larger)
    Large,
}

#[derive(Copy, Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum KnownField {
    BabyBearField,
    KoalaBearField,
    Mersenne31Field,
    GoldilocksField,
    Bn254Field,
}

impl KnownField {
    pub fn field_size(&self) -> FieldSize {
        match self {
            KnownField::BabyBearField
            | KnownField::KoalaBearField
            | KnownField::Mersenne31Field => FieldSize::Small,
            KnownField::GoldilocksField | KnownField::Bn254Field => FieldSize::Large,
        }
    }
}

impl Display for KnownField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KnownField::BabyBearField => write!(f, "BabyBear"),
            KnownField::KoalaBearField => write!(f, "KoalaBear"),
            KnownField::Mersenne31Field => write!(f, "Mersenne31"),
            KnownField::GoldilocksField => write!(f, "Goldilocks"),
            KnownField::Bn254Field => write!(f, "Bn254"),
        }
    }
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
    + SubAssign
    + Mul<Output = Self>
    + MulAssign
    + Div<Output = Self>
    + Neg<Output = Self>
    + Zero
    + ConstZero
    + ConstOne
    + One
    + fmt::Display
    + fmt::Debug
    + From<Self::Integer>
    + From<crate::BigUint>
    + FromStr<Err = String>
    + From<u32>
    + From<u64>
    + From<i32>
    + From<i64>
    + From<bool>
    + fmt::LowerHex
    + Serialize
    + DeserializeOwned
    + JsonSchema
{
    /// The underlying fixed-width integer type
    type Integer: LargeInt;
    /// Number of bits required to represent elements of this field.
    const BITS: u32;

    fn to_degree(&self) -> DegreeType;

    fn to_integer(&self) -> Self::Integer;

    fn to_arbitrary_integer(&self) -> BigUint {
        self.to_integer().to_arbitrary_integer()
    }

    fn modulus() -> Self::Integer;

    fn pow(self, exponent: Self::Integer) -> Self;

    fn to_bytes_le(&self) -> Vec<u8>;

    fn from_bytes_le(bytes: &[u8]) -> Self;

    fn from_str_radix(s: &str, radix: u32) -> Result<Self, String>;

    /// Only converts the value to a field element if it is less than the modulus.
    fn checked_from(value: BigUint) -> Option<Self>;

    /// Returns true if the value is in the "lower half" of the field,
    /// i.e. the value <= (modulus() - 1) / 2
    fn is_in_lower_half(&self) -> bool;

    /// If the field is a known field (as listed in the `KnownField` enum), returns the field variant.
    fn known_field() -> Option<KnownField>;

    /// Converts to a signed integer.
    ///
    /// Negative values are in relation to 0 in the field.
    /// Values up to the modulus / 2 are positive, values above are negative.
    fn to_signed_integer(&self) -> IBig {
        if self.is_in_lower_half() {
            self.to_arbitrary_integer().into()
        } else {
            IBig::from(self.to_arbitrary_integer())
                - IBig::from(Self::modulus().to_arbitrary_integer())
        }
    }

    /// Returns `true` if values of this type are directly stored as their integer
    /// value, i.e
    /// - montgomery representation is not used
    /// - values are always canonical (i.e. smaller than the modulus)
    /// - there are no additional fields and
    /// - `repr(transparent)` is used.
    ///
    /// In other words, the `to_integer` function can be implemented as
    /// a mem::transmute operation on pointers.
    fn has_direct_repr() -> bool;
}

#[cfg(test)]
pub fn int_from_hex_str<T: FieldElement>(s: &str) -> T::Integer {
    T::Integer::from_hex(s)
}
