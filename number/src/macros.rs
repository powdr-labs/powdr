macro_rules! powdr_field {
    ($name:ident, $ark_type:ty) => {
        use crate::{
            traits::{BigInt, FieldElement, KnownField},
            DegreeType,
        };
        use ark_ff::{BigInteger, Field, PrimeField};
        use num_bigint::BigUint;
        use num_traits::{Num, One, Zero};
        use std::fmt;
        use std::ops::*;
        use std::str::FromStr;

        #[derive(
            Clone,
            Copy,
            PartialEq,
            Eq,
            Debug,
            Default,
            PartialOrd,
            Ord,
            Hash,
            Serialize,
            Deserialize,
            JsonSchema,
        )]
        pub struct $name {
            #[serde(
                serialize_with = "crate::serialize::ark_se",
                deserialize_with = "crate::serialize::ark_de"
            )]
            #[schemars(skip)]
            value: $ark_type,
        }

        #[derive(Clone, Copy, PartialEq, Eq, Debug, Default, PartialOrd, Ord, Hash)]
        pub struct BigIntImpl {
            value: <$ark_type as PrimeField>::BigInt,
        }

        impl fmt::Display for BigIntImpl {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{}", self.value)
            }
        }

        impl fmt::LowerHex for BigIntImpl {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let val = self.to_arbitrary_integer();

                fmt::LowerHex::fmt(&val, f)
            }
        }

        impl BigIntImpl {
            fn new(value: <$ark_type as PrimeField>::BigInt) -> Self {
                Self { value }
            }
        }

        impl From<u32> for BigIntImpl {
            fn from(v: u32) -> Self {
                Self::new(v.into())
            }
        }

        impl From<u64> for BigIntImpl {
            fn from(v: u64) -> Self {
                Self::new(v.into())
            }
        }

        impl Shl<u64> for BigIntImpl {
            type Output = Self;

            fn shl(self, other: u64) -> Self {
                // TODO: avoid using BigUint
                Self {
                    value: (BigUint::from(self.value) << other).try_into().unwrap(),
                }
            }
        }

        impl Shr<u64> for BigIntImpl {
            type Output = Self;

            fn shr(self, other: u64) -> Self {
                // TODO: avoid using BigUint
                Self {
                    value: (BigUint::from(self.value) >> other).try_into().unwrap(),
                }
            }
        }

        impl BitAnd for BigIntImpl {
            type Output = Self;

            fn bitand(mut self, other: Self) -> Self {
                for (x, y) in self
                    .value
                    .as_mut()
                    .iter_mut()
                    .zip(other.value.as_ref().iter())
                {
                    *x &= y;
                }
                self
            }
        }

        impl BitOr for BigIntImpl {
            type Output = Self;

            fn bitor(mut self, other: Self) -> Self {
                for (x, y) in self
                    .value
                    .as_mut()
                    .iter_mut()
                    .zip(other.value.as_ref().iter())
                {
                    *x |= y;
                }
                self
            }
        }

        impl BitXor for BigIntImpl {
            type Output = Self;

            fn bitxor(mut self, other: Self) -> Self {
                for (x, y) in self
                    .value
                    .as_mut()
                    .iter_mut()
                    .zip(other.value.as_ref().iter())
                {
                    *x ^= y;
                }
                self
            }
        }

        impl BitOrAssign for BigIntImpl {
            fn bitor_assign(&mut self, other: Self) {
                for (x, y) in self
                    .value
                    .as_mut()
                    .iter_mut()
                    .zip(other.value.as_ref().iter())
                {
                    *x |= y;
                }
            }
        }

        impl BitAndAssign for BigIntImpl {
            fn bitand_assign(&mut self, other: Self) {
                for (x, y) in self
                    .value
                    .as_mut()
                    .iter_mut()
                    .zip(other.value.as_ref().iter())
                {
                    *x &= y;
                }
            }
        }

        impl Not for BigIntImpl {
            type Output = Self;

            fn not(mut self) -> Self::Output {
                for limb in self.value.as_mut() {
                    *limb = !*limb;
                }
                self
            }
        }

        impl AddAssign for BigIntImpl {
            fn add_assign(&mut self, other: Self) {
                self.value.add_with_carry(&other.value);
            }
        }

        impl Add for BigIntImpl {
            type Output = Self;
            fn add(mut self, other: Self) -> Self {
                self.add_assign(other);
                self
            }
        }

        impl Zero for BigIntImpl {
            fn zero() -> Self {
                BigIntImpl::new(<$ark_type as PrimeField>::BigInt::zero())
            }
            fn is_zero(&self) -> bool {
                self.value.is_zero()
            }
        }

        impl TryFrom<BigUint> for BigIntImpl {
            type Error = ();

            fn try_from(n: BigUint) -> Result<Self, ()> {
                Ok(Self {
                    value: <$ark_type as PrimeField>::BigInt::try_from(n)?,
                })
            }
        }

        impl BigInt for BigIntImpl {
            const NUM_BITS: usize = <$ark_type as PrimeField>::BigInt::NUM_LIMBS * 64;
            fn to_arbitrary_integer(self) -> BigUint {
                self.value.into()
            }
            fn num_bits(&self) -> u32 {
                self.value.num_bits()
            }
            fn one() -> Self {
                BigIntImpl::new(<$ark_type as PrimeField>::BigInt::one())
            }
            fn is_one(&self) -> bool {
                self.value == <$ark_type as PrimeField>::BigInt::one()
            }
        }

        impl From<BigUint> for $name {
            fn from(n: BigUint) -> Self {
                Self { value: n.into() }
            }
        }

        impl From<BigIntImpl> for $name {
            fn from(n: BigIntImpl) -> Self {
                Self {
                    value: n.value.into(),
                }
            }
        }

        impl From<u32> for $name {
            fn from(n: u32) -> Self {
                (<$ark_type>::from(n)).into()
            }
        }

        impl From<u64> for $name {
            fn from(n: u64) -> Self {
                (<$ark_type>::from(n)).into()
            }
        }

        impl From<i32> for $name {
            fn from(n: i32) -> Self {
                (<$ark_type>::from(n)).into()
            }
        }

        impl From<i64> for $name {
            fn from(n: i64) -> Self {
                (<$ark_type>::from(n)).into()
            }
        }

        impl From<bool> for $name {
            fn from(n: bool) -> Self {
                (<$ark_type>::from(n)).into()
            }
        }

        impl FromStr for $name {
            type Err = String;
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                let n = BigUint::from_str(s).map_err(|e| e.to_string())?;
                if n >= <$ark_type>::MODULUS.into() {
                    Err(format!("Decimal number \"{s}\" too large for field."))
                } else {
                    Ok(n.into())
                }
            }
        }

        impl fmt::LowerHex for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt::LowerHex::fmt(&self.to_integer(), f)
            }
        }

        impl FieldElement for $name {
            type Integer = BigIntImpl;
            const BITS: u32 = <$ark_type>::MODULUS_BIT_SIZE;

            fn known_field() -> Option<KnownField> {
                Some(KnownField::$name)
            }

            fn from_str_radix(s: &str, radix: u32) -> Result<Self, String> {
                let n = BigUint::from_str_radix(s, radix).map_err(|e| e.to_string())?;
                if n >= <$ark_type>::MODULUS.into() {
                    Err(format!("Hexadecimal number \"0x{s}\" too large for field."))
                } else {
                    Ok(n.into())
                }
            }

            fn to_degree(&self) -> DegreeType {
                let degree: BigUint = self.to_integer().to_arbitrary_integer();
                degree.try_into().unwrap()
            }

            fn to_integer(&self) -> Self::Integer {
                Self::Integer::new(self.value.into_bigint())
            }

            fn modulus() -> Self::Integer {
                Self::Integer::new(<$ark_type>::MODULUS)
            }

            fn pow(self, exponent: Self::Integer) -> Self {
                Self {
                    value: self.value.pow(exponent.value),
                }
            }

            fn integer_div(self, other: Self) -> Self {
                (self.to_arbitrary_integer() / other.to_arbitrary_integer())
                    .try_into()
                    .unwrap()
            }

            fn integer_mod(self, other: Self) -> Self {
                (self.to_arbitrary_integer() % other.to_arbitrary_integer())
                    .try_into()
                    .unwrap()
            }

            fn to_bytes_le(&self) -> Vec<u8> {
                self.value.into_bigint().to_bytes_le()
            }

            fn from_bytes_le(bytes: &[u8]) -> Self {
                assert_eq!(
                    bytes.len(),
                    <$ark_type as PrimeField>::BigInt::NUM_LIMBS * 8,
                    "wrong number of bytes for field type"
                );

                let mut limbs = [0u64; <$ark_type as PrimeField>::BigInt::NUM_LIMBS];
                for (from, to) in bytes.chunks(8).zip(limbs.iter_mut()) {
                    *to = u64::from_le_bytes(from.try_into().unwrap());
                }

                Self {
                    value: <$ark_type as PrimeField>::BigInt::new(limbs).into(),
                }
            }

            fn is_in_lower_half(&self) -> bool {
                self.to_integer().value <= <$ark_type>::MODULUS_MINUS_ONE_DIV_TWO
            }
        }

        impl From<$ark_type> for $name {
            #[inline]
            fn from(value: $ark_type) -> Self {
                Self { value }
            }
        }

        // Add

        impl std::ops::Add for $name {
            type Output = $name;

            fn add(self, rhs: Self) -> Self::Output {
                (self.value + rhs.value).into()
            }
        }

        impl AddAssign for $name {
            fn add_assign(&mut self, rhs: Self) {
                self.value.add_assign(rhs.value);
            }
        }

        // Sub

        impl std::ops::Sub for $name {
            type Output = $name;

            fn sub(self, rhs: Self) -> Self::Output {
                (self.value - rhs.value).into()
            }
        }

        impl SubAssign for $name {
            fn sub_assign(&mut self, rhs: Self) {
                self.value.sub_assign(rhs.value);
            }
        }

        // Mul

        impl std::ops::Mul for $name {
            type Output = $name;

            fn mul(self, rhs: Self) -> Self::Output {
                (self.value * rhs.value).into()
            }
        }

        // Div

        impl std::ops::Div for $name {
            type Output = $name;

            fn div(self, rhs: Self) -> Self::Output {
                (self.value / rhs.value).into()
            }
        }

        impl std::ops::Neg for $name {
            type Output = $name;

            fn neg(self) -> Self::Output {
                (-self.value).into()
            }
        }

        impl Zero for $name {
            fn zero() -> Self {
                <$ark_type as Zero>::zero().into()
            }
            fn is_zero(&self) -> bool {
                self.value.is_zero()
            }
        }

        impl One for $name {
            fn one() -> Self {
                <$ark_type as One>::one().into()
            }
            fn is_one(&self) -> bool {
                self.value.is_one()
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let value = self.to_integer().value;
                write!(f, "{value}")
            }
        }
    };
}
