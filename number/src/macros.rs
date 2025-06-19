macro_rules! powdr_field {
    ($name:ident, $ark_type:ty) => {
        use crate::{
            traits::{FieldElement, KnownField, LargeInt},
            BigUint, DegreeType,
        };
        use ark_ff::{BigInteger, Field, PrimeField};
        use num_traits::{ConstOne, ConstZero, One, Zero};
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
        pub struct LargeIntImpl {
            value: <$ark_type as PrimeField>::BigInt,
        }

        impl fmt::Display for LargeIntImpl {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{}", self.value)
            }
        }

        impl fmt::LowerHex for LargeIntImpl {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let val = self.to_arbitrary_integer();

                fmt::LowerHex::fmt(&val, f)
            }
        }

        impl LargeIntImpl {
            const fn new(value: <$ark_type as PrimeField>::BigInt) -> Self {
                Self { value }
            }
        }

        impl From<u32> for LargeIntImpl {
            fn from(v: u32) -> Self {
                Self::new(v.into())
            }
        }

        impl From<u64> for LargeIntImpl {
            fn from(v: u64) -> Self {
                Self::new(v.into())
            }
        }

        impl Shl<usize> for LargeIntImpl {
            type Output = Self;

            fn shl(self, other: usize) -> Self {
                (BigUint::from_le_bytes(&self.value.to_bytes_le()) << other)
                    .try_into()
                    .unwrap()
            }
        }

        impl Shr<usize> for LargeIntImpl {
            type Output = Self;

            fn shr(self, other: usize) -> Self {
                (BigUint::from_le_bytes(&self.value.to_bytes_le()) >> other)
                    .try_into()
                    .unwrap()
            }
        }

        impl BitAnd for LargeIntImpl {
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

        impl BitOr for LargeIntImpl {
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

        impl BitXor for LargeIntImpl {
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

        impl BitOrAssign for LargeIntImpl {
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

        impl BitAndAssign for LargeIntImpl {
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

        impl Not for LargeIntImpl {
            type Output = Self;

            fn not(mut self) -> Self::Output {
                for limb in self.value.as_mut() {
                    *limb = !*limb;
                }
                self
            }
        }

        impl AddAssign for LargeIntImpl {
            fn add_assign(&mut self, other: Self) {
                self.value.add_with_carry(&other.value);
            }
        }

        impl Add for LargeIntImpl {
            type Output = Self;
            fn add(mut self, other: Self) -> Self {
                self.add_assign(other);
                self
            }
        }

        impl SubAssign for LargeIntImpl {
            fn sub_assign(&mut self, other: Self) {
                self.value.sub_with_borrow(&other.value);
            }
        }

        impl Sub for LargeIntImpl {
            type Output = Self;
            fn sub(mut self, other: Self) -> Self {
                self.sub_assign(other);
                self
            }
        }

        impl Zero for LargeIntImpl {
            #[inline]
            fn zero() -> Self {
                LargeIntImpl::new(<$ark_type as PrimeField>::BigInt::zero())
            }
            #[inline]
            fn is_zero(&self) -> bool {
                self.value.is_zero()
            }
        }

        impl TryFrom<BigUint> for LargeIntImpl {
            type Error = ();

            fn try_from(n: BigUint) -> Result<Self, ()> {
                let n = num_bigint::BigUint::from_bytes_le(&n.to_le_bytes());
                Ok(Self {
                    value: <$ark_type as PrimeField>::BigInt::try_from(n)?,
                })
            }
        }

        impl LargeInt for LargeIntImpl {
            const MAX: Self = LargeIntImpl::new(<$ark_type as PrimeField>::BigInt::new(
                [u64::MAX; <$ark_type as PrimeField>::BigInt::NUM_LIMBS],
            ));
            const NUM_BITS: usize = <$ark_type as PrimeField>::BigInt::NUM_LIMBS * 64;
            #[inline]
            fn to_arbitrary_integer(self) -> BigUint {
                BigUint::from_le_bytes(&self.value.to_bytes_le())
            }
            fn num_bits(&self) -> usize {
                self.value.num_bits() as usize
            }
            #[inline]
            fn one() -> Self {
                LargeIntImpl::new(<$ark_type as PrimeField>::BigInt::one())
            }
            #[inline]
            fn is_one(&self) -> bool {
                self.value == <$ark_type as PrimeField>::BigInt::one()
            }

            fn try_into_u64(&self) -> Option<u64> {
                for v in self.value.0[1..].iter() {
                    if *v != 0 {
                        return None;
                    }
                }
                Some(self.value.0[0])
            }

            fn try_into_u32(&self) -> Option<u32> {
                let v = self.try_into_u64()?;
                v.try_into().ok()
            }

            fn from_hex(s: &str) -> Self {
                BigUint::from_str_radix(s, 16).unwrap().try_into().unwrap()
            }
        }

        impl ConstZero for LargeIntImpl {
            const ZERO: Self = LargeIntImpl::new(<$ark_type as PrimeField>::BigInt::zero());
        }

        impl From<BigUint> for $name {
            fn from(n: BigUint) -> Self {
                let n = num_bigint::BigUint::from_bytes_le(&n.to_le_bytes());
                Self { value: n.into() }
            }
        }

        impl From<LargeIntImpl> for $name {
            fn from(n: LargeIntImpl) -> Self {
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
                let modulus = <$ark_type>::MODULUS.to_bytes_le();
                if n >= BigUint::from_le_bytes(&modulus) {
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
            type Integer = LargeIntImpl;
            const BITS: u32 = <$ark_type>::MODULUS_BIT_SIZE;

            fn known_field() -> Option<KnownField> {
                Some(KnownField::$name)
            }

            fn from_str_radix(s: &str, radix: u32) -> Result<Self, String> {
                let n = BigUint::from_str_radix(s, radix).map_err(|e| e.to_string())?;
                let modulus = <$ark_type>::MODULUS.to_bytes_le();
                if n >= BigUint::from_le_bytes(&modulus) {
                    Err(format!("Hexadecimal number \"0x{s}\" too large for field."))
                } else {
                    Ok(n.into())
                }
            }

            fn checked_from(value: BigUint) -> Option<Self> {
                let modulus = <$ark_type>::MODULUS.to_bytes_le();
                if value < BigUint::from_le_bytes(&modulus) {
                    Some(value.into())
                } else {
                    None
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

            fn has_direct_repr() -> bool {
                false
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

            #[inline]
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

        impl std::ops::MulAssign for $name {
            fn mul_assign(&mut self, rhs: Self) {
                self.value.mul_assign(rhs.value);
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

            #[inline]
            fn neg(self) -> Self::Output {
                (-self.value).into()
            }
        }

        impl Zero for $name {
            #[inline]
            fn zero() -> Self {
                <$ark_type>::ZERO.into()
            }
            #[inline]
            fn is_zero(&self) -> bool {
                self.value == <$ark_type>::ZERO
            }
        }

        impl ConstZero for $name {
            const ZERO: Self = Self {
                value: <$ark_type>::ZERO,
            };
        }

        impl One for $name {
            #[inline]
            fn one() -> Self {
                <$ark_type>::ONE.into()
            }
            #[inline]
            fn is_one(&self) -> bool {
                self.value == <$ark_type>::ONE
            }
        }

        impl ConstOne for $name {
            const ONE: Self = Self {
                value: <$ark_type>::ONE,
            };
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let value = self.to_integer().value;
                write!(f, "{value}")
            }
        }
    };
}
