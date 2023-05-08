macro_rules! powdr_field {
    ($name:ident, $ark_type:ty) => {
        use crate::{
            traits::{BigInt, FieldElementTrait},
            DegreeType,
        };
        use ark_ff::{BigInteger, Field, PrimeField};
        use num_bigint::BigUint;
        use num_traits::Num;
        use std::fmt;
        use std::ops::*;
        use std::str::FromStr;

        #[derive(Clone, Copy, PartialEq, Eq, Debug, Default, PartialOrd, Ord, Hash)]
        pub struct $name {
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
                let val = self.into_biguint();

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

        impl BigInt for BigIntImpl {
            fn into_biguint(self) -> BigUint {
                self.value.into()
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

        impl fmt::LowerHex for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt::LowerHex::fmt(&self.to_integer(), f)
            }
        }

        impl FieldElementTrait for $name {
            type Integer = BigIntImpl;

            fn from_str(s: &str) -> Self {
                Self {
                    value: <$ark_type>::from_str(s).unwrap(),
                }
            }

            fn from_str_radix(s: &str, radix: u32) -> Result<Self, String> {
                BigUint::from_str_radix(s, radix)
                    .map(|n| n.into())
                    .map_err(|e| e.to_string())
            }

            fn to_degree(&self) -> DegreeType {
                let degree: BigUint = self.to_integer().into_biguint();
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
                (BigUint::from(self.to_arbitrary_integer())
                    / BigUint::from(other.to_arbitrary_integer()))
                .try_into()
                .unwrap()
            }

            fn to_bytes_le(&self) -> Vec<u8> {
                self.value.into_bigint().to_bytes_le()
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

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let value = self.to_integer().value;
                if value > <$ark_type>::MODULUS_MINUS_ONE_DIV_TWO {
                    let mut res = Self::modulus();
                    assert!(!res.value.sub_with_borrow(&value));
                    write!(f, "-{}", res)
                } else {
                    write!(f, "{value}")
                }
            }
        }
    };
}
