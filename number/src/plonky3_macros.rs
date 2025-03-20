#[macro_export]
macro_rules! powdr_field_plonky3 {
    ($name:ident, $p3_type:ty) => {
        use schemars::{
            schema::{Schema, SchemaObject},
            JsonSchema,
        };
        use serde::{Deserialize, Serialize};

        use num_traits::{ConstOne, ConstZero};
        use std::ops::{Add, AddAssign, Div, Mul, MulAssign, Neg, Not, Sub, SubAssign};
        use std::str::FromStr;
        use std::{collections::BTreeSet, fmt::LowerHex};

        use ark_ff::{One, Zero};
        use $crate::{BigUint, FieldElement, KnownField, LargeInt};

        use core::fmt::{self, Debug, Formatter};
        use core::hash::Hash;

        use p3_field::{AbstractField, Field, PrimeField32};

        #[derive(
            Debug,
            Copy,
            Clone,
            Default,
            Eq,
            Hash,
            PartialEq,
            Ord,
            PartialOrd,
            Serialize,
            Deserialize,
            derive_more::Display,
        )]
        pub struct $name($p3_type);

        impl $name {
            #[inline(always)]
            fn from_canonical_u32(n: u32) -> Self {
                Self(<$p3_type>::from_canonical_u32(n))
            }

            #[inline]
            fn to_canonical_u32(self) -> u32 {
                self.0.as_canonical_u32()
            }

            pub fn into_inner(self) -> $p3_type {
                self.0
            }

            pub fn from_inner(e: $p3_type) -> Self {
                Self(e)
            }
        }

        impl FieldElement for $name {
            type Integer = BBLargeInt;

            const BITS: u32 = 31;

            fn to_degree(&self) -> $crate::DegreeType {
                self.to_canonical_u32() as u64
            }

            fn to_integer(&self) -> Self::Integer {
                self.to_canonical_u32().into()
            }

            #[inline]
            fn modulus() -> Self::Integer {
                let p: u32 = <$p3_type>::order().try_into().unwrap();
                p.into()
            }

            fn pow(self, exp: Self::Integer) -> Self {
                Self(<$p3_type>::exp_u64_generic(
                    self.0,
                    exp.try_into_u64().unwrap(),
                ))
            }

            fn to_bytes_le(&self) -> Vec<u8> {
                self.to_canonical_u32().to_le_bytes().to_vec()
            }

            fn from_bytes_le(bytes: &[u8]) -> Self {
                let u = u32::from_le_bytes(bytes.try_into().unwrap());
                Self::from_canonical_u32(u)
            }

            fn from_str_radix(s: &str, radix: u32) -> Result<Self, String> {
                u32::from_str_radix(s, radix)
                    .map(Self::from_canonical_u32)
                    .map_err(|e| e.to_string())
            }

            fn checked_from(value: ibig::UBig) -> Option<Self> {
                if value < Self::modulus().to_arbitrary_integer() {
                    Some(u32::try_from(value).unwrap().into())
                } else {
                    None
                }
            }

            fn is_in_lower_half(&self) -> bool {
                let p: u32 = <$p3_type>::order().try_into().unwrap();
                self.to_canonical_u32() <= (p - 1) / 2
            }

            fn known_field() -> Option<$crate::KnownField> {
                Some(KnownField::$name)
            }

            fn has_direct_repr() -> bool {
                // No direct repr, because 'mod' is not always applied.
                false
            }
        }

        impl LowerHex for $name {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                LowerHex::fmt(&self.to_canonical_u32(), f)
            }
        }

        impl From<bool> for $name {
            fn from(b: bool) -> Self {
                Self(<$p3_type>::from_bool(b))
            }
        }

        impl From<i64> for $name {
            fn from(n: i64) -> Self {
                Self::from(if n < 0 {
                    // If n < 0, then this is guaranteed to overflow since
                    // both arguments have their high bit set, so the result
                    // is in the canonical range.
                    Self::modulus()
                        .try_into_u64()
                        .unwrap()
                        .wrapping_add(n as u64)
                } else {
                    n as u64
                })
            }
        }

        impl From<i32> for $name {
            fn from(n: i32) -> Self {
                From::<i64>::from(n as i64)
            }
        }

        impl From<u32> for $name {
            fn from(n: u32) -> Self {
                Self(<$p3_type>::from_wrapped_u32(n))
            }
        }

        impl From<u64> for $name {
            #[inline]
            fn from(n: u64) -> Self {
                Self(<$p3_type>::from_wrapped_u64(n))
            }
        }

        impl From<$crate::BigUint> for $name {
            fn from(n: $crate::BigUint) -> Self {
                u64::try_from(n).unwrap().into()
            }
        }

        impl From<BBLargeInt> for $name {
            #[inline]
            fn from(n: BBLargeInt) -> Self {
                n.0.into()
            }
        }

        impl ConstZero for $name {
            const ZERO: Self = $name(<$p3_type>::new(0));
        }

        impl Zero for $name {
            fn zero() -> Self {
                Self(<$p3_type>::zero())
            }

            fn is_zero(&self) -> bool {
                self.0.is_zero()
            }
        }

        impl ConstOne for $name {
            const ONE: Self = $name(<$p3_type>::new(1));
        }

        impl One for $name {
            fn one() -> Self {
                Self(<$p3_type>::one())
            }

            fn is_one(&self) -> bool {
                self.to_canonical_u32() == 1
            }
        }

        impl FromStr for $name {
            type Err = String;
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                let n = BigUint::from_str(s).map_err(|e| e.to_string())?;
                let modulus = Self::modulus();
                if n >= modulus.to_arbitrary_integer() {
                    Err(format!("Decimal number \"{s}\" too large for field."))
                } else {
                    Ok(n.into())
                }
            }
        }

        impl Neg for $name {
            type Output = Self;

            #[inline]
            fn neg(self) -> Self {
                Self(self.0.neg())
            }
        }

        impl Add for $name {
            type Output = Self;

            #[inline]
            fn add(self, rhs: Self) -> Self {
                Self(self.0.add(rhs.0))
            }
        }

        impl AddAssign for $name {
            #[inline]
            fn add_assign(&mut self, rhs: Self) {
                self.0.add_assign(rhs.0)
            }
        }

        impl Sub for $name {
            type Output = Self;

            #[inline]
            fn sub(self, rhs: Self) -> Self {
                Self(self.0.sub(rhs.0))
            }
        }

        impl SubAssign for $name {
            #[inline]
            fn sub_assign(&mut self, rhs: Self) {
                self.0.sub_assign(rhs.0)
            }
        }

        impl Mul for $name {
            type Output = Self;

            fn mul(self, rhs: Self) -> Self {
                Self(self.0.mul(rhs.0))
            }
        }

        impl MulAssign for $name {
            fn mul_assign(&mut self, rhs: Self) {
                self.0.mul_assign(rhs.0)
            }
        }

        impl Div for $name {
            type Output = Self;

            fn div(self, rhs: Self) -> Self::Output {
                Self(self.0.div(rhs.0))
            }
        }

        impl JsonSchema for $name {
            fn schema_name() -> String {
                "$name".to_string()
            }

            fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> Schema {
                // Since $p3_type is just a wrapper around u32, use the schema for u32
                let u32_schema = gen.subschema_for::<u32>();

                SchemaObject {
                    // Define the schema for $name, where field is of type $p3_type (which is u32)
                    instance_type: Some(schemars::schema::InstanceType::Object.into()),
                    object: Some(Box::new(schemars::schema::ObjectValidation {
                        properties: vec![("field".to_string(), u32_schema)]
                            .into_iter()
                            .collect(),
                        required: BTreeSet::from(["field".to_string()]), // Convert Vec to BTreeSet
                        ..Default::default()
                    })),
                    ..Default::default()
                }
                .into()
            }
        }

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
            derive_more::Display,
            Serialize,
            Deserialize,
            JsonSchema,
            derive_more::Mul,
            derive_more::Add,
            derive_more::Sub,
            derive_more::AddAssign,
            derive_more::SubAssign,
            derive_more::MulAssign,
            derive_more::Shr,
            derive_more::Shl,
            derive_more::BitAnd,
            derive_more::BitOr,
            derive_more::BitXor,
            derive_more::BitAndAssign,
            derive_more::BitOrAssign,
            derive_more::BitXorAssign,
        )]
        pub struct BBLargeInt(u32);

        impl LargeInt for BBLargeInt {
            const MAX: Self = Self(u32::MAX);
            const NUM_BITS: usize = 32;

            fn to_arbitrary_integer(self) -> ibig::UBig {
                self.0.into()
            }

            fn num_bits(&self) -> usize {
                Self::NUM_BITS - self.0.leading_zeros() as usize
            }

            fn one() -> Self {
                Self(1)
            }

            fn is_one(&self) -> bool {
                self.0 == 1
            }

            fn try_into_u64(&self) -> Option<u64> {
                Some(self.0 as u64)
            }

            fn try_into_u32(&self) -> Option<u32> {
                Some(self.0)
            }

            fn from_hex(s: &str) -> Self {
                Self(u32::from_str_radix(s, 16).unwrap())
            }
        }

        impl From<u32> for BBLargeInt {
            fn from(value: u32) -> Self {
                Self(value)
            }
        }

        impl From<u64> for BBLargeInt {
            fn from(value: u64) -> Self {
                Self(value as u32)
            }
        }

        impl Zero for BBLargeInt {
            fn zero() -> Self {
                Self(0)
            }

            fn is_zero(&self) -> bool {
                self.0 == 0
            }
        }

        impl ConstZero for BBLargeInt {
            const ZERO: Self = Self(0);
        }

        impl LowerHex for BBLargeInt {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                LowerHex::fmt(&self.0, f)
            }
        }

        impl Not for BBLargeInt {
            type Output = Self;

            fn not(self) -> Self::Output {
                Self(!self.0)
            }
        }
    };
}
