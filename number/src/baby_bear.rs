use p3_baby_bear::BabyBear;
use schemars::{
    schema::{Schema, SchemaObject},
    JsonSchema,
};
use serde::{Deserialize, Serialize};

use num_traits::{ConstOne, ConstZero};
use std::ops::{Add, AddAssign, Div, Mul, MulAssign, Neg, Not, Sub, SubAssign};
use std::str::FromStr;
use std::{collections::BTreeSet, fmt::LowerHex};

use crate::{BigUint, FieldElement, KnownField, LargeInt};
use ark_ff::{One, Zero};

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
pub struct BabyBearField(BabyBear);

const P: u32 = 0x78000001;

impl BabyBearField {
    const ORDER: u32 = P;

    #[inline(always)]
    fn from_canonical_u32(n: u32) -> Self {
        Self(BabyBear::from_canonical_u32(n))
    }

    #[inline]
    fn to_canonical_u32(self) -> u32 {
        self.0.as_canonical_u32()
    }
}

impl FieldElement for BabyBearField {
    type Integer = BBLargeInt;

    const BITS: u32 = 31;

    fn to_degree(&self) -> crate::DegreeType {
        self.to_canonical_u32() as u64
    }

    fn to_integer(&self) -> Self::Integer {
        self.to_canonical_u32().into()
    }

    #[inline]
    fn modulus() -> Self::Integer {
        Self::ORDER.into()
    }

    fn pow(self, exp: Self::Integer) -> Self {
        Self(BabyBear::exp_u64_generic(
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
        self.to_canonical_u32() <= (Self::ORDER - 1) / 2
    }

    fn known_field() -> Option<crate::KnownField> {
        Some(KnownField::BabyBearField)
    }

    fn try_into_i32(&self) -> Option<i32> {
        Some(self.to_canonical_u32() as i32)
    }
}

impl LowerHex for BabyBearField {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        LowerHex::fmt(&self.to_canonical_u32(), f)
    }
}

impl From<bool> for BabyBearField {
    fn from(b: bool) -> Self {
        Self(BabyBear::from_bool(b))
    }
}

impl From<i64> for BabyBearField {
    fn from(n: i64) -> Self {
        From::<u64>::from(n as u64)
    }
}

impl From<i32> for BabyBearField {
    fn from(n: i32) -> Self {
        From::<i64>::from(n as i64)
    }
}

impl From<u32> for BabyBearField {
    fn from(n: u32) -> Self {
        Self(BabyBear::from_wrapped_u32(n))
    }
}

impl From<u64> for BabyBearField {
    #[inline]
    fn from(n: u64) -> Self {
        Self(BabyBear::from_wrapped_u64(n))
    }
}

impl From<crate::BigUint> for BabyBearField {
    fn from(n: crate::BigUint) -> Self {
        u64::try_from(n).unwrap().into()
    }
}

impl From<BBLargeInt> for BabyBearField {
    #[inline]
    fn from(n: BBLargeInt) -> Self {
        n.0.into()
    }
}

impl ConstZero for BabyBearField {
    const ZERO: Self = BabyBearField(BabyBear::new(0));
}

impl Zero for BabyBearField {
    fn zero() -> Self {
        Self(BabyBear::zero())
    }

    fn is_zero(&self) -> bool {
        self.0.is_zero()
    }
}

impl ConstOne for BabyBearField {
    const ONE: Self = BabyBearField(BabyBear::new(1));
}

impl One for BabyBearField {
    fn one() -> Self {
        Self(BabyBear::one())
    }

    fn is_one(&self) -> bool {
        self.to_canonical_u32() == 1
    }
}

impl FromStr for BabyBearField {
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

impl Neg for BabyBearField {
    type Output = Self;

    #[inline]
    fn neg(self) -> Self {
        Self(self.0.neg())
    }
}

impl Add for BabyBearField {
    type Output = Self;

    #[inline]
    fn add(self, rhs: Self) -> Self {
        Self(self.0.add(rhs.0))
    }
}

impl AddAssign for BabyBearField {
    #[inline]
    fn add_assign(&mut self, rhs: Self) {
        self.0.add_assign(rhs.0)
    }
}

impl Sub for BabyBearField {
    type Output = Self;

    #[inline]
    fn sub(self, rhs: Self) -> Self {
        Self(self.0.sub(rhs.0))
    }
}

impl SubAssign for BabyBearField {
    #[inline]
    fn sub_assign(&mut self, rhs: Self) {
        self.0.sub_assign(rhs.0)
    }
}

impl Mul for BabyBearField {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self {
        Self(self.0.mul(rhs.0))
    }
}

impl MulAssign for BabyBearField {
    fn mul_assign(&mut self, rhs: Self) {
        self.0.mul_assign(rhs.0)
    }
}

impl Div for BabyBearField {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self(self.0.div(rhs.0))
    }
}

impl JsonSchema for BabyBearField {
    fn schema_name() -> String {
        "BabyBearField".to_string()
    }

    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> Schema {
        // Since BabyBear is just a wrapper around u32, use the schema for u32
        let u32_schema = gen.subschema_for::<u32>();

        SchemaObject {
            // Define the schema for BabyBearField, where field is of type BabyBear (which is u32)
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

#[cfg(test)]
mod test {
    use crate::traits::int_from_hex_str;
    use test_log::test;

    use super::*;

    #[test]
    fn bitwise() {
        let n = int_from_hex_str::<BabyBearField>("00ff00ff");
        let p = int_from_hex_str::<BabyBearField>("f00ff00f");
        let not_n = int_from_hex_str::<BabyBearField>("ff00ff00");
        let n_shr_4 = int_from_hex_str::<BabyBearField>("000ff00f");
        let n_shl_4 = int_from_hex_str::<BabyBearField>("0ff00ff0");
        let n_or_p = int_from_hex_str::<BabyBearField>("f0fff0ff");
        let n_and_p = int_from_hex_str::<BabyBearField>("000f000f");
        let n_xor_p = int_from_hex_str::<BabyBearField>("f0f0f0f0");

        assert_eq!(n.not().not(), n);
        assert_eq!(n.not(), not_n);
        assert_eq!(n >> 4, n_shr_4);
        assert_eq!(n << 4, n_shl_4);
        assert_eq!(n & p, n_and_p);
        assert_eq!(n | p, n_or_p);
        assert_eq!(n ^ p, n_xor_p);
    }

    #[test]
    fn zero_one() {
        let x = BabyBearField::ZERO;
        assert_eq!(x, BabyBearField::zero());
        assert_eq!(x.to_canonical_u32(), 0);
        let y = BabyBearField::ONE;
        assert_eq!(y, BabyBearField::one());
        assert_eq!(y.to_canonical_u32(), 1);
        let z = x + y + y;
        assert_eq!(z.to_canonical_u32(), 2);
    }

    #[test]
    fn lower_half() {
        let x = BabyBearField::from(0);
        assert!(x.is_in_lower_half());
        assert!(!(x - 1.into()).is_in_lower_half());

        let y = BabyBearField::from_str_radix("3c000000", 16).unwrap();
        assert!(y.is_in_lower_half());
        assert!(!(y + 1.into()).is_in_lower_half());
    }

    #[test]
    #[should_panic]
    fn integer_div_by_zero() {
        let _ = BabyBearField::from(1).to_arbitrary_integer()
            / BabyBearField::from(0).to_arbitrary_integer();
    }

    #[test]
    #[should_panic]
    fn div_by_zero() {
        let _ = BabyBearField::from(1) / BabyBearField::from(0);
    }
}
