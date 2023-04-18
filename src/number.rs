use std::{fmt, ops::AddAssign};

use ark_ff::{
    fields::{Field, Fp64, MontBackend, MontConfig},
    BigInteger, PrimeField, Zero,
};

#[derive(MontConfig)]
#[modulus = "18446744069414584321"]
#[generator = "7"]
pub struct GoldilocksBaseFieldConfig;
pub type GoldilocksBaseField = Fp64<MontBackend<GoldilocksBaseFieldConfig, 1>>;

/// The abstract type of numbers to be computed with.
/// TODO: use arbitrary precision
pub type AbstractNumberType = u128;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default, PartialOrd, Ord, Hash)]
pub struct FieldElement {
    value: GoldilocksBaseField,
}
/// The type of polynomial degrees and indices into columns.
pub type DegreeType = u64;

impl FieldElement {
    pub fn to_degree(&self) -> DegreeType {
        self.to_integer() as DegreeType
    }

    pub fn to_integer(&self) -> AbstractNumberType {
        let value_big = self.value.into_bigint();
        assert_eq!(value_big.0.len(), 1);
        value_big.0[0] as AbstractNumberType
    }

    pub fn modulus() -> AbstractNumberType {
        GoldilocksBaseField::MODULUS.0[0] as AbstractNumberType
    }

    pub fn zero() -> Self {
        Self::from(0)
    }

    pub fn is_zero(&self) -> bool {
        self.value.is_zero()
    }

    pub fn pow(self, exponent: AbstractNumberType) -> Self {
        Self {
            value: self.value.pow([exponent as u64]),
        }
    }

    pub fn integer_div(self, other: Self) -> Self {
        (self.to_integer() / other.to_integer()).into()
    }

    pub fn to_bytes_le(&self) -> Vec<u8> {
        self.value.into_bigint().to_bytes_le()
    }
}

impl<V: Into<GoldilocksBaseField>> From<V> for FieldElement {
    fn from(value: V) -> Self {
        Self {
            value: value.into(),
        }
    }
}

// Add

impl std::ops::Add for FieldElement {
    type Output = FieldElement;

    fn add(self, rhs: Self) -> Self::Output {
        FieldElement {
            value: self.value + rhs.value,
        }
    }
}

impl<'a> std::ops::Add<&'a FieldElement> for FieldElement {
    type Output = FieldElement;

    fn add(self, rhs: &'a FieldElement) -> Self::Output {
        FieldElement {
            value: self.value + rhs.value,
        }
    }
}

impl<'a> std::ops::Add for &'a FieldElement {
    type Output = FieldElement;

    fn add(self, rhs: Self) -> Self::Output {
        FieldElement {
            value: self.value + rhs.value,
        }
    }
}

impl<'a> std::ops::Add<FieldElement> for &'a FieldElement {
    type Output = FieldElement;

    fn add(self, rhs: FieldElement) -> Self::Output {
        FieldElement {
            value: self.value + rhs.value,
        }
    }
}

impl AddAssign for FieldElement {
    fn add_assign(&mut self, rhs: Self) {
        self.value.add_assign(rhs.value);
    }
}

// Sub

impl std::ops::Sub for FieldElement {
    type Output = FieldElement;

    fn sub(self, rhs: Self) -> Self::Output {
        FieldElement {
            value: self.value - rhs.value,
        }
    }
}

impl<'a> std::ops::Sub<&'a FieldElement> for FieldElement {
    type Output = FieldElement;

    fn sub(self, rhs: &'a FieldElement) -> Self::Output {
        FieldElement {
            value: self.value - rhs.value,
        }
    }
}

impl<'a> std::ops::Sub for &'a FieldElement {
    type Output = FieldElement;

    fn sub(self, rhs: Self) -> Self::Output {
        FieldElement {
            value: self.value - rhs.value,
        }
    }
}

impl<'a> std::ops::Sub<FieldElement> for &'a FieldElement {
    type Output = FieldElement;

    fn sub(self, rhs: FieldElement) -> Self::Output {
        FieldElement {
            value: self.value - rhs.value,
        }
    }
}

// Mul

impl std::ops::Mul for FieldElement {
    type Output = FieldElement;

    fn mul(self, rhs: Self) -> Self::Output {
        FieldElement {
            value: self.value * rhs.value,
        }
    }
}

impl<'a> std::ops::Mul<&'a FieldElement> for FieldElement {
    type Output = FieldElement;

    fn mul(self, rhs: &'a FieldElement) -> Self::Output {
        FieldElement {
            value: self.value * rhs.value,
        }
    }
}

impl<'a> std::ops::Mul for &'a FieldElement {
    type Output = FieldElement;

    fn mul(self, rhs: Self) -> Self::Output {
        FieldElement {
            value: self.value * rhs.value,
        }
    }
}

impl<'a> std::ops::Mul<FieldElement> for &'a FieldElement {
    type Output = FieldElement;

    fn mul(self, rhs: FieldElement) -> Self::Output {
        FieldElement {
            value: self.value * rhs.value,
        }
    }
}

// Div

impl std::ops::Div for FieldElement {
    type Output = FieldElement;

    fn div(self, rhs: Self) -> Self::Output {
        FieldElement {
            value: self.value / rhs.value,
        }
    }
}

impl<'a> std::ops::Div<&'a FieldElement> for FieldElement {
    type Output = FieldElement;

    fn div(self, rhs: &'a FieldElement) -> Self::Output {
        FieldElement {
            value: self.value / rhs.value,
        }
    }
}

impl<'a> std::ops::Div for &'a FieldElement {
    type Output = FieldElement;

    fn div(self, rhs: Self) -> Self::Output {
        FieldElement {
            value: self.value / rhs.value,
        }
    }
}

impl<'a> std::ops::Div<FieldElement> for &'a FieldElement {
    type Output = FieldElement;

    fn div(self, rhs: FieldElement) -> Self::Output {
        FieldElement {
            value: self.value / rhs.value,
        }
    }
}

impl std::ops::Neg for FieldElement {
    type Output = FieldElement;

    fn neg(self) -> Self::Output {
        Self { value: -self.value }
    }
}

impl fmt::Display for FieldElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = self.to_integer();
        if value > (Self::modulus() - 1) / 2 {
            write!(f, "-{}", Self::modulus() - value)
        } else {
            write!(f, "{value}")
        }
    }
}
