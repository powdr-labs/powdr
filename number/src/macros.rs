macro_rules! powdr_field {
    ($name:ident, $ark_type:ty) => {
        use crate::{traits::FieldElementTrait, AbstractNumberType, DegreeType};
        use ark_ff::{BigInteger, Field, PrimeField};
        use std::fmt;
        use std::ops::*;

        #[derive(Clone, Copy, PartialEq, Eq, Debug, Default, PartialOrd, Ord, Hash)]
        pub struct $name {
            value: $ark_type,
        }

        impl FieldElementTrait for $name {
            fn to_degree(&self) -> DegreeType {
                let degree = self.to_integer();
                let mut limbs = degree.iter_u64_digits();
                let res = (&mut limbs).next().unwrap_or_default();
                assert!(limbs.next().is_none());
                res
            }

            fn to_integer(&self) -> AbstractNumberType {
                self.value.into_bigint().into()
            }

            fn modulus() -> AbstractNumberType {
                <$ark_type>::MODULUS.into()
            }

            fn pow(self, exponent: AbstractNumberType) -> Self {
                Self {
                    value: self.value.pow([exponent.try_into().unwrap()]),
                }
            }

            fn integer_div(self, other: Self) -> Self {
                (self.to_integer() / other.to_integer()).into()
            }

            fn to_bytes_le(&self) -> Vec<u8> {
                self.value.into_bigint().to_bytes_le()
            }
        }

        impl num_traits::Zero for $name {
            fn zero() -> Self {
                Self::from(0)
            }

            fn is_zero(&self) -> bool {
                self.value.is_zero()
            }
        }

        impl<V: Into<$ark_type>> From<V> for $name {
            fn from(value: V) -> Self {
                Self {
                    value: value.into(),
                }
            }
        }

        // Add

        impl std::ops::Add for $name {
            type Output = $name;

            fn add(self, rhs: Self) -> Self::Output {
                $name {
                    value: self.value + rhs.value,
                }
            }
        }

        impl<'a> std::ops::Add<&'a $name> for $name {
            type Output = $name;

            fn add(self, rhs: &'a $name) -> Self::Output {
                $name {
                    value: self.value + rhs.value,
                }
            }
        }

        impl<'a> std::ops::Add for &'a $name {
            type Output = $name;

            fn add(self, rhs: Self) -> Self::Output {
                $name {
                    value: self.value + rhs.value,
                }
            }
        }

        impl<'a> std::ops::Add<$name> for &'a $name {
            type Output = $name;

            fn add(self, rhs: $name) -> Self::Output {
                $name {
                    value: self.value + rhs.value,
                }
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
                $name {
                    value: self.value - rhs.value,
                }
            }
        }

        impl<'a> std::ops::Sub<&'a $name> for $name {
            type Output = $name;

            fn sub(self, rhs: &'a $name) -> Self::Output {
                $name {
                    value: self.value - rhs.value,
                }
            }
        }

        impl<'a> std::ops::Sub for &'a $name {
            type Output = $name;

            fn sub(self, rhs: Self) -> Self::Output {
                $name {
                    value: self.value - rhs.value,
                }
            }
        }

        impl<'a> std::ops::Sub<$name> for &'a $name {
            type Output = $name;

            fn sub(self, rhs: $name) -> Self::Output {
                $name {
                    value: self.value - rhs.value,
                }
            }
        }

        // Mul

        impl std::ops::Mul for $name {
            type Output = $name;

            fn mul(self, rhs: Self) -> Self::Output {
                $name {
                    value: self.value * rhs.value,
                }
            }
        }

        impl<'a> std::ops::Mul<&'a $name> for $name {
            type Output = $name;

            fn mul(self, rhs: &'a $name) -> Self::Output {
                $name {
                    value: self.value * rhs.value,
                }
            }
        }

        impl<'a> std::ops::Mul for &'a $name {
            type Output = $name;

            fn mul(self, rhs: Self) -> Self::Output {
                $name {
                    value: self.value * rhs.value,
                }
            }
        }

        impl<'a> std::ops::Mul<$name> for &'a $name {
            type Output = $name;

            fn mul(self, rhs: $name) -> Self::Output {
                $name {
                    value: self.value * rhs.value,
                }
            }
        }

        // Div

        impl std::ops::Div for $name {
            type Output = $name;

            fn div(self, rhs: Self) -> Self::Output {
                $name {
                    value: self.value / rhs.value,
                }
            }
        }

        impl<'a> std::ops::Div<&'a $name> for $name {
            type Output = $name;

            fn div(self, rhs: &'a $name) -> Self::Output {
                $name {
                    value: self.value / rhs.value,
                }
            }
        }

        impl<'a> std::ops::Div for &'a $name {
            type Output = $name;

            fn div(self, rhs: Self) -> Self::Output {
                $name {
                    value: self.value / rhs.value,
                }
            }
        }

        impl<'a> std::ops::Div<$name> for &'a $name {
            type Output = $name;

            fn div(self, rhs: $name) -> Self::Output {
                $name {
                    value: self.value / rhs.value,
                }
            }
        }

        impl std::ops::Neg for $name {
            type Output = $name;

            fn neg(self) -> Self::Output {
                Self { value: -self.value }
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let value = self.to_integer();
                if value > (Self::modulus() - 1usize) / 2usize {
                    write!(f, "-{}", Self::modulus() - value)
                } else {
                    write!(f, "{value}")
                }
            }
        }
    };
}
