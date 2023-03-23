// TODO this should probably rather be a finite field element.
use crate::number::{format_number, is_zero, AbstractNumberType, get_field_mod};

use super::util::WitnessColumnNamer;

/// An expression affine in the committed polynomials.
#[derive(Debug, Clone)]
pub struct AffineExpression {
    pub coefficients: Vec<AbstractNumberType>,
    pub offset: AbstractNumberType,
}

impl From<AbstractNumberType> for AffineExpression {
    fn from(value: AbstractNumberType) -> Self {
        AffineExpression {
            coefficients: Vec::new(),
            offset: clamp(value),
        }
    }
}

impl From<u32> for AffineExpression {
    fn from(value: u32) -> Self {
        AffineExpression {
            coefficients: Vec::new(),
            offset: clamp(value.into()),
        }
    }
}

impl AffineExpression {
    pub fn from_wittness_poly_value(poly_id: usize) -> AffineExpression {
        AffineExpression {
            coefficients: [vec![0.into(); poly_id], vec![1.into()]].concat(),
            offset: 0.into(),
        }
    }

    pub fn is_constant(&self) -> bool {
        self.nonzero_coefficients().next().is_none()
    }

    pub fn constant_value(&self) -> Option<AbstractNumberType> {
        if self.is_constant() {
            Some(self.offset.clone())
        } else {
            None
        }
    }

    pub fn nonzero_variables(&self) -> Vec<usize> {
        self.nonzero_coefficients().map(|(i, _)| i).collect()
    }

    /// @returns an iterator of the nonzero coefficients and their variable IDs (but not the offset).
    pub fn nonzero_coefficients(&self) -> impl Iterator<Item = (usize, &AbstractNumberType)> {
        self.coefficients
            .iter()
            .enumerate()
            .filter(|(_, c)| !is_zero(c))
    }

    pub fn mul(mut self, factor: AbstractNumberType) -> AffineExpression {
        let fac = clamp(factor);
        for f in &mut self.coefficients {
            *f = clamp(f.clone() * fac.clone());
        }
        self.offset = clamp(self.offset.clone() * fac);
        self
    }

    /// If the affine expression has only a single variable (with nonzero coefficient),
    /// returns the index of the variable and the assignment that evaluates the
    /// affine expression to zero.
    pub fn solve(&self) -> Option<(usize, AbstractNumberType)> {
        let mut nonzero = self.nonzero_coefficients();
        nonzero.next().and_then(|(i, c)| {
            if nonzero.next().is_none() {
                // c * a + o = 0 <=> a = -o/c
                if *c == 1.into() {
                    Some((i, clamp(-self.offset.clone())))
                } else if *c == (-1).into() || *c == (get_field_mod() - 1u64).into() {
                    Some((i, self.offset.clone()))
                } else {
                    Some((
                        i,
                        clamp(-clamp(
                            self.offset.clone() * inv(c.clone(), (get_field_mod()).into()),
                        )),
                    ))
                }
            } else {
                None
            }
        })
    }

    /// Returns true if it can be determined that this expression can never be zero.
    pub fn is_invalid(&self) -> bool {
        self.constant_value().map(|v| v != 0.into()) == Some(true)
    }

    pub fn format(&self, namer: &impl WitnessColumnNamer) -> String {
        self.nonzero_coefficients()
            .map(|(i, c)| {
                let name = namer.name(i);
                if *c == 1.into() {
                    name
                } else if *c == (-1).into() {
                    format!("-{name}")
                } else {
                    format!("{} * {name}", format_number(c))
                }
            })
            .chain(self.constant_value().map(|v| format!("{v}")))
            .collect::<Vec<_>>()
            .join(" + ")
    }
}

fn clamp(mut x: AbstractNumberType) -> AbstractNumberType {
    while x < 0.into() {
        x += get_field_mod().clone()
    }
    x % get_field_mod().clone()
}

fn pow(
    mut x: AbstractNumberType,
    mut y: AbstractNumberType,
    m: AbstractNumberType,
) -> AbstractNumberType {
    assert!(y >= 0.into());
    if y == 0.into() {
        return 1.into();
    }
    let mut r: AbstractNumberType = 1.into();
    while y >= 2.into() {
        if y.bit(0) {
            r = (r * x.clone()) % m.clone();
        }
        x = (x.clone() * x) % m.clone();
        y = y.clone() >> 1;
    }
    (r * x) % m
}

fn inv(x: AbstractNumberType, m: AbstractNumberType) -> AbstractNumberType {
    pow(x, m.clone() - 2, m)
}

impl PartialEq for AffineExpression {
    fn eq(&self, other: &Self) -> bool {
        self.offset == other.offset && self.nonzero_coefficients().eq(other.nonzero_coefficients())
    }
}

impl std::ops::Add for AffineExpression {
    type Output = AffineExpression;

    fn add(self, rhs: Self) -> Self::Output {
        let mut coefficients = rhs.coefficients;
        if self.coefficients.len() > coefficients.len() {
            coefficients.resize(self.coefficients.len(), 0.into());
        }
        for (i, v) in self.coefficients.iter().enumerate() {
            coefficients[i] = clamp(coefficients[i].clone() + v);
        }
        AffineExpression {
            coefficients,
            offset: clamp(self.offset + rhs.offset),
        }
    }
}

impl std::ops::Neg for AffineExpression {
    type Output = AffineExpression;

    fn neg(mut self) -> Self::Output {
        self.coefficients
            .iter_mut()
            .for_each(|v| *v = clamp(-v.clone()));
        self.offset = clamp(-self.offset);
        self
    }
}

impl std::ops::Sub for AffineExpression {
    type Output = AffineExpression;

    fn sub(self, rhs: Self) -> Self::Output {
        self + -rhs
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::number::AbstractNumberType;

    use super::{AffineExpression, get_field_mod};

    fn convert(input: Vec<i32>) -> Vec<AbstractNumberType> {
        input.into_iter().map(|x| x.into()).collect()
    }

    #[test]
    pub fn test_affine_neg() {
        let a = AffineExpression {
            coefficients: convert(vec![1, 0, 2]),
            offset: 9.into(),
        };
        assert_eq!(
            -a,
            AffineExpression {
                coefficients: vec![
                    (get_field_mod() - 1u64).into(),
                    0.into(),
                    (get_field_mod() - 2u64).into()
                ],
                offset: (get_field_mod().clone() - 9u64).into(),
            },
        );
    }

    #[test]
    pub fn test_affine_add() {
        let a = AffineExpression {
            coefficients: convert(vec![1, 2]),
            offset: 3.into(),
        };
        let b = AffineExpression {
            coefficients: convert(vec![11]),
            offset: 13.into(),
        };
        assert_eq!(
            a.clone() + b.clone(),
            AffineExpression {
                coefficients: convert(vec![12, 2]),
                offset: 16.into(),
            },
        );
        assert_eq!(b.clone() + a.clone(), a + b,);
    }

    #[test]
    pub fn mod_arith() {
        assert_eq!(
            pow(7.into(), 0.into(), (get_field_mod()).into()),
            1.into()
        );
        assert_eq!(
            pow(7.into(), 1.into(), (get_field_mod()).into()),
            7.into()
        );
        assert_eq!(
            pow(7.into(), 2.into(), (get_field_mod()).into()),
            (7 * 7).into()
        );
        assert_eq!(inv(1.into(), (get_field_mod()).into()), 1.into());
        let inverse_of_four = 13835058052060938241u64;
        assert_eq!(inv(4.into(), (get_field_mod()).into()), inverse_of_four.into());

        assert_eq!(
            (4u128 * inverse_of_four as u128) % (get_field_mod().iter_u64_digits().next().unwrap() as u128),
            1
        );
    }
}
