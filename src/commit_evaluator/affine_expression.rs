// TODO this should probably rather be a finite field element.
use crate::number::{is_zero, AbstractNumberType};

const GOLDILOCKS_MOD: u64 = 0xffffffff00000001u64;

/// An expression affine in the committed polynomials.
#[derive(Debug, Clone, PartialEq)]
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
    pub fn from_committed_poly_value(poly_id: usize) -> AffineExpression {
        AffineExpression {
            coefficients: [vec![0.into(); poly_id], vec![1.into()]].concat(),
            offset: 0.into(),
        }
    }

    pub fn is_constant(&self) -> bool {
        self.coefficients.iter().all(is_zero)
    }

    pub fn constant_value(&self) -> Option<AbstractNumberType> {
        if self.is_constant() {
            Some(self.offset.clone())
        } else {
            None
        }
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
        let mut nonzero = self
            .coefficients
            .iter()
            .enumerate()
            .filter(|(_, c)| !is_zero(c));
        nonzero.next().and_then(|(i, c)| {
            if nonzero.next().is_none() {
                // c * a + o = 0 <=> a = -o/c
                if *c == 1.into() {
                    Some((i, clamp(-self.offset.clone())))
                } else if *c == (-1).into() || *c == (GOLDILOCKS_MOD - 1).into() {
                    Some((i, self.offset.clone()))
                } else {
                    Some((
                        i,
                        clamp(-clamp(
                            self.offset.clone() * inv(c.clone(), GOLDILOCKS_MOD.into()),
                        )),
                    ))
                }
            } else {
                None
            }
        })
    }
}

fn clamp(mut x: AbstractNumberType) -> AbstractNumberType {
    while x < 0.into() {
        x += GOLDILOCKS_MOD
    }
    x % GOLDILOCKS_MOD
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

    use super::{AffineExpression, GOLDILOCKS_MOD};

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
                    (GOLDILOCKS_MOD - 1).into(),
                    0.into(),
                    (GOLDILOCKS_MOD - 2).into()
                ],
                offset: (GOLDILOCKS_MOD - 9).into(),
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
        assert_eq!(pow(7.into(), 0.into(), GOLDILOCKS_MOD.into()), 1.into());
        assert_eq!(pow(7.into(), 1.into(), GOLDILOCKS_MOD.into()), 7.into());
        assert_eq!(
            pow(7.into(), 2.into(), GOLDILOCKS_MOD.into()),
            (7 * 7).into()
        );
        assert_eq!(inv(1.into(), GOLDILOCKS_MOD.into()), 1.into());
        let inverse_of_four = 13835058052060938241u64;
        assert_eq!(inv(4.into(), GOLDILOCKS_MOD.into()), inverse_of_four.into());
        assert_eq!(
            (4u128 * inverse_of_four as u128) % GOLDILOCKS_MOD as u128,
            1
        );
    }
}
