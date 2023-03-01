// TODO this should probably rather be a finite field element.
use crate::number::{is_zero, AbstractNumberType};

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
            offset: value,
        }
    }
}

impl From<u32> for AffineExpression {
    fn from(value: u32) -> Self {
        AffineExpression {
            coefficients: Vec::new(),
            offset: value.into(),
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
        for f in &mut self.coefficients {
            *f *= factor.clone();
        }
        self.offset *= factor;
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
                Some((i, -self.offset.clone() / c))
            } else {
                None
            }
        })
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
            coefficients[i] += v;
        }
        AffineExpression {
            coefficients,
            offset: self.offset + rhs.offset,
        }
    }
}

impl std::ops::Neg for AffineExpression {
    type Output = AffineExpression;

    fn neg(mut self) -> Self::Output {
        self.coefficients
            .iter_mut()
            .for_each(|v| *v = v.clone().neg());
        self.offset = -self.offset;
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
    use crate::number::AbstractNumberType;

    use super::AffineExpression;

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
                coefficients: convert(vec![-1, -0, -2]),
                offset: (-9).into(),
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
}
