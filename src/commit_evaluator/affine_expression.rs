use crate::analyzer::ConstantNumberType;

/// An expression affine in the committed polynomials.
#[derive(Debug, Clone, PartialEq)]
pub struct AffineExpression {
    coefficients: Vec<ConstantNumberType>,
    offset: ConstantNumberType,
}

impl From<ConstantNumberType> for AffineExpression {
    fn from(value: ConstantNumberType) -> Self {
        AffineExpression {
            coefficients: Vec::new(),
            offset: value,
        }
    }
}

impl AffineExpression {
    pub fn from_committed_poly_value(poly_id: usize) -> AffineExpression {
        AffineExpression {
            coefficients: [vec![0; poly_id], vec![1]].concat(),
            offset: 0,
        }
    }

    pub fn is_constant(&self) -> bool {
        self.coefficients.iter().all(|v| *v == 0)
    }

    pub fn constant_value(&self) -> Option<ConstantNumberType> {
        if self.is_constant() {
            Some(self.offset)
        } else {
            None
        }
    }

    pub fn mul(mut self, factor: ConstantNumberType) -> AffineExpression {
        for f in &mut self.coefficients {
            *f *= factor;
        }
        self.offset *= factor;
        self
    }

    /// If the affine expression has only a single variable (with nonzero coefficient),
    /// returns the index of the variable and the assignment that evaluates the
    /// affine expression to zero.
    pub fn solve(&self) -> Option<(usize, ConstantNumberType)> {
        let mut nonzero = self
            .coefficients
            .iter()
            .enumerate()
            .filter(|(_, &c)| c != 0);
        nonzero.next().and_then(|(i, c)| {
            if nonzero.next().is_none() {
                // c * a + o = 0 <=> a = -o/c
                Some((i, -self.offset / c))
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
            coefficients.resize(self.coefficients.len(), 0);
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
        self.coefficients.iter_mut().for_each(|v| *v = v.neg());
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
    use super::AffineExpression;

    #[test]
    pub fn test_affine_neg() {
        let a = AffineExpression {
            coefficients: vec![1, 0, 2],
            offset: 9,
        };
        assert_eq!(
            -a,
            AffineExpression {
                coefficients: vec![-1, -0, -2],
                offset: -9,
            },
        );
    }

    #[test]
    pub fn test_affine_add() {
        let a = AffineExpression {
            coefficients: vec![1, 2],
            offset: 3,
        };
        let b = AffineExpression {
            coefficients: vec![11],
            offset: 13,
        };
        assert_eq!(
            a.clone() + b.clone(),
            AffineExpression {
                coefficients: vec![12, 2],
                offset: 16,
            },
        );
        assert_eq!(b.clone() + a.clone(), a + b,);
    }
}
