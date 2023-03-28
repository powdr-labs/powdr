use std::{collections::HashSet, ops::Not};

// TODO this should probably rather be a finite field element.
use crate::number::{format_number, is_zero, AbstractNumberType, GOLDILOCKS_MOD};

use super::bit_constraints::{BitConstraint, BitConstraintSet};
use super::eval_error::EvalError::ConflictingBitConstraints;
use super::util::WitnessColumnNamer;
use super::Constraint;
use super::EvalResult;

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
    pub fn from_witness_poly_value(poly_id: usize) -> AffineExpression {
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
    pub fn solve(&self) -> EvalResult {
        let mut nonzero = self.nonzero_coefficients();
        nonzero
            .next()
            .and_then(|(i, c)| {
                if nonzero.next().is_none() {
                    // c * a + o = 0 <=> a = -o/c
                    Some(vec![(
                        i,
                        Constraint::Assignment(if *c == 1.into() {
                            clamp(-self.offset.clone())
                        } else if *c == (-1).into() || *c == (GOLDILOCKS_MOD - 1).into() {
                            self.offset.clone()
                        } else {
                            clamp(-clamp(
                                self.offset.clone() * inv(c.clone(), GOLDILOCKS_MOD.into()),
                            ))
                        }),
                    )])
                } else {
                    None
                }
            })
            .ok_or_else(|| "Cannot solve affine expression.".to_string().into())
    }

    /// Tries to solve "self = 0", or at least propagate a bit constraint:
    /// If we know that some components can only have certain bits set and the offset is zero,
    /// this property might transfer to another component.
    /// Furthermore, if we know that all components are bit-constrained and do not overlap,
    /// we can deduce the values of all components from the offset part.
    pub fn solve_with_bit_constraints(
        &self,
        known_constraints: &impl BitConstraintSet,
    ) -> EvalResult {
        // Try to solve directly.
        if let Ok(result) = self.solve() {
            return Ok(result);
        }
        let new_constraints: Option<_> = if self
            .nonzero_coefficients()
            .all(|(i, _coeff)| known_constraints.bit_constraint(i).is_some())
        {
            // We might be able to solve for one or more variables, if all
            // bit constraints are disjoint.

            // Try positive and negative. We might also experiment with other strategies.

            let result = self
                .try_solve_through_constraints(known_constraints)
                .and_then(|new_constraints| {
                    if new_constraints.is_empty() {
                        (-self.clone()).try_solve_through_constraints(known_constraints)
                    } else {
                        Ok(new_constraints)
                    }
                })?;
            if result.is_empty() {
                None
            } else {
                Some(result)
            }
        } else if self.offset == 0.into() {
            // We might be able to deduce bit constraints on one variable.
            self.try_transfer_constraints(known_constraints)
        } else {
            None
        };
        new_constraints.ok_or_else(|| {
            "Unable to solve or determine constraints."
                .to_string()
                .into()
        })
    }

    fn try_transfer_constraints(
        &self,
        known_constraints: &impl BitConstraintSet,
    ) -> Option<Vec<(usize, Constraint)>> {
        // We need the form X = a * Y + b * Z + ...
        // where X is unconstrained and all others are bit-constrained.
        let mut unconstrained = self
            .nonzero_coefficients()
            .filter(|(i, _c)| known_constraints.bit_constraint(*i).is_none());
        let solve_for = unconstrained.next()?;
        if unconstrained.next().is_some() {
            return None;
        }
        if *solve_for.1 == 1.into() {
            return (-self.clone()).try_transfer_constraints(known_constraints);
        } else if *solve_for.1 != clamp((-1).into()) {
            // We could try to divide by this in the future.
            return None;
        }

        // We can assume that nonzero coefficients is not empty, otherwise we could have solved
        // the affine expression directly.
        let parts = self
            .nonzero_coefficients()
            .filter(|(i, _)| *i != solve_for.0)
            .map(|(i, coeff)| {
                known_constraints
                    .bit_constraint(i)
                    .and_then(|con| con.multiple(coeff.clone()))
            });

        parts
            .reduce(|c1, c2| match (c1, c2) {
                (Some(c1), Some(c2)) => c1.try_combine(&c2),
                _ => None,
            })
            .flatten()
            .map(|con| vec![(solve_for.0, Constraint::BitConstraint(con))])
    }

    /// Tries to assign values to all variables through their bit constraints.
    /// This can also determine if the equation is not satisfiable,
    /// if the bit-constraints do not cover all the bits of the offset.
    /// Returns an empty vector if it is not able to solve the equation.
    fn try_solve_through_constraints(
        &self,
        known_constraints: &impl BitConstraintSet,
    ) -> EvalResult {
        let parts = self
            .nonzero_coefficients()
            .map(|(i, coeff)| {
                (
                    i,
                    known_constraints
                        .bit_constraint(i)
                        .unwrap()
                        .multiple(coeff.clone()),
                )
            })
            .collect::<Vec<_>>();
        if parts.iter().any(|(_i, con)| con.is_none()) {
            return Ok(vec![]);
        }
        // Check if they are mutually exclusive and compute assignments.
        let mut covered_bits = HashSet::<u64>::new();
        let mut assignments = vec![];
        let mut offset = clamp(-self.offset.clone());
        for (i, con) in parts {
            let con = con.clone().unwrap();
            let BitConstraint { min_bit, max_bit } = con;
            for bit in min_bit..=max_bit {
                if !covered_bits.insert(bit) {
                    return Ok(vec![]);
                }
            }
            let mask: AbstractNumberType = con.mask();
            assignments.push((
                i,
                Constraint::Assignment((offset.clone() & mask.clone()) >> min_bit),
            ));
            offset &= mask.not();
        }

        if offset != 0.into() {
            // We were not able to cover all of the offset, so this equation cannot be solved.
            Err(ConflictingBitConstraints)
        } else {
            Ok(assignments)
        }
    }

    /// Returns true if it can be determined that this expression can never be zero.
    pub fn is_invalid(&self) -> bool {
        // TODO add constraint validity.
        self.constant_value().map(|v| v != 0.into()) == Some(true)
    }

    pub fn format(&self, namer: &impl WitnessColumnNamer) -> String {
        self.nonzero_coefficients()
            .map(|(i, c)| {
                let name = namer.name(i);
                if *c == 1.into() {
                    name
                } else if *c == clamp((-1).into()) {
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
    use std::collections::BTreeMap;

    use super::*;
    use crate::{
        number::AbstractNumberType,
        witness_generator::{bit_constraints::BitConstraint, eval_error::EvalError},
    };

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

    struct TestBitConstraints(BTreeMap<usize, BitConstraint>);
    impl BitConstraintSet for TestBitConstraints {
        fn bit_constraint(&self, id: usize) -> Option<BitConstraint> {
            self.0.get(&id).cloned()
        }
    }

    #[test]
    pub fn derive_constraints() {
        let expr = AffineExpression::from_witness_poly_value(1)
            - AffineExpression::from_witness_poly_value(2).mul(16.into())
            - AffineExpression::from_witness_poly_value(3);
        let known_constraints = TestBitConstraints(
            vec![
                (2, BitConstraint::from_max(7)),
                (3, BitConstraint::from_max(3)),
            ]
            .into_iter()
            .collect(),
        );
        assert_eq!(
            expr.solve_with_bit_constraints(&known_constraints).unwrap(),
            vec![(1, Constraint::BitConstraint(BitConstraint::from_max(11)))]
        );
        assert_eq!(
            (-expr)
                .solve_with_bit_constraints(&known_constraints)
                .unwrap(),
            vec![(1, Constraint::BitConstraint(BitConstraint::from_max(11)))]
        );

        // Replace factor 16 by 32.
        let expr = AffineExpression::from_witness_poly_value(1)
            - AffineExpression::from_witness_poly_value(2).mul(32.into())
            - AffineExpression::from_witness_poly_value(3);
        assert!(expr.solve_with_bit_constraints(&known_constraints).is_err());

        // Replace factor 16 by 8.
        let expr = AffineExpression::from_witness_poly_value(1)
            - AffineExpression::from_witness_poly_value(2).mul(8.into())
            - AffineExpression::from_witness_poly_value(3);
        assert!(expr.solve_with_bit_constraints(&known_constraints).is_err());
    }

    #[test]
    pub fn solve_through_constraints_success() {
        let value = 0x1504u32;
        let expr = AffineExpression::from(value)
            - AffineExpression::from_witness_poly_value(2).mul(256.into())
            - AffineExpression::from_witness_poly_value(3);
        let known_constraints = TestBitConstraints(
            vec![
                (2, BitConstraint::from_max(7)),
                (3, BitConstraint::from_max(3)),
            ]
            .into_iter()
            .collect(),
        );
        assert_eq!(value, 0x15 * 256 + 0x4);
        assert_eq!(
            expr.solve_with_bit_constraints(&known_constraints).unwrap(),
            vec![
                (2, Constraint::Assignment(0x15.into())),
                (3, Constraint::Assignment(0x4.into()))
            ]
        );
    }

    #[test]
    pub fn solve_through_constraints_conflict() {
        let value = 0x1554u32;
        let expr = AffineExpression::from(value)
            - AffineExpression::from_witness_poly_value(2).mul(256.into())
            - AffineExpression::from_witness_poly_value(3);
        let known_constraints = TestBitConstraints(
            vec![
                (2, BitConstraint::from_max(7)),
                (3, BitConstraint::from_max(3)),
            ]
            .into_iter()
            .collect(),
        );
        match expr.solve_with_bit_constraints(&known_constraints) {
            Err(EvalError::ConflictingBitConstraints) => {}
            _ => panic!(),
        };
    }
}
