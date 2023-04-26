use number::AbstractNumberType;
// TODO this should probably rather be a finite field element.
use number::FieldElement;

use super::bit_constraints::BitConstraintSet;
use super::util::WitnessColumnNamer;
use super::Constraint;
use super::{EvalError::*, EvalResult, EvalValue, IncompleteCause};

/// An expression affine in the committed polynomials.
#[derive(Debug, Clone)]
pub struct AffineExpression {
    pub coefficients: Vec<FieldElement>,
    pub offset: FieldElement,
}

pub type AffineResult = Result<AffineExpression, IncompleteCause>;

impl From<FieldElement> for AffineExpression {
    fn from(value: FieldElement) -> Self {
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
    pub fn from_witness_poly_value(poly_id: usize) -> AffineExpression {
        AffineExpression {
            coefficients: [vec![0.into(); poly_id], vec![1.into()]].concat(),
            offset: 0.into(),
        }
    }

    pub fn is_constant(&self) -> bool {
        self.nonzero_coefficients().next().is_none()
    }

    pub fn constant_value(&self) -> Option<FieldElement> {
        if self.is_constant() {
            Some(self.offset)
        } else {
            None
        }
    }

    pub fn nonzero_variables(&self) -> Vec<usize> {
        self.nonzero_coefficients().map(|(i, _)| i).collect()
    }

    /// @returns an iterator of the nonzero coefficients and their variable IDs (but not the offset).
    pub fn nonzero_coefficients(&self) -> impl Iterator<Item = (usize, &FieldElement)> {
        self.coefficients
            .iter()
            .enumerate()
            .filter(|(_, c)| !c.is_zero())
    }

    pub fn mul(mut self, factor: FieldElement) -> AffineExpression {
        for f in &mut self.coefficients {
            *f = *f * factor;
        }
        self.offset = self.offset * factor;
        self
    }

    /// If the affine expression has only a single variable (with nonzero coefficient),
    /// returns the index of the variable and the assignment that evaluates the
    /// affine expression to zero.
    /// Returns an error if the constraint is unsat
    pub fn solve(&self) -> Result<EvalValue, ()> {
        let mut nonzero = self.nonzero_coefficients();
        let first = nonzero.next();
        let second = nonzero.next();
        match (first, second) {
            (Some((i, c)), None) => {
                // c * a + o = 0 <=> a = -o/c
                Ok(EvalValue::complete(vec![(
                    i,
                    Constraint::Assignment(if *c == 1.into() {
                        -self.offset
                    } else if *c == (-1).into() {
                        self.offset
                    } else {
                        -self.offset / *c
                    }),
                )]))
            }
            (Some(_), Some(_)) => Ok(EvalValue::incomplete(
                IncompleteCause::MultipleLinearSolutions,
            )),
            (None, None) => {
                if self.offset == 0.into() {
                    Ok(EvalValue::complete(vec![]))
                } else {
                    Err(())
                }
            }
            (None, Some(_)) => panic!(),
        }
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
        match self.solve() {
            Ok(value) if value.is_complete() => return Ok(value),
            Err(()) => return Err(ConstraintUnsatisfiable(String::new())),
            Ok(value) => {
                // sanity check that we are not ignoring anything useful here
                assert!(value.constraints.is_empty());
            }
        };
        Ok(
            if self
                .nonzero_coefficients()
                .all(|(i, _coeff)| known_constraints.bit_constraint(i).is_some())
            {
                // We might be able to solve for one or more variables, if all
                // bit constraints are disjoint.

                // Try positive and negative. We might also experiment with other strategies.

                self.try_solve_through_constraints(known_constraints)
                    .and_then(|new_constraints| {
                        if !new_constraints.is_complete() {
                            (-self.clone()).try_solve_through_constraints(known_constraints)
                        } else {
                            Ok(new_constraints)
                        }
                    })?
            } else if self.offset == 0.into() {
                // We might be able to deduce bit constraints on one variable.
                self.try_transfer_constraints(known_constraints)
                    .unwrap_or_else(|| {
                        EvalValue::incomplete(IncompleteCause::NoProgressTransferring)
                    })
            } else {
                EvalValue::incomplete(IncompleteCause::SolvingFailed)
            },
        )
    }

    fn try_transfer_constraints(
        &self,
        known_constraints: &impl BitConstraintSet,
    ) -> Option<EvalValue> {
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
        } else if *solve_for.1 != (-1).into() {
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
                    .and_then(|con| con.multiple(*coeff))
            });

        parts
            .reduce(|c1, c2| match (c1, c2) {
                (Some(c1), Some(c2)) => c1.try_combine_sum(&c2),
                _ => None,
            })
            .flatten()
            .map(|con| {
                EvalValue::incomplete_with_constraints(
                    vec![(solve_for.0, Constraint::BitConstraint(con))],
                    IncompleteCause::NotConcrete,
                )
            })
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
                    coeff,
                    known_constraints
                        .bit_constraint(i)
                        .unwrap()
                        .multiple(*coeff),
                )
            })
            .collect::<Vec<_>>();

        let unconstrained: Vec<usize> = parts
            .iter()
            .filter_map(|(i, _, con)| con.is_none().then_some(*i))
            .collect();

        if !unconstrained.is_empty() {
            return Ok(EvalValue::incomplete(IncompleteCause::BitUnconstrained(
                unconstrained,
            )));
        }

        // Check if they are mutually exclusive and compute assignments.
        let mut covered_bits: AbstractNumberType = 0;
        let mut assignments = EvalValue::complete(vec![]);
        let mut offset = (-self.offset).to_integer();
        for (i, coeff, constraint) in parts {
            let constraint = constraint.clone().unwrap();
            let mask = constraint.mask();
            if mask & covered_bits != 0 {
                return Ok(EvalValue::incomplete(
                    IncompleteCause::OverlappingBitConstraints,
                ));
            } else {
                covered_bits |= mask;
            }
            assignments.combine(EvalValue::complete(vec![(
                i,
                Constraint::Assignment(((offset & mask) / coeff.to_integer()).into()),
            )]));
            offset &= !mask;
        }

        if offset != 0 {
            // We were not able to cover all of the offset, so this equation cannot be solved.
            Err(ConflictingBitConstraints)
        } else {
            Ok(assignments)
        }
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
                    format!("{c} * {name}")
                }
            })
            .chain(self.constant_value().map(|v| format!("{v}")))
            .collect::<Vec<_>>()
            .join(" + ")
    }
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
            coefficients[i] = coefficients[i] + v;
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
        self.coefficients.iter_mut().for_each(|v| *v = -*v);
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
    use std::collections::BTreeMap;

    use super::*;
    use crate::witgen::{bit_constraints::BitConstraint, EvalError};
    use number::FieldElement;

    fn convert(input: Vec<i32>) -> Vec<FieldElement> {
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
                    FieldElement::from(0) - FieldElement::from(1u64),
                    0.into(),
                    FieldElement::from(0) - FieldElement::from(2u64),
                ],
                offset: FieldElement::from(0) - FieldElement::from(9u64),
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
                (2, BitConstraint::from_max_bit(7)),
                (3, BitConstraint::from_max_bit(3)),
            ]
            .into_iter()
            .collect(),
        );
        assert_eq!(
            expr.solve_with_bit_constraints(&known_constraints).unwrap(),
            EvalValue::incomplete_with_constraints(
                vec![(
                    1,
                    Constraint::BitConstraint(BitConstraint::from_max_bit(11))
                )],
                IncompleteCause::NotConcrete
            )
        );
        assert_eq!(
            (-expr)
                .solve_with_bit_constraints(&known_constraints)
                .unwrap(),
            EvalValue::incomplete_with_constraints(
                vec![(
                    1,
                    Constraint::BitConstraint(BitConstraint::from_max_bit(11))
                )],
                IncompleteCause::NotConcrete
            )
        );

        // Replace factor 16 by 32.
        let expr = AffineExpression::from_witness_poly_value(1)
            - AffineExpression::from_witness_poly_value(2).mul(32.into())
            - AffineExpression::from_witness_poly_value(3);
        assert_eq!(
            expr.solve_with_bit_constraints(&known_constraints).unwrap(),
            EvalValue::incomplete_with_constraints(
                vec![(
                    1,
                    Constraint::BitConstraint(BitConstraint::from_mask(0x1fef))
                )],
                IncompleteCause::NotConcrete
            )
        );

        // Replace factor 16 by 8.
        let expr = AffineExpression::from_witness_poly_value(1)
            - AffineExpression::from_witness_poly_value(2).mul(8.into())
            - AffineExpression::from_witness_poly_value(3);
        assert_eq!(
            expr.solve_with_bit_constraints(&known_constraints),
            Ok(EvalValue::incomplete(
                IncompleteCause::NoProgressTransferring
            ))
        );
    }

    #[test]
    pub fn solve_through_constraints_success() {
        let value = 0x1504u32;
        let expr = AffineExpression::from(value)
            - AffineExpression::from_witness_poly_value(2).mul(256.into())
            - AffineExpression::from_witness_poly_value(3);
        let known_constraints = TestBitConstraints(
            vec![
                (2, BitConstraint::from_max_bit(7)),
                (3, BitConstraint::from_max_bit(3)),
            ]
            .into_iter()
            .collect(),
        );
        assert_eq!(value, 0x15 * 256 + 0x4);
        assert_eq!(
            expr.solve_with_bit_constraints(&known_constraints).unwrap(),
            EvalValue::complete(vec![
                (2, Constraint::Assignment(0x15.into())),
                (3, Constraint::Assignment(0x4.into()))
            ],)
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
                (2, BitConstraint::from_max_bit(7)),
                (3, BitConstraint::from_max_bit(3)),
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
