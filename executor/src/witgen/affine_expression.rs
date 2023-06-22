use std::collections::BTreeMap;
use std::fmt::Display;

use itertools::Itertools;

use number::{BigInt, FieldElement};

use super::bit_constraints::BitConstraintSet;
use super::Constraint;
use super::{EvalError::*, EvalResult, EvalValue, IncompleteCause};

/// An expression affine in the committed polynomials (or symbolic variables in general).
#[derive(Debug, Clone)]
pub struct AffineExpression<K, T> {
    pub coefficients: BTreeMap<K, T>,
    pub offset: T,
}

pub type AffineResult<K, T> = Result<AffineExpression<K, T>, IncompleteCause<K>>;

impl<K, T> From<T> for AffineExpression<K, T> {
    fn from(value: T) -> Self {
        Self {
            coefficients: Default::default(),
            offset: value,
        }
    }
}

impl<'x, K, T> AffineExpression<K, T>
where
    K: Copy + Ord + 'x,
    T: FieldElement,
{
    pub fn from_variable_id(var_id: K) -> AffineExpression<K, T> {
        Self {
            coefficients: BTreeMap::from([(var_id, 1.into())]),
            offset: 0.into(),
        }
    }

    pub fn is_constant(&self) -> bool {
        self.nonzero_coefficients().next().is_none()
    }

    pub fn constant_value(&self) -> Option<T> {
        if self.is_constant() {
            Some(self.offset)
        } else {
            None
        }
    }

    pub fn nonzero_variables(&self) -> Vec<K> {
        self.nonzero_coefficients().map(|(i, _)| i).collect()
    }

    /// @returns an iterator of the nonzero coefficients and their variable IDs (but not the offset).
    pub fn nonzero_coefficients(&self) -> impl Iterator<Item = (K, &T)> {
        self.coefficients
            .iter()
            .filter_map(|(i, c)| (c != &T::from(0)).then_some((*i, c)))
    }
}

impl<'x, K, T> AffineExpression<K, T>
where
    K: Copy + Ord + Display + 'x,
    T: FieldElement,
{
    /// If the affine expression has only a single variable (with nonzero coefficient),
    /// returns the index of the variable and the assignment that evaluates the
    /// affine expression to zero.
    /// Returns an error if the constraint is unsat
    pub fn solve(&self) -> Result<EvalValue<K, T>, ()> {
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
        known_constraints: &impl BitConstraintSet<K, T>,
    ) -> EvalResult<T, K> {
        // Try to solve directly.
        match self.solve() {
            Ok(value) if value.is_complete() => return Ok(value),
            Err(()) => return Err(ConstraintUnsatisfiable(self.to_string())),
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
        known_constraints: &impl BitConstraintSet<K, T>,
    ) -> Option<EvalValue<K, T>> {
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
        known_constraints: &impl BitConstraintSet<K, T>,
    ) -> EvalResult<T, K> {
        let parts = self
            .nonzero_coefficients()
            .map(|(i, coeff)| {
                (
                    i,
                    *coeff,
                    known_constraints
                        .bit_constraint(i)
                        .unwrap()
                        .multiple(*coeff),
                )
            })
            .collect::<Vec<_>>();

        let unconstrained: Vec<K> = parts
            .iter()
            .filter_map(|(i, _, con)| con.is_none().then_some(*i))
            .collect();

        if !unconstrained.is_empty() {
            return Ok(EvalValue::incomplete(IncompleteCause::BitUnconstrained(
                unconstrained,
            )));
        }

        // Check if they are mutually exclusive and compute assignments.
        let mut covered_bits: <T as FieldElement>::Integer = 0u32.into();
        let mut assignments = EvalValue::complete(vec![]);
        let mut offset = (-self.offset).to_integer();
        for (i, coeff, constraint) in parts {
            let constraint = constraint.clone().unwrap();
            let mask = constraint.mask();
            if *mask & covered_bits != 0u32.into() {
                return Ok(EvalValue::incomplete(
                    IncompleteCause::OverlappingBitConstraints,
                ));
            } else {
                covered_bits |= *mask;
            }
            assignments.combine(EvalValue::complete(vec![(
                i,
                Constraint::Assignment(
                    ((offset & *mask).to_arbitrary_integer() / coeff.to_arbitrary_integer())
                        .try_into()
                        .unwrap(),
                ),
            )]));
            offset &= !*mask;
        }

        if offset != 0u32.into() {
            // We were not able to cover all of the offset, so this equation cannot be solved.
            Err(ConflictingBitConstraints)
        } else {
            Ok(assignments)
        }
    }
}

impl<K, T> PartialEq for AffineExpression<K, T>
where
    K: Copy + Ord,
    T: FieldElement,
{
    fn eq(&self, other: &Self) -> bool {
        self.offset == other.offset && self.nonzero_coefficients().eq(other.nonzero_coefficients())
    }
}

impl<K, T> std::ops::Add for AffineExpression<K, T>
where
    K: Copy + Ord,
    T: FieldElement,
{
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let mut coefficients = rhs.coefficients;
        for (i, v) in self.coefficients.into_iter() {
            coefficients
                .entry(i)
                .and_modify(|x| *x += v)
                .or_insert_with(|| v);
        }
        Self {
            coefficients,
            offset: self.offset + rhs.offset,
        }
    }
}

impl<K, T> std::ops::Neg for AffineExpression<K, T>
where
    K: Copy + Ord,
    T: FieldElement,
{
    type Output = Self;

    fn neg(mut self) -> Self::Output {
        self.coefficients.values_mut().for_each(|v| *v = -*v);
        self.offset = -self.offset;
        self
    }
}

impl<K, T> std::ops::Sub for AffineExpression<K, T>
where
    K: Copy + Ord,
    T: FieldElement,
{
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self + -rhs
    }
}

impl<K, T: FieldElement> std::ops::Mul<T> for AffineExpression<K, T> {
    type Output = Self;
    fn mul(mut self, factor: T) -> Self {
        for f in self.coefficients.values_mut() {
            *f = *f * factor;
        }
        self.offset = self.offset * factor;
        self
    }
}

impl<K, T: FieldElement> Display for AffineExpression<K, T>
where
    K: Copy + Ord + Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_constant() {
            write!(f, "{}", self.offset)
        } else {
            write!(
                f,
                "{}",
                self.nonzero_coefficients()
                    .map(|(i, c)| {
                        if *c == 1.into() {
                            i.to_string()
                        } else if *c == (-1).into() {
                            format!("-{i}")
                        } else {
                            format!("{c} * {i}")
                        }
                    })
                    .chain((self.offset != 0.into()).then_some(self.offset.to_string()))
                    .join(" + ")
            )
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::BTreeMap;

    use super::*;
    use crate::witgen::{bit_constraints::BitConstraint, EvalError};
    use number::{FieldElement, GoldilocksField};
    use test_log::test;

    impl<K> std::ops::Mul<AffineExpression<K, GoldilocksField>> for GoldilocksField {
        type Output = AffineExpression<K, GoldilocksField>;
        fn mul(
            self,
            expr: AffineExpression<K, GoldilocksField>,
        ) -> AffineExpression<K, GoldilocksField> {
            expr * self
        }
    }

    fn convert<U, T>(input: Vec<U>) -> BTreeMap<usize, T>
    where
        U: Copy + Into<T>,
        T: FieldElement,
    {
        input.iter().map(|x| (*x).into()).enumerate().collect()
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
                coefficients: convert(vec![
                    GoldilocksField::from(0) - GoldilocksField::from(1u64),
                    0.into(),
                    GoldilocksField::from(0) - GoldilocksField::from(2u64),
                ]),
                offset: GoldilocksField::from(0) - GoldilocksField::from(9u64),
            },
        );
    }

    #[test]
    pub fn test_affine_add() {
        let a = AffineExpression::<_, GoldilocksField> {
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
    pub fn test_affine_add_with_ref_key() {
        let names = ["abc", "def", "ghi"];
        let a = AffineExpression::from_variable_id(names[0])
            + GoldilocksField::from(2) * AffineExpression::from_variable_id(names[1])
            + GoldilocksField::from(3).into();
        let b = AffineExpression::from_variable_id(names[0]) * GoldilocksField::from(11)
            + GoldilocksField::from(13).into();
        let result = a.clone() + b.clone();
        assert_eq!(&result.to_string(), "12 * abc + 2 * def + 16");
        assert_eq!(b.clone() + a.clone(), a + b,);
    }
    struct TestBitConstraints<T: FieldElement>(BTreeMap<usize, BitConstraint<T>>);
    impl<T: FieldElement> BitConstraintSet<usize, T> for TestBitConstraints<T> {
        fn bit_constraint(&self, id: usize) -> Option<BitConstraint<T>> {
            self.0.get(&id).cloned()
        }
    }

    #[test]
    pub fn derive_constraints() {
        let expr = AffineExpression::from_variable_id(1)
            - AffineExpression::from_variable_id(2) * 16.into()
            - AffineExpression::from_variable_id(3);
        let known_constraints: TestBitConstraints<GoldilocksField> = TestBitConstraints(
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
        let expr = AffineExpression::from_variable_id(1)
            - AffineExpression::from_variable_id(2) * 32.into()
            - AffineExpression::from_variable_id(3);
        assert_eq!(
            expr.solve_with_bit_constraints(&known_constraints).unwrap(),
            EvalValue::incomplete_with_constraints(
                vec![(
                    1,
                    Constraint::BitConstraint(BitConstraint::from_mask(0x1fef_u32))
                )],
                IncompleteCause::NotConcrete
            )
        );

        // Replace factor 16 by 8.
        let expr = AffineExpression::from_variable_id(1)
            - AffineExpression::from_variable_id(2) * 8.into()
            - AffineExpression::from_variable_id(3);
        assert_eq!(
            expr.solve_with_bit_constraints(&known_constraints),
            Ok(EvalValue::incomplete(
                IncompleteCause::NoProgressTransferring
            ))
        );
    }

    #[test]
    pub fn solve_through_constraints_success() {
        let value: GoldilocksField = 0x1504u32.into();
        let expr = AffineExpression::from(value)
            - AffineExpression::from_variable_id(2) * 256.into()
            - AffineExpression::from_variable_id(3);
        let known_constraints: TestBitConstraints<GoldilocksField> = TestBitConstraints(
            vec![
                (2, BitConstraint::from_max_bit(7)),
                (3, BitConstraint::from_max_bit(3)),
            ]
            .into_iter()
            .collect(),
        );
        assert_eq!(value, GoldilocksField::from(0x15 * 256 + 0x4));
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
        let value: GoldilocksField = 0x1554u32.into();
        let expr = AffineExpression::from(value)
            - AffineExpression::from_variable_id(2) * 256.into()
            - AffineExpression::from_variable_id(3);
        let known_constraints: TestBitConstraints<GoldilocksField> = TestBitConstraints(
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
