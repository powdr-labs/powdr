use std::collections::BTreeMap;
use std::fmt::Display;

use itertools::{Either, Itertools};

use num_traits::Zero;
use number::{BigInt, FieldElement};

use super::global_constraints::RangeConstraintSet;
use super::range_constraints::RangeConstraint;
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
            coefficients: BTreeMap::from([(var_id, T::one())]),
            offset: T::zero(),
        }
    }

    pub fn is_constant(&self) -> bool {
        self.nonzero_coefficients().next().is_none()
    }

    pub fn constant_value(&self) -> Option<T> {
        self.is_constant().then_some(self.offset)
    }

    pub fn nonzero_variables(&self) -> Vec<K> {
        self.nonzero_coefficients().map(|(i, _)| i).collect()
    }

    /// @returns an iterator of the nonzero coefficients and their variable IDs (but not the offset).
    pub fn nonzero_coefficients(&self) -> impl Iterator<Item = (K, &T)> {
        self.coefficients
            .iter()
            .filter_map(|(i, c)| (!c.is_zero()).then_some((*i, c)))
    }

    pub fn assign(&mut self, key: K, value: T) {
        if let Some(coefficient) = self.coefficients.remove(&key) {
            self.offset -= coefficient * value;
        }
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
    pub fn solve(&self) -> EvalResult<T, K> {
        let mut nonzero = self.nonzero_coefficients();
        let first = nonzero.next();
        let second = nonzero.next();
        match (first, second) {
            (Some((i, c)), None) => {
                // c * a + o = 0 <=> a = -o/c
                Ok(EvalValue::complete([(
                    i,
                    Constraint::Assignment(if c.is_one() {
                        -self.offset
                    } else if *c == -T::one() {
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
                if self.offset.is_zero() {
                    Ok(EvalValue::complete([]))
                } else {
                    Err(ConstraintUnsatisfiable(self.to_string()))
                }
            }
            (None, Some(_)) => panic!(),
        }
    }

    /// Tries to solve "self = 0", or at least propagate a bit / range constraint:
    /// If we know that some components can only have certain bits set and the offset is zero,
    /// this property might transfer to another component.
    /// Furthermore, if we know that all components are bit-constrained and do not overlap,
    /// we can deduce the values of all components from the offset part.
    pub fn solve_with_range_constraints(
        &self,
        known_constraints: &impl RangeConstraintSet<K, T>,
    ) -> EvalResult<T, K> {
        // Try to solve directly.
        let value = self.solve()?;
        if value.is_complete() {
            return Ok(value);
        }

        // sanity check that we are not ignoring anything useful here
        assert!(value.constraints.is_empty());

        let negated = -self.clone();

        // Try to find a division-with-remainder pattern and solve it.
        if let Some(result) = self.try_solve_division(known_constraints).transpose()? {
            if !result.is_empty() {
                return Ok(result);
            }
        };

        if let Some(result) = negated.try_solve_division(known_constraints).transpose()? {
            if !result.is_empty() {
                return Ok(result);
            }
        };

        // If we have bit mask constraints on all variables and they are non-overlapping,
        // we can deduce values for all of them.
        let result = self.try_solve_through_constraints(known_constraints)?;
        if !result.is_empty() {
            return Ok(result);
        }

        let result = negated.try_solve_through_constraints(known_constraints)?;
        if !result.is_empty() {
            return Ok(result);
        }

        // Now at least try to propagate constraints to a variable from the other parts of the equation.
        let constraints = (match (
            self.try_transfer_constraints(known_constraints),
            negated.try_transfer_constraints(known_constraints),
        ) {
            (None, None) => vec![],
            (Some((p, c)), None) | (None, Some((p, c))) => vec![(p, c)],
            (Some((p1, c1)), Some((p2, c2))) => {
                if p1 == p2 {
                    vec![(p1, c1.conjunction(&c2))]
                } else {
                    vec![(p1, c1), (p2, c2)]
                }
            }
        })
        .into_iter()
        .map(|(poly, con)| (poly, Constraint::RangeConstraint(con)))
        .collect::<Vec<_>>();
        if constraints.is_empty() {
            Ok(EvalValue::incomplete(
                IncompleteCause::NoProgressTransferring,
            ))
        } else {
            Ok(EvalValue::incomplete_with_constraints(
                constraints,
                IncompleteCause::NotConcrete,
            ))
        }
    }

    fn try_solve_division(
        &self,
        known_constraints: &impl RangeConstraintSet<K, T>,
    ) -> Option<EvalResult<T, K>> {
        let mut coeffs = self.nonzero_coefficients();
        let first = coeffs.next()?;
        let second = coeffs.next()?;
        if coeffs.next().is_some() {
            return None;
        }
        if !first.1.is_one() && !second.1.is_one() {
            return None;
        }
        let (dividend, divisor, quotient, remainder) = if first.1.is_one() {
            (-self.offset, second.1, second.0, first.0)
        } else {
            (-self.offset, first.1, first.0, second.0)
        };
        // Now we have: dividend = remainder + divisor * quotient
        let (remainder_lower, remainder_upper) =
            known_constraints.range_constraint(remainder)?.range();

        // Check that remainder is in [0, divisor - 1].
        if remainder_lower > remainder_upper || remainder_upper >= *divisor {
            return None;
        }
        let (quotient_lower, quotient_upper) =
            known_constraints.range_constraint(quotient)?.range();
        // Check that divisor * quotient + remainder is range-constraint to not overflow.
        let result_upper = quotient_upper.to_arbitrary_integer() * divisor.to_arbitrary_integer()
            + remainder_upper.to_arbitrary_integer();
        if quotient_lower > quotient_upper || result_upper >= T::modulus().to_arbitrary_integer() {
            return None;
        }

        let quotient_value =
            (dividend.to_arbitrary_integer() / divisor.to_arbitrary_integer()).into();
        let remainder_value =
            (dividend.to_arbitrary_integer() % divisor.to_arbitrary_integer()).into();
        Some(
            if quotient_value < quotient_lower
                || quotient_value > quotient_upper
                || remainder_value < remainder_lower
                || remainder_value > remainder_upper
            {
                Err(ConflictingRangeConstraints)
            } else {
                Ok(EvalValue::complete([
                    (quotient, Constraint::Assignment(quotient_value)),
                    (remainder, Constraint::Assignment(remainder_value)),
                ]))
            },
        )
    }

    fn try_transfer_constraints(
        &self,
        known_constraints: &impl RangeConstraintSet<K, T>,
    ) -> Option<(K, RangeConstraint<T>)> {
        // We are looking for X = a * Y + b * Z + ... or -X = a * Y + b * Z + ...
        // where X is least constrained.

        let (solve_for, solve_for_coefficient) = self
            .nonzero_coefficients()
            .filter(|(_i, c)| **c == -T::one() || c.is_one())
            .max_by_key(|(i, _c)| {
                // Sort so that we get the least constrained variable.
                known_constraints
                    .range_constraint(*i)
                    .map(|c| c.range_width())
                    .unwrap_or_else(|| T::modulus())
            })?;

        let summands = self
            .nonzero_coefficients()
            .filter(|(i, _)| *i != solve_for)
            .map(|(i, coeff)| {
                known_constraints
                    .range_constraint(i)
                    .map(|con| con.multiple(*coeff))
            })
            .chain(
                (!self.offset.is_zero()).then_some(Some(RangeConstraint::from_value(self.offset))),
            )
            .collect::<Option<Vec<_>>>()?;
        let mut constraint = summands.into_iter().reduce(|c1, c2| c1.combine_sum(&c2))?;
        if solve_for_coefficient.is_one() {
            constraint = -constraint;
        }
        if let Some(previous) = known_constraints.range_constraint(solve_for) {
            if previous.conjunction(&constraint) == previous {
                return None;
            }
        }
        Some((solve_for, constraint))
    }

    /// Tries to assign values to all variables through their bit constraints.
    /// This can also determine if the equation is not satisfiable,
    /// if the bit-constraints do not cover all the bits of the offset.
    /// Returns an empty vector if it is not able to solve the equation.
    fn try_solve_through_constraints(
        &self,
        known_constraints: &impl RangeConstraintSet<K, T>,
    ) -> EvalResult<T, K> {
        // Get constraints from coefficients and also collect unconstrained indices.
        let (constraints, unconstrained): (Vec<_>, Vec<K>) = self
            .nonzero_coefficients()
            .partition_map(|(i, coeff)| match known_constraints.range_constraint(i) {
                None => Either::Right(i),
                Some(constraint) => Either::Left((i, *coeff, constraint)),
            });

        if !unconstrained.is_empty() {
            return Ok(EvalValue::incomplete(IncompleteCause::BitUnconstrained(
                unconstrained,
            )));
        }

        // Check if they are mutually exclusive and compute assignments.
        let mut covered_bits: <T as FieldElement>::Integer = Zero::zero();
        let mut assignments = EvalValue::complete([]);
        let mut offset = (-self.offset).to_integer();
        for (i, coeff, constraint) in constraints {
            let mask = *constraint.multiple(coeff).mask();
            if !(mask & covered_bits).is_zero() {
                return Ok(EvalValue::incomplete(
                    IncompleteCause::OverlappingBitConstraints,
                ));
            } else {
                covered_bits |= mask;
            }
            assignments.combine(EvalValue::complete([(
                i,
                Constraint::Assignment(
                    ((offset & mask).to_arbitrary_integer() / coeff.to_arbitrary_integer())
                        .try_into()
                        .unwrap(),
                ),
            )]));
            offset &= !mask;
        }

        if !offset.is_zero() {
            // We were not able to cover all of the offset, so this equation cannot be solved.
            Err(ConflictingRangeConstraints)
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
                        if c.is_one() {
                            i.to_string()
                        } else if *c == -T::one() {
                            format!("-{i}")
                        } else {
                            format!("{c} * {i}")
                        }
                    })
                    .chain((!self.offset.is_zero()).then(|| self.offset.to_string()))
                    .join(" + ")
            )
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::BTreeMap;

    use super::*;
    use crate::witgen::{range_constraints::RangeConstraint, EvalError};
    use number::{FieldElement, GoldilocksField};
    use pretty_assertions::assert_eq;
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
    struct TestRangeConstraints<T: FieldElement>(BTreeMap<usize, RangeConstraint<T>>);
    impl<T: FieldElement> RangeConstraintSet<usize, T> for TestRangeConstraints<T> {
        fn range_constraint(&self, id: usize) -> Option<RangeConstraint<T>> {
            self.0.get(&id).cloned()
        }
    }

    #[test]
    pub fn derive_constraints() {
        let expr = AffineExpression::from_variable_id(1)
            - AffineExpression::from_variable_id(2) * 16.into()
            - AffineExpression::from_variable_id(3);
        let known_constraints: TestRangeConstraints<GoldilocksField> = TestRangeConstraints(
            [
                (2, RangeConstraint::from_max_bit(7)),
                (3, RangeConstraint::from_max_bit(3)),
            ]
            .into_iter()
            .collect(),
        );
        assert_eq!(
            expr.solve_with_range_constraints(&known_constraints)
                .unwrap(),
            EvalValue::incomplete_with_constraints(
                [(
                    1,
                    Constraint::RangeConstraint(RangeConstraint::from_max_bit(11))
                )],
                IncompleteCause::NotConcrete
            )
        );
        assert_eq!(
            (-expr)
                .solve_with_range_constraints(&known_constraints)
                .unwrap(),
            EvalValue::incomplete_with_constraints(
                [(
                    1,
                    Constraint::RangeConstraint(RangeConstraint::from_max_bit(11))
                )],
                IncompleteCause::NotConcrete
            )
        );

        // Replace factor 16 by 32.
        let expr = AffineExpression::from_variable_id(1)
            - AffineExpression::from_variable_id(2) * 32.into()
            - AffineExpression::from_variable_id(3);
        assert_eq!(
            expr.solve_with_range_constraints(&known_constraints)
                .unwrap(),
            EvalValue::incomplete_with_constraints(
                [(
                    1,
                    Constraint::RangeConstraint(RangeConstraint::from_mask(0x1fef_u32))
                )],
                IncompleteCause::NotConcrete
            )
        );

        // Replace factor 16 by 8.
        let expr = AffineExpression::from_variable_id(1)
            - AffineExpression::from_variable_id(2) * 8.into()
            - AffineExpression::from_variable_id(3);
        assert_eq!(
            expr.solve_with_range_constraints(&known_constraints),
            Ok(EvalValue::incomplete_with_constraints(
                [(
                    1,
                    Constraint::RangeConstraint(RangeConstraint::from_range(
                        0.into(),
                        (0xff * 8 + 0xf).into()
                    ))
                )],
                IncompleteCause::NotConcrete
            ))
        );
    }

    #[test]
    pub fn solve_through_constraints_success() {
        let value: GoldilocksField = 0x1504u32.into();
        let expr = AffineExpression::from(value)
            - AffineExpression::from_variable_id(2) * 256.into()
            - AffineExpression::from_variable_id(3);
        let known_constraints: TestRangeConstraints<GoldilocksField> = TestRangeConstraints(
            [
                (2, RangeConstraint::from_max_bit(7)),
                (3, RangeConstraint::from_max_bit(3)),
            ]
            .into_iter()
            .collect(),
        );
        assert_eq!(value, GoldilocksField::from(0x15 * 256 + 0x4));
        assert_eq!(
            expr.solve_with_range_constraints(&known_constraints)
                .unwrap(),
            EvalValue::complete([
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
        let known_constraints: TestRangeConstraints<GoldilocksField> = TestRangeConstraints(
            [
                (2, RangeConstraint::from_max_bit(7)),
                (3, RangeConstraint::from_max_bit(3)),
            ]
            .into_iter()
            .collect(),
        );
        match expr.solve_with_range_constraints(&known_constraints) {
            Err(EvalError::ConflictingRangeConstraints) => {}
            _ => panic!(),
        };
    }

    #[test]
    pub fn transfer_range_constraints() {
        // x2 * 0x100 + x3 - x1 - 200 = 0,
        // x2: & 0xff
        // x3: & 0xf
        // => x2 * 0x100 + x3: & 0xff0f = [0, 0xff0f]
        // => x1: [-200, 65095]
        let expr = AffineExpression::from_variable_id(2) * 256.into()
            + AffineExpression::from_variable_id(3)
            - AffineExpression::from_variable_id(1)
            - AffineExpression::from(GoldilocksField::from(200));
        let known_constraints: TestRangeConstraints<GoldilocksField> = TestRangeConstraints(
            [
                (2, RangeConstraint::from_max_bit(7)),
                (3, RangeConstraint::from_max_bit(3)),
            ]
            .into_iter()
            .collect(),
        );
        let result = expr
            .solve_with_range_constraints(&known_constraints)
            .unwrap();
        assert_eq!(
            result,
            EvalValue::incomplete_with_constraints(
                [(
                    1,
                    Constraint::RangeConstraint(
                        RangeConstraint::from_range(-GoldilocksField::from(200), 65095.into())
                            .conjunction(&RangeConstraint::from_mask(0xffffffffffffffffu64))
                    )
                ),],
                IncompleteCause::NotConcrete
            )
        );
    }

    #[test]
    pub fn solve_division() {
        // 3 * x1 + x2 - 14 = 0
        let expr = AffineExpression::from_variable_id(1) * 3.into()
            + AffineExpression::from_variable_id(2)
            - AffineExpression::from(GoldilocksField::from(14));
        let known_constraints: TestRangeConstraints<GoldilocksField> = TestRangeConstraints(
            [
                (2, RangeConstraint::from_range(0.into(), 2.into())),
                (1, RangeConstraint::from_range(0.into(), 400.into())),
            ]
            .into_iter()
            .collect(),
        );
        let result = expr
            .solve_with_range_constraints(&known_constraints)
            .unwrap();
        assert_eq!(
            result,
            EvalValue::complete([
                (1, Constraint::Assignment(4.into())),
                (2, Constraint::Assignment(2.into()))
            ])
        );
    }

    #[test]
    pub fn overflowing_division() {
        // -3 * x1 + x2 - 2 = 0
        // where x1 in [0, 1] and x2 in [0, 7]
        // This equation has two solutions: x1 = 0, x2 = 2 and x1 = 1, x2 = 5.
        // It does fit the division pattern for computing 2 / (p - 3), but because
        // -3 * x1 + x2 can overflow, it should not be applied.
        let expr = AffineExpression::from_variable_id(1) * (-3).into()
            + AffineExpression::from_variable_id(2)
            - AffineExpression::from(GoldilocksField::from(2));
        let known_constraints: TestRangeConstraints<GoldilocksField> = TestRangeConstraints(
            [
                (1, RangeConstraint::from_range(0.into(), 1.into())),
                (2, RangeConstraint::from_range(0.into(), 7.into())),
            ]
            .into_iter()
            .collect(),
        );
        let result = expr
            .solve_with_range_constraints(&known_constraints)
            .unwrap();
        assert_eq!(
            result,
            EvalValue::incomplete_with_constraints(
                [(
                    2,
                    Constraint::RangeConstraint(RangeConstraint::from_range(2.into(), 5.into()))
                )],
                IncompleteCause::NotConcrete
            )
        );
    }

    #[test]
    pub fn solve_is_zero() {
        // 1 - (3 * inv) - is_zero = 0
        // 1 - (3 * x2) - x1 = 0
        // x1 in [0, 1]
        // This is almost suitable for the division pattern, but inv is not properly
        // constrained. So we should get "no progress" here.
        let expr = AffineExpression::from(GoldilocksField::from(1))
            - AffineExpression::from_variable_id(2) * 3.into()
            - AffineExpression::from_variable_id(1);
        let known_constraints: TestRangeConstraints<GoldilocksField> = TestRangeConstraints(
            [(1, RangeConstraint::from_range(0.into(), 1.into()))]
                .into_iter()
                .collect(),
        );
        let result = expr
            .solve_with_range_constraints(&known_constraints)
            .unwrap();
        assert_eq!(
            result,
            EvalValue::incomplete(IncompleteCause::NoProgressTransferring)
        );
    }
}
