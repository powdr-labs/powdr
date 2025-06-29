use std::fmt::Display;

use itertools::{Either, Itertools};

use num_traits::Zero;
use powdr_ast::analyzed::{AlgebraicExpression, AlgebraicReference};
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_number::{FieldElement, LargeInt};

use super::global_constraints::RangeConstraintSet;
use super::Constraint;
use super::{EvalError::*, EvalResult, EvalValue, IncompleteCause};

/// An expression affine in the committed polynomials (or symbolic variables in general).
#[derive(Debug, Clone)]
pub enum AffineExpression<K, T> {
    Constant(T),
    OneVar((K, T), T),
    /// Many contains 1 or more variables, in variable order, without duplicates or zero coefficients
    ManyVars(Vec<(K, T)>, T),
}

/// A variable in an affine expression.
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Copy, derive_more::Display)]
pub enum AlgebraicVariable<'a> {
    /// Reference to a (witness) column
    Column(&'a AlgebraicReference),
    /// Reference to a public value
    // TODO: This should be using the ID instead of the name, but we
    //       currently store the name in AlgebraicExpression::PublicReference.
    Public(&'a str),
}

impl<'a, T> TryFrom<&'a AlgebraicExpression<T>> for AlgebraicVariable<'a> {
    type Error = ();

    fn try_from(expr: &'a AlgebraicExpression<T>) -> Result<Self, Self::Error> {
        match expr {
            AlgebraicExpression::Reference(r) => Ok(AlgebraicVariable::Column(r)),
            AlgebraicExpression::PublicReference(name) => Ok(AlgebraicVariable::Public(name)),
            _ => Err(()),
        }
    }
}

impl AlgebraicVariable<'_> {
    /// Returns the column reference if the variable is a column, otherwise None.
    pub fn try_as_column(&self) -> Option<&AlgebraicReference> {
        match self {
            AlgebraicVariable::Column(r) => Some(r),
            AlgebraicVariable::Public(_) => None,
        }
    }
}

pub type AffineResult<K, T> = Result<AffineExpression<K, T>, IncompleteCause<K>>;

impl<K, T> From<T> for AffineExpression<K, T> {
    #[inline]
    fn from(value: T) -> Self {
        Self::Constant(value)
    }
}

impl<K, T> AffineExpression<K, T>
where
    K: Ord,
    T: FieldElement,
{
    pub fn from_variable_id(var_id: K) -> AffineExpression<K, T> {
        Self::OneVar((var_id, T::ONE), T::ZERO)
    }

    pub fn is_constant(&self) -> bool {
        matches!(self, Self::Constant(_))
    }

    pub fn constant_value(&self) -> Option<T> {
        if let Self::Constant(c) = self {
            Some(*c)
        } else {
            None
        }
    }

    #[inline]
    pub fn is_zero(&self) -> bool {
        if let Self::Constant(n) = self {
            n.is_zero()
        } else {
            false
        }
    }

    #[inline]
    pub fn is_one(&self) -> bool {
        if let Self::Constant(n) = self {
            n.is_one()
        } else {
            false
        }
    }

    /// check that variables are sorted, unique and non-zero (used in sanity debug asserts)
    fn is_proper_coefficients(coefficients: &[(K, T)]) -> bool {
        coefficients.windows(2).all(|w| w[0].0 < w[1].0)
            && coefficients.iter().all(|(_, c)| !c.is_zero())
    }

    /// @returns the nonzero coefficients and their variable IDs (but not the offset).
    /// The coefficients are ordered by variable
    pub fn nonzero_coefficients(&self) -> impl Iterator<Item = &(K, T)> {
        // We want to return impl `Iterator`, but there are 3 possible concrete implementations to return here.
        // General solution is to use `Box<dyn>` but we want to avoid allocations and dynamic dispatch.
        // Instead, we use `itertools::Either`, which implements `Iterator` when both sides implement it, and this works recursively.
        match self {
            AffineExpression::ManyVars(coefficients, _) => {
                debug_assert!(Self::is_proper_coefficients(coefficients));
                Either::Right(Either::Left(coefficients.iter()))
            }
            AffineExpression::OneVar(coeff, _) => Either::Left(std::iter::once(coeff)),
            AffineExpression::Constant(_) => Either::Right(Either::Right(std::iter::empty())),
        }
    }

    pub fn offset(&self) -> T {
        match self {
            AffineExpression::Constant(offset)
            | AffineExpression::OneVar(_, offset)
            | AffineExpression::ManyVars(_, offset) => *offset,
        }
    }

    /// Incorporates the case where the symbolic variable `key` is assigned
    /// the value `value`.
    pub fn assign(&mut self, key: K, value: T) {
        match self {
            AffineExpression::OneVar((v, c), offset) if &key == v => {
                *self = AffineExpression::Constant(*offset + (value * *c));
            }
            AffineExpression::Constant(_) | AffineExpression::OneVar(_, _) => {}
            AffineExpression::ManyVars(coefficients, offset) => {
                debug_assert!(Self::is_proper_coefficients(coefficients));
                if let Ok(idx) = coefficients.binary_search_by_key(&&key, |(k, _)| k) {
                    *offset += coefficients[idx].1 * value;
                    coefficients.remove(idx);
                }
                if coefficients.is_empty() {
                    *self = Self::Constant(*offset);
                }
            }
        };
    }
}

impl<K, T> AffineExpression<K, T>
where
    K: Copy + Ord + Display,
    T: FieldElement,
{
    /// If the affine expression has only a single variable (with nonzero coefficient),
    /// returns the index of the variable and the assignment that evaluates the
    /// affine expression to zero.
    /// Returns an error if the constraint is unsatisfiable
    pub fn solve(&self) -> EvalResult<T, K> {
        let ((v, c), offset) = match self {
            AffineExpression::Constant(c) if c.is_zero() => {
                return Ok(EvalValue::complete(vec![]));
            }
            AffineExpression::Constant(_) => {
                return Err(ConstraintUnsatisfiable(self.to_string()));
            }
            AffineExpression::OneVar((v, c), offset) => ((v, c), offset),
            AffineExpression::ManyVars(coefficients, offset) if coefficients.len() == 1 => (
                coefficients.iter().map(|(v, c)| (v, c)).next().unwrap(),
                offset,
            ),
            _ => {
                return Ok(EvalValue::incomplete(
                    IncompleteCause::MultipleLinearSolutions,
                ));
            }
        };
        Ok(EvalValue::complete(vec![(
            *v,
            Constraint::Assignment(if c.is_one() {
                -*offset
            } else if *c == -T::one() {
                *offset
            } else {
                -*offset / *c
            }),
        )]))
    }

    /// Tries to solve "self = 0", or at least propagate a bit / range constraint:
    /// If we know that some components can only have certain bits set and the offset is zero,
    /// this property might transfer to another component.
    /// Furthermore, if we know that all components are bit-constrained and do not overlap,
    /// we can deduce the values of all components from the offset part.
    pub fn solve_with_range_constraints(
        &self,
        known_constraints: &dyn RangeConstraintSet<K, T>,
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

    /// Solves equations of the form `dividend = divisor * quotient + remainder`
    /// where `dividend` and `divisor` are known and `remainder` is range-constrained to be smaller than `divisor`.
    fn try_solve_division(
        &self,
        known_constraints: &dyn RangeConstraintSet<K, T>,
    ) -> Option<EvalResult<T, K>> {
        // Detect pattern: `dividend = divisor * quotient + remainder`
        let (first, second, offset) = match self {
            AffineExpression::ManyVars(coefficients, offset) => {
                let [first, second] = &coefficients[..] else {
                    return None;
                };
                (first, second, offset)
            }
            _ => {
                return None;
            }
        };
        if !first.1.is_one() && !second.1.is_one() {
            return None;
        }
        let (dividend, divisor, quotient, remainder) = if first.1.is_one() {
            (-*offset, second.1, second.0, first.0)
        } else {
            (-*offset, first.1, first.0, second.0)
        };

        // Check that remainder is in [0, divisor - 1].
        let (remainder_lower, remainder_upper) =
            known_constraints.range_constraint(remainder)?.range();
        if remainder_lower > remainder_upper || remainder_upper >= divisor {
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
                Ok(EvalValue::complete(vec![
                    (quotient, Constraint::Assignment(quotient_value)),
                    (remainder, Constraint::Assignment(remainder_value)),
                ]))
            },
        )
    }

    fn try_transfer_constraints(
        &self,
        known_constraints: &dyn RangeConstraintSet<K, T>,
    ) -> Option<(K, RangeConstraint<T>)> {
        // We are looking for X = a * Y + b * Z + ... or -X = a * Y + b * Z + ...
        // where X is least constrained.

        let (solve_for, solve_for_coefficient) = self
            .nonzero_coefficients()
            .filter(|(_i, c)| *c == -T::one() || c.is_one())
            .max_by_key(|(i, _c)| {
                // Sort so that we get the least constrained variable.
                known_constraints
                    .range_constraint(*i)
                    .map(|c| c.range_width())
                    .unwrap_or_else(|| T::modulus())
            })?;

        let offset = self.offset();

        let summands = self
            .nonzero_coefficients()
            .filter(|(i, _)| i != solve_for)
            .map(|(i, coeff)| {
                known_constraints
                    .range_constraint(*i)
                    .map(|con| con.multiple(*coeff))
            })
            .chain((!offset.is_zero()).then_some(Some(RangeConstraint::from_value(offset))))
            .collect::<Option<Vec<_>>>()?;
        let mut constraint = summands.into_iter().reduce(|c1, c2| c1.combine_sum(&c2))?;
        if solve_for_coefficient.is_one() {
            constraint = -constraint;
        }
        if let Some(previous) = known_constraints.range_constraint(*solve_for) {
            if previous.conjunction(&constraint) == previous {
                return None;
            }
        }
        Some((*solve_for, constraint))
    }

    /// Tries to assign values to all variables through their bit constraints.
    /// This can also determine if the equation is not satisfiable,
    /// if the bit-constraints do not cover all the bits of the offset.
    /// Returns an empty vector if it is not able to solve the equation.
    fn try_solve_through_constraints(
        &self,
        known_constraints: &dyn RangeConstraintSet<K, T>,
    ) -> EvalResult<T, K> {
        // Get constraints from coefficients and also collect unconstrained indices.
        let (constraints, unconstrained): (Vec<_>, Vec<K>) = self
            .nonzero_coefficients()
            .partition_map(|(i, coeff)| match known_constraints.range_constraint(*i) {
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
        let mut assignments = EvalValue::complete(vec![]);
        let mut offset = (-self.offset()).to_integer();
        for (i, coeff, constraint) in constraints {
            let mask = *constraint.multiple(coeff).mask();
            if !(mask & covered_bits).is_zero() {
                return Ok(EvalValue::incomplete(
                    IncompleteCause::OverlappingBitConstraints,
                ));
            } else {
                covered_bits |= mask;
            }
            assignments.combine(EvalValue::complete(vec![(
                *i,
                Constraint::Assignment(
                    ((offset & mask).to_arbitrary_integer() / coeff.to_arbitrary_integer()).into(),
                ),
            )]));
            offset &= !mask;
        }

        if covered_bits >= T::modulus() {
            return Ok(EvalValue::incomplete(
                IncompleteCause::OverflowingBitConstraints,
            ));
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
        if self.offset() != other.offset() {
            return false;
        };
        self.nonzero_coefficients().eq(other.nonzero_coefficients())
    }
}

impl<K, T> std::ops::Add for AffineExpression<K, T>
where
    K: Copy + Ord,
    T: FieldElement,
{
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        use std::cmp::Ordering;
        use AffineExpression::*;
        match (self, rhs) {
            // constant + constant
            (Constant(o1), Constant(o2)) => Constant(o1 + o2),
            // constant + one
            (OneVar(coeff, o1), Constant(o2)) | (Constant(o2), OneVar(coeff, o1)) => {
                OneVar(coeff, o1 + o2)
            }
            // constant + many
            (ManyVars(coefficients, offset), Constant(o))
            | (Constant(o), ManyVars(coefficients, offset)) => ManyVars(coefficients, offset + o),
            // one + one
            (OneVar((v1, c1), o1), OneVar((v2, c2), o2)) => match v1.cmp(&v2) {
                Ordering::Equal => {
                    let c = c1 + c2;
                    if c.is_zero() {
                        Constant(o1 + o2)
                    } else {
                        OneVar((v1, c), o1 + o2)
                    }
                }
                Ordering::Less => ManyVars(vec![(v1, c1), (v2, c2)], o1 + o2),
                Ordering::Greater => ManyVars(vec![(v2, c2), (v1, c1)], o1 + o2),
            },
            // one + many
            (OneVar((v, c), o), ManyVars(mut coefficients, offset))
            | (ManyVars(mut coefficients, offset), OneVar((v, c), o)) => {
                debug_assert!(Self::is_proper_coefficients(&coefficients));
                match coefficients.binary_search_by_key(&&v, |(k, _)| k) {
                    Ok(idx) => coefficients[idx].1 += c,
                    Err(idx) => coefficients.insert(idx, (v, c)),
                }
                coefficients.retain(|(_, c)| !c.is_zero());
                if coefficients.is_empty() {
                    Constant(offset + o)
                } else {
                    ManyVars(coefficients, offset + o)
                }
            }
            // many + many
            (ManyVars(c1, o1), ManyVars(c2, o2)) => {
                debug_assert!(Self::is_proper_coefficients(&c1));
                let mut coefficients = Vec::with_capacity(c1.len() + c2.len());
                let mut c1 = c1.into_iter();
                let mut c2 = c2.into_iter();
                // loop over the two expressions in parallel, merging their variables in order
                let mut ov1 = c1.next();
                let mut ov2 = c2.next();
                loop {
                    match (ov1, ov2) {
                        (Some(v1), Some(v2)) => match v1.0.cmp(&v2.0) {
                            Ordering::Less => {
                                coefficients.push(v1);
                                ov1 = c1.next();
                            }
                            Ordering::Equal => {
                                let c = v1.1 + v2.1;
                                if !c.is_zero() {
                                    coefficients.push((v1.0, c));
                                }
                                ov1 = c1.next();
                                ov2 = c2.next();
                            }
                            Ordering::Greater => {
                                coefficients.push(v2);
                                ov2 = c2.next();
                            }
                        },
                        (Some(v1), None) => {
                            coefficients.push(v1);
                            coefficients.extend(c1);
                            break;
                        }
                        (None, Some(v2)) => {
                            coefficients.push(v2);
                            coefficients.extend(c2);
                            break;
                        }
                        (None, None) => {
                            break;
                        }
                    }
                }
                if coefficients.is_empty() {
                    Constant(o1 + o2)
                } else {
                    ManyVars(coefficients, o1 + o2)
                }
            }
        }
    }
}

impl<K, T> std::ops::Neg for AffineExpression<K, T>
where
    K: Ord,
    T: FieldElement,
{
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            AffineExpression::Constant(offset) => AffineExpression::Constant(-offset),
            AffineExpression::OneVar((v, c), offset) => AffineExpression::OneVar((v, -c), -offset),
            AffineExpression::ManyVars(mut coefficients, offset) => {
                for (_, v) in coefficients.iter_mut() {
                    *v = -*v;
                }
                AffineExpression::ManyVars(coefficients, -offset)
            }
        }
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
    fn mul(self, factor: T) -> Self {
        if factor.is_zero() {
            factor.into()
        } else {
            match self {
                AffineExpression::Constant(offset) => AffineExpression::Constant(offset * factor),
                AffineExpression::OneVar((v, c), offset) => {
                    AffineExpression::OneVar((v, c * factor), offset * factor)
                }
                AffineExpression::ManyVars(mut coefficients, offset) => {
                    for (_, v) in coefficients.iter_mut() {
                        *v *= factor;
                    }
                    AffineExpression::ManyVars(coefficients, offset * factor)
                }
            }
        }
    }
}

impl<K, T: FieldElement> Display for AffineExpression<K, T>
where
    K: Ord + Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_constant() {
            write!(f, "{}", self.offset())
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
                    .chain((!self.offset().is_zero()).then(|| self.offset().to_string()))
                    .format(" + ")
            )
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::BTreeMap;

    use super::*;
    use crate::witgen::EvalError;
    use powdr_constraint_solver::range_constraint::RangeConstraint;
    use powdr_number::{FieldElement, GoldilocksField};
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

    fn convert<U, T>(input: Vec<U>) -> Vec<(usize, T)>
    where
        U: Copy + Into<T>,
        T: FieldElement,
    {
        input.iter().map(|x| (*x).into()).enumerate().collect()
    }

    #[test]
    fn affine_assign() {
        let mut a = AffineExpression::<_, GoldilocksField>::ManyVars(convert(vec![2, 3]), 3.into());
        a.assign(0, 3.into());
        assert_eq!(
            a,
            AffineExpression::ManyVars(vec![(1, 3.into())], 9.into(),),
        );

        // Now, the expression is 3b + 9. It should be able to solve for b
        // such that 3b + 9 = 0.
        let updates = a.solve().unwrap();
        assert_eq!(
            updates.constraints,
            [(1, Constraint::Assignment((-3).into()))]
        );
        a.assign(1, (-3).into());
        assert_eq!(a.constant_value().unwrap(), 0.into());
    }

    #[test]
    fn affine_neg() {
        let a = AffineExpression::ManyVars(convert(vec![1, 2]), 9.into());
        assert_eq!(
            -a,
            AffineExpression::ManyVars(
                convert(vec![
                    GoldilocksField::from(0) - GoldilocksField::from(1u64),
                    GoldilocksField::from(0) - GoldilocksField::from(2u64),
                ]),
                GoldilocksField::from(0) - GoldilocksField::from(9u64),
            ),
        );
    }

    #[test]
    fn affine_add() {
        let a = AffineExpression::<_, GoldilocksField>::ManyVars(convert(vec![1, 2]), 3.into());
        let b = AffineExpression::ManyVars(convert(vec![11]), 13.into());
        assert_eq!(
            a.clone() + b.clone(),
            AffineExpression::ManyVars(convert(vec![12, 2]), 16.into()),
        );
        assert_eq!(b.clone() + a.clone(), a + b,);
    }

    #[test]
    fn affine_add_with_ref_key() {
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

    #[test]
    fn affine_clean() {
        let a = AffineExpression::<_, GoldilocksField>::ManyVars(convert(vec![1, 2]), 3.into());
        let b = AffineExpression::ManyVars(convert(vec![11, 80]), 13.into());
        assert_eq!(
            (a.clone() * 3.into()) + b.clone(),
            AffineExpression::ManyVars(convert(vec![14, 86]), 22.into(),),
        );
        assert_eq!(a * 0.into(), GoldilocksField::zero().into());
        assert_eq!(b * 0.into(), GoldilocksField::zero().into());
    }

    #[test]
    fn affine_clean_long() {
        let a = AffineExpression::<_, GoldilocksField>::ManyVars(
            convert(vec![1, 2, 4, 9, 8]),
            3.into(),
        );
        let b = AffineExpression::ManyVars(convert(vec![11, 12, -4, 10, -8]), 1.into());
        assert_eq!(
            (a.clone() + b.clone())
                .nonzero_coefficients()
                .map(|(k, v)| (*k, *v))
                .collect::<Vec<_>>(),
            vec![(0, 12.into()), (1, 14.into()), (3, 19.into()),]
        );
        assert_eq!(a * 0.into(), GoldilocksField::zero().into());
        assert_eq!(b * 0.into(), GoldilocksField::zero().into());
    }

    #[test]
    fn equality() {
        let a = AffineExpression::<_, GoldilocksField>::ManyVars(convert(vec![1, 2]), 3.into());
        let b = AffineExpression::ManyVars(convert(vec![1, 2, 3]), 13.into());
        assert_eq!(a.clone() + b.clone(), b.clone() + a.clone());
    }

    struct TestRangeConstraints<T: FieldElement>(BTreeMap<usize, RangeConstraint<T>>);
    impl<T: FieldElement> RangeConstraintSet<usize, T> for TestRangeConstraints<T> {
        fn range_constraint(&self, id: usize) -> Option<RangeConstraint<T>> {
            self.0.get(&id).cloned()
        }
    }

    #[test]
    fn derive_constraints() {
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
                vec![(
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
                vec![(
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
                vec![(
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
                vec![(
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
    fn solve_through_constraints_success() {
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
            EvalValue::complete(vec![
                (2, Constraint::Assignment(0x15.into())),
                (3, Constraint::Assignment(0x4.into()))
            ],)
        );
    }

    #[test]
    fn solve_through_constraints_conflict() {
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
    fn transfer_range_constraints() {
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
                vec![(
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
    fn solve_division() {
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
            EvalValue::complete(vec![
                (1, Constraint::Assignment(4.into())),
                (2, Constraint::Assignment(2.into()))
            ])
        );
    }

    #[test]
    fn overflowing_division() {
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
                vec![(
                    2,
                    Constraint::RangeConstraint(RangeConstraint::from_range(2.into(), 5.into()))
                )],
                IncompleteCause::NotConcrete
            )
        );
    }

    #[test]
    fn solve_is_zero() {
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
