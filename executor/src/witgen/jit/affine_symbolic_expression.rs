use std::{
    collections::BTreeMap,
    fmt::{self, Display, Formatter},
    iter::once,
    ops::{Add, Mul, Neg, Sub},
};

use itertools::Itertools;
use num_traits::Zero;
use powdr_number::FieldElement;

use crate::witgen::global_constraints::RangeConstraintSet;

use super::{
    super::range_constraints::RangeConstraint, cell::Cell, symbolic_expression::SymbolicExpression,
};

/// The effect of solving a symbolic equation.
pub enum Effect<T: FieldElement> {
    Assignment(Cell, SymbolicExpression<T>),
    RangeConstraint(Cell, RangeConstraint<T>),
    Code(String),
}

/// Represents an expression `a_1 * cell_1 + ... + a_k * cell_k + offset`,
/// where `a_i` are symbolic expressions, i.e. values known at run-time,
/// and the cells are references to the trace table that are not yet known.
#[derive(Debug, Clone)]
pub struct AffineSymbolicExpression<T: FieldElement> {
    coefficients: BTreeMap<Cell, SymbolicExpression<T>>,
    offset: SymbolicExpression<T>,
}

impl<T: FieldElement> Display for AffineSymbolicExpression<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.coefficients.is_empty() {
            write!(f, "{}", self.offset)
        } else {
            write!(
                f,
                "{}",
                self.coefficients
                    .iter()
                    .map(|(cell, coeff)| if coeff.is_known_one() {
                        cell_to_variable(cell).to_string()
                    } else {
                        format!("{} * {}", coeff, cell_to_variable(cell))
                    })
                    .join(" + ")
            )?;
            if !self.offset.is_known_zero() {
                write!(f, "+ {}", self.offset)?;
            }
            Ok(())
        }
    }
}

pub fn cell_to_variable(
    Cell {
        column_name,
        row_offset,
        ..
    }: &Cell,
) -> String {
    // TODO we need to check that the resulting name is actually unique.
    let column_name = &column_name[column_name.rfind("::").unwrap() + 2..]
        .replace("[", "_")
        .replace("]", "_");
    if *row_offset < 0 {
        format!("{column_name}_u{}", -row_offset)
    } else {
        format!("{column_name}_d{row_offset}")
    }
}

impl<T: FieldElement> From<SymbolicExpression<T>> for AffineSymbolicExpression<T> {
    fn from(k: SymbolicExpression<T>) -> Self {
        AffineSymbolicExpression {
            coefficients: Default::default(),
            offset: k,
        }
    }
}

impl<T: FieldElement> AffineSymbolicExpression<T> {
    pub fn from_known_cell(cell: &Cell) -> Self {
        SymbolicExpression::from_known_cell(cell).into()
    }
    pub fn from_unknown_cell(cell: &Cell) -> Self {
        AffineSymbolicExpression {
            coefficients: [(cell.clone(), T::from(1).into())].into_iter().collect(),
            offset: SymbolicExpression::from(T::from(0)),
        }
    }
    pub fn from_number(n: T) -> Self {
        SymbolicExpression::from(n).into()
    }

    pub fn is_known_zero(&self) -> bool {
        self.coefficients.is_empty() && self.offset.is_known_zero()
    }

    pub fn is_known_one(&self) -> bool {
        self.coefficients.is_empty() && self.offset.is_known_one()
    }

    pub fn is_known(&self) -> bool {
        self.coefficients.is_empty()
    }

    /// Tries to multiply this expression with another one.
    /// Returns `None` if the result would be quadratic.
    pub fn try_mul(&self, other: &Self) -> Option<Self> {
        if self.is_known_zero() || other.is_known_zero() {
            return Some(AffineSymbolicExpression::from_number(T::from(0)));
        }
        if !self.coefficients.is_empty() && !other.coefficients.is_empty() {
            return None;
        }
        let (multiplier, coefficients, offset) = if self.coefficients.is_empty() {
            (&self.offset, &other.coefficients, &other.offset)
        } else {
            (&other.offset, &self.coefficients, &self.offset)
        };
        let coefficients = coefficients
            .iter()
            .map(|(cell, coeff)| (cell.clone(), coeff * multiplier))
            .collect();
        let offset = offset * multiplier;
        Some(AffineSymbolicExpression {
            coefficients,
            offset,
        })
    }

    /// Solves the equation `self = 0` and returns how to compute the solution.
    pub fn solve(&self, range_constraints: &impl RangeConstraintSet<Cell, T>) -> Vec<Effect<T>> {
        match self.coefficients.len() {
            0 => {
                return if self.offset.is_known_zero() {
                    vec![]
                } else {
                    // TODO this is a non-satisfiable constraint - should we panic?
                    vec![]
                };
            }
            1 => {
                let (cell, coeff) = self.coefficients.iter().next().unwrap();
                assert!(!coeff.is_known_zero());
                let value = &self.offset / &coeff.neg();
                return vec![Effect::Assignment(cell.clone(), value)];
            }
            _ => {}
        }

        let r = self.solve_through_constraints(range_constraints);
        if !r.is_empty() {
            return r;
        }
        let negated = -self;
        let r = negated.solve_through_constraints(range_constraints);
        if !r.is_empty() {
            return r;
        }
        self.transfer_constraints(range_constraints)
            .into_iter()
            .chain(self.transfer_constraints(range_constraints))
            .collect()
    }

    fn solve_through_constraints(
        &self,
        range_constraints: &impl RangeConstraintSet<Cell, T>,
    ) -> Vec<Effect<T>> {
        let constrained_coefficients = self
            .coefficients
            .iter()
            .filter_map(|(cell, coeff)| {
                coeff.try_to_number().and_then(|c| {
                    range_constraints
                        .range_constraint(cell.clone())
                        .map(|rc| (cell.clone(), c, rc))
                })
            })
            .collect_vec();

        // All the coefficients need to have known range constraints.
        if constrained_coefficients.len() != self.coefficients.len() {
            return Vec::new();
        }

        // Check if they are mutually exclusive and compute assignments.
        let mut covered_bits: <T as FieldElement>::Integer = 0.into();
        let mut assignments = vec![];
        for (cell, coeff, constraint) in constrained_coefficients {
            let mask = *constraint.multiple(coeff).mask();
            if !(mask & covered_bits).is_zero() {
                // Overlapping range constraints.
                return vec![];
            } else {
                covered_bits |= mask;
            }
            let mask = T::from(mask);
            let masked = &(-&self.offset) & &mask.into();
            let rhs = &masked / &coeff.into();
            assignments.push(Effect::Assignment(cell.clone(), rhs));
            // TODO can we store the cange constraint in RHS directly?
            assignments.push(Effect::RangeConstraint(
                cell.clone(),
                // TODO is that the right constraint?
                constraint,
            ));
        }

        if covered_bits >= T::modulus() {
            return vec![];
        }

        // TODO we need to add an assertion that offset is covered by the masks.
        // Otherwise the equation is not solvable.
        // TODO is this really the case?
        assignments
    }

    fn transfer_constraints(
        &self,
        range_constraints: &impl RangeConstraintSet<Cell, T>,
    ) -> Vec<Effect<T>> {
        // We are looking for X = a * Y + b * Z + ... or -X = a * Y + b * Z + ...
        // where X is least constrained.

        let Some((solve_for, solve_for_coefficient)) = self
            .coefficients
            .iter()
            .filter(|(_cell, coeff)| coeff.is_known_one() || coeff.is_known_minus_one())
            .max_by_key(|(cell, _c)| {
                // Sort so that we get the least constrained variable.
                range_constraints
                    .range_constraint((*cell).clone())
                    .map(|c| c.range_width())
                    .unwrap_or_else(|| T::modulus())
            })
        else {
            return vec![];
        };

        // This only works if the coefficients are all known.
        let Some(summands) = self
            .coefficients
            .iter()
            .filter(|(cell, _)| *cell != solve_for)
            .map(|(cell, coeff)| {
                coeff.try_to_number().and_then(|coeff| {
                    range_constraints
                        .range_constraint(cell.clone())
                        .map(|constraint| constraint.multiple(coeff))
                })
            })
            .chain(std::iter::once(self.offset.range_constraint()))
            .collect::<Option<Vec<_>>>()
        else {
            return vec![];
        };
        let Some(constraint) = summands.into_iter().reduce(|c1, c2| c1.combine_sum(&c2)) else {
            return vec![];
        };
        let constraint = if solve_for_coefficient.is_known_one() {
            -constraint
        } else {
            constraint
        };
        vec![Effect::RangeConstraint(solve_for.clone(), constraint)]
    }
}

impl<T: FieldElement> Add for &AffineSymbolicExpression<T> {
    type Output = AffineSymbolicExpression<T>;

    fn add(self, rhs: Self) -> Self::Output {
        // TODO could iterate in parallel.
        let mut coefficients = self.coefficients.clone();
        for (cell, coeff) in rhs.coefficients.iter() {
            coefficients
                .entry(cell.clone())
                .and_modify(|f| *f = &*f + coeff)
                .or_insert_with(|| coeff.clone());
        }
        let coefficients = coefficients
            .into_iter()
            .filter(|(_, f)| !f.is_known_zero())
            .collect();
        let offset = &self.offset + &rhs.offset;
        AffineSymbolicExpression {
            coefficients,
            offset,
        }
    }
}

impl<T: FieldElement> Sub for &AffineSymbolicExpression<T> {
    type Output = AffineSymbolicExpression<T>;

    fn sub(self, rhs: Self) -> Self::Output {
        self + &-rhs
    }
}

impl<T: FieldElement> Neg for &AffineSymbolicExpression<T> {
    type Output = AffineSymbolicExpression<T>;

    fn neg(self) -> Self::Output {
        AffineSymbolicExpression {
            coefficients: self
                .coefficients
                .iter()
                .map(|(cell, coeff)| (cell.clone(), -coeff))
                .collect(),
            offset: -&self.offset,
        }
    }
}

impl<T: FieldElement> TryFrom<&AffineSymbolicExpression<T>> for SymbolicExpression<T> {
    type Error = ();

    fn try_from(value: &AffineSymbolicExpression<T>) -> Result<Self, Self::Error> {
        if value.coefficients.is_empty() {
            Ok(value.offset.clone())
        } else {
            Err(())
        }
    }
}
