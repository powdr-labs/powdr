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

use super::{super::range_constraints::RangeConstraint, cell::Cell, witgen_inference::Effect};

/// A value that is known at run-time, either through a symbolic expression
/// or its value is already known at compile-time and fixed.
#[derive(Debug, Clone)]
pub enum KnownValue<T: FieldElement> {
    /// A concrete constant value known at compile time.
    Concrete(T),
    /// A symbolic value known at run-time.
    Symbolic(String, Option<RangeConstraint<T>>), // TODO change the string to a proper expression, then we also don't need NegatedSymbolic
    NegatedSymbolic(String, Option<RangeConstraint<T>>),
}

impl<T: FieldElement> KnownValue<T> {
    pub fn is_known_zero(&self) -> bool {
        self.try_to_number().map_or(false, |n| n.is_zero())
    }

    pub fn is_known_one(&self) -> bool {
        self.try_to_number().map_or(false, |n| n.is_one())
    }

    pub fn is_known_minus_one(&self) -> bool {
        self.try_to_number().map_or(false, |n| n == -T::from(1))
    }

    pub fn range_constraint(&self) -> Option<RangeConstraint<T>> {
        match self {
            KnownValue::Concrete(v) => Some(RangeConstraint::from_value(*v)),
            KnownValue::Symbolic(_, rc) => rc.clone(),
            KnownValue::NegatedSymbolic(_, rc) => rc.clone(),
        }
    }

    pub fn try_to_number(&self) -> Option<T> {
        match self {
            KnownValue::Concrete(n) => Some(*n),
            KnownValue::Symbolic(_, rc) | KnownValue::NegatedSymbolic(_, rc) => {
                rc.as_ref().and_then(|rc| rc.try_to_single_value())
            }
        }
    }
}

impl<T: FieldElement> Display for KnownValue<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            KnownValue::Concrete(n) => {
                if n.is_in_lower_half() {
                    write!(f, "{n}")
                } else {
                    write!(f, "-{}", -*n)
                }
            }
            KnownValue::Symbolic(s, _) => write!(f, "{s}"),
            KnownValue::NegatedSymbolic(s, _) => write!(f, "-{s}"),
        }
    }
}

impl<T: FieldElement> KnownValue<T> {
    pub fn from_known_cell(cell: &Cell) -> Self {
        KnownValue::Symbolic(
            format!("get(state, {}, {})", cell.row_offset, cell.id),
            None,
        )
    }

    pub fn from_known_local_var(name: &str) -> Self {
        KnownValue::Symbolic(name.to_string(), None)
    }
}

impl<T: FieldElement> From<T> for KnownValue<T> {
    fn from(n: T) -> Self {
        KnownValue::Concrete(n)
    }
}

impl<T: FieldElement> Add for &KnownValue<T> {
    type Output = KnownValue<T>;

    fn add(self, rhs: Self) -> Self::Output {
        if self.is_known_zero() {
            return rhs.clone();
        }
        if rhs.is_known_zero() {
            return self.clone();
        }
        match (self, rhs) {
            (KnownValue::Concrete(a), KnownValue::Concrete(b)) => KnownValue::Concrete(*a + *b),
            _ => {
                let rc = self
                    .range_constraint()
                    .zip(rhs.range_constraint())
                    .map(|(a, b)| a.combine_sum(&b));
                KnownValue::Symbolic(format!("{self} + {rhs}"), rc)
            }
        }
    }
}

impl<T: FieldElement> Neg for &KnownValue<T> {
    type Output = KnownValue<T>;

    fn neg(self) -> Self::Output {
        match self {
            KnownValue::Concrete(n) => KnownValue::Concrete(-*n),
            KnownValue::Symbolic(s, rc) => KnownValue::NegatedSymbolic(
                s.clone(),
                rc.as_ref().map(|rc| rc.multiple(-T::from(1))),
            ),
            KnownValue::NegatedSymbolic(s, rc) => {
                KnownValue::Symbolic(s.clone(), rc.as_ref().map(|rc| rc.multiple(-T::from(1))))
            }
        }
    }
}

impl<T: FieldElement> Mul for &KnownValue<T> {
    type Output = KnownValue<T>;

    fn mul(self, rhs: Self) -> Self::Output {
        if self.is_known_zero() || rhs.is_known_zero() {
            return KnownValue::Concrete(T::from(0));
        }
        if self.is_known_one() {
            return rhs.clone();
        }
        if rhs.is_known_one() {
            return self.clone();
        }
        if self.is_known_minus_one() {
            return -rhs;
        }
        if rhs.is_known_minus_one() {
            return -self;
        }
        match (self, rhs) {
            (KnownValue::Concrete(a), KnownValue::Concrete(b)) => KnownValue::Concrete(*a * *b),
            _ => {
                let rc = self
                    .range_constraint()
                    .zip(rhs.range_constraint())
                    .and_then(|(a, b)| {
                        if let Some(v) = a.try_to_single_value() {
                            Some(b.multiple(v))
                        } else {
                            b.try_to_single_value().map(|v| a.multiple(v))
                        }
                    });
                KnownValue::Symbolic(format!("{self} * {rhs}"), rc)
            }
        }
    }
}

/// Represents an expression `a_1 * cell_1 + ... + a_k * cell_k + offset`.
#[derive(Debug, Clone)]
pub struct EvalResult<T: FieldElement> {
    coefficients: BTreeMap<Cell, KnownValue<T>>,
    offset: KnownValue<T>,
}

impl<T: FieldElement> Display for EvalResult<T> {
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

fn cell_to_variable(
    Cell {
        column_name,
        row_offset,
        ..
    }: &Cell,
) -> String {
    format!("get(state, {}, {})", row_offset, column_name)
    // if *row_offset < 0 {
    //     format!("{column_name}_u{}", -row_offset)
    // } else {
    //     format!("{column_name}_d{row_offset}")
    // }
}

impl<T: FieldElement> From<KnownValue<T>> for EvalResult<T> {
    fn from(k: KnownValue<T>) -> Self {
        EvalResult {
            coefficients: Default::default(),
            offset: k,
        }
    }
}

impl<T: FieldElement> EvalResult<T> {
    pub fn from_known_cell(cell: &Cell) -> Self {
        KnownValue::from_known_cell(cell).into()
    }
    pub fn from_unknown_cell(cell: &Cell) -> Self {
        EvalResult {
            coefficients: [(cell.clone(), T::from(1).into())].into_iter().collect(),
            offset: KnownValue::from(T::from(0)),
        }
    }
    pub fn from_number(n: T) -> Self {
        KnownValue::from(n).into()
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

    pub fn try_mul(&self, other: &Self) -> Option<Self> {
        if self.is_known_zero() || other.is_known_zero() {
            return Some(EvalResult::from_number(T::from(0)));
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
        Some(EvalResult {
            coefficients,
            offset,
        })
    }

    pub fn solve(&self, range_constraints: &impl RangeConstraintSet<Cell, T>) -> Vec<Effect<T>> {
        match self.coefficients.len() {
            0 => {
                return if self.offset.is_known_zero() {
                    vec![]
                } else {
                    // TODO add assertion?
                    vec![]
                };
            }
            1 => {
                let (cell, coeff) = self.coefficients.iter().next().unwrap();
                assert!(!coeff.is_known_zero());
                let (assignment, rc) = if coeff.is_known_one() {
                    let v = -&self.offset;
                    (v.to_string(), v.range_constraint())
                } else if coeff.is_known_minus_one() {
                    (self.offset.to_string(), self.offset.range_constraint())
                } else {
                    // TODO range constraint?
                    (format!("{} / {coeff}", -&self.offset), None)
                };
                return once(Effect::Assignment(
                    cell.clone(),
                    //format!("let {} = {assignment}", cell_to_variable(cell)),
                    format!(
                        "set(state, {}, {}, {assignment});",
                        cell.row_offset, cell.id
                    ),
                ))
                .chain(rc.map(|rc| Effect::RangeConstraint(cell.clone(), rc)))
                .collect();
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

        // // Check if they are mutually exclusive and compute assignments.
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
            let masked = format!("{} & {mask:#x}", -&self.offset);
            let rhs = if coeff.is_one() {
                masked
            } else if coeff == -T::from(1) {
                format!("-({masked})")
            } else {
                format!("({masked}) / {coeff}")
            };
            assignments.push(Effect::Assignment(
                cell.clone(),
                format!("set(state, {}, {}, {rhs});", cell.row_offset, cell.id),
            ));
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

impl<T: FieldElement> Add for &EvalResult<T> {
    type Output = EvalResult<T>;

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
        EvalResult {
            coefficients,
            offset,
        }
    }
}

impl<T: FieldElement> Sub for &EvalResult<T> {
    type Output = EvalResult<T>;

    fn sub(self, rhs: Self) -> Self::Output {
        self + &-rhs
    }
}

impl<T: FieldElement> Neg for &EvalResult<T> {
    type Output = EvalResult<T>;

    fn neg(self) -> Self::Output {
        EvalResult {
            coefficients: self
                .coefficients
                .iter()
                .map(|(cell, coeff)| (cell.clone(), -coeff))
                .collect(),
            offset: -&self.offset,
        }
    }
}

impl<T: FieldElement> TryFrom<&EvalResult<T>> for KnownValue<T> {
    type Error = ();

    fn try_from(value: &EvalResult<T>) -> Result<Self, Self::Error> {
        if value.coefficients.is_empty() {
            Ok(value.offset.clone())
        } else {
            Err(())
        }
    }
}
