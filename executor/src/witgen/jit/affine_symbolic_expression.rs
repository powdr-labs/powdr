use std::{
    collections::BTreeMap,
    fmt::{self, Display, Formatter},
    ops::{Add, Mul, Neg, Sub},
};

use itertools::Itertools;
use num_traits::Zero;
use powdr_number::FieldElement;

use crate::witgen::EvalError;

use super::{super::range_constraints::RangeConstraint, symbolic_expression::SymbolicExpression};

/// The effect of solving a symbolic equation.
pub enum Effect<T: FieldElement, V> {
    /// variable can be assigned a value.
    Assignment(V, SymbolicExpression<T, V>),
    /// we learnt a new range constraint on variable.
    RangeConstraint(V, RangeConstraint<T>),
    /// a run-time assertion. If this fails, we have conflicting constraints.
    Assertion(Assertion<T, V>),
}

/// A run-time assertion. If this fails, we have conflicting constraints.
pub struct Assertion<T: FieldElement, V> {
    pub lhs: SymbolicExpression<T, V>,
    pub rhs: SymbolicExpression<T, V>,
    /// If this is true, we assert that both sides are equal.
    /// Otherwise, we assert that they are different.
    pub expected_equal: bool,
}

impl<T: FieldElement, V> Assertion<T, V> {
    pub fn assert_is_zero(condition: SymbolicExpression<T, V>) -> Effect<T, V> {
        Self::assert_eq(condition, SymbolicExpression::from(T::from(0)))
    }
    pub fn assert_is_nonzero(condition: SymbolicExpression<T, V>) -> Effect<T, V> {
        Self::assert_neq(condition, SymbolicExpression::from(T::from(0)))
    }
    pub fn assert_eq(lhs: SymbolicExpression<T, V>, rhs: SymbolicExpression<T, V>) -> Effect<T, V> {
        Effect::Assertion(Assertion {
            lhs,
            rhs,
            expected_equal: true,
        })
    }
    pub fn assert_neq(
        lhs: SymbolicExpression<T, V>,
        rhs: SymbolicExpression<T, V>,
    ) -> Effect<T, V> {
        Effect::Assertion(Assertion {
            lhs,
            rhs,
            expected_equal: false,
        })
    }
}

/// Represents an expression `a_1 * x_1 + ... + a_k * x_k + offset`,
/// where the `a_i` and `offset` are symbolic expressions, i.e. values known at run-time
/// (which can still include variables or symbols, which are only known at run-time),
/// and the `x_i` are variables that are unknown at this point.
/// It also stores range constraints for all unknown variables.
#[derive(Debug, Clone)]
pub struct AffineSymbolicExpression<T: FieldElement, V> {
    coefficients: BTreeMap<V, SymbolicExpression<T, V>>,
    offset: SymbolicExpression<T, V>,
    range_constraints: BTreeMap<V, RangeConstraint<T>>,
}

/// Display for affine symbolic expressions, for informational purposes only.
impl<T: FieldElement, V: Display> Display for AffineSymbolicExpression<T, V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.coefficients.is_empty() {
            write!(f, "{}", self.offset)
        } else {
            write!(
                f,
                "{}",
                self.coefficients
                    .iter()
                    .map(|(var, coeff)| if coeff.is_known_one() {
                        var.to_string()
                    } else if coeff.is_known_minus_one() {
                        format!("-{var}")
                    } else {
                        format!("{coeff} * {var}")
                    })
                    .join(" + ")
            )?;
            if !self.offset.is_known_zero() {
                write!(f, " + {}", self.offset)?;
            }
            Ok(())
        }
    }
}

impl<T: FieldElement, V> From<SymbolicExpression<T, V>> for AffineSymbolicExpression<T, V> {
    fn from(k: SymbolicExpression<T, V>) -> Self {
        AffineSymbolicExpression {
            coefficients: Default::default(),
            offset: k,
            range_constraints: Default::default(),
        }
    }
}

impl<T: FieldElement, V: Ord + Clone + Display> AffineSymbolicExpression<T, V> {
    pub fn from_known_variable(var: V, rc: Option<RangeConstraint<T>>) -> Self {
        SymbolicExpression::from_var(var, rc).into()
    }
    pub fn from_unknown_variable(var: V, rc: Option<RangeConstraint<T>>) -> Self {
        AffineSymbolicExpression {
            coefficients: [(var.clone(), T::from(1).into())].into_iter().collect(),
            offset: SymbolicExpression::from(T::from(0)),
            range_constraints: rc.into_iter().map(|rc| (var.clone(), rc)).collect(),
        }
    }
    pub fn from_number(n: T) -> Self {
        SymbolicExpression::from(n).into()
    }

    /// If this expression does not contain unknown variables, returns the symbolic expression.
    pub fn try_to_known(&self) -> Option<&SymbolicExpression<T, V>> {
        if self.coefficients.is_empty() {
            Some(&self.offset)
        } else {
            None
        }
    }

    /// Tries to multiply this expression with another one.
    /// Returns `None` if the result would be quadratic, i.e.
    /// if both expressions contain unknown variables.
    pub fn try_mul(&self, other: &Self) -> Option<Self> {
        if let Some(multiplier) = other.try_to_known() {
            Some(self.clone() * multiplier)
        } else if let Some(multiplier) = self.try_to_known() {
            Some(other.clone() * multiplier)
        } else {
            None
        }
    }

    /// Solves the equation `self = 0` and returns how to compute the solution.
    pub fn solve(&self) -> Result<Vec<Effect<T, V>>, EvalError<T>> {
        Ok(match self.coefficients.len() {
            0 => {
                if self.offset.is_known_zero() {
                    vec![]
                } else {
                    return Err(EvalError::ConstraintUnsatisfiable(self.to_string()));
                }
            }
            1 => {
                let (var, coeff) = self.coefficients.iter().next().unwrap();
                // Solve "coeff * X + self.offset = 0" by division.
                assert!(
                    !coeff.is_known_zero(),
                    "Zero coefficient has not been removed."
                );
                if coeff.is_known_nonzero() {
                    // In this case, we can always compute a solution.
                    let value = self.offset.field_div(&-coeff);
                    vec![Effect::Assignment(var.clone(), value)]
                } else if self.offset.is_known_nonzero() {
                    // If the offset is not zero, then the coefficient must be non-zero,
                    // otherwise the constraint is violated.
                    let value = self.offset.field_div(&-coeff);
                    vec![
                        Assertion::assert_is_nonzero(coeff.clone()),
                        Effect::Assignment(var.clone(), value),
                    ]
                } else {
                    // If this case, we could have an equation of the form
                    // 0 * X = 0, which is valid and generates no information about X.
                    vec![]
                }
            }
            _ => {
                let r = self.solve_bit_decomposition();
                if !r.is_empty() {
                    r
                } else {
                    let negated = -self;
                    let r = negated.solve_bit_decomposition();
                    if !r.is_empty() {
                        r
                    } else {
                        self.transfer_constraints()
                            .into_iter()
                            .chain(negated.transfer_constraints())
                            .collect()
                    }
                }
            }
        })
    }

    /// Tries to solve a bit-decomposition equation.
    fn solve_bit_decomposition(&self) -> Vec<Effect<T, V>> {
        // All the coefficients need to be known numbers and the
        // variables need to be range-constrained.
        let constrained_coefficients = self
            .coefficients
            .iter()
            .map(|(var, coeff)| {
                let c = coeff.try_to_number()?;
                let rc = self.range_constraints.get(var)?;
                Some((var.clone(), c, rc))
            })
            .collect::<Option<Vec<_>>>();
        let Some(constrained_coefficients) = constrained_coefficients else {
            return vec![];
        };

        // Check if they are mutually exclusive and compute assignments.
        let mut covered_bits: <T as FieldElement>::Integer = 0.into();
        let mut effects = vec![];
        for (var, coeff, constraint) in constrained_coefficients {
            let mask = *constraint.multiple(coeff).mask();
            if !(mask & covered_bits).is_zero() {
                // Overlapping range constraints.
                return vec![];
            } else {
                covered_bits |= mask;
            }
            let masked = -&self.offset & T::from(mask).into();
            effects.push(Effect::Assignment(
                var.clone(),
                masked.integer_div(&coeff.into()),
            ));
        }

        if covered_bits >= T::modulus() {
            return vec![];
        }

        // We need to assert that the masks cover the offset,
        // otherwise the equation is not solvable.
        // We assert offset & !masks == 0 <=> offset == offset | masks.
        // We use the latter since we cannot properly bit-negate inside the field.
        effects.push(Assertion::assert_eq(
            self.offset.clone(),
            &self.offset | &T::from(covered_bits).into(),
        ));

        effects
    }

    fn transfer_constraints(&self) -> Vec<Effect<T, V>> {
        // We are looking for X = a * Y + b * Z + ... or -X = a * Y + b * Z + ...
        // where X is least constrained.

        let Some((solve_for, solve_for_coefficient)) = self
            .coefficients
            .iter()
            .filter(|(_var, coeff)| coeff.is_known_one() || coeff.is_known_minus_one())
            .max_by_key(|(var, _c)| {
                // Sort so that we get the least constrained variable.
                self.range_constraints
                    .get(var)
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
            .filter(|(var, _)| *var != solve_for)
            .map(|(var, coeff)| {
                let coeff = coeff.try_to_number()?;
                let rc = self.range_constraints.get(var)?;
                Some(rc.multiple(coeff))
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

impl<T: FieldElement, V: Clone + Ord> Add for &AffineSymbolicExpression<T, V> {
    type Output = AffineSymbolicExpression<T, V>;

    fn add(self, rhs: Self) -> Self::Output {
        let mut coefficients = self.coefficients.clone();
        let mut range_constraints = self.range_constraints.clone();
        for (var, coeff) in &rhs.coefficients {
            coefficients
                .entry(var.clone())
                .and_modify(|f| *f = &*f + coeff)
                .or_insert_with(|| coeff.clone());
            if let Some(range_right) = rhs.range_constraints.get(var) {
                range_constraints
                    .entry(var.clone())
                    .and_modify(|rc| *rc = rc.conjunction(range_right))
                    .or_insert_with(|| range_right.clone());
            }
        }
        coefficients.retain(|_, f| !f.is_known_zero());
        let offset = &self.offset + &rhs.offset;
        AffineSymbolicExpression {
            coefficients,
            offset,
            range_constraints,
        }
    }
}

impl<T: FieldElement, V: Clone + Ord> Add for AffineSymbolicExpression<T, V> {
    type Output = AffineSymbolicExpression<T, V>;

    fn add(self, rhs: Self) -> Self::Output {
        &self + &rhs
    }
}

impl<T: FieldElement, V: Clone + Ord> Sub for &AffineSymbolicExpression<T, V> {
    type Output = AffineSymbolicExpression<T, V>;

    fn sub(self, rhs: Self) -> Self::Output {
        self + &-rhs
    }
}

impl<T: FieldElement, V: Clone + Ord> Sub for AffineSymbolicExpression<T, V> {
    type Output = AffineSymbolicExpression<T, V>;

    fn sub(self, rhs: Self) -> Self::Output {
        &self - &rhs
    }
}

impl<T: FieldElement, V: Clone + Ord> Neg for &AffineSymbolicExpression<T, V> {
    type Output = AffineSymbolicExpression<T, V>;

    fn neg(self) -> Self::Output {
        AffineSymbolicExpression {
            coefficients: self
                .coefficients
                .iter()
                .map(|(var, coeff)| (var.clone(), -coeff))
                .collect(),
            offset: -&self.offset,
            range_constraints: self.range_constraints.clone(),
        }
    }
}

impl<T: FieldElement, V: Clone + Ord> Neg for AffineSymbolicExpression<T, V> {
    type Output = AffineSymbolicExpression<T, V>;

    fn neg(self) -> Self::Output {
        -&self
    }
}

/// Multiply by known symbolic expression.
impl<T: FieldElement, V: Clone + Ord> Mul<&SymbolicExpression<T, V>>
    for AffineSymbolicExpression<T, V>
{
    type Output = AffineSymbolicExpression<T, V>;

    fn mul(mut self, rhs: &SymbolicExpression<T, V>) -> Self::Output {
        for coeff in self.coefficients.values_mut() {
            *coeff = &*coeff * rhs;
        }
        self.offset = &self.offset * rhs;
        self
    }
}

#[cfg(test)]
mod test {
    use powdr_number::GoldilocksField;

    use super::*;

    type Ase = AffineSymbolicExpression<GoldilocksField, &'static str>;

    fn from_number(x: i32) -> Ase {
        Ase::from_number(GoldilocksField::from(x))
    }

    fn mul(a: &Ase, b: &Ase) -> Ase {
        a.try_mul(b).unwrap()
    }

    #[test]
    fn unsolvable() {
        let r = from_number(10).solve();
        assert!(r.is_err());
    }

    #[test]
    fn unsolvable_with_vars() {
        let x = &Ase::from_known_variable("X", None);
        let y = &Ase::from_known_variable("Y", None);
        let constr = x + y - from_number(10);
        let r = constr.solve();
        assert!(r.is_err());
    }

    #[test]
    fn solvable_without_vars() {
        let constr = &from_number(0);
        assert!(constr.solve().unwrap().is_empty());
    }

    #[test]
    fn solve_simple_eq() {
        let y = Ase::from_known_variable("y", None);
        let x = Ase::from_unknown_variable("X", None);
        // 2 * X + 7 * y - 10 = 0
        let two = from_number(2);
        let seven = from_number(7);
        let ten = from_number(10);
        let constr = mul(&two, &x) + mul(&seven, &y) - ten;
        let effects = constr.solve().unwrap();
        assert_eq!(effects.len(), 1);
        let Effect::Assignment(var, expr) = &effects[0] else {
            panic!("Expected assignment");
        };
        assert_eq!(var.to_string(), "X");
        assert_eq!(expr.to_string(), "(((7 * y) + -10) / -2)");
    }

    #[test]
    fn solve_div_by_range_constrained_var() {
        let y = Ase::from_known_variable("y", None);
        let z = Ase::from_known_variable("z", None);
        let x = Ase::from_unknown_variable("X", None);
        // z * X + 7 * y - 10 = 0
        let seven = from_number(7);
        let ten = from_number(10);
        let constr = mul(&z, &x) + mul(&seven, &y) - ten.clone();
        // If we do not range-constrain z, we cannot solve since we don't know if it might be zero.
        let effects = constr.solve().unwrap();
        assert_eq!(effects.len(), 0);
        let z =
            Ase::from_known_variable("z", Some(RangeConstraint::from_range(10.into(), 20.into())));
        let constr = mul(&z, &x) + mul(&seven, &y) - ten;
        let effects = constr.solve().unwrap();
        let Effect::Assignment(var, expr) = &effects[0] else {
            panic!("Expected assignment");
        };
        assert_eq!(var.to_string(), "X");
        assert_eq!(expr.to_string(), "(((7 * y) + -10) / -z)");
    }

    #[test]
    fn solve_bit_decomposition() {
        let rc = Some(RangeConstraint::from_mask(0xffu32));
        // First try without range constrain on a
        let a = Ase::from_unknown_variable("a", None);
        let b = Ase::from_unknown_variable("b", rc.clone());
        let c = Ase::from_unknown_variable("c", rc.clone());
        let z = Ase::from_known_variable("Z", None);
        // a * 0x100 + b * 0x10000 + c * 0x1000000 + 10 + Z = 0
        let ten = from_number(10);
        let constr = mul(&a, &from_number(0x100))
            + mul(&b, &from_number(0x10000))
            + mul(&c, &from_number(0x1000000))
            + ten.clone()
            + z.clone();
        // Without range constraints, this is not solvable.
        assert!(constr.solve().unwrap().is_empty());
        // Now add the range constraint on a, it should be solvable.
        let a = Ase::from_unknown_variable("a", rc.clone());
        let constr = mul(&a, &from_number(0x100))
            + mul(&b, &from_number(0x10000))
            + mul(&c, &from_number(0x1000000))
            + ten.clone()
            + z;
        let effects = constr
            .solve()
            .unwrap()
            .into_iter()
            .map(|effect| match effect {
                Effect::Assignment(v, expr) => format!("{v} = {expr};\n"),
                Effect::Assertion(Assertion {
                    lhs,
                    rhs,
                    expected_equal,
                }) => {
                    format!(
                        "assert {lhs} {} {rhs};\n",
                        if expected_equal { "==" } else { "!=" }
                    )
                }
                _ => panic!(),
            })
            .format("")
            .to_string();
        assert_eq!(
            effects,
            "a = ((-(10 + Z) & 65280) // 256);
b = ((-(10 + Z) & 16711680) // 65536);
c = ((-(10 + Z) & 4278190080) // 16777216);
assert (10 + Z) == ((10 + Z) | 4294967040);
"
        );
    }

    #[test]
    fn solve_constraint_transfer() {
        let rc = Some(RangeConstraint::from_mask(0xffu32));
        let a = Ase::from_unknown_variable("a", rc.clone());
        let b = Ase::from_unknown_variable("b", rc.clone());
        let c = Ase::from_unknown_variable("c", rc.clone());
        let z = Ase::from_unknown_variable("Z", None);
        // a * 0x100 + b * 0x10000 + c * 0x1000000 + 10 - Z = 0
        let ten = from_number(10);
        let constr = mul(&a, &from_number(0x100))
            + mul(&b, &from_number(0x10000))
            + mul(&c, &from_number(0x1000000))
            + ten
            - z;
        let effects = constr
            .solve()
            .unwrap()
            .into_iter()
            .map(|effect| match effect {
                Effect::RangeConstraint(v, rc) => format!("{v}: {rc};\n"),
                _ => panic!(),
            })
            .format("")
            .to_string();
        // It appears twice because we solve the positive and the negated equation.
        // Note that the negated version has a different bit mask.
        assert_eq!(
            effects,
            "Z: [10, 4294967050] & 0xffffff0a;
Z: [10, 4294967050] & 0xffffffff;
"
        );
    }
}
