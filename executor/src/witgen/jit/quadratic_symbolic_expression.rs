use std::{
    collections::BTreeMap,
    fmt::Display,
    hash::Hash,
    ops::{Add, AddAssign, Mul, MulAssign, Neg, Sub},
};

use itertools::Itertools;
use powdr_number::FieldElement;

use crate::witgen::range_constraints::RangeConstraint;

use super::symbolic_expression::SymbolicExpression;

/// A symbolic expression in unknown variables of type `V` and (symbolically)
/// known terms, representing a sum of (super-)quadratic, linear and constant parts.
/// The quadratic terms are of the form `X * Y`, where `X` and `Y` are
/// `QuadraticSymbolicExpression`s that have at least one unknown.
/// The linear terms are of the form `a * X`, where `a` is a (symbolically) known
/// value and `X` is an unknown variable.
/// The constant term is a (symbolically) known value.
///
/// It also provides ways to quickly update the expression when the value of
/// an unknown variable gets known and provides functions to solve
/// (some kinds of) equations.
#[derive(Debug, Clone)]
pub struct QuadraticSymbolicExpression<T: FieldElement, V> {
    /// Quadratic terms of the form `a * X * Y`, where `a` is a (symbolically)
    /// known value and `X` and `Y` are quadratic symbolic expressions that
    /// have at least one unknown.
    quadratic: Vec<(Self, Self)>,
    /// Linear terms of the form `a * X`, where `a` is a (symbolically) known
    /// value and `X` is an unknown variable.
    linear: BTreeMap<V, SymbolicExpression<T, V>>,
    /// Constant term, a (symbolically) known value.
    constant: SymbolicExpression<T, V>,
}

// TODO We need occurrence lists for all variables, both in their unknon
// version and in their known version (in the symbolic expressions),
// because range constraints therein can also change.
// they could also change to simpler expressions if one sub-expression turns to one or zero.
// So we also need update functions for the symbolic expressions.

/// An update representing new information about the variable.
pub struct VariableUpdate<T: FieldElement, V> {
    pub variable: V,
    /// If true, the variable is symbolically or concretely known.
    pub known: bool,
    /// The current range constraint of the variable. It can be a single number.
    pub range_constraint: RangeConstraint<T>,
}

impl<T: FieldElement, V: Clone + Hash + Eq> From<SymbolicExpression<T, V>>
    for QuadraticSymbolicExpression<T, V>
{
    fn from(k: SymbolicExpression<T, V>) -> Self {
        Self {
            quadratic: Default::default(),
            linear: Default::default(),
            constant: k,
        }
    }
}

impl<T: FieldElement, V: Clone + Hash + Eq> From<T> for QuadraticSymbolicExpression<T, V> {
    fn from(k: T) -> Self {
        SymbolicExpression::from(k).into()
    }
}

impl<T: FieldElement, V: Ord + Clone + Hash + Eq> QuadraticSymbolicExpression<T, V> {
    pub fn from_known_symbol(symbol: V, rc: RangeConstraint<T>) -> Self {
        SymbolicExpression::from_symbol(symbol, rc).into()
    }
    pub fn from_unknown_variable(var: V) -> Self {
        Self {
            quadratic: Default::default(),
            linear: [(var.clone(), T::from(1).into())].into_iter().collect(),
            constant: T::from(0).into(),
        }
    }

    /// If this expression does not contain unknown variables, returns the symbolic expression.
    pub fn try_to_known(&self) -> Option<&SymbolicExpression<T, V>> {
        if self.quadratic.is_empty() && self.linear.is_empty() {
            Some(&self.constant)
        } else {
            None
        }
    }

    pub fn apply_update(&mut self, var_update: &VariableUpdate<T, V>) {
        let VariableUpdate {
            variable,
            known,
            range_constraint,
        } = var_update;
        self.constant.apply_update(var_update);
        // If the variable is a key in `linear`, it must be unknown
        // and thus can only occur there. Otherwise, it can be in
        // any symbolic expression.
        if self.linear.contains_key(variable) {
            if *known {
                let coeff = self.linear.remove(variable).unwrap();
                let expr =
                    SymbolicExpression::from_symbol(variable.clone(), range_constraint.clone());
                self.constant += expr * coeff;
                self.linear.remove(variable);
            }
        } else {
            for coeff in self.linear.values_mut() {
                coeff.apply_update(var_update);
            }
        }

        // TODO can we do that without moving everything?
        // In the end, the order does not matter much.

        let mut to_add = QuadraticSymbolicExpression::from(T::zero());
        self.quadratic.retain_mut(|(l, r)| {
            l.apply_update(var_update);
            r.apply_update(var_update);
            match (l.try_to_known(), r.try_to_known()) {
                (Some(l), Some(r)) => {
                    to_add += (l * r).into();
                    false
                }
                (Some(l), None) => {
                    to_add += r.clone() * l;
                    false
                }
                (None, Some(r)) => {
                    to_add += l.clone() * r;
                    false
                }
                _ => true,
            }
        });
        if to_add.try_to_known().map(|ta| ta.is_known_zero()) != Some(true) {
            *self += to_add;
        }
    }

    /// Returns the set of referenced variables, both know and unknown.
    pub fn referenced_variables(&self) -> Box<dyn Iterator<Item = &V> + '_> {
        let quadr = self
            .quadratic
            .iter()
            .flat_map(|(a, b)| a.referenced_variables().chain(b.referenced_variables()));

        let linear = self
            .linear
            .iter()
            .flat_map(|(var, coeff)| std::iter::once(var).chain(coeff.referenced_symbols()));
        let constant = self.constant.referenced_symbols();
        Box::new(quadr.chain(linear).chain(constant))
    }
}

impl<T: FieldElement, V: Clone + Ord + Hash + Eq> Add for QuadraticSymbolicExpression<T, V> {
    type Output = QuadraticSymbolicExpression<T, V>;

    fn add(mut self, rhs: Self) -> Self {
        self += rhs;
        self
    }
}

impl<T: FieldElement, V: Clone + Ord + Hash + Eq> Add for &QuadraticSymbolicExpression<T, V> {
    type Output = QuadraticSymbolicExpression<T, V>;

    fn add(self, rhs: Self) -> Self::Output {
        self.clone() + rhs.clone()
    }
}

impl<T: FieldElement, V: Clone + Ord + Hash + Eq> AddAssign<QuadraticSymbolicExpression<T, V>>
    for QuadraticSymbolicExpression<T, V>
{
    fn add_assign(&mut self, rhs: Self) {
        self.quadratic.extend(rhs.quadratic);
        for (var, coeff) in rhs.linear {
            self.linear
                .entry(var.clone())
                .and_modify(|f| *f += coeff.clone())
                .or_insert_with(|| coeff);
        }
        self.constant += rhs.constant.clone();
        self.linear.retain(|_, f| !f.is_known_zero());
    }
}

impl<T: FieldElement, V: Clone + Ord + Hash + Eq> Sub for &QuadraticSymbolicExpression<T, V> {
    type Output = QuadraticSymbolicExpression<T, V>;

    fn sub(self, rhs: Self) -> Self::Output {
        self + &-rhs
    }
}

impl<T: FieldElement, V: Clone + Ord + Hash + Eq> Sub for QuadraticSymbolicExpression<T, V> {
    type Output = QuadraticSymbolicExpression<T, V>;

    fn sub(self, rhs: Self) -> Self::Output {
        &self - &rhs
    }
}

impl<T: FieldElement, V: Clone + Ord> QuadraticSymbolicExpression<T, V> {
    fn negate(&mut self) {
        for (first, _) in &mut self.quadratic {
            first.negate()
        }
        for coeff in self.linear.values_mut() {
            *coeff = -coeff.clone();
        }
        self.constant = -self.constant.clone();
    }
}

impl<T: FieldElement, V: Clone + Ord> Neg for QuadraticSymbolicExpression<T, V> {
    type Output = QuadraticSymbolicExpression<T, V>;

    fn neg(mut self) -> Self {
        self.negate();
        self
    }
}

impl<T: FieldElement, V: Clone + Ord> Neg for &QuadraticSymbolicExpression<T, V> {
    type Output = QuadraticSymbolicExpression<T, V>;

    fn neg(self) -> Self::Output {
        -((*self).clone())
    }
}

/// Multiply by known symbolic expression.
impl<T: FieldElement, V: Clone + Ord + Hash + Eq> Mul<&SymbolicExpression<T, V>>
    for QuadraticSymbolicExpression<T, V>
{
    type Output = QuadraticSymbolicExpression<T, V>;

    fn mul(mut self, rhs: &SymbolicExpression<T, V>) -> Self {
        self *= rhs;
        self
    }
}

impl<T: FieldElement, V: Clone + Ord + Hash + Eq> Mul<SymbolicExpression<T, V>>
    for QuadraticSymbolicExpression<T, V>
{
    type Output = QuadraticSymbolicExpression<T, V>;

    fn mul(self, rhs: SymbolicExpression<T, V>) -> Self {
        self * &rhs
    }
}

impl<T: FieldElement, V: Clone + Ord + Hash + Eq> MulAssign<&SymbolicExpression<T, V>>
    for QuadraticSymbolicExpression<T, V>
{
    fn mul_assign(&mut self, rhs: &SymbolicExpression<T, V>) {
        if rhs.is_known_zero() {
            *self = T::zero().into();
        } else {
            for (first, _) in &mut self.quadratic {
                *first *= rhs;
            }
            for coeff in self.linear.values_mut() {
                *coeff *= rhs.clone();
            }
            self.constant *= rhs.clone();
        }
    }
}

impl<T: FieldElement, V: Clone + Ord + Hash + Eq> Mul for QuadraticSymbolicExpression<T, V> {
    type Output = QuadraticSymbolicExpression<T, V>;

    fn mul(self, rhs: QuadraticSymbolicExpression<T, V>) -> Self {
        if let Some(k) = rhs.try_to_known() {
            self * k
        } else if let Some(k) = self.try_to_known() {
            rhs * k
        } else {
            Self {
                quadratic: vec![(self, rhs)],
                linear: Default::default(),
                constant: T::from(0).into(),
            }
        }
    }
}

impl<T: FieldElement, V: Clone + Ord + Display> Display for QuadraticSymbolicExpression<T, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let formatted = self
            .quadratic
            .iter()
            .map(|(a, b)| format!("({a}) * ({b})"))
            .chain(
                self.linear
                    .iter()
                    .map(|(var, coeff)| match coeff.try_to_number() {
                        Some(k) if k == 1.into() => format!("{var}"),
                        Some(k) if k == (-1).into() => format!("-{var}"),
                        _ => format!("{coeff} * {var}"),
                    }),
            )
            .chain(match self.constant.try_to_number() {
                Some(k) if k == T::zero() => None,
                _ => Some(format!("{}", self.constant)),
            })
            .format(" + ")
            .to_string();
        write!(
            f,
            "{}",
            if formatted.is_empty() {
                "0".to_string()
            } else {
                formatted
            }
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::witgen::range_constraints::RangeConstraint;
    use powdr_number::GoldilocksField;

    use pretty_assertions::assert_eq;

    type Qse = QuadraticSymbolicExpression<GoldilocksField, String>;

    #[test]
    fn test_mul() {
        type Qse = QuadraticSymbolicExpression<GoldilocksField, String>;
        let x = Qse::from_unknown_variable("X".to_string());
        let y = Qse::from_unknown_variable("Y".to_string());
        let a = Qse::from_known_symbol("A".to_string(), RangeConstraint::default());
        let t = x * y + a;
        assert_eq!(t.to_string(), "(X) * (Y) + A");
    }

    #[test]
    fn test_add() {
        let x = Qse::from_unknown_variable("X".to_string());
        let y = Qse::from_unknown_variable("Y".to_string());
        let a = Qse::from_unknown_variable("A".to_string());
        let b = Qse::from_known_symbol("B".to_string(), RangeConstraint::default());
        let t: Qse = x * y - a + b;
        assert_eq!(t.to_string(), "(X) * (Y) + -A + B");
        assert_eq!(
            (t.clone() + t).to_string(),
            "(X) * (Y) + (X) * (Y) + -2 * A + (B + B)"
        );
    }

    #[test]
    fn test_mul_by_known() {
        let x = Qse::from_unknown_variable("X".to_string());
        let y = Qse::from_unknown_variable("Y".to_string());
        let a = Qse::from_known_symbol("A".to_string(), RangeConstraint::default());
        let b = Qse::from_known_symbol("B".to_string(), RangeConstraint::default());
        let t: Qse = (x * y + a) * b;
        assert_eq!(t.to_string(), "B * (X) * (Y) + B * A");
    }

    #[test]
    fn test_mul_by_zero() {
        let x = Qse::from_unknown_variable("X".to_string());
        let y = Qse::from_unknown_variable("Y".to_string());
        let a = Qse::from_known_symbol("A".to_string(), RangeConstraint::default());
        let zero = Qse::from(GoldilocksField::from(0));
        let t: Qse = x * y + a;
        assert_eq!(t.to_string(), "(X) * (Y) + A");
        assert_eq!((t.clone() * zero).to_string(), "0");
    }

    #[test]
    fn test_apply_update() {
        let x = Qse::from_unknown_variable("X".to_string());
        let y = Qse::from_unknown_variable("Y".to_string());
        let a = Qse::from_known_symbol("A".to_string(), RangeConstraint::default());
        let b = Qse::from_known_symbol("B".to_string(), RangeConstraint::default());
        let mut t: Qse = (x * y + a) * b;
        assert_eq!(t.to_string(), "(B * X) * (Y) + (A * B)");
        t.apply_update(&VariableUpdate {
            variable: "B".to_string(),
            known: true,
            range_constraint: RangeConstraint::from_value(7.into()),
        });
        assert_eq!(t.to_string(), "(7 * X) * (Y) + (A * 7)");
        t.apply_update(&VariableUpdate {
            variable: "X".to_string(),
            known: true,
            range_constraint: RangeConstraint::from_range(1.into(), 2.into()),
        });
        assert_eq!(t.to_string(), "(X * 7) * Y + (A * 7)");
        t.apply_update(&VariableUpdate {
            variable: "Y".to_string(),
            known: true,
            range_constraint: RangeConstraint::from_value(3.into()),
        });
        assert_eq!(t.to_string(), "((A * 7) + (3 * (X * 7)))");
    }
}
