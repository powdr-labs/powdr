use std::cmp::Ordering;

use std::iter;

use bit_vec::BitVec;
use itertools::Itertools;
use powdr_ast::indent;
use powdr_number::FieldElement;
use std::hash::Hash;

use crate::witgen::range_constraints::RangeConstraint;

use super::{symbolic_expression::SymbolicExpression, variable::Variable};

/// The effect of solving a symbolic equation.
#[derive(Clone, PartialEq, Eq)]
pub enum Effect<T: FieldElement, V> {
    /// Variable can be assigned a value.
    Assignment(V, SymbolicExpression<T, V>),
    /// We learnt a new range constraint on variable.
    RangeConstraint(V, RangeConstraint<T>),
    /// A run-time assertion. If this fails, we have conflicting constraints.
    Assertion(Assertion<T, V>),
    /// A call to a different machine, with identity ID, known inputs and argument variables.
    MachineCall(u64, BitVec, Vec<V>),
    /// A branch on a variable.
    Branch(BranchCondition<T, V>, Vec<Effect<T, V>>, Vec<Effect<T, V>>),
}

impl<T: FieldElement> Effect<T, Variable> {
    /// Returns an iterator over all variables written to in the effect.
    /// The flag indicates if the variable is the return value of a machine call and thus needs
    /// to be declared mutable.
    pub fn written_vars(&self) -> Box<dyn Iterator<Item = (&Variable, bool)> + '_> {
        match self {
            Effect::Assignment(var, _) => Box::new(iter::once((var, false))),
            Effect::RangeConstraint(..) => unreachable!(),
            Effect::Assertion(..) => Box::new(iter::empty()),
            Effect::MachineCall(_, known, vars) => Box::new(
                vars.iter()
                    .zip_eq(known)
                    .flat_map(|(v, known)| (!known).then_some((v, true))),
            ),
            Effect::Branch(_, first, second) => {
                Box::new(first.iter().chain(second).flat_map(|e| e.written_vars()))
            }
        }
    }
}

impl<T: FieldElement, V: Hash + Eq> Effect<T, V> {
    pub fn referenced_variables(&self) -> impl Iterator<Item = &V> {
        let iter: Box<dyn Iterator<Item = &V>> = match self {
            Effect::Assignment(v, expr) => Box::new(iter::once(v).chain(expr.referenced_symbols())),
            Effect::RangeConstraint(v, _) => Box::new(iter::once(v)),
            Effect::Assertion(Assertion { lhs, rhs, .. }) => {
                Box::new(lhs.referenced_symbols().chain(rhs.referenced_symbols()))
            }
            Effect::MachineCall(_, _, args) => Box::new(args.iter()),
            Effect::Branch(branch_condition, first, second) => Box::new(
                iter::once(&branch_condition.variable).chain(
                    [first, second]
                        .into_iter()
                        .flatten()
                        .flat_map(|effect| effect.referenced_variables()),
                ),
            ),
        };
        iter.unique()
    }
}

/// A run-time assertion. If this fails, we have conflicting constraints.
#[derive(Clone, PartialEq, Eq)]
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

#[derive(Clone, PartialEq, Eq)]
pub struct BranchCondition<T: FieldElement, V> {
    pub variable: V,
    pub condition: RangeConstraint<T>,
}

/// Helper function to render a list of effects. Used for informational purposes only.
pub fn format_code<T: FieldElement>(effects: &[Effect<T, Variable>]) -> String {
    effects
        .iter()
        .map(|effect| match effect {
            Effect::Assignment(v, expr) => format!("{v} = {expr};"),
            Effect::Assertion(Assertion {
                lhs,
                rhs,
                expected_equal,
            }) => {
                format!(
                    "assert {lhs} {} {rhs};",
                    if *expected_equal { "==" } else { "!=" }
                )
            }
            Effect::MachineCall(id, known, vars) => {
                format!(
                    "machine_call({id}, [{}]);",
                    vars.iter()
                        .zip(known)
                        .map(|(v, known)| if known {
                            format!("Known({v})")
                        } else {
                            format!("Unknown({v})")
                        })
                        .join(", ")
                )
            }
            Effect::RangeConstraint(..) => {
                panic!("Range constraints should not be part of the code.")
            }
            Effect::Branch(condition, first, second) => {
                let first = indent(format_code(first), 1);
                let second = indent(format_code(second), 1);
                let condition = format_condition(condition);

                format!("if ({condition}) {{\n{first}\n}} else {{\n{second}\n}}")
            }
        })
        .join("\n")
}

fn format_condition<T: FieldElement>(condition: &BranchCondition<T, Variable>) -> String {
    let var = &condition.variable;
    let (min, max) = condition.condition.range();
    match min.cmp(&max) {
        Ordering::Equal => format!("{var} == {min}"),
        Ordering::Less => format!("{min} <= {var} && {var} <= {max}"),
        Ordering::Greater => format!("{var} <= {min} || {var} >= {max}"),
    }
}
