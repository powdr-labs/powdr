use itertools::Itertools;
use powdr_number::FieldElement;

use crate::witgen::range_constraints::RangeConstraint;

use super::{symbolic_expression::SymbolicExpression, variable::Variable};

/// The effect of solving a symbolic equation.
pub enum Effect<T: FieldElement, V> {
    /// Variable can be assigned a value.
    Assignment(V, SymbolicExpression<T, V>),
    /// We learnt a new range constraint on variable.
    RangeConstraint(V, RangeConstraint<T>),
    /// A run-time assertion. If this fails, we have conflicting constraints.
    Assertion(Assertion<T, V>),
    /// a call to a different machine.
    MachineCall(u64, Vec<MachineCallArgument<T, V>>),
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

pub enum MachineCallArgument<T: FieldElement, V> {
    Known(SymbolicExpression<T, V>),
    Unknown(V),
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
            Effect::MachineCall(id, args) => {
                format!(
                    "machine_call({id}, [{}]);",
                    args.iter()
                        .map(|arg| match arg {
                            MachineCallArgument::Known(k) => format!("Known({k})"),
                            MachineCallArgument::Unknown(u) => format!("Unknown({u})"),
                        })
                        .join(", ")
                )
            }
            Effect::RangeConstraint(..) => {
                panic!("Range constraints should not be part of the code.")
            }
        })
        .join("\n")
}
