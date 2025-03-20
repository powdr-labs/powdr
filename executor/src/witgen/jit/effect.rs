use std::fmt::Formatter;
use std::{cmp::Ordering, fmt::Display};

use std::{fmt, iter};

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
    /// Perform a bit decomposition of a known value, and assign multiple variables.
    BitDecomposition(BitDecomposition<T, V>),
    /// We learnt a new range constraint on variable.
    RangeConstraint(V, RangeConstraint<T>),
    /// A run-time assertion. If this fails, we have conflicting constraints.
    Assertion(Assertion<T, V>),
    /// A call to a different machine, with bus ID, known inputs and argument variables.
    MachineCall(T, BitVec, Vec<V>),
    /// Compute one variable by executing a prover function (given by index) on the value of other variables.
    ProverFunctionCall(ProverFunctionCall<V>),
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
            Effect::BitDecomposition(BitDecomposition { components, .. }) => Box::new(
                components
                    .iter()
                    .map(|BitDecompositionComponent { variable, .. }| (variable, false)),
            ),
            Effect::Assertion(..) => Box::new(iter::empty()),
            Effect::MachineCall(_, known, vars) => Box::new(
                vars.iter()
                    .zip_eq(known)
                    .flat_map(|(v, known)| (!known).then_some((v, true))),
            ),
            Effect::ProverFunctionCall(ProverFunctionCall { targets, .. }) => {
                Box::new(targets.iter().map(|v| (v, false)))
            }
            Effect::Branch(_, first, second) => {
                Box::new(first.iter().chain(second).flat_map(|e| e.written_vars()))
            }
        }
    }
}

impl<T: FieldElement, V: Hash + Eq> Effect<T, V> {
    /// Returns an iterator over all referenced variables, both read and written.
    pub fn referenced_variables(&self) -> impl Iterator<Item = &V> {
        let iter: Box<dyn Iterator<Item = &V>> = match self {
            Effect::Assignment(v, expr) => Box::new(iter::once(v).chain(expr.referenced_symbols())),
            Effect::RangeConstraint(v, _) => Box::new(iter::once(v)),
            Effect::BitDecomposition(BitDecomposition { value, components }) => Box::new(
                value.referenced_symbols().chain(
                    components
                        .iter()
                        .map(|BitDecompositionComponent { variable, .. }| variable),
                ),
            ),
            Effect::Assertion(Assertion { lhs, rhs, .. }) => {
                Box::new(lhs.referenced_symbols().chain(rhs.referenced_symbols()))
            }
            Effect::MachineCall(_, _, args) => Box::new(args.iter()),
            Effect::ProverFunctionCall(ProverFunctionCall {
                targets, inputs, ..
            }) => Box::new(targets.iter().chain(inputs)),
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

/// A bit decomposition of a value.
/// Executing this effect solves the following equation:
/// value = sum_{i=0}^{components.len() - 1} (-1)**components[i].negative * 2**components[i].exponent * components[i].variable
///
/// This effect can only be created if the equation has a unique solution.
/// It might be that it leads to a contradiction, which should result in an assertion failure.
#[derive(Clone, PartialEq, Eq)]
pub struct BitDecomposition<T: FieldElement, V> {
    /// The value that is decomposed.
    pub value: SymbolicExpression<T, V>,
    /// The components of the decomposition.
    pub components: Vec<BitDecompositionComponent<T, V>>,
}

/// A component in the bit decomposition.
/// In a simplified form, we can solve for `variable` using
/// `(value & bit_mask) >> exponent`.
#[derive(Clone, PartialEq, Eq)]
pub struct BitDecompositionComponent<T: FieldElement, V> {
    /// The variables that will be assigned to.
    pub variable: V,
    /// If the variable occurs negatively in the equation.
    /// Note that the range constraint of the variable itself is always non-negative.
    pub is_negative: bool,
    /// The exponent of two, which forms the coefficient of the variable.
    pub exponent: u64,
    /// The bit mask for this component, relative to the value to be decomposed,
    /// i.e. already scaled by the coefficient.
    pub bit_mask: T::Integer,
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

#[derive(Clone, PartialEq, Eq)]
pub struct ProverFunctionCall<V> {
    /// Which variables to assign the result to.
    pub targets: Vec<V>,
    /// The index of the prover function in the list.
    pub function_index: usize,
    /// The row offset to supply to the prover function.
    pub row_offset: i32,
    /// The input variables, to be evaluated before calling the prover function.
    pub inputs: Vec<V>,
}

/// Helper function to render a list of effects. Used for informational purposes only.
pub fn format_code<T: FieldElement>(effects: &[Effect<T, Variable>]) -> String {
    effects
        .iter()
        .map(|effect| match effect {
            Effect::Assignment(v, expr) => format!("{v} = {expr};"),
            Effect::BitDecomposition(bit_decomp) => format!("{bit_decomp}"),
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
            Effect::ProverFunctionCall(ProverFunctionCall {
                targets,
                function_index,
                row_offset,
                inputs,
            }) => {
                format!(
                    "[{}] = prover_function_{function_index}({row_offset}, [{}]);",
                    targets.iter().join(", "),
                    inputs.iter().join(", ")
                )
            }
            Effect::Branch(condition, first, second) => {
                let first = format_code(first);
                let second_str = format_code(second);
                let condition = format_condition(condition);

                if matches!(second[..], [Effect::Branch(..)]) {
                    format!(
                        "if ({condition}) {{\n{}\n}} else {second_str}",
                        indent(first, 1),
                    )
                } else {
                    format!(
                        "if ({condition}) {{\n{}\n}} else {{\n{}\n}}",
                        indent(first, 1),
                        indent(second_str, 1)
                    )
                }
            }
        })
        .join("\n")
}

fn format_condition<T: FieldElement>(
    BranchCondition {
        variable,
        condition,
    }: &BranchCondition<T, Variable>,
) -> String {
    let (min, max) = condition.range();
    match min.cmp(&max) {
        Ordering::Equal => format!("{variable} == {min}"),
        Ordering::Less => format!("{min} <= {variable} && {variable} <= {max}"),
        Ordering::Greater => format!("{variable} <= {min} || {variable} >= {max}"),
    }
}

impl<T: FieldElement, V: Display> Display for BitDecomposition<T, V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let BitDecomposition { value, components } = self;
        write!(f, "{} := {value};", components.iter().format(" + "))
    }
}

impl<T: FieldElement, V: Display> Display for BitDecompositionComponent<T, V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let BitDecompositionComponent {
            variable,
            is_negative,
            exponent,
            bit_mask: _,
        } = self;
        write!(
            f,
            "{}2**{exponent} * {variable}",
            if *is_negative { "-" } else { "" },
        )
    }
}

#[cfg(test)]
mod test {
    use powdr_number::GoldilocksField;

    use crate::witgen::jit::variable::Cell;

    use pretty_assertions::assert_eq;

    use super::*;
    type T = GoldilocksField;

    fn var(id: u64) -> Variable {
        Variable::WitnessCell(Cell {
            column_name: format!("v{id}"),
            id,
            row_offset: 0,
        })
    }

    #[test]
    fn combine_if_else() {
        let effects = vec![
            Effect::Assignment(var(0), SymbolicExpression::from(T::from(1))),
            Effect::Branch(
                BranchCondition {
                    variable: var(0),
                    condition: RangeConstraint::from_range(T::from(1), T::from(2)),
                },
                vec![Effect::Assignment(
                    var(1),
                    SymbolicExpression::from(T::from(2)),
                )],
                vec![Effect::Branch(
                    BranchCondition {
                        variable: var(1),
                        condition: RangeConstraint::from_range(T::from(5), T::from(6)),
                    },
                    vec![Effect::Branch(
                        BranchCondition {
                            variable: var(2),
                            condition: RangeConstraint::from_range(T::from(5), T::from(6)),
                        },
                        vec![Effect::Assignment(
                            var(8),
                            SymbolicExpression::from(T::from(3)),
                        )],
                        vec![Effect::Assignment(
                            var(9),
                            SymbolicExpression::from(T::from(4)),
                        )],
                    )],
                    vec![Effect::Branch(
                        BranchCondition {
                            variable: var(3),
                            condition: RangeConstraint::from_range(T::from(5), T::from(6)),
                        },
                        vec![Effect::Assignment(
                            var(21),
                            SymbolicExpression::from(T::from(3)),
                        )],
                        vec![Effect::Assignment(
                            var(22),
                            SymbolicExpression::from(T::from(4)),
                        )],
                    )],
                )],
            ),
        ];
        let code = format_code(&effects);
        assert_eq!(
            code,
            "v0[0] = 1;
if (1 <= v0[0] && v0[0] <= 2) {
    v1[0] = 2;
} else if (5 <= v1[0] && v1[0] <= 6) {
    if (5 <= v2[0] && v2[0] <= 6) {
        v8[0] = 3;
    } else {
        v9[0] = 4;
    }
} else if (5 <= v3[0] && v3[0] <= 6) {
    v21[0] = 3;
} else {
    v22[0] = 4;
}"
        );
    }
}
