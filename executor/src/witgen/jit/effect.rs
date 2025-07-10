use std::cmp::Ordering;
use std::hash::Hash;
use std::iter;

use bit_vec::BitVec;
use itertools::Itertools;
use powdr_ast::indent;
use powdr_constraint_solver::effect::{
    Assertion, BitDecomposition, BitDecompositionComponent, Condition,
};
use powdr_constraint_solver::runtime_constant::RuntimeConstant;
use powdr_constraint_solver::symbolic_expression::SymbolicExpression;
use powdr_number::FieldElement;

use powdr_constraint_solver::effect::Effect as ConstraintSolverEffect;
use powdr_constraint_solver::range_constraint::RangeConstraint;

use super::variable::Variable;

/// The effect of solving a symbolic equation.
#[derive(Clone, PartialEq, Eq)]
pub enum Effect<T: FieldElement, V>
where
    SymbolicExpression<T, V>: RuntimeConstant,
{
    /// Variable can be assigned a value.
    Assignment(V, SymbolicExpression<T, V>),
    /// Perform a bit decomposition of a known value, and assign multiple variables.
    BitDecomposition(BitDecomposition<SymbolicExpression<T, V>, V>),
    /// We learnt a new range constraint on variable.
    RangeConstraint(V, RangeConstraint<T>),
    /// A run-time assertion. If this fails, we have conflicting constraints.
    Assertion(Assertion<SymbolicExpression<T, V>>),
    /// A call to a different machine, with bus ID, known inputs and argument variables.
    MachineCall(T, BitVec, Vec<V>),
    /// Compute one variable by executing a prover function (given by index) on the value of other variables.
    ProverFunctionCall(ProverFunctionCall<V>),
    /// A branch on a variable.
    Branch(
        Condition<SymbolicExpression<T, V>>,
        Vec<Effect<T, V>>,
        Vec<Effect<T, V>>,
    ),
}

impl<T: FieldElement, V: Clone + Hash + Eq + Ord>
    From<ConstraintSolverEffect<SymbolicExpression<T, V>, V>> for Effect<T, V>
{
    fn from(effect: ConstraintSolverEffect<SymbolicExpression<T, V>, V>) -> Self {
        match effect {
            ConstraintSolverEffect::Assignment(v, expr) => Effect::Assignment(v, expr),
            ConstraintSolverEffect::RangeConstraint(v, range) => Effect::RangeConstraint(v, range),
            ConstraintSolverEffect::BitDecomposition(bit_decomp) => {
                Effect::BitDecomposition(bit_decomp)
            }
            ConstraintSolverEffect::Assertion(assertion) => Effect::Assertion(assertion),
            ConstraintSolverEffect::ConditionalAssignment {
                variable,
                condition,
                in_range_value,
                out_of_range_value,
            } => Effect::Branch(
                condition,
                vec![Effect::Assignment(variable.clone(), in_range_value)],
                vec![Effect::Assignment(variable, out_of_range_value)],
            ),
        }
    }
}

impl<T: FieldElement> Effect<T, Variable> {
    /// Returns an iterator over all variables written to in the effect.
    /// The flag indicates if the variable is the return value of a machine call and thus needs
    /// to be declared mutable.
    pub fn written_vars(&self) -> Box<dyn Iterator<Item = &Variable> + '_> {
        match self {
            Effect::Assignment(var, _) => Box::new(iter::once(var)),
            Effect::RangeConstraint(..) => unreachable!(),
            Effect::BitDecomposition(BitDecomposition { components, .. }) => Box::new(
                components
                    .iter()
                    .map(|BitDecompositionComponent { variable, .. }| variable),
            ),
            Effect::Assertion(..) => Box::new(iter::empty()),
            Effect::MachineCall(_, known, vars) => Box::new(
                vars.iter()
                    .zip_eq(known)
                    .flat_map(|(v, known)| (!known).then_some(v)),
            ),
            Effect::ProverFunctionCall(ProverFunctionCall { targets, .. }) => {
                Box::new(targets.iter().flatten())
            }
            Effect::Branch(_, first, second) => {
                Box::new(first.iter().chain(second).flat_map(|e| e.written_vars()))
            }
        }
    }
}

impl<T: FieldElement, V: Hash + Eq> Effect<T, V>
where
    SymbolicExpression<T, V>: RuntimeConstant,
{
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
            }) => Box::new(targets.iter().flatten().chain(inputs)),
            Effect::Branch(branch_condition, first, second) => Box::new(
                branch_condition.value.referenced_symbols().chain(
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

#[derive(Clone, PartialEq, Eq)]
pub struct ProverFunctionCall<V> {
    /// Which variables to assign the result to. If an element is None, it is ignored.
    pub targets: Vec<Option<V>>,
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
                ..
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
                    targets
                        .iter()
                        .map(|v| v
                            .as_ref()
                            .map(|v| v.to_string())
                            .unwrap_or_else(|| "_".to_string()))
                        .join(", "),
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
    Condition { value, condition }: &Condition<SymbolicExpression<T, Variable>>,
) -> String {
    let (min, max) = condition.range();
    match min.cmp(&max) {
        Ordering::Equal => format!("{value} == {min}"),
        Ordering::Less => format!("{min} <= {value} && {value} <= {max}"),
        Ordering::Greater => format!("{value} <= {min} || {value} >= {max}"),
    }
}

#[cfg(test)]
mod test {
    use powdr_constraint_solver::effect::Condition;
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

    fn var_as_expr(id: u64) -> SymbolicExpression<T, Variable> {
        SymbolicExpression::from_symbol(var(id), Default::default())
    }

    #[test]
    fn combine_if_else() {
        let effects = vec![
            Effect::Assignment(var(0), SymbolicExpression::from(T::from(1))),
            Effect::Branch(
                Condition {
                    value: var_as_expr(0),
                    condition: RangeConstraint::from_range(T::from(1), T::from(2)),
                },
                vec![Effect::Assignment(
                    var(1),
                    SymbolicExpression::from(T::from(2)),
                )],
                vec![Effect::Branch(
                    Condition {
                        value: var_as_expr(1),
                        condition: RangeConstraint::from_range(T::from(5), T::from(6)),
                    },
                    vec![Effect::Branch(
                        Condition {
                            value: var_as_expr(2),
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
                        Condition {
                            value: var_as_expr(3),
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
