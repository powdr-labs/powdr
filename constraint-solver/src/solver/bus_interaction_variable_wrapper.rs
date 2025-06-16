use std::{collections::BTreeMap, fmt::Display};

use powdr_number::FieldElement;
use std::hash::Hash;

use crate::{
    constraint_system::{BusInteraction, ConstraintSystem},
    quadratic_symbolic_expression::QuadraticSymbolicExpression,
    solver::SolveResult,
};

#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
/// A wrapped variable: Either a regular variable or a bus interaction field.
pub enum Variable<V> {
    Variable(V),
    BusInteractionField(usize, usize),
}

impl<V: Display> Display for Variable<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Variable::Variable(v) => write!(f, "{v}"),
            Variable::BusInteractionField(bus_index, field_index) => {
                write!(f, "BusInteractionField({bus_index}, {field_index})")
            }
        }
    }
}

/// An assignment of a wrapped variable.
pub type IntermediateAssignment<T, V> = (Variable<V>, QuadraticSymbolicExpression<T, Variable<V>>);

pub struct BusInteractionVariableWrapper<T: FieldElement, V> {
    pub bus_interaction_vars: BTreeMap<Variable<V>, QuadraticSymbolicExpression<T, V>>,
}

impl<T: FieldElement, V: Ord + Clone + Hash + Eq + Display> BusInteractionVariableWrapper<T, V> {
    pub fn new(
        constraint_system: ConstraintSystem<T, V>,
    ) -> (Self, ConstraintSystem<T, Variable<V>>) {
        let mut new_constraints = Vec::new();
        let mut bus_interaction_vars = BTreeMap::new();
        let bus_interactions = constraint_system
            .bus_interactions
            .iter()
            .enumerate()
            .map(|(bus_interaction_index, bus_interaction)| {
                BusInteraction::from_iter(bus_interaction.fields().enumerate().map(
                    |(field_index, expr)| {
                        let transformed_expr =
                            expr.transform_var_type(&mut |v| Variable::Variable(v.clone()));
                        let v = Variable::BusInteractionField(bus_interaction_index, field_index);
                        new_constraints.push(
                            transformed_expr
                                - QuadraticSymbolicExpression::from_unknown_variable(v.clone()),
                        );
                        bus_interaction_vars.insert(v.clone(), expr.clone());
                        QuadraticSymbolicExpression::from_unknown_variable(v)
                    },
                ))
            })
            .collect();
        let constraint_system = ConstraintSystem {
            algebraic_constraints: constraint_system
                .algebraic_constraints
                .iter()
                .map(|expr| expr.transform_var_type(&mut |v| Variable::Variable(v.clone())))
                .chain(new_constraints)
                .collect(),
            bus_interactions,
        };
        (
            Self {
                bus_interaction_vars,
            },
            constraint_system,
        )
    }

    pub fn finalize(mut self, assignments: Vec<IntermediateAssignment<T, V>>) -> SolveResult<T, V> {
        for (variable, expr) in &assignments {
            if let Variable::BusInteractionField(..) = variable {
                // TODO: Can there be more complex expressions here?
                let value = expr.try_to_number().unwrap();
                self.bus_interaction_vars
                    .insert(variable.clone(), value.into());
            }
        }

        let assignments = assignments
            .into_iter()
            .filter_map(|(v, expr)| match v {
                Variable::Variable(v) => {
                    let expr = expr.transform_var_type(&mut |v| match v {
                        Variable::Variable(v) => v.clone(),
                        Variable::BusInteractionField(..) => {
                            // TODO: Is this really unreachable?
                            unreachable!();
                        }
                    });
                    Some((v, expr))
                }
                Variable::BusInteractionField(..) => None,
            })
            .collect();
        let bus_field_assignments = self
            .bus_interaction_vars
            .into_iter()
            .filter_map(|(v, expr)| match (v, expr.try_to_number()) {
                (Variable::BusInteractionField(bus_index, field_index), Some(value)) => {
                    Some(((bus_index, field_index), value))
                }
                _ => None,
            })
            .collect();

        SolveResult {
            assignments,
            bus_field_assignments,
        }
    }
}
