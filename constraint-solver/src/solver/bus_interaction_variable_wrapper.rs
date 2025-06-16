use std::{collections::BTreeMap, fmt::Display};

use powdr_number::FieldElement;
use std::hash::Hash;

use crate::{
    constraint_system::{BusInteraction, ConstraintSystem},
    quadratic_symbolic_expression::QuadraticSymbolicExpression,
    solver::SolveResult,
};

#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Variable<V> {
    Variable(V),
    BusInteractionArg(usize, usize),
}

impl<V: Display> Display for Variable<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Variable::Variable(v) => write!(f, "{v}"),
            Variable::BusInteractionArg(bus_index, field_index) => {
                write!(f, "BusInteractionArg({bus_index}, {field_index})")
            }
        }
    }
}

pub fn wrap_variables<T: FieldElement, V: Ord + Clone + Hash + Eq + Display>(
    constraint_system: ConstraintSystem<T, V>,
) -> (
    ConstraintSystem<T, Variable<V>>,
    BTreeMap<Variable<V>, QuadraticSymbolicExpression<T, V>>,
) {
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
                    let v = Variable::BusInteractionArg(bus_interaction_index, field_index);
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
    (constraint_system, bus_interaction_vars)
}

pub fn make_result<T: FieldElement, V: Ord + Clone + Hash + Eq + Display>(
    assignments: Vec<(Variable<V>, QuadraticSymbolicExpression<T, Variable<V>>)>,
    mut bus_interaction_vars: BTreeMap<Variable<V>, QuadraticSymbolicExpression<T, V>>,
) -> SolveResult<T, V> {
    for (variable, expr) in &assignments {
        if let Variable::BusInteractionArg(..) = variable {
            // TODO: Can there be more complex expressions here?
            let value = expr.try_to_number().unwrap();
            bus_interaction_vars.insert(variable.clone(), value.into());
        }
    }

    let assignments = assignments
        .into_iter()
        .filter_map(|(v, expr)| match v {
            Variable::Variable(v) => {
                let expr = expr.transform_var_type(&mut |v| match v {
                    Variable::Variable(v) => v.clone(),
                    Variable::BusInteractionArg(..) => {
                        // TODO: Is this really unreachable?
                        unreachable!();
                    }
                });
                Some((v, expr))
            }
            Variable::BusInteractionArg(..) => None,
        })
        .collect();
    let bus_field_assignments = bus_interaction_vars
        .into_iter()
        .filter_map(|(v, expr)| match (v, expr.try_to_number()) {
            (Variable::BusInteractionArg(bus_index, field_index), Some(value)) => {
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
