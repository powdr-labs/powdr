use std::hash::Hash;
use std::ops::Mul;
use std::{collections::BTreeMap, fmt::Display};

use powdr_constraint_solver::{
    constraint_system::{BusInteraction, ConstraintSystem},
    grouped_expression::GroupedExpression,
};
use powdr_number::FieldElement;

use crate::range_constraint_optimizer::RangeConstraintHandler;
use powdr_constraint_solver::constraint_system::BusInteractionHandler;

pub trait IMM0Optimizer<T: FieldElement> {
    /// Give a constraint system, extracts sign/unsign information from its range constraints
    fn extract_memory_limbs_from_range_constraints<V: Ord + Clone + Eq + Display + Hash>(
        &self,
        bus_interaction: &BusInteraction<GroupedExpression<T, V>>,
    ) -> Option<GroupedExpression<T, V>> {
        None
    }
}

/// extract sign/unsign information from range constraints
pub fn extract_memory_limbs_from_range_constraints<
    T: FieldElement,
    V: Ord + Clone + Hash + Eq + Display,
>(
    mut system: ConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T>
        + RangeConstraintHandler<T>
        + IMM0Optimizer<T>
        + Clone,
    //  degree_bound: DegreeBound,
) -> ConstraintSystem<T, V> {
    let mut pattern_expressions: Vec<GroupedExpression<T, V>> = Vec::new();
    system
        .bus_interactions
        .iter_mut()
        .for_each(|bus_interaction| {
            let memory_limb1 = bus_interaction_handler
                .extract_memory_limbs_from_range_constraints(bus_interaction);

            if let Some(expr) = memory_limb1 {
                println!("Extracted memory limb expression: {}", expr);
                expr.linear_components().for_each(|(var, _)| {
                    system
                        .algebraic_constraints
                        .iter_mut()
                        .for_each(|constraint| {
                            constraint
                                .expression
                                .quadratic_components()
                                .iter()
                                .for_each(|(expr1, expr2)| {
                                    if expr1.linear_components().any(|(v, _)| v == var)
                                        && expr2.linear_components().any(|(v, _)| v == var)
                                    {
                                        if let Some(coffs) =
                                            expr1.coefficient_of_variable_in_affine_part(var)
                                        {
                                            if let Some(coffs2) =
                                                expr2.coefficient_of_variable_in_affine_part(var)
                                            {
                                                if coffs != coffs2 {
                                                    return;
                                                }
                                            }

                                            println!(
                                            "  Found matching quadratic component with coeff: {}",
                                            coffs
                                        );

                                            println!("inverse coeff: {}", *coffs / (T::from(1u64)));

                                            let expr10 =
                                                expr1.clone().mul(T::from(1u64).div(*coffs));
                                            let expr20 =
                                                expr2.clone().mul(T::from(1u64).div(*coffs));
                                            let coeffs: Vec<_> = expr10
                                                .linear_components()
                                                .map(|(_, coeff)| coeff.clone())
                                                .collect();
                                            if !expr10.constant_offset().is_zero()
                                                && *expr20.constant_offset() != T::from(65536u64)
                                            {
                                                return;
                                            }
                                            let pattern1: Vec<T> = vec![
                                                T::from(30720u64),
                                                T::from(7864320u64),
                                                -T::from(1u64),
                                                -T::from(256u64),
                                                -T::from(30720u64),
                                                T::from(1u64),
                                            ];
                                            if coeffs == pattern1 {
                                                println!(
                                                    "  Found IMM0 pattern in expr1: {}",
                                                    expr10
                                                );
                                            } else {
                                                return;
                                            }
                                            let pattern2: Vec<T> = vec![
                                                T::from(30720u64),
                                                T::from(7864320u64),
                                                -T::from(1u64),
                                                -T::from(256u64),
                                                -T::from(30720u64),
                                                T::from(1u64),
                                            ];
                                            let coeffs2: Vec<_> = expr20
                                                .linear_components()
                                                .map(|(_, coeff)| coeff.clone())
                                                .collect();
                                            if coeffs2 == pattern2 {
                                                println!(
                                                    "  Found IMM0 pattern in expr2: {}",
                                                    expr20
                                                );
                                                println!("push expr to pattern_expressions {}", constraint.expression);
                                                pattern_expressions.push(constraint.expression.clone());
                                            } else {
                                                return;
                                            }
                                        } else {
                                            return;
                                        }
                                    }
                                });
                        });
                });
            }
        });
    
    println!("pattern expressions found: {}", pattern_expressions.len());

    for expr in pattern_expressions {
        let (expr1, _) = expr.quadratic_components().first().unwrap();
        let variables = expr1
            .linear_components()
            .enumerate()
            .map(|(i, (var, _))| (i, var.clone()))
            .collect::<BTreeMap<_, _>>();
        let new_expr1 =
            GroupedExpression::from_unknown_variable(variables.get(&0).unwrap().clone())
                * T::from(30720u64)
                + GroupedExpression::from_unknown_variable(variables.get(&1).unwrap().clone())
                    * T::from(7864320u64)
                - GroupedExpression::from_unknown_variable(variables.get(&4).unwrap().clone());
        let new_expr2 =
            GroupedExpression::from_unknown_variable(variables.get(&2).unwrap().clone())
                * T::from(30720u64)
                + GroupedExpression::from_unknown_variable(variables.get(&3).unwrap().clone())
                    * T::from(7864320u64)
                - GroupedExpression::from_unknown_variable(variables.get(&5).unwrap().clone());

        println!("new constraints are {} and {}", new_expr1, new_expr2);

        system
            .algebraic_constraints
            .retain(|constraint| constraint.expression != expr);

        system
            .algebraic_constraints
            .extend(vec![new_expr1.into(), new_expr2.into()]);
    }

    system
}
