use crate::constraint_system::{AlgebraicConstraint, ConstraintRef};
use crate::grouped_expression::GroupedExpression;
use crate::indexed_constraint_system::IndexedConstraintSystem;
use crate::journaling_constraint_system::JournalingConstraintSystem;
use crate::runtime_constant::{RuntimeConstant, Substitutable};

use itertools::Itertools;
use powdr_number::ExpressionConvertible;

use std::collections::HashSet;
use std::fmt::Display;
use std::hash::Hash;

#[derive(Debug, Clone, Copy)]
pub struct DegreeBound {
    pub identities: usize,
    pub bus_interactions: usize,
}

/// Reduce variables in the constraint system by inlining them,
/// if the callback `should_inline` returns true.
pub fn replace_constrained_witness_columns<
    T: RuntimeConstant + ExpressionConvertible<T::FieldType, V> + Substitutable<V> + Display,
    V: Ord + Clone + Hash + Eq + Display,
>(
    mut constraint_system: JournalingConstraintSystem<T, V>,
    should_inline: impl Fn(&V, &GroupedExpression<T, V>, &IndexedConstraintSystem<T, V>) -> bool,
) -> JournalingConstraintSystem<T, V> {
    let mut to_remove_idx = HashSet::new();
    let mut inlined_vars = HashSet::new();
    let constraint_count = constraint_system
        .indexed_system()
        .algebraic_constraints()
        .len();
    loop {
        let inlined_vars_count = inlined_vars.len();
        for curr_idx in (0..constraint_count).rev() {
            let constraint = &constraint_system.indexed_system().algebraic_constraints()[curr_idx];

            for (var, expr) in find_inlinable_variables(constraint) {
                if should_inline(&var, &expr, constraint_system.indexed_system()) {
                    log::trace!("Substituting {var} = {expr}");
                    log::trace!("  (from identity {constraint})");

                    constraint_system.substitute_by_unknown(&var, &expr);
                    to_remove_idx.insert(curr_idx);
                    inlined_vars.insert(var);

                    break;
                }
            }
        }
        if inlined_vars.len() == inlined_vars_count {
            // No more variables to inline
            break;
        }
    }

    // remove inlined constraints from system
    let mut counter = 0;
    constraint_system.retain_algebraic_constraints(|_| {
        let retain = !to_remove_idx.contains(&(counter));
        counter += 1;
        retain
    });

    // sanity check
    assert!(constraint_system.expressions().all(|expr| {
        expr.referenced_unknown_variables()
            .all(|var| !inlined_vars.contains(var))
    }));

    constraint_system
}

/// Returns an inlining discriminator that allows everything to be inlined as long as
/// the given degree bound is not violated.
pub fn inline_everything_below_degree_bound<T: RuntimeConstant, V: Ord + Clone + Hash + Eq>(
    degree_bound: DegreeBound,
) -> impl Fn(&V, &GroupedExpression<T, V>, &IndexedConstraintSystem<T, V>) -> bool {
    move |var, expr, constraint_system| {
        substitution_would_not_violate_degree_bound(var, expr, constraint_system, degree_bound)
    }
}

/// Returns true if substituting `var` by `expr` inside `constraint_system` would
/// not create new constraints with a degree larger than `degree_bound`
pub fn substitution_would_not_violate_degree_bound<
    T: RuntimeConstant,
    V: Ord + Clone + Hash + Eq,
>(
    var: &V,
    expr: &GroupedExpression<T, V>,
    constraint_system: &IndexedConstraintSystem<T, V>,
    degree_bound: DegreeBound,
) -> bool {
    let replacement_deg = expr.degree();

    constraint_system
        .constraints_referencing_variables(std::iter::once(var.clone()))
        .all(|cref| match cref {
            ConstraintRef::AlgebraicConstraint(identity) => {
                let degree = expression_degree_with_virtual_substitution(
                    &identity.expression,
                    var,
                    replacement_deg,
                );
                degree <= degree_bound.identities
            }
            ConstraintRef::BusInteraction(interaction) => interaction.fields().all(|expr| {
                let degree =
                    expression_degree_with_virtual_substitution(expr, var, replacement_deg);
                degree <= degree_bound.bus_interactions
            }),
        })
}

/// Returns substitutions of variables that appear linearly and do not depend on themselves.
fn find_inlinable_variables<
    T: RuntimeConstant + ExpressionConvertible<T::FieldType, V> + Display,
    V: Ord + Clone + Hash + Eq + Display,
>(
    constraint: &AlgebraicConstraint<GroupedExpression<T, V>>,
) -> Vec<(V, GroupedExpression<T, V>)> {
    let (_, linear, _) = constraint.expression.components();
    linear
        .rev()
        .filter_map(|(target_var, _)| {
            let rhs_expr = constraint.as_ref().try_solve_for(target_var)?;
            assert!(!rhs_expr.referenced_unknown_variables().contains(target_var));
            Some((target_var.clone(), rhs_expr))
        })
        .collect()
}

/// Calculate the degree of a GroupedExpression assuming a variable is
/// replaced by an expression of known degree.
fn expression_degree_with_virtual_substitution<T: RuntimeConstant, V: Ord + Clone + Eq>(
    expr: &GroupedExpression<T, V>,
    var: &V,
    replacement_deg: usize,
) -> usize {
    let (quadratic, linear, _) = expr.components();

    quadratic
        .iter()
        .map(|(l, r)| {
            expression_degree_with_virtual_substitution(l, var, replacement_deg)
                + expression_degree_with_virtual_substitution(r, var, replacement_deg)
        })
        .chain(linear.map(|(v, _)| if v == var { replacement_deg } else { 1 }))
        .max()
        .unwrap_or(0)
}

#[cfg(test)]
mod test {
    use crate::{
        constraint_system::{BusInteraction, ConstraintSystem},
        test_utils::{constant, var},
    };

    use super::*;

    use test_log::test;

    fn bounds<T: RuntimeConstant, V: Ord + Clone + Hash + Eq>(
        identities: usize,
        bus_interactions: usize,
    ) -> impl Fn(&V, &GroupedExpression<T, V>, &IndexedConstraintSystem<T, V>) -> bool {
        inline_everything_below_degree_bound(DegreeBound {
            identities,
            bus_interactions,
        })
    }

    #[test]
    fn test_no_substitution() {
        let constraint_system = ConstraintSystem::default()
            .with_constraints(vec![
                var("a") * var("b") + var("c") * var("d"),
                var("e") * var("e") - constant(2),
            ])
            .into();

        let constraint_system =
            replace_constrained_witness_columns(constraint_system, bounds(3, 3));
        assert_eq!(constraint_system.algebraic_constraints().count(), 2);
    }

    #[test]
    fn test_replace_witness_columns() {
        // keep column result
        let bus_interactions = vec![BusInteraction {
            bus_id: constant(1),
            payload: vec![var("0result"), var("b")],
            multiplicity: constant(1),
        }];

        let constraint_system = ConstraintSystem::default()
            .with_constraints(vec![
                var("a") + var("b") + var("c"),
                var("b") + var("d") - constant(1),
                var("c") + var("b") + var("a") + var("d") - var("0result"),
            ])
            .with_bus_interactions(bus_interactions)
            .into();

        let constraint_system =
            replace_constrained_witness_columns(constraint_system, bounds(3, 3));
        // 1) a + b + c = 0        => a = -b - c
        // 2) b + d - 1 = 0        => d = -b + 1
        // 3) c + b + a + d = result
        //    =(1)=> c + b + (-b - c) + d
        //         = (c - c) + (b - b) + d
        //         = 0 + 0 + d
        //    => result = d = -b + 1
        //    => b = -result + 1
        assert_eq!(constraint_system.algebraic_constraints().count(), 0);

        let bus_interactions = constraint_system.bus_interactions().collect_vec();
        let [BusInteraction { payload, .. }] = &bus_interactions[..] else {
            panic!();
        };
        let [result, b] = payload.as_slice() else {
            panic!();
        };
        assert_eq!(result.to_string(), "0result");
        assert_eq!(b.to_string(), "-(0result - 1)");
    }

    #[test]
    fn test_replace_witness_columns_with_multiplication() {
        let mut identities = Vec::new();

        // a * b = c
        let constraint1 = var("c") - var("a") * var("b");
        identities.push(constraint1);

        // b + d = 0
        let constraint2 = var("b") + var("d");
        identities.push(constraint2);

        // a + b + c + d - result = 0
        let expr = var("a") + var("b") + var("c") + var("d");
        let expr_constraint = expr.clone() - var("result");
        identities.push(expr_constraint);

        // keep column `result`
        let bus_interactions = vec![BusInteraction {
            bus_id: constant(1),
            payload: vec![var("result")],
            multiplicity: constant(1),
        }];

        let constraint_system = ConstraintSystem::default()
            .with_constraints(identities)
            .with_bus_interactions(bus_interactions)
            .into();

        let constraint_system =
            replace_constrained_witness_columns(constraint_system, bounds(3, 3));

        let constraints = constraint_system.algebraic_constraints().collect_vec();
        assert_eq!(constraints.len(), 0);
    }

    #[test]
    fn test_replace_witness_columns_no_keep() {
        let mut identities = Vec::new();

        // a * b = c
        let constraint1 = var("c") - var("a") * var("b");
        identities.push(constraint1);

        // b + d = 0
        let constraint2 = var("b") + var("d");
        identities.push(constraint2);

        // c * d = e
        let constraint3 = var("e") - var("c") * var("d");
        identities.push(constraint3);

        // a + b + c + d + e - result = 0
        let expr = var("a") + var("b") + var("c") + var("d") + var("e");
        let expr_constraint = expr.clone() - var("result");
        identities.push(expr_constraint);

        // no columns to keep
        let constraint_system = ConstraintSystem::default()
            .with_constraints(identities)
            .into();

        let constraint_system =
            replace_constrained_witness_columns(constraint_system, bounds(3, 3));

        let constraints = constraint_system.algebraic_constraints().collect_vec();
        assert_eq!(constraints.len(), 0);
    }

    #[test]
    fn test_replace_constrained_witness_suboptimal() {
        // Keep x and result
        let bus_interactions = vec![BusInteraction {
            bus_id: constant(1),
            payload: vec![var("result"), var("x")],
            multiplicity: constant(1),
        }];

        let constraint_system = ConstraintSystem::default()
            .with_constraints(vec![
                var("y") - (var("x") + constant(3)),
                var("z") - (var("y") + constant(2)),
                var("result") - (var("z") + constant(1)),
            ])
            .with_bus_interactions(bus_interactions)
            .into();

        let constraint_system =
            replace_constrained_witness_columns(constraint_system, bounds(3, 3));
        // 1) y = x + 3
        // 2) z = y + 2 ⇒ z = (x + 3) + 2 = x + 5
        // 3) result = z + 1 ⇒ result = (x + 5) + 1 = x + 6
        let bus_interactions = constraint_system.bus_interactions().collect_vec();
        let [BusInteraction { payload, .. }] = &bus_interactions[..] else {
            panic!();
        };
        let [result, x] = payload.as_slice() else {
            panic!();
        };
        assert_eq!(result.to_string(), "result");
        assert_eq!(x.to_string(), "result - 6");
    }

    #[test]
    fn test_replace_constrained_witness_columns_max_degree_limit() {
        let constraint_system = ConstraintSystem::default()
            .with_constraints(vec![
                var("a") - (var("b") + constant(1)),
                var("c") - (var("a") * var("a")),
                var("d") - (var("c") * var("a")),
                var("e") - (var("d") * var("a")),
                var("f") - (var("e") + constant(5)),
                var("result") - (var("f") * constant(2)),
            ])
            .with_bus_interactions(
                // Get all variables
                vec![BusInteraction {
                    bus_id: constant(1),
                    payload: vec![
                        var("a"),
                        var("b"),
                        var("c"),
                        var("d"),
                        var("e"),
                        var("f"),
                        var("result"),
                    ],
                    multiplicity: constant(1),
                }],
            )
            .into();

        let constraint_system =
            replace_constrained_witness_columns(constraint_system, bounds(3, 3));

        let constraints = constraint_system.algebraic_constraints().collect_vec();
        let [identity] = &constraints[..] else {
            panic!();
        };
        let bus_interactions = constraint_system.bus_interactions().collect_vec();
        let [BusInteraction { payload, .. }] = &bus_interactions[..] else {
            panic!();
        };
        let [a, b, c, d, e, f, result] = payload.as_slice() else {
            panic!();
        };
        assert_eq!(a.to_string(), "a");
        assert_eq!(b.to_string(), "a - 1");
        // From second identity: c = a * a
        // In-lining c would violate the degree bound, so it is kept as a symbol
        // with a constraint to enforce the equality.
        assert_eq!(c.to_string(), "c");
        assert_eq!(identity.to_string(), "-((a) * (a) - c) = 0");
        // From third identity: d = c * a
        assert_eq!(d.to_string(), "(c) * (a)");
        // From fourth identity: e = d * a
        assert_eq!(e.to_string(), "((c) * (a)) * (a)");
        // From fifth identity: f = e + 5
        assert_eq!(f.to_string(), "((c) * (a)) * (a) + 5");
        // From sixth identity: result = f * 2
        assert_eq!(result.to_string(), "((2 * c) * (a)) * (a) + 10");
    }

    #[test]
    fn test_inline_max_degree_suboptimal_greedy() {
        // Show how constraint order affects optimization results

        // Define the constraints in both orders
        let mut optimal_order_identities = Vec::new();
        let mut suboptimal_order_identities = Vec::new();

        // a = b * b * b
        let constraint1 = var("a") - var("b") * var("b") * var("b");
        // b = c + d
        let constraint2 = var("b") - (var("c") + var("d"));
        // a * c * c = 10
        let constraint3 = var("a") * var("c") * var("c") - constant(10);
        // c = d * d
        let constraint4 = var("c") - var("d") * var("d");
        // a + b + c + d = 100
        let constraint5 = var("a") + var("b") + var("c") + var("d") - constant(100);

        // Optimal order
        optimal_order_identities.push(constraint1.clone()); // a = b * b * b
        optimal_order_identities.push(constraint2.clone()); // b = c + d
        optimal_order_identities.push(constraint3.clone()); // a * c * c = 10
        optimal_order_identities.push(constraint4.clone()); // c = d * d
        optimal_order_identities.push(constraint5.clone()); // a + b + c + d = 100

        // Suboptimal order
        suboptimal_order_identities.push(constraint5.clone()); // a + b + c + d = 100
        suboptimal_order_identities.push(constraint3.clone()); // a * c * c = 10
        suboptimal_order_identities.push(constraint1.clone()); // a = b * b * b
        suboptimal_order_identities.push(constraint2.clone()); // b = c + d
        suboptimal_order_identities.push(constraint4.clone()); // c = d * d

        let optimal_system = ConstraintSystem::default()
            .with_constraints(optimal_order_identities)
            .into();

        let suboptimal_system = ConstraintSystem::default()
            .with_constraints(suboptimal_order_identities)
            .into();

        // Apply the same optimization to both systems
        let optimal_system = replace_constrained_witness_columns(optimal_system, bounds(5, 5));

        let suboptimal_system =
            replace_constrained_witness_columns(suboptimal_system, bounds(5, 5));

        // Assert the difference in optimization results
        assert_eq!(optimal_system.algebraic_constraints().count(), 3);
        assert_eq!(suboptimal_system.algebraic_constraints().count(), 4);
    }
}
