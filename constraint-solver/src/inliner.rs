use crate::constraint_system::ConstraintRef;
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
/// as long as the resulting degree stays within `max_degree`.
/// The degree is just the degree in the unknown variables, i.e.
/// potential variables inside runtime constants do not count towards the degree.
pub fn replace_constrained_witness_columns<
    T: RuntimeConstant + ExpressionConvertible<T::FieldType, V> + Substitutable<V> + Display,
    V: Ord + Clone + Hash + Eq + Display,
>(
    mut constraint_system: JournalingConstraintSystem<T, V>,
    degree_bound: DegreeBound,
) -> JournalingConstraintSystem<T, V> {
    let mut to_remove_idx = HashSet::new();
    let mut inlined_vars = HashSet::new();
    let constraint_count = constraint_system
        .indexed_system()
        .algebraic_constraints()
        .len();
    for curr_idx in (0..constraint_count).rev() {
        let constraint = &constraint_system.indexed_system().algebraic_constraints()[curr_idx];

        for (var, expr) in find_inlinable_variables(constraint) {
            if is_valid_substitution(
                &var,
                &expr,
                constraint_system.indexed_system(),
                degree_bound,
            ) {
                log::trace!("Substituting {var} = {expr}");
                log::trace!("  (from identity {constraint})");

                constraint_system.substitute_by_unknown(&var, &expr);
                to_remove_idx.insert(curr_idx);
                inlined_vars.insert(var);

                break;
            }
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

/// Returns substitutions of variables that appear linearly and do not depend on themselves.
fn find_inlinable_variables<
    T: RuntimeConstant + ExpressionConvertible<T::FieldType, V> + Display,
    V: Ord + Clone + Hash + Eq + Display,
>(
    constraint: &GroupedExpression<T, V>,
) -> Vec<(V, GroupedExpression<T, V>)> {
    let mut substitutions = vec![];

    let (_, linear, _) = constraint.components();

    for (target_var, _) in linear {
        let Some(rhs_expr) = constraint.try_solve_for(target_var) else {
            continue;
        };

        assert!(!rhs_expr.referenced_unknown_variables().contains(target_var));

        substitutions.push((target_var.clone(), rhs_expr));
    }

    substitutions
}

/// Checks whether a substitution is valid under `max_degree` constraint.
fn is_valid_substitution<T: RuntimeConstant, V: Ord + Clone + Hash + Eq>(
    var: &V,
    expr: &GroupedExpression<T, V>,
    constraint_system: &IndexedConstraintSystem<T, V>,
    degree_bound: DegreeBound,
) -> bool {
    let replacement_deg = expression_degree(expr);

    constraint_system
        .constraints_referencing_variables(std::iter::once(var.clone()))
        .all(|cref| match cref {
            ConstraintRef::AlgebraicConstraint(identity) => {
                let degree =
                    expression_degree_with_virtual_substitution(identity, var, replacement_deg);
                degree <= degree_bound.identities
            }
            ConstraintRef::BusInteraction(interaction) => interaction.fields().all(|expr| {
                let degree =
                    expression_degree_with_virtual_substitution(expr, var, replacement_deg);
                degree <= degree_bound.bus_interactions
            }),
        })
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

/// Computes the degree of a GroupedExpression in the unknown variables.
/// Variables inside runtime constants are ignored.
fn expression_degree<T: RuntimeConstant, V: Ord + Clone>(expr: &GroupedExpression<T, V>) -> usize {
    let (quadratic, linear, _) = expr.components();

    quadratic
        .iter()
        .map(|(l, r)| expression_degree(l) + expression_degree(r))
        .chain(linear.map(|_| 1))
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

    fn bounds(identities: usize, bus_interactions: usize) -> DegreeBound {
        DegreeBound {
            identities,
            bus_interactions,
        }
    }

    #[test]
    fn test_no_substitution() {
        let constraint_system = ConstraintSystem {
            algebraic_constraints: vec![
                var("a") * var("b") + var("c") * var("d"),
                var("e") * var("e") - constant(2),
            ],
            bus_interactions: vec![],
        }
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
            payload: vec![var("result"), var("b")],
            multiplicity: constant(1),
        }];

        let constraint_system = ConstraintSystem {
            algebraic_constraints: vec![
                var("a") + var("b") + var("c"),
                var("b") + var("d") - constant(1),
                var("c") + var("b") + var("a") + var("d") - var("result"),
            ],
            bus_interactions,
        }
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
        assert_eq!(result.to_string(), "result");
        assert_eq!(b.to_string(), "-(result - 1)");
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

        let constraint_system = ConstraintSystem {
            algebraic_constraints: identities,
            bus_interactions,
        }
        .into();

        let constraint_system =
            replace_constrained_witness_columns(constraint_system, bounds(3, 3));
        // 1) b + d = 0            => b = -d
        // 2) a * b = c            => a * (-d) = c => a * d + c = 0
        // 3) a + b + c + d = result
        //    =(1)=> a - d + c + d = result
        //         = a + c
        //    => a + c - result = 0
        //    × (-d): -a*d - c*d + d*result = 0
        //    =(2)=> c - c*d + d*result = 0
        // ⇒ (c + -result) * (-d) + c = 0

        let constraints = constraint_system.algebraic_constraints().collect_vec();
        assert_eq!(constraints.len(), 1);
        assert_eq!(constraints[0].to_string(), "-((c - result) * (d) - c)");
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
        let constraint_system = ConstraintSystem {
            algebraic_constraints: identities,
            bus_interactions: vec![],
        }
        .into();

        let constraint_system =
            replace_constrained_witness_columns(constraint_system, bounds(3, 3));
        // 1) b + d = 0        => b = -d
        // 2) c * d = e        => e = c * d
        // 3) a + b + c + d + e = result
        //    =⇒ a + (-d) + c + d + (c * d) = result
        //    =⇒ a + c + (c * d) = result ⇒ a = result - c - c*d
        //
        // Replace a and b in (a * b = c):
        //    (result - c - c*d) * (-d) = c
        // ⇒ ((c * d) + c - result) * (-d) + c = 0
        let constraints = constraint_system.algebraic_constraints().collect_vec();
        assert_eq!(constraints.len(), 1);
        assert_eq!(
            constraints[0].to_string(),
            "-(((c) * (d) + c - result) * (d) - c)"
        );
    }

    #[test]
    fn test_replace_constrained_witness_suboptimal() {
        // Keep x and result
        let bus_interactions = vec![BusInteraction {
            bus_id: constant(1),
            payload: vec![var("result"), var("x")],
            multiplicity: constant(1),
        }];

        let constraint_system = ConstraintSystem {
            algebraic_constraints: vec![
                var("y") - (var("x") + constant(3)),
                var("z") - (var("y") + constant(2)),
                var("result") - (var("z") + constant(1)),
            ],
            bus_interactions,
        }
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
        assert_eq!(result.to_string(), "z + 1");
        assert_eq!(x.to_string(), "z - 5");
    }

    #[test]
    fn test_replace_constrained_witness_columns_max_degree_limit() {
        let constraint_system = ConstraintSystem {
            algebraic_constraints: vec![
                var("a") - (var("b") + constant(1)),
                var("c") - (var("a") * var("a")),
                var("d") - (var("c") * var("a")),
                var("e") - (var("d") * var("a")),
                var("f") - (var("e") + constant(5)),
                var("result") - (var("f") * constant(2)),
            ],
            // Get all variables
            bus_interactions: vec![BusInteraction {
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
        }
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
        // From first identity: a = b + 1
        assert_eq!(a.to_string(), "b + 1");
        // b kept as a symbol
        assert_eq!(b.to_string(), "b");
        // From second identity: c = a * a
        // In-lining c would violate the degree bound, so it is kept as a symbol
        // with a constraint to enforce the equality.
        assert_eq!(c.to_string(), "c");
        assert_eq!(identity.to_string(), "-((b + 1) * (b + 1) - c)");
        // From third identity: d = c * a
        assert_eq!(d.to_string(), "(c) * (b + 1)");
        // From fourth identity: e = d * a
        assert_eq!(e.to_string(), "((c) * (b + 1)) * (b + 1)");
        // From fifth identity: f = e + 5
        assert_eq!(f.to_string(), "((c) * (b + 1)) * (b + 1) + 5");
        // From sixth identity: result = f * 2
        assert_eq!(result.to_string(), "((2 * c) * (b + 1)) * (b + 1) + 10");
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

        let optimal_system = ConstraintSystem {
            algebraic_constraints: optimal_order_identities,
            bus_interactions: vec![],
        }
        .into();

        let suboptimal_system = ConstraintSystem {
            algebraic_constraints: suboptimal_order_identities,
            bus_interactions: vec![],
        }
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
