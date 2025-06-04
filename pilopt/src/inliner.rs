use itertools::Itertools;
use powdr_constraint_solver::constraint_system::ConstraintRef;
use powdr_constraint_solver::indexed_constraint_system::IndexedConstraintSystem;
use powdr_constraint_solver::{
    constraint_system::ConstraintSystem, quadratic_symbolic_expression::QuadraticSymbolicExpression,
};
use powdr_number::FieldElement;
use std::collections::HashSet;
use std::fmt::Display;
use std::hash::Hash;

/// Reduce variables in the constraint system by inlining them,
/// as long as the resulting degree stays within `max_degree`.
pub fn replace_constrained_witness_columns<
    T: FieldElement,
    V: Ord + Clone + Hash + Eq + Display,
>(
    constraint_system: ConstraintSystem<T, V>,
    max_degree: usize,
) -> ConstraintSystem<T, V> {
    let mut to_remove_idx = HashSet::new();
    let mut inlined_vars = HashSet::new();

    let mut constraint_system = IndexedConstraintSystem::from(constraint_system);

    for curr_idx in (0..constraint_system.algebraic_constraints().len()).rev() {
        let constraint = &constraint_system.algebraic_constraints()[curr_idx];

        for (var, expr) in find_inlinable_variables(constraint) {
            if is_valid_substitution(&var, &expr, &constraint_system, max_degree) {
                log::trace!("Substituting {var} = {expr}");
                log::trace!("  (from identity {constraint})");

                constraint_system.substitute_by_unknown(&var, &expr);
                to_remove_idx.insert(curr_idx);
                inlined_vars.insert(var);

                break;
            }
        }
    }

    let mut constraint_system = ConstraintSystem::from(constraint_system);

    // remove inlined constraints from system
    constraint_system.algebraic_constraints = constraint_system
        .algebraic_constraints
        .into_iter()
        .enumerate()
        .filter_map(|(idx, qse)| (!to_remove_idx.contains(&idx)).then_some(qse))
        .collect();

    // sanity check
    assert!(constraint_system.expressions().all(|expr| {
        expr.referenced_unknown_variables()
            .all(|var| !inlined_vars.contains(var))
    }));

    constraint_system
}

/// Returns substitutions of variables that appear linearly and do not depend on themselves.
fn find_inlinable_variables<T: FieldElement, V: Ord + Clone + Hash + Eq + Display>(
    constraint: &QuadraticSymbolicExpression<T, V>,
) -> Vec<(V, QuadraticSymbolicExpression<T, V>)> {
    let mut substitutions = vec![];

    let (_, linear, _) = constraint.components();

    for (target_var, _) in linear {
        let Some(rhs_qse) = constraint.try_solve_for(target_var) else {
            continue;
        };

        assert!(!rhs_qse.referenced_unknown_variables().contains(target_var));

        substitutions.push((target_var.clone(), rhs_qse));
    }

    substitutions
}

/// Checks whether a substitution is valid under `max_degree` constraint.
fn is_valid_substitution<T: FieldElement, V: Ord + Clone + Hash + Eq>(
    var: &V,
    expr: &QuadraticSymbolicExpression<T, V>,
    constraint_system: &IndexedConstraintSystem<T, V>,
    max_degree: usize,
) -> bool {
    let replacement_deg = qse_degree(expr);

    constraint_system
        .constraints_referencing_variables(std::iter::once(var.clone()))
        .all(|cref| match cref {
            ConstraintRef::AlgebraicConstraint(identity) => {
                let degree = qse_degree_with_virtual_substitution(identity, var, replacement_deg);
                degree <= max_degree
            }
            ConstraintRef::BusInteraction(interaction) => interaction.fields().all(|expr| {
                let degree = qse_degree_with_virtual_substitution(expr, var, replacement_deg);
                degree <= max_degree
            }),
        })
}

/// Calculate the degree of a QuadraticSymbolicExpression assuming a variable is
/// replaced by an expression of known degree.
fn qse_degree_with_virtual_substitution<T: FieldElement, V: Ord + Clone + Hash + Eq>(
    qse: &QuadraticSymbolicExpression<T, V>,
    var: &V,
    replacement_deg: usize,
) -> usize {
    let (quadratic, linear, _) = qse.components();

    let quad_deg = quadratic
        .iter()
        .map(|(l, r)| {
            qse_degree_with_virtual_substitution(l, var, replacement_deg)
                + qse_degree_with_virtual_substitution(r, var, replacement_deg)
        })
        .max()
        .unwrap_or(0);

    let linear_deg = linear
        .map(|(v, _)| if v == var { replacement_deg } else { 1 })
        .max()
        .unwrap_or(0);

    quad_deg.max(linear_deg)
}

/// Computes the degree of a QuadraticSymbolicExpression.
fn qse_degree<T: FieldElement, V: Ord + Clone + Hash + Eq>(
    qse: &QuadraticSymbolicExpression<T, V>,
) -> usize {
    let (quadratic, linear, _) = qse.components();

    let quad_deg = quadratic
        .iter()
        .map(|(l, r)| qse_degree(l) + qse_degree(r))
        .max()
        .unwrap_or(0);

    let linear_deg = if linear.peekable().peek().is_some() {
        1
    } else {
        0
    };

    quad_deg.max(linear_deg)
}

#[cfg(test)]
mod test {
    use powdr_constraint_solver::{
        constraint_system::BusInteraction,
        test_utils::{constant, var},
    };

    use super::*;

    #[test]
    fn test_no_substitution() {
        let constraint_system = ConstraintSystem {
            algebraic_constraints: vec![
                var("a") * var("b") + var("c") * var("d"),
                var("e") * var("e") - constant(2),
            ],
            bus_interactions: vec![],
        };

        let constraint_system = replace_constrained_witness_columns(constraint_system, 3);
        assert_eq!(constraint_system.algebraic_constraints.len(), 2);
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
        };

        let constraint_system = replace_constrained_witness_columns(constraint_system, 3);
        // 1) a + b + c = 0        => a = -b - c
        // 2) b + d - 1 = 0        => d = -b + 1
        // 3) c + b + a + d = result
        //    =(1)=> c + b + (-b - c) + d
        //         = (c - c) + (b - b) + d
        //         = 0 + 0 + d
        //    => result = d = -b + 1
        //    => b = -result + 1
        assert_eq!(constraint_system.algebraic_constraints.len(), 0);
        let [BusInteraction { payload, .. }] = &constraint_system.bus_interactions[..] else {
            panic!();
        };
        let [result, b] = payload.as_slice() else {
            panic!();
        };
        assert_eq!(result.to_string(), "result");
        assert_eq!(b.to_string(), "-result + 1");
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
        };

        let constraint_system = replace_constrained_witness_columns(constraint_system, 3);
        // 1) b + d = 0            => b = -d
        // 2) a * b = c            => a * (-d) = c => a * d + c = 0
        // 3) a + b + c + d = result
        //    =(1)=> a - d + c + d = result
        //         = a + c
        //    => a + c - result = 0
        //    × (-d): -a*d - c*d + d*result = 0
        //    =(2)=> c - c*d + d*result = 0
        // ⇒ (c + -result) * (-d) + c = 0

        assert_eq!(constraint_system.algebraic_constraints.len(), 1);
        assert_eq!(
            constraint_system.algebraic_constraints[0].to_string(),
            "(c + -result) * (-d) + c"
        );
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
        };

        let constraint_system = replace_constrained_witness_columns(constraint_system, 3);
        // 1) b + d = 0        => b = -d
        // 2) c * d = e        => e = c * d
        // 3) a + b + c + d + e = result
        //    =⇒ a + (-d) + c + d + (c * d) = result
        //    =⇒ a + c + (c * d) = result ⇒ a = result - c - c*d
        //
        // Replace a and b in (a * b = c):
        //    (result - c - c*d) * (-d) = c
        // ⇒ ((c * d) + c - result) * (-d) + c = 0
        assert_eq!(constraint_system.algebraic_constraints.len(), 1);
        assert_eq!(
            constraint_system.algebraic_constraints[0].to_string(),
            "((c) * (d) + c + -result) * (-d) + c"
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
        };

        let constraint_system = replace_constrained_witness_columns(constraint_system, 3);
        // 1) y = x + 3
        // 2) z = y + 2 ⇒ z = (x + 3) + 2 = x + 5
        // 3) result = z + 1 ⇒ result = (x + 5) + 1 = x + 6
        let [BusInteraction { payload, .. }] = &constraint_system.bus_interactions[..] else {
            panic!();
        };
        let [result, x] = payload.as_slice() else {
            panic!();
        };
        assert_eq!(result.to_string(), "z + 1");
        assert_eq!(x.to_string(), "z + -5");
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
        };
        let constraint_system = replace_constrained_witness_columns(constraint_system, 3);

        let [identity] = &constraint_system.algebraic_constraints[..] else {
            panic!();
        };
        let [BusInteraction { payload, .. }] = &constraint_system.bus_interactions[..] else {
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
        assert_eq!(identity.to_string(), "(-b + -1) * (b + 1) + c");
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
        };

        let suboptimal_system = ConstraintSystem {
            algebraic_constraints: suboptimal_order_identities,
            bus_interactions: vec![],
        };

        // Apply the same optimization to both systems
        let optimal_system = replace_constrained_witness_columns(optimal_system, 5);
        let suboptimal_system = replace_constrained_witness_columns(suboptimal_system, 5);

        // Assert the difference in optimization results
        assert_eq!(optimal_system.algebraic_constraints.len(), 3);
        assert_eq!(suboptimal_system.algebraic_constraints.len(), 4);
    }
}
