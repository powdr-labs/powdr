use powdr_constraint_solver::{
    constraint_system::ConstraintSystem, quadratic_symbolic_expression::QuadraticSymbolicExpression,
};
use powdr_number::FieldElement;
use rayon::prelude::*;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::time::Instant;

/// Reduce variables in the constraint system by inlining them,
/// as long as the resulting degree stays within `max_degree`.
pub fn replace_constrained_witness_columns<
    T: FieldElement,
    V: Ord + Clone + Hash + Eq + Display + Send + Sync + Debug,
>(
    mut constraint_system: ConstraintSystem<T, V>,
    max_degree: usize,
) -> ConstraintSystem<T, V> {
    let mut deleted_constraints: HashSet<usize> = HashSet::new();

    let loop_init = Instant::now();

    let (mut inlinable_cache, var_to_constraints) = {
        let constraint_indices: Vec<_> = constraint_system.iter().enumerate().collect();

        let results: Vec<_> = constraint_indices
            .into_par_iter()
            .map(|(idx, constraint)| {
                let inlinables = find_inlinable_variables(constraint)
                    .into_iter()
                    .filter_map(|(v, e)| {
                        score_substitution(&v, &e, &constraint_system, max_degree)
                            .map(|s| (v, e, s))
                    })
                    .collect::<Vec<_>>();

                let vars: HashSet<V> = constraint.referenced_unknown_variables().cloned().collect();

                (idx, inlinables, vars)
            })
            .collect();

        let mut inlinable_cache: HashMap<usize, Vec<(V, QuadraticSymbolicExpression<T, V>, f64)>> =
            HashMap::new();
        let mut var_to_constraints: HashMap<V, HashSet<usize>> = HashMap::new();

        for (idx, inlinables, vars) in results {
            inlinable_cache.insert(idx, inlinables);

            for var in vars {
                var_to_constraints
                    .entry(var)
                    .or_insert_with(HashSet::new)
                    .insert(idx);
            }
        }

        (inlinable_cache, var_to_constraints)
    };
    println!("Init {:?}", loop_init.elapsed(),);

    let loop_start = Instant::now();

    loop {
        let did_something = try_apply_disjoint_substitutions(
            &mut constraint_system,
            max_degree,
            &mut deleted_constraints,
            &mut inlinable_cache,
            &var_to_constraints,
        );

        if !did_something {
            break;
        }
    }

    println!("[sustitución] Finalizado en {:?}", loop_start.elapsed());

    constraint_system.algebraic_constraints = constraint_system
        .algebraic_constraints
        .into_iter()
        .enumerate()
        .filter(|(i, _)| !deleted_constraints.contains(i))
        .map(|(_, c)| c)
        .collect();

    constraint_system
}

/// Attempts to apply one valid variable substitution across the constraint system.
///
/// Skips substitutions that would increase the degree beyond `max_degree`
/// or affect variables in the `keep` set. Returns true if a substitution was applied.
fn try_apply_disjoint_substitutions<
    T: FieldElement,
    V: Ord + Clone + Hash + Eq + Display + Send + Sync + Display + Debug,
>(
    constraint_system: &mut ConstraintSystem<T, V>,
    max_degree: usize,
    deleted_constraints: &mut HashSet<usize>,
    inlinable_cache: &mut HashMap<usize, Vec<(V, QuadraticSymbolicExpression<T, V>, f64)>>,
    var_to_constraints: &HashMap<V, HashSet<usize>>,
) -> bool {
    let candidates: Vec<(usize, V, QuadraticSymbolicExpression<T, V>, f64)> = inlinable_cache
        .iter()
        .filter(|(idx, _)| !deleted_constraints.contains(idx))
        .flat_map(|(&idx, entries)| {
            entries
                .iter()
                .map(move |(v, e, s)| (idx, v.clone(), e.clone(), *s))
        })
        .collect();

    if candidates.is_empty() {
        return false;
    }

    let mut affected_constraints: HashSet<usize> = HashSet::new();
    let selected: Vec<(usize, V, QuadraticSymbolicExpression<T, V>)> = {
        if let Some((idx, var, expr, _)) = candidates.into_iter().max_by(|a, b| a.3.total_cmp(&b.3))
        {
            for vars in expr.referenced_unknown_variables().chain([&var]) {
                if let Some(touched) = var_to_constraints.get(&vars) {
                    affected_constraints.extend(touched);
                }
            }

            vec![(idx, var.clone(), expr.clone())]
        } else {
            vec![]
        }
    };

    if selected.is_empty() {
        return false;
    }

    for (idx, var, expr) in &selected {
        for (_, constraint) in constraint_system.iter_mut().enumerate() {
            constraint.substitute_by_unknown(&var, &expr);
        }
        deleted_constraints.insert(*idx);
    }

    constraint_system
        .iter()
        .enumerate()
        .filter(|(i, _)| affected_constraints.contains(i))
        .for_each(|(i, constraint)| {
            let entries = find_inlinable_variables(constraint)
                .into_iter()
                .filter_map(|(v, e)| {
                    score_substitution(&v, &e, constraint_system, max_degree).map(|s| (v, e, s))
                })
                .collect();
            inlinable_cache.insert(i, entries);
        });

    true
}

/// Score a potential substitution based on heuristics
fn score_substitution<T: FieldElement, V: Ord + Clone + Hash + Eq + Display>(
    var: &V,
    expr: &QuadraticSymbolicExpression<T, V>,
    constraint_system: &ConstraintSystem<T, V>,
    max_degree: usize,
) -> Option<f64> {
    let replacement_deg = qse_degree(expr);
    let mut eliminable_constraints = 0;
    let mut simplifiable_constraints = 0;
    let mut total_degree_increase = 0.0;

    for constraint in &constraint_system.algebraic_constraints {
        let current_deg = qse_degree(constraint);
        let new_deg = qse_degree_with_virtual_substitution(constraint, var, replacement_deg);

        if new_deg > max_degree {
            return None;
        }

        let mut vars = constraint
            .referenced_unknown_variables()
            .collect::<Vec<_>>();
        let var_count_before = vars.len();
        vars.retain(|v| **v != *var);
        let var_count_after = vars.len();

        if var_count_after == 0 {
            eliminable_constraints += 1;
        } else if var_count_after < var_count_before {
            simplifiable_constraints += 1;
        }

        total_degree_increase += new_deg as f64 - current_deg as f64;
    }

    let num_constraints = constraint_system.algebraic_constraints.len() as f64;
    let avg_degree_increase = total_degree_increase / num_constraints;

    // Score formula:
    //   +1.0 for each eliminable constraint
    //   +0.5 for each simplifiable constraint
    //   -0.5 for each unit of average degree increase
    let score = eliminable_constraints as f64 * 1.0 + simplifiable_constraints as f64 * 0.5
        - avg_degree_increase * 0.5;

    Some(score)
}

/// Returns substitutions of variables that appear linearly and do not depend on themselves.
fn find_inlinable_variables<T: FieldElement, V: Ord + Clone + Hash + Eq + Send + Sync + Display>(
    constraint: &QuadraticSymbolicExpression<T, V>,
) -> Vec<(V, QuadraticSymbolicExpression<T, V>)> {
    let (_, linear, _) = constraint.components();

    let linear_vars: Vec<_> = linear.collect();

    linear_vars
        .into_par_iter()
        .filter_map(|(target_var, coeff)| {
            let coeff_const = coeff.try_to_number()?;

            if coeff_const.is_zero() {
                return None;
            }

            // Isolate target_var from the constraint equation.
            let rhs_qse = -constraint.clone()
                * QuadraticSymbolicExpression::from(T::one() / coeff_const)
                + QuadraticSymbolicExpression::from_unknown_variable(target_var.clone());

            if rhs_qse.try_to_known().is_some() {
                return None;
            }

            //Check if there is any target_var in the substitution.
            if rhs_qse
                .referenced_unknown_variables()
                .any(|v| v == target_var)
            {
                None
            } else {
                Some((target_var.clone(), rhs_qse))
            }
        })
        .collect()
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
        for (idx, algebraic_constraint) in constraint_system.iter().enumerate() {
            println!("Constraint {idx}: {algebraic_constraint}");
        }
        // 1) a + b + c = 0        => a = -b - c
        // 2) b + d - 1 = 0        => d = -b + 1
        // 3) c + b + a + d = result
        //    =(1)=> c + b + (-b - c) + d
        //         = (c - c) + (b - b) + d
        //         = 0 + 0 + d
        //    => result = d = -b + 1
        //    => b = -result + 1
        //assert_eq!(constraint_system.algebraic_constraints.len(), 0);
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

        assert_eq!(constraint_system.algebraic_constraints.len(), 2);
        for constraint in constraint_system.iter() {
            println!("- {}", constraint);
        }
        let [BusInteraction { payload, .. }] = &constraint_system.bus_interactions[..] else {
            panic!();
        };
        let [result] = payload.as_slice() else {
            panic!();
        };

        assert_eq!(result.to_string(), "(a) * (b) + a");
    }

    #[test]
    fn test_replace_constrained_witness_bus_interaction() {
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
        for constraint in constraint_system.iter() {
            println!("- {}", constraint);
        }
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
        assert_eq!(x.to_string(), "y + -3");
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

        for (i, constraint) in constraint_system.algebraic_constraints.iter().enumerate() {
            println!("Constraint {}: {}", i, constraint);
        }

        let [identity1, identity2] = &constraint_system.algebraic_constraints[..] else {
            panic!();
        };
        let [BusInteraction { payload, .. }] = &constraint_system.bus_interactions[..] else {
            panic!();
        };
        let [a, b, c, d, e, f, result] = payload.as_slice() else {
            panic!();
        };
        // From first identity: a = b + 1
        assert_eq!(a.to_string(), "a");
        // b kept as a symbol
        assert_eq!(b.to_string(), "b");
        // From second identity: c = a * a
        assert_eq!(c.to_string(), "c");
        // From third identity: d = c * a
        // In-lining d would violate the degree bound, so it is kept as a symbol
        // with constraints to enforce the equality.
        assert_eq!(d.to_string(), "(c) * (a)");
        assert_eq!(identity1.to_string(), "a + -b + -1");
        // From fourth identity: e = d * a
        assert_eq!(e.to_string(), "((c) * (a)) * (a)");
        // From fifth identity: f = e + 5
        assert_eq!(f.to_string(), "((c) * (a)) * (a) + 5");
        // From sixth identity: result = f * 2
        assert_eq!(result.to_string(), "((2 * c) * (a)) * (a) + 10");
    }

    #[test]
    fn test_inline_max_degree_old_suboptimal_greedy() {
        let mut optimal_order_identities = Vec::new();

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

        let optimal_system = ConstraintSystem {
            algebraic_constraints: optimal_order_identities,
            bus_interactions: vec![],
        };

        let optimal_system = replace_constrained_witness_columns(optimal_system, 5);
        for constraint in &optimal_system.algebraic_constraints {
            println!("{}", constraint);
        }
        assert_eq!(optimal_system.algebraic_constraints.len(), 3);
    }
}
