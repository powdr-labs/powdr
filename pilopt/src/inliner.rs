use powdr_constraint_solver::{
    constraint_system::ConstraintSystem,
    quadratic_symbolic_expression::QuadraticSymbolicExpression,
    symbolic_expression::{BinaryOperator, SymbolicExpression},
};
use powdr_number::FieldElement;
use std::hash::Hasher;
use std::{
    collections::BTreeSet,
    hash::{DefaultHasher, Hash},
};

fn hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

/// Reduce variables in the constraint system by inlining them,
/// as long as the resulting degree stays within `max_degree`.
pub fn replace_constrained_witness_columns<T: FieldElement, V: Ord + Clone + Hash + Eq>(
    constraint_system: &mut ConstraintSystem<T, V>,
    max_degree: usize,
) {
    let keep: BTreeSet<V> = constraint_system
        .bus_interactions
        .iter()
        .flat_map(|b| {
            b.payload.iter().flat_map(|expr| {
                expr.referenced_unknown_variables()
                    .cloned()
                    .collect::<Vec<_>>()
            })
        })
        .collect();

    loop {
        if !try_apply_substitution(constraint_system, &keep, max_degree) {
            break;
        }
    }
}

/// Attempts to apply one valid variable substitution across the constraint system.
///
/// Skips substitutions that would increase the degree beyond `max_degree`
/// or affect variables in the `keep` set. Returns true if a substitution was applied.
fn try_apply_substitution<T: FieldElement, V: Ord + Clone + Hash + Eq>(
    constraint_system: &mut ConstraintSystem<T, V>,
    keep: &BTreeSet<V>,
    max_degree: usize,
) -> bool {
    let indices: Vec<usize> = (0..constraint_system.algebraic_constraints.len()).collect();

    for idx in indices.into_iter().rev() {
        let constraint = &constraint_system.algebraic_constraints[idx];

        for (var, expr) in find_inlinable_variable(constraint) {
            if keep.contains(&var) {
                continue;
            }

            if is_valid_substitution(
                &var,
                &expr,
                &constraint_system.algebraic_constraints,
                max_degree,
                idx,
            ) {
                let changed = constraint_system
                    .algebraic_constraints
                    .iter_mut()
                    .enumerate()
                    .filter(|(i, _)| *i != idx)
                    // Fold to ensure substitution is applied to all constraints
                    .fold(false, |acc, (_, identity)| {
                        let hash_before = hash(identity);
                        identity.substitute_by_unknown(&var, &expr);
                        acc || hash_before != hash(identity)
                    });

                if changed {
                    constraint_system.algebraic_constraints.remove(idx);
                    return true;
                }
            }
        }
    }

    false
}

/// Finds variables in a constraint that can be isolated as var = expr.
///
/// Returns substitutions of variables that appear linearly and do not depend on themselves.
fn find_inlinable_variable<T: FieldElement, V: Ord + Clone + Hash + Eq>(
    constraint: &QuadraticSymbolicExpression<T, V>,
) -> Vec<(V, QuadraticSymbolicExpression<T, V>)> {
    let mut substitutions = vec![];

    if constraint.linear.is_empty() {
        return substitutions;
    }

    for (target_var, coeff) in &constraint.linear {
        let Some(coeff_const) = coeff.try_to_number() else {
            continue;
        };

        if coeff_const.is_zero() {
            continue;
        }

        let mut rhs = QuadraticSymbolicExpression::from(T::zero());

        for (l, r) in &constraint.quadratic {
            rhs += QuadraticSymbolicExpression {
                quadratic: vec![(l.clone(), r.clone())],
                linear: Default::default(),
                constant: T::zero().into(),
            };
        }

        for (other_var, other_coeff) in &constraint.linear {
            if other_var != target_var {
                let var_expr =
                    QuadraticSymbolicExpression::from_unknown_variable(other_var.clone());
                rhs += var_expr * other_coeff.clone();
            }
        }

        rhs += constraint.constant.clone().into();

        rhs = -rhs;

        let rhs_qse = if coeff_const.is_one() {
            rhs
        } else if coeff_const == -T::from(1) {
            -rhs
        } else {
            let inv = SymbolicExpression::Concrete(T::one())
                .field_div(&SymbolicExpression::Concrete(coeff_const));
            rhs * inv
        };

        if rhs_qse
            .referenced_unknown_variables()
            .any(|v| v == target_var)
        {
            continue;
        }

        substitutions.push((target_var.clone(), rhs_qse));
    }

    substitutions
}

/// Checks whether a substitution is valid under `max_degree` constraint.
fn is_valid_substitution<T: FieldElement, V: Ord + Clone + Hash + Eq>(
    var: &V,
    expr: &QuadraticSymbolicExpression<T, V>,
    identities: &[QuadraticSymbolicExpression<T, V>],
    max_degree: usize,
    exclude_idx: usize,
) -> bool {
    let appears_in_other_constraint = identities.iter().enumerate().any(|(idx, constraint)| {
        idx != exclude_idx && constraint.referenced_unknown_variables().any(|v| v == var)
    });

    if !appears_in_other_constraint {
        return false;
    }

    let replacement_deg = qse_degree(expr);
    for (idx, constraint) in identities.iter().enumerate() {
        if idx == exclude_idx {
            continue;
        }

        if constraint.referenced_unknown_variables().any(|v| v == var) {
            let degree = qse_degree_with_virtual_substitution(constraint, var, replacement_deg);

            if degree > max_degree {
                return false;
            }
        }
    }

    true
}

/// Calculate the degree of a QuadraticSymbolicExpression assuming a variable is
/// replaced by an expression of known degree.
fn qse_degree_with_virtual_substitution<T: FieldElement, V: Ord + Clone + Hash + Eq>(
    qse: &QuadraticSymbolicExpression<T, V>,
    var: &V,
    replacement_deg: usize,
) -> usize {
    let quad_deg = qse
        .quadratic
        .iter()
        .map(|(l, r)| {
            qse_degree_with_virtual_substitution(l, var, replacement_deg)
                + qse_degree_with_virtual_substitution(r, var, replacement_deg)
        })
        .max()
        .unwrap_or(0);

    let linear_deg = qse
        .linear
        .keys()
        .map(|v| if v == var { replacement_deg } else { 1 })
        .max()
        .unwrap_or(0);

    quad_deg.max(linear_deg)
}

/// Computes the degree of a QuadraticSymbolicExpression.
fn qse_degree<T: FieldElement, V: Ord + Clone + Hash + Eq>(
    qse: &QuadraticSymbolicExpression<T, V>,
) -> usize {
    let quad_deg = qse
        .quadratic
        .iter()
        .map(|(l, r)| qse_degree(l) + qse_degree(r))
        .max()
        .unwrap_or(0);

    let linear_deg = if qse.linear.is_empty() { 0 } else { 1 };

    let const_deg = symbolic_expression_degree(&qse.constant);

    quad_deg.max(linear_deg).max(const_deg)
}

/// Computes the degree of a SymbolicExpression.
fn symbolic_expression_degree<T: FieldElement, V: Ord + Clone + Hash + Eq>(
    expr: &SymbolicExpression<T, V>,
) -> usize {
    match expr {
        SymbolicExpression::Concrete(_) => 0,
        SymbolicExpression::Symbol(_, _) => 1,
        SymbolicExpression::BinaryOperation(left, op, right, _) => {
            let left_degree = symbolic_expression_degree(left);
            let right_degree = symbolic_expression_degree(right);
            match op {
                // For addition and subtraction, the degree is the maximum of the operands
                BinaryOperator::Add | BinaryOperator::Sub => left_degree.max(right_degree),
                // For multiplication, the degree is the sum of the operands
                BinaryOperator::Mul => left_degree + right_degree,
                // For division, we assume the divisor is constant
                BinaryOperator::Div => left_degree,
            }
        }
        // For unary operations, the degree is the same as the operand
        SymbolicExpression::UnaryOperation(_, inner, _) => symbolic_expression_degree(inner),
    }
}
#[cfg(test)]
mod test {
    use powdr_constraint_solver::constraint_system::BusInteraction;
    use powdr_number::GoldilocksField;

    use super::*;

    #[test]
    fn test_replace_witness_columns() {
        type T = powdr_number::GoldilocksField;
        type Qse = QuadraticSymbolicExpression<T, &'static str>;

        let mut identities = Vec::new();

        let col_a = "a";
        let col_b = "b";
        let col_c = "c";
        let col_d = "d";

        // a + b + c = 0
        let constraint1 = Qse::from_unknown_variable(col_a)
            + Qse::from_unknown_variable(col_b)
            + Qse::from_unknown_variable(col_c);
        identities.push(constraint1);

        // b + d = 0
        let constraint2 = Qse::from_unknown_variable(col_b) + Qse::from_unknown_variable(col_d);
        identities.push(constraint2);

        let expr = Qse::from_unknown_variable(col_c)
            + Qse::from_unknown_variable(col_b)
            + Qse::from_unknown_variable(col_a)
            + Qse::from_unknown_variable(col_d);

        // c + b + a + d - result = 0
        let expr_constraint = expr.clone() - Qse::from_unknown_variable("result");
        identities.push(expr_constraint);

        // keep column result
        let bus_interactions = vec![BusInteraction {
            bus_id: constant(1),
            payload: vec![Qse::from_unknown_variable("result")],
            multiplicity: constant(1),
        }];

        let mut constraint_system = ConstraintSystem {
            algebraic_constraints: identities,
            bus_interactions,
        };

        replace_constrained_witness_columns(&mut constraint_system, 3);
        // 1) a + b + c = 0        => a = -b - c
        // 2) b + d = 0            => b = -d
        // 3) c + b + a + d = result
        //    =(1)=> c + b + (-b - c) + d
        //         = (c - c) + (b - b) + d
        //         = 0 + 0 + d
        //    => result = d
        // ⇒ result - d = 0
        assert_eq!(constraint_system.algebraic_constraints.len(), 1);
        assert_eq!(
            constraint_system.algebraic_constraints[0].to_string(),
            "b + result"
        );
    }

    #[test]
    fn test_replace_witness_columns_with_multiplication() {
        type T = powdr_number::GoldilocksField;
        type Qse = QuadraticSymbolicExpression<T, &'static str>;

        let mut identities = Vec::new();

        let col_a = "a";
        let col_b = "b";
        let col_c = "c";
        let col_d = "d";

        // a * b = c
        let constraint1 = Qse::from_unknown_variable(col_c)
            - Qse::from_unknown_variable(col_a) * Qse::from_unknown_variable(col_b);
        identities.push(constraint1);

        // b + d = 0
        let constraint2 = Qse::from_unknown_variable(col_b) + Qse::from_unknown_variable(col_d);
        identities.push(constraint2);

        let expr = Qse::from_unknown_variable(col_a)
            + Qse::from_unknown_variable(col_b)
            + Qse::from_unknown_variable(col_c)
            + Qse::from_unknown_variable(col_d);

        // a + b + c + d - result = 0
        let expr_constraint = expr.clone() - Qse::from_unknown_variable("result");
        identities.push(expr_constraint);

        // keep column `result`
        let bus_interactions = vec![BusInteraction {
            bus_id: constant(1),
            payload: vec![Qse::from_unknown_variable("result")],
            multiplicity: constant(1),
        }];

        let mut constraint_system = ConstraintSystem {
            algebraic_constraints: identities,
            bus_interactions,
        };

        replace_constrained_witness_columns(&mut constraint_system, 3);
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
        type T = powdr_number::GoldilocksField;
        type Qse = QuadraticSymbolicExpression<T, &'static str>;

        let mut identities = Vec::new();

        let col_a = "a";
        let col_b = "b";
        let col_c = "c";
        let col_d = "d";
        let col_e = "e";

        // a * b = c
        let constraint1 = Qse::from_unknown_variable(col_c)
            - Qse::from_unknown_variable(col_a) * Qse::from_unknown_variable(col_b);
        identities.push(constraint1);

        // b + d = 0
        let constraint2 = Qse::from_unknown_variable(col_b) + Qse::from_unknown_variable(col_d);
        identities.push(constraint2);

        // c * d = e
        let constraint3 = Qse::from_unknown_variable(col_e)
            - Qse::from_unknown_variable(col_c) * Qse::from_unknown_variable(col_d);
        identities.push(constraint3);

        let expr = Qse::from_unknown_variable(col_a)
            + Qse::from_unknown_variable(col_b)
            + Qse::from_unknown_variable(col_c)
            + Qse::from_unknown_variable(col_d)
            + Qse::from_unknown_variable(col_e);

        // a + b + c + d + e - result = 0
        let expr_constraint = expr.clone() - Qse::from_unknown_variable("result");
        identities.push(expr_constraint);

        // no columns to keep
        let mut constraint_system = ConstraintSystem {
            algebraic_constraints: identities,
            bus_interactions: vec![],
        };

        replace_constrained_witness_columns(&mut constraint_system, 3);
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
        type T = powdr_number::GoldilocksField;
        type Qse = QuadraticSymbolicExpression<T, &'static str>;

        let mut identities = Vec::new();

        // y = x + 3
        let constraint2 =
            Qse::from_unknown_variable("y") - (Qse::from_unknown_variable("x") + constant(3));
        identities.push(constraint2);

        // z = y + 2
        let constraint3 =
            Qse::from_unknown_variable("z") - (Qse::from_unknown_variable("y") + constant(2));
        identities.push(constraint3);

        // result = z + 1
        let constraint4 =
            Qse::from_unknown_variable("result") - (Qse::from_unknown_variable("z") + constant(1));
        identities.push(constraint4);

        // keep column results
        let bus_interactions = vec![BusInteraction {
            bus_id: constant(1),
            payload: vec![Qse::from_unknown_variable("result")],
            multiplicity: constant(1),
        }];

        let mut constraint_system = ConstraintSystem {
            algebraic_constraints: identities,
            bus_interactions,
        };

        replace_constrained_witness_columns(&mut constraint_system, 3);
        // 1) y = x + 3
        // 2) z = y + 2 ⇒ z = (x + 3) + 2 = x + 5
        // 3) result = z + 1 ⇒ result = (x + 5) + 1 = x + 6
        // ⇒ result - x - 6 = 0 ⇒ result + -x + -6 = 0
        assert_eq!(constraint_system.algebraic_constraints.len(), 1);
        assert_eq!(
            constraint_system.algebraic_constraints[0].to_string(),
            "result + -x + -6"
        );
    }

    #[test]
    fn test_replace_constrained_witness_columns_max_degree_limit() {
        type T = powdr_number::GoldilocksField;
        type Qse = QuadraticSymbolicExpression<T, &'static str>;

        let mut identities = Vec::new();

        // a = b + 1
        let constraint1 =
            Qse::from_unknown_variable("a") - (Qse::from_unknown_variable("b") + constant(1));
        identities.push(constraint1);

        // c = a * a
        let constraint2 = Qse::from_unknown_variable("c")
            - (Qse::from_unknown_variable("a") * Qse::from_unknown_variable("a"));
        identities.push(constraint2);

        // d = c * a
        let constraint3 = Qse::from_unknown_variable("d")
            - (Qse::from_unknown_variable("c") * Qse::from_unknown_variable("a"));
        identities.push(constraint3);

        // e = d * a
        let constraint4 = Qse::from_unknown_variable("e")
            - (Qse::from_unknown_variable("d") * Qse::from_unknown_variable("a"));
        identities.push(constraint4);

        // f = e + 5
        let constraint5 =
            Qse::from_unknown_variable("f") - (Qse::from_unknown_variable("e") + constant(5));
        identities.push(constraint5);

        // result = f * 2
        let constraint6 =
            Qse::from_unknown_variable("result") - (Qse::from_unknown_variable("f") * constant(2));
        identities.push(constraint6);

        let mut constraint_system = ConstraintSystem {
            algebraic_constraints: identities,
            bus_interactions: vec![],
        };

        replace_constrained_witness_columns(&mut constraint_system, 3);
        // 1) a = b + 1
        //    ⇒ a = b + 1
        //
        // 2) c = a * a
        //    ⇒ c = (b + 1)^2
        //    BUT: we choose not to inline this in further constraints, to prevent exceeding degree 3
        //    So instead, we keep this as an explicit identity:
        //    ⇒ (-b - 1)(b + 1) + c = 0
        //       ⤴ This becomes Constraint 0
        //
        // 3) d = c * a
        //    = c * (b + 1)
        //
        // 4) e = d * a
        //    = c * (b + 1)^2
        //      → would be (b + 1)^4 if c is inlined ⇒ degree 4 ❌ → STOP
        //
        // 5) f = e + 5
        //    = c * (b + 1)^2 + 5
        //
        // 6) result = f * 2
        //    = 2 * (c * (b + 1)^2 + 5)
        //    = 2 * c * (b + 1)^2 + 10
        //    ⇒ 0 = result - 2 * c * (b + 1)^2 - 10
        //    ⇒ 0 = (-c) * (b + 1) * (b + 1) - 2 * result - 5
        //       ⤴ This becomes Constraint 1
        //
        // Final result:
        //    Constraint 0 encodes the definition of c without inlining
        //    Constraint 1 uses c symbolically to prevent degree overflow

        assert_eq!(constraint_system.algebraic_constraints.len(), 2);
        assert_eq!(
            constraint_system.algebraic_constraints[0].to_string(),
            "(-b + -1) * (b + 1) + c"
        );
        assert_eq!(
            constraint_system.algebraic_constraints[1].to_string(),
            "((-c) * (b + 1)) * (b + 1) + -9223372034707292160 * result + -5"
        );
    }

    pub fn constant(value: u64) -> QuadraticSymbolicExpression<GoldilocksField, &'static str> {
        GoldilocksField::from(value).into()
    }
}
