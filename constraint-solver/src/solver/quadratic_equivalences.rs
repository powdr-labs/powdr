//! Module to help find certain pairs of equivalent variables in a system of quadratic constraints.

use std::{
    collections::{BTreeMap, BTreeSet, HashSet},
    fmt::Display,
    hash::Hash,
};

use itertools::Itertools;
use powdr_number::FieldElement;

use crate::{
    boolean_extractor::extract_boolean,
    quadratic_symbolic_expression::{QuadraticSymbolicExpression, RangeConstraintProvider},
    range_constraint::RangeConstraint,
    symbolic_expression::SymbolicExpression,
};

struct RangeConstraintHack<R>(R);

impl<T: FieldElement, V: Ord + Clone + Hash + Eq + Display, R: RangeConstraintProvider<T, V>>
    RangeConstraintProvider<T, V> for RangeConstraintHack<R>
{
    fn get(&self, var: &V) -> RangeConstraint<T> {
        let name = var.to_string();
        if name.starts_with("mem_ptr_limbs_") {
            RangeConstraint::from_mask(0xffffu64)
        } else {
            RangeConstraint::default()
        }
        .conjunction(&self.0.get(var))
    }
}

// TODO rename, this is more about pairs of affine constraints now.
pub fn find_quadratic_equalities<T: FieldElement, V: Ord + Clone + Hash + Eq + Display>(
    constraints: &[QuadraticSymbolicExpression<T, V>],
    range_constraints: impl RangeConstraintProvider<T, V>,
) -> Vec<(V, V)> {
    let mut equalities = vec![];
    let constraints = constraints
        .iter()
        .cloned()
        .filter(|c| c.is_affine())
        .collect::<Vec<_>>();

    // TODO do we still need the hack?
    let range_constraints = RangeConstraintHack(range_constraints);

    // TODO create index?
    for (i, c1) in constraints.iter().enumerate() {
        if i % 100 == 0 {
            println!("Processing {i} of {}", constraints.len());
        }
        let vars1 = c1.referenced_unknown_variables().collect::<HashSet<_>>();
        for c2 in &constraints[i + 1..] {
            // TODO It's unlikely that we get something with a non-zero constant differnce,
            // but it could happen!
            if !(c1.components().2 - c2.components().2).is_known_zero() {
                continue;
            }
            let vars2 = c2.referenced_unknown_variables().collect::<HashSet<_>>();
            // Check if the two constraints share at least one variable.
            let Some(var) = vars1.intersection(&vars2).next() else {
                continue;
            };
            let coeff1 = c1.coefficient_of_variable(var).unwrap();
            let coeff2 = c2.coefficient_of_variable(var).unwrap();
            let difference = if coeff1 == coeff2 {
                c1 - c2
            } else if coeff1.is_known_nonzero() {
                &(c1.clone() * (coeff2.field_div(coeff1))) - c2
            } else if coeff2.is_known_nonzero() {
                c1 - &(c2.clone() * (coeff1.field_div(coeff2)))
            } else {
                // TODO we could try other shared variables
                continue;
            };

            let bool_vars = difference
                .referenced_unknown_variables()
                .cloned()
                .filter(|v| range_constraints.get(&v).range_width() == 2.into())
                .collect_vec();
            if bool_vars.len() > 6 {
                continue;
            }
            let Some(solve_for) = difference
                .referenced_unknown_variables()
                .cloned()
                .find(|v| {
                    let rc = range_constraints.get(&v);
                    rc.range_width() > 2.into()
                })
            else {
                // TODO In this case, there are only booleans left in the differenc,
                // which should be a valid case!
                // We really shouldn't be calling `solve_for()` below, but instead
                // detect inconsistency itself.
                continue;
            };
            let solve_for_rc = range_constraints.get(&solve_for);
            let results = get_all_possible_assignments(bool_vars, &range_constraints)
                .filter_map(|assignment| {
                    let mut diff = difference.clone();
                    for (var, val) in assignment {
                        diff.substitute_by_known(&var, &SymbolicExpression::from(val));
                    }
                    let solved = diff.try_solve_for(&solve_for).unwrap();
                    // TODO there has to be a better way, we should get an error
                    // just when calling solve().
                    if solved
                        .range_constraint(&range_constraints)
                        .is_disjoint(&solve_for_rc)
                    {
                        None
                    } else {
                        Some(solved)
                    }
                })
                .collect::<BTreeSet<_>>();
            // Conflicting system
            assert!(!results.is_empty());
            if let Some(result) = results.into_iter().exactly_one().ok() {
                if let Some(var) = result.try_to_simple_unknown() {
                    if var < solve_for {
                        equalities.push((var, solve_for));
                    } else {
                        equalities.push((solve_for, var));
                    }
                }
            }
        }
    }
    equalities
}

// TODO copied from exhaustive search
fn get_all_possible_assignments<T: FieldElement, V: Ord + Clone + Hash + Eq + Display>(
    variables: impl IntoIterator<Item = V>,
    range_constraints: &impl RangeConstraintProvider<T, V>,
) -> impl Iterator<Item = BTreeMap<V, T>> {
    let variables = variables.into_iter().collect_vec();
    variables
        .iter()
        .map(|v| range_constraints.get(v))
        .map(|rc| rc.allowed_values().collect::<Vec<_>>())
        .multi_cartesian_product()
        .map(|assignment| {
            variables
                .iter()
                .cloned()
                .zip(assignment)
                .collect::<BTreeMap<_, _>>()
        })
        .collect::<Vec<_>>()
        .into_iter()
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use crate::test_utils::{constant, var};

    use super::*;

    // impl<T: FieldElement> RangeConstraintProvider<T, &'static str>
    //     for HashMap<&'static str, RangeConstraint<T>>
    // {
    //     fn get(&self, var: &&'static str) -> RangeConstraint<T> {
    //         self.get(var).cloned().unwrap_or_default()
    //     }
    // }

    #[test]
    fn test_find_quadratic_equalities() {
        // (-943718400 * mem_ptr_limbs__0_175 + 30720 * mem_ptr_limbs__1_175 + 943718400 * rs1_data__0_661 + -120 * rs1_data__1_661 + -30720 * rs1_data__2_661 + -7864320 * rs1_data__3_661 + 1006632893) * (-943718400 * mem_ptr_limbs__0_175 + 30720 * mem_ptr_limbs__1_175 + 943718400 * rs1_data__0_661 + -120 * rs1_data__1_661 + -30720 * rs1_data__2_661 + -7864320 * rs1_data__3_661 + 1006632892)

        // (-943718400 * mem_ptr_limbs__0_279 + 30720 * mem_ptr_limbs__1_279 + 943718400 * rs1_data__0_661 + -120 * rs1_data__1_661 + -30720 * rs1_data__2_661 + -7864320 * rs1_data__3_661 + 251658182) * (-943718400 * mem_ptr_limbs__0_279 + 30720 * mem_ptr_limbs__1_279 + 943718400 * rs1_data__0_661 + -120 * rs1_data__1_661 + -30720 * rs1_data__2_661 + -7864320 * rs1_data__3_661 + 251658181)

        let constraints = vec![
            // (30720 * mem_ptr_limbs__0_175 + -30720 * rs1_data__0_661 + -7864320 * rs1_data__1_661 + -4423680) * (30720 * mem_ptr_limbs__0_175 + -30720 * rs1_data__0_661 + -7864320 * rs1_data__1_661 + -4423681)
            (var("mem_ptr_limbs__0_175") * constant(30720)
                - var("rs1_data__0_661") * constant(30720)
                - var("rs1_data__1_661") * constant(7864320)
                - constant(4423680))
                * (var("mem_ptr_limbs__0_175") * constant(30720)
                    - var("rs1_data__0_661") * constant(30720)
                    - var("rs1_data__1_661") * constant(7864320)
                    - constant(4423681)),
            // (30720 * mem_ptr_limbs__0_279 + -30720 * rs1_data__0_661 + -7864320 * rs1_data__1_661 + -3809280) * (30720 * mem_ptr_limbs__0_279 + -30720 * rs1_data__0_661 + -7864320 * rs1_data__1_661 + -3809281)
            (var("mem_ptr_limbs__0_279") * constant(30720)
                - var("rs1_data__0_661") * constant(30720)
                - var("rs1_data__1_661") * constant(7864320)
                - constant(4423680))
                * (var("mem_ptr_limbs__0_279") * constant(30720)
                    - var("rs1_data__0_661") * constant(30720)
                    - var("rs1_data__1_661") * constant(7864320)
                    - constant(4423681)),
        ];
        let range_constraints = [
            (
                "mem_ptr_limbs__0_175",
                RangeConstraint::from_mask(0xffffu32),
            ),
            (
                "mem_ptr_limbs__0_279",
                RangeConstraint::from_mask(0xffffu32),
            ),
            ("rs1_data__0_661", RangeConstraint::from_mask(0xffffu32)),
            ("rs1_data__1_661", RangeConstraint::from_mask(0xffffu32)),
        ]
        .into_iter()
        .collect::<HashMap<_, _>>();
        let result = find_quadratic_equalities(&constraints, range_constraints);
        assert_eq!(result.len(), 0);
    }
}
