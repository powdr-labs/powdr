use itertools::Itertools;
use powdr_number::FieldElement;
use powdr_number::LargeInt;

use crate::constraint_system::BusInteractionHandler;
use crate::constraint_system::ConstraintRef;
use crate::effect::Effect;
use crate::grouped_expression::{GroupedExpression, RangeConstraintProvider};
use crate::indexed_constraint_system::IndexedConstraintSystem;
use crate::range_constraint::RangeConstraint;
use crate::runtime_constant::RuntimeConstant;
use crate::utils::has_few_possible_assignments;

use std::collections::btree_map::Entry;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Display;
use std::hash::Hash;

use super::Error;

/// The maximum number of possible assignments to try when doing exhaustive search.
const MAX_SEARCH_WIDTH: u64 = 1 << 10;
/// The maximum range width of a variable to be considered for exhaustive search.
const MAX_VAR_RANGE_WIDTH: u64 = 5;

/// Goes through all possible assignments for the given variables and tries no deduce
/// new range constraints (on any variable) for each of the assignments. Returns the union of the obtained
/// range constraints over all assignments.
/// Can also return range constraints for the input variables if some of them lead
/// to a contradiction.
/// Returns an error if all assignments are contradictory.
pub fn exhaustive_search_on_variable_set<T: FieldElement, V: Clone + Hash + Ord + Eq + Display>(
    constraint_system: &IndexedConstraintSystem<T, V>,
    variables: &BTreeSet<V>,
    range_constraints: impl RangeConstraintProvider<T, V> + Clone,
    bus_interaction_handler: &impl BusInteractionHandler<T>,
) -> Result<BTreeMap<V, RangeConstraint<T>>, Error> {
    // The set of constraints to consider is the same for all assignments,
    // so we prepare them only once, pre-computing everything that does not
    // depend on the concrete assignment.
    let variables_vec = variables.iter().cloned().collect_vec();
    let prepared_constraints = constraint_system
        .constraints_referencing_variables(variables.iter())
        .map(|constraint| {
            PreparedConstraint::new(constraint, variables, &variables_vec, &range_constraints)
        })
        .collect_vec();
    // This enumerates the same assignments in the same order as
    // `get_all_possible_assignments`, but avoids building a `BTreeMap` per
    // assignment: the values are aligned with `variables_vec`.
    let mut new_constraints = variables
        .iter()
        .map(|v| {
            range_constraints
                .get(v)
                .allowed_values()
                .collect_vec()
                .into_iter()
        })
        .multi_cartesian_product()
        .filter_map(|values| {
            derive_new_range_constraints(
                &prepared_constraints,
                &variables_vec,
                &values,
                &range_constraints,
                bus_interaction_handler,
            )
            .ok()
        });
    let Some(first_assignment_constraints) = new_constraints.next() else {
        // No assignment satisfied the constraint system.
        return Err(Error::ExhaustiveSearchError);
    };
    // Compute the disjunction of the effects af each assignment.
    let result = new_constraints.try_fold(first_assignment_constraints, |mut acc, new_constr| {
        for (var, rc) in &mut acc {
            let other_rc = new_constr.get(var).cloned().unwrap_or_default();
            *rc = rc.disjunction(&other_rc)
        }
        // Remove the constraints that are not better than the ones we already know.
        acc.retain(|v, rc| range_constraints.get(v) != *rc);
        if acc.is_empty() {
            // Exiting early here is crucial for performance.
            // This is not an error though, it only means we could not find an improvement.
            return Err(());
        }
        Ok(acc)
    });
    match result {
        Ok(assignments) => Ok(assignments),
        Err(_) => Ok(Default::default()),
    }
}

/// Returns all unique sets of variables that appear together in an identity
/// (either in an algebraic constraint or in the same field of a bus interaction),
/// IF the number of possible assignments is less than `MAX_SEARCH_WIDTH`.
pub fn get_brute_force_candidates<'a, T: FieldElement, V: Clone + Hash + Ord>(
    constraint_system: &'a IndexedConstraintSystem<T, V>,
    rc: impl RangeConstraintProvider<T, V> + Clone + 'a,
) -> impl Iterator<Item = BTreeSet<V>> + 'a {
    constraint_system
        .algebraic_constraints()
        .iter()
        .map(|c| &c.expression)
        .chain(
            constraint_system
                .bus_interactions()
                .iter()
                .flat_map(|b| b.fields()),
        )
        .map(|expression| {
            expression
                .referenced_unknown_variables()
                .cloned()
                .collect::<BTreeSet<_>>()
        })
        .unique()
        .filter_map(move |variables| {
            match is_candidate_for_exhaustive_search(&variables, &rc) {
                true => Some(variables),
                false => {
                    // It could be that only one variable has a large range, but that the rest uniquely determine it.
                    // In that case, searching through all combinations of the other variables would be enough.
                    // Check if removing the variable results in a small enough set of possible assignments.
                    let num_variables = variables.len();
                    let variables_without_largest_range = variables
                        .into_iter()
                        .sorted_by(|a, b| rc.get(a).size_estimate().cmp(&rc.get(b).size_estimate()))
                        .take(num_variables - 1)
                        .collect::<BTreeSet<_>>();
                    is_candidate_for_exhaustive_search(&variables_without_largest_range, &rc)
                        .then_some(variables_without_largest_range)
                }
            }
        })
        .filter(|variables| !variables.is_empty())
        .unique()
}

fn is_candidate_for_exhaustive_search<T: FieldElement, V: Clone + Ord>(
    variables: &BTreeSet<V>,
    rc: &impl RangeConstraintProvider<T, V>,
) -> bool {
    has_few_possible_assignments(variables.iter().cloned(), rc, MAX_SEARCH_WIDTH)
        && has_small_max_range_constraint_size(variables.iter().cloned(), rc, MAX_VAR_RANGE_WIDTH)
}

fn has_small_max_range_constraint_size<T: FieldElement, V: Clone + Ord>(
    mut variables: impl Iterator<Item = V>,
    rc: &impl RangeConstraintProvider<T, V>,
    threshold: u64,
) -> bool {
    variables.all(|v| {
        if let Some(size) = rc.get(&v).size_estimate().try_into_u64() {
            size <= threshold
        } else {
            false
        }
    })
}

/// The provided assignments lead to a contradiction in the constraint system.
struct ContradictingConstraintError;

/// A constraint prepared for exhaustive search over a fixed set of variables:
/// everything that does not depend on the concrete assignment of those
/// variables is precomputed.
enum PreparedConstraint<'a, T: FieldElement, V> {
    /// An affine algebraic constraint. Assigning concrete values to the search
    /// variables only changes its constant offset, so the range constraints of
    /// the remaining terms (and their per-variable folds) are precomputed.
    Affine(PreparedAffineConstraint<'a, T, V>),
    /// An algebraic constraint all of whose variables are search variables:
    /// it becomes fully known under any assignment and can just be evaluated.
    FullyDetermined(&'a GroupedExpression<T, V>),
    /// Any other constraint: cloned, substituted and solved per assignment.
    Generic(ConstraintRef<'a, T, V>),
}

/// See [`PreparedConstraint::Affine`]. The precomputed data exactly mirrors
/// the computations that solving the substituted constraint would perform,
/// so the derived effects are identical.
struct PreparedAffineConstraint<'a, T: FieldElement, V> {
    /// The coefficients of the search variables occurring in the constraint,
    /// as indices into the assignment values.
    set_coefficients: Vec<(usize, T)>,
    /// The constant offset of the constraint (before assignment).
    base_constant: T,
    /// The (variable, coefficient) pairs of the non-search variables,
    /// in linear component order.
    remaining: Vec<(&'a V, T)>,
    /// The sum of the range constraints of the `remaining` terms
    /// (`None` if there are no remaining variables). Adding the range
    /// constraint of the assigned constant yields the range constraint
    /// of the full (substituted) expression.
    gate_prefix: Option<RangeConstraint<T>>,
    /// For each remaining variable `x_j` (only if there are at least two):
    /// the factor `-1 / c_j` and the sum of the range constraints of the other
    /// remaining terms scaled by that factor. Adding the range constraint of
    /// the scaled assigned constant yields the range constraint of the result
    /// of solving the (substituted) constraint for `x_j`
    /// (see `AlgebraicConstraint::transfer_constraints`).
    transfer: Vec<(T, RangeConstraint<T>)>,
    /// If there is exactly one remaining variable with coefficient `c`,
    /// this is `1 / -c`, so that solving for the variable is a single
    /// multiplication per assignment.
    single_var_solve_factor: Option<T>,
}

impl<'a, T: FieldElement, V: Clone + Hash + Ord + Eq + Display> PreparedConstraint<'a, T, V> {
    fn new(
        constraint: ConstraintRef<'a, T, V>,
        variables: &BTreeSet<V>,
        variables_vec: &[V],
        range_constraints: &impl RangeConstraintProvider<T, V>,
    ) -> Self {
        match constraint {
            ConstraintRef::AlgebraicConstraint(identity) if identity.expression.is_affine() => {
                let expr = identity.expression;
                let set_coefficients = variables_vec
                    .iter()
                    .enumerate()
                    .filter_map(|(index, v)| {
                        expr.coefficient_of_variable_in_affine_part(v)
                            .map(|coeff| (index, *coeff))
                    })
                    .collect_vec();
                let remaining = expr
                    .linear_components()
                    .filter(|(v, _)| !variables.contains(v))
                    .map(|(v, coeff)| (v, *coeff))
                    .collect_vec();
                let variable_rcs = remaining
                    .iter()
                    .map(|(v, _)| range_constraints.get(v))
                    .collect_vec();
                let gate_prefix = remaining
                    .iter()
                    .zip(&variable_rcs)
                    .map(|((_, coeff), rc)| rc.combine_product(&coeff.range_constraint()))
                    .reduce(|rc1, rc2| rc1.combine_sum(&rc2));
                let transfer = if remaining.len() >= 2 {
                    remaining
                        .iter()
                        .enumerate()
                        .map(|(j, (_, coeff))| {
                            let factor = -coeff.field_inverse();
                            let prefix = remaining
                                .iter()
                                .zip(&variable_rcs)
                                .enumerate()
                                .filter(|(i, _)| *i != j)
                                .map(|(_, ((_, c), var_rc))| {
                                    var_rc.combine_product(&(*c * factor).range_constraint())
                                })
                                .reduce(|rc1, rc2| rc1.combine_sum(&rc2))
                                .unwrap();
                            (factor, prefix)
                        })
                        .collect_vec()
                } else {
                    Vec::new()
                };
                let single_var_solve_factor = match remaining.as_slice() {
                    [(_, coeff)] => Some(T::one().field_div(&-*coeff)),
                    _ => None,
                };
                PreparedConstraint::Affine(PreparedAffineConstraint {
                    set_coefficients,
                    base_constant: *expr.constant_offset(),
                    remaining,
                    gate_prefix,
                    transfer,
                    single_var_solve_factor,
                })
            }
            ConstraintRef::AlgebraicConstraint(identity)
                if identity
                    .expression
                    .referenced_unknown_variables()
                    .all(|v| variables.contains(v)) =>
            {
                PreparedConstraint::FullyDetermined(identity.expression)
            }
            _ => PreparedConstraint::Generic(constraint),
        }
    }

    /// Computes the effects of solving the constraint after assigning `values`
    /// (aligned with `variables`) to the search variables. The result is
    /// identical to substituting the values and solving the constraint.
    fn derive_effects(
        &self,
        variables: &[V],
        values: &[T],
        range_constraints: &impl RangeConstraintProvider<T, V>,
        bus_interaction_handler: &impl BusInteractionHandler<T>,
    ) -> Result<Vec<Effect<T, V>>, ContradictingConstraintError> {
        match self {
            PreparedConstraint::Affine(prepared) => {
                let constant = prepared
                    .set_coefficients
                    .iter()
                    .fold(prepared.base_constant, |acc, (index, coeff)| {
                        acc + *coeff * values[*index]
                    });
                // Check satisfiability, like `AlgebraicConstraint::solve` does.
                let constant_rc = constant.range_constraint();
                let full_rc = match &prepared.gate_prefix {
                    Some(prefix) => prefix.combine_sum(&constant_rc),
                    None => constant_rc,
                };
                if !full_rc.allows_value(T::zero()) {
                    return Err(ContradictingConstraintError);
                }
                match prepared.remaining.as_slice() {
                    // Fully known: the satisfiability check above ensures it is zero.
                    [] => Ok(vec![]),
                    // A single unknown variable left: solve for it.
                    [(var, _)] => {
                        let value = constant * prepared.single_var_solve_factor.unwrap();
                        if range_constraints
                            .get(var)
                            .is_disjoint(&value.range_constraint())
                        {
                            return Err(ContradictingConstraintError);
                        }
                        Ok(vec![Effect::Assignment((*var).clone(), value)])
                    }
                    // Multiple unknown variables: transfer range constraints.
                    _ => Ok(prepared
                        .transfer
                        .iter()
                        .zip(&prepared.remaining)
                        .filter_map(|((factor, prefix), (var, _))| {
                            let rc = prefix.combine_sum(&(constant * *factor).range_constraint());
                            (!rc.is_unconstrained())
                                .then(|| Effect::RangeConstraint((*var).clone(), rc))
                        })
                        .collect()),
                }
            }
            PreparedConstraint::FullyDetermined(expr) => {
                // The constraint becomes fully known under the assignment:
                // if it evaluates to a non-zero value, the assignment is
                // contradictory, otherwise there is nothing to derive.
                let value =
                    expr.evaluate_concrete(&mut |v| values[variables.binary_search(v).unwrap()]);
                if !value.is_zero() {
                    return Err(ContradictingConstraintError);
                }
                Ok(vec![])
            }
            PreparedConstraint::Generic(ConstraintRef::AlgebraicConstraint(identity)) => {
                let mut identity = identity.cloned();
                for (variable, value) in variables.iter().zip(values) {
                    identity.substitute_by_known(variable, value);
                }
                identity
                    .as_ref()
                    .solve(range_constraints)
                    .map(|result| result.effects)
                    .map_err(|_| ContradictingConstraintError)
            }
            PreparedConstraint::Generic(ConstraintRef::BusInteraction(bus_interaction)) => {
                let mut bus_interaction = (*bus_interaction).clone();
                for (variable, value) in variables.iter().zip(values) {
                    bus_interaction
                        .fields_mut()
                        .for_each(|expr| expr.substitute_by_known(variable, value))
                }
                bus_interaction
                    .solve(bus_interaction_handler, range_constraints)
                    .map_err(|_| ContradictingConstraintError)
            }
        }
    }
}

/// Given a list of assignments of concrete values to variables (`values` is
/// aligned with `variables`), tries to derive new range constraints from them.
/// To keep this function relatively fast, only tries each algebraic or bus
/// constraint in isolation.
/// Fails if any of the assignments *directly* contradicts any of the constraints.
/// Note that getting an OK(_) here does not mean that there is no contradiction, as
/// this function only does one step of the derivation.
fn derive_new_range_constraints<T: FieldElement, V: Clone + Hash + Ord + Eq + Display>(
    constraints: &[PreparedConstraint<T, V>],
    variables: &[V],
    values: &[T],
    range_constraints: &impl RangeConstraintProvider<T, V>,
    bus_interaction_handler: &impl BusInteractionHandler<T>,
) -> Result<BTreeMap<V, RangeConstraint<T>>, ContradictingConstraintError> {
    let effects = constraints
        .iter()
        .map(|constraint| {
            constraint.derive_effects(
                variables,
                values,
                range_constraints,
                bus_interaction_handler,
            )
        })
        // Early return if any constraint leads to a contradiction.
        .collect::<Result<Vec<_>, _>>()?;

    effects
        .into_iter()
        .flatten()
        .filter_map(|effect| match effect {
            Effect::Assignment(variable, value) => {
                // Turn assignment into range constraint, we can recover it later.
                Some((variable, RangeConstraint::from_value(value)))
            }
            Effect::RangeConstraint(variable, rc) => Some((variable, rc)),
            _ => None,
        })
        .chain(
            variables
                .iter()
                .zip(values)
                .map(|(v, val)| (v.clone(), RangeConstraint::from_value(*val))),
        )
        // All range constraints in this iterator hold simultaneously,
        // so we compute the intersection for each variable.
        .try_fold(BTreeMap::new(), |mut map, (variable, rc)| {
            match map.entry(variable.clone()) {
                Entry::Vacant(entry) => {
                    entry.insert(rc);
                }
                Entry::Occupied(mut entry) => {
                    let existing = entry.get();
                    if existing.is_disjoint(&rc) {
                        return Err(ContradictingConstraintError);
                    }
                    entry.insert(existing.conjunction(&rc));
                }
            }
            Ok(map)
        })
}
