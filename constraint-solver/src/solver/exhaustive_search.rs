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
use crate::utils::{get_all_possible_assignments, has_few_possible_assignments};

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
    let prepared_constraints = constraint_system
        .constraints_referencing_variables(variables.iter())
        .map(|constraint| PreparedConstraint::new(constraint, variables, &range_constraints))
        .collect_vec();
    let mut new_constraints =
        get_all_possible_assignments(variables.iter().cloned(), &range_constraints).filter_map(
            |assignments| {
                derive_new_range_constraints(
                    &prepared_constraints,
                    assignments,
                    &range_constraints,
                    bus_interaction_handler,
                )
                .ok()
            },
        );
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
/// data that does not depend on the concrete assignment of those
/// variables can be precomputed.
enum PreparedConstraint<'a, T: FieldElement, V> {
    /// An algebraic constraint all of whose variables are search variables:
    /// can just be evaluated.
    FullyDetermined(&'a GroupedExpression<T, V>),
    /// An affine algebraic constraint with at least one non-search variable.
    /// The range constraint of the remaining terms can be precomputed.
    Affine(PreparedAffineConstraint<'a, T, V>),
    /// Any other constraint: cloned, substituted and solved per assignment.
    Generic(ConstraintRef<'a, T, V>),
}

/// See [`PreparedConstraint::Affine`].
struct PreparedAffineConstraint<'a, T: FieldElement, V> {
    /// The coefficient of each search variable in the constraint (zero for
    /// variables that do not occur), in sorted search-variable order.
    search_var_coefficients: Vec<T>,
    /// The constant offset of the constraint (before assignment).
    constant_offset: T,
    /// The sum of the range constraints of the non-search terms.
    remaining_range_constraint: RangeConstraint<T>,
    /// The precomputed data to solve for the non-search variables.
    remaining: RemainingVariablesPrecompute<'a, T, V>,
}

/// The non-search variables of a [`PreparedAffineConstraint`], together with
/// precomputed data to solve for them. There is at least one such variable,
/// as constraints without one are handled by
/// [`PreparedConstraint::FullyDetermined`].
enum RemainingVariablesPrecompute<'a, T: FieldElement, V> {
    /// A single remaining variable with the negated inverse of its coefficient.
    Single { variable: &'a V, coeff_factor: T },
    /// More than one remaining variable. For each remaining variable `x_j`,
    /// in linear component order: the variable, the negated inverse of its coefficient and the
    /// sum of the range constraints of the other remaining terms scaled by it.
    Multiple {
        var_precomputes: Vec<(&'a V, T, RangeConstraint<T>)>,
    },
}

impl<'a, T: FieldElement, V: Clone + Hash + Ord + Eq + Display> PreparedConstraint<'a, T, V> {
    fn new(
        constraint: ConstraintRef<'a, T, V>,
        variables: &BTreeSet<V>,
        range_constraints: &impl RangeConstraintProvider<T, V>,
    ) -> Self {
        match constraint {
            ConstraintRef::AlgebraicConstraint(identity)
                if identity
                    .expression
                    .referenced_unknown_variables()
                    .all(|v| variables.contains(v)) =>
            {
                PreparedConstraint::FullyDetermined(identity.expression)
            }
            ConstraintRef::AlgebraicConstraint(identity) if identity.expression.is_affine() => {
                let expr = identity.expression;
                let search_var_coefficients = variables
                    .iter()
                    .map(|v| {
                        expr.coefficient_of_variable_in_affine_part(v)
                            .copied()
                            .unwrap_or_else(T::zero)
                    })
                    .collect_vec();
                let remaining_terms = expr
                    .linear_components()
                    .filter(|(v, _)| !variables.contains(v))
                    .map(|(v, coeff)| (v, *coeff))
                    .collect_vec();
                let variable_rcs = remaining_terms
                    .iter()
                    .map(|(v, _)| range_constraints.get(v))
                    .collect_vec();
                // `remaining_terms` is not empty, because fully determined
                // constraints are handled above.
                let remaining_range_constraint = remaining_terms
                    .iter()
                    .zip(&variable_rcs)
                    .map(|((_, coeff), rc)| rc.combine_product(&coeff.range_constraint()))
                    .reduce(|rc1, rc2| rc1.combine_sum(&rc2))
                    .unwrap();
                let remaining = match remaining_terms.as_slice() {
                    [(variable, coeff)] => RemainingVariablesPrecompute::Single {
                        variable: *variable,
                        coeff_factor: -coeff.field_inverse(),
                    },
                    _ => RemainingVariablesPrecompute::Multiple {
                        var_precomputes: remaining_terms
                            .iter()
                            .enumerate()
                            .map(|(j, (variable, coeff))| {
                                let factor = -coeff.field_inverse();
                                let prefix = remaining_terms
                                    .iter()
                                    .zip(&variable_rcs)
                                    .enumerate()
                                    .filter(|(i, _)| *i != j)
                                    .map(|(_, ((_, c), var_rc))| {
                                        var_rc.combine_product(&(*c * factor).range_constraint())
                                    })
                                    .reduce(|rc1, rc2| rc1.combine_sum(&rc2))
                                    .unwrap();
                                (*variable, factor, prefix)
                            })
                            .collect_vec(),
                    },
                };
                PreparedConstraint::Affine(PreparedAffineConstraint {
                    search_var_coefficients,
                    constant_offset: *expr.constant_offset(),
                    remaining_range_constraint,
                    remaining,
                })
            }
            _ => PreparedConstraint::Generic(constraint),
        }
    }

    /// Computes the effects of solving the constraint after assigning concrete
    /// values to the search variables. The result is identical to substituting
    /// the values and solving the constraint.
    fn derive_effects(
        &self,
        assignments: &BTreeMap<V, T>,
        range_constraints: &impl RangeConstraintProvider<T, V>,
        bus_interaction_handler: &impl BusInteractionHandler<T>,
    ) -> Result<Vec<Effect<T, V>>, ContradictingConstraintError> {
        match self {
            PreparedConstraint::Affine(prepared) => {
                let constant = prepared
                    .search_var_coefficients
                    .iter()
                    .zip(assignments.values())
                    .fold(prepared.constant_offset, |acc, (coeff, value)| {
                        acc + *coeff * *value
                    });
                // Check satisfiability, like `AlgebraicConstraint::solve` does.
                let full_rc = prepared
                    .remaining_range_constraint
                    .combine_sum(&constant.range_constraint());
                if !full_rc.allows_value(T::zero()) {
                    return Err(ContradictingConstraintError);
                }
                match &prepared.remaining {
                    // A single unknown variable left: solve for it.
                    RemainingVariablesPrecompute::Single {
                        variable,
                        coeff_factor,
                    } => {
                        let value = constant * *coeff_factor;
                        if range_constraints
                            .get(variable)
                            .is_disjoint(&value.range_constraint())
                        {
                            return Err(ContradictingConstraintError);
                        }
                        Ok(vec![Effect::Assignment((*variable).clone(), value)])
                    }
                    // Multiple unknown variables: transfer range constraints.
                    RemainingVariablesPrecompute::Multiple { var_precomputes } => {
                        Ok(var_precomputes
                            .iter()
                            .filter_map(|(variable, factor, prefix)| {
                                let rc =
                                    prefix.combine_sum(&(constant * *factor).range_constraint());
                                (!rc.is_unconstrained())
                                    .then(|| Effect::RangeConstraint((*variable).clone(), rc))
                            })
                            .collect())
                    }
                }
            }
            PreparedConstraint::FullyDetermined(expr) => {
                // The constraint becomes fully known under the assignment:
                // if it evaluates to a non-zero value, the assignment is
                // contradictory, otherwise there is nothing to derive.
                let value = expr.evaluate_assignment(&mut |v| assignments[v]);
                if !value.is_zero() {
                    return Err(ContradictingConstraintError);
                }
                Ok(vec![])
            }
            PreparedConstraint::Generic(ConstraintRef::AlgebraicConstraint(identity)) => {
                let mut identity = identity.cloned();
                for (variable, value) in assignments.iter() {
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
                for (variable, value) in assignments.iter() {
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

/// Given a list of assignments of concrete values to variables, tries to derive
/// new range constraints from them. To keep this function relatively fast,
/// only tries each algebraic or bus constraint in isolation.
/// Fails if any of the assignments *directly* contradicts any of the constraints.
/// Note that getting an OK(_) here does not mean that there is no contradiction, as
/// this function only does one step of the derivation.
fn derive_new_range_constraints<T: FieldElement, V: Clone + Hash + Ord + Eq + Display>(
    constraints: &[PreparedConstraint<T, V>],
    assignments: BTreeMap<V, T>,
    range_constraints: &impl RangeConstraintProvider<T, V>,
    bus_interaction_handler: &impl BusInteractionHandler<T>,
) -> Result<BTreeMap<V, RangeConstraint<T>>, ContradictingConstraintError> {
    let effects = constraints
        .iter()
        .map(|constraint| {
            constraint.derive_effects(&assignments, range_constraints, bus_interaction_handler)
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
            assignments
                .into_iter()
                .map(|(v, val)| (v, RangeConstraint::from_value(val))),
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
