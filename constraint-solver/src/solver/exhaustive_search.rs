use itertools::Itertools;
use powdr_number::FieldElement;
use powdr_number::LargeInt;

use crate::constraint_system::BusInteractionHandler;
use crate::constraint_system::ConstraintRef;
use crate::effect::Effect;
use crate::grouped_expression::RangeConstraintProvider;
use crate::indexed_constraint_system::IndexedConstraintSystem;
use crate::range_constraint::RangeConstraint;
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

/// Result of an exhaustive search on a variable set.
pub struct ExhaustiveSearchResult<T: FieldElement, V> {
    /// New range constraints deduced from the search.
    pub range_constraints: BTreeMap<V, RangeConstraint<T>>,
    /// All valid assignments of the flag variables that did not lead to a contradiction.
    pub valid_assignments: Vec<BTreeMap<V, T>>,
}

/// Goes through all possible assignments for the given variables and tries to deduce
/// new range constraints (on any variable) for each of the assignments. Returns the union of the obtained
/// range constraints over all assignments, along with the list of valid assignments.
/// Can also return range constraints for the input variables if some of them lead
/// to a contradiction.
/// Returns an error if all assignments are contradictory.
pub fn exhaustive_search_on_variable_set<T: FieldElement, V: Clone + Hash + Ord + Eq + Display>(
    constraint_system: &IndexedConstraintSystem<T, V>,
    variables: &BTreeSet<V>,
    range_constraints: impl RangeConstraintProvider<T, V> + Clone,
    bus_interaction_handler: &impl BusInteractionHandler<T>,
) -> Result<ExhaustiveSearchResult<T, V>, Error> {
    // Collect all valid assignments and their derived range constraints.
    let valid: Vec<_> = get_all_possible_assignments(variables.iter().cloned(), &range_constraints)
        .filter_map(|assignments| {
            let derived = derive_new_range_constraints(
                constraint_system,
                assignments.clone(),
                &range_constraints,
                bus_interaction_handler,
            )
            .ok()?;
            Some((assignments, derived))
        })
        .collect();

    if valid.is_empty() {
        return Err(Error::ExhaustiveSearchError);
    }

    let valid_assignments: Vec<_> = valid.iter().map(|(a, _)| a.clone()).collect();

    // Compute the disjunction of the range constraints across all valid assignments.
    let mut derived_iter = valid.into_iter().map(|(_, d)| d);
    let first = derived_iter.next().unwrap();
    let range_constraints_result = derived_iter.try_fold(first, |mut acc, new_constr| {
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
    let new_range_constraints = range_constraints_result.unwrap_or_default();

    Ok(ExhaustiveSearchResult {
        range_constraints: new_range_constraints,
        valid_assignments,
    })
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

/// Given a list of assignments of concrete values to variables, tries to derive
/// new range constraints from them. To keep this function relatively fast,
/// only tries to each algebraic or bus constraint it isolation.
/// Fails if any of the assignments *directly* contradicts any of the constraints.
/// Note that getting an OK(_) here does not mean that there is no contradiction, as
/// this function only does one step of the derivation.
fn derive_new_range_constraints<T: FieldElement, V: Clone + Hash + Ord + Eq + Display>(
    constraint_system: &IndexedConstraintSystem<T, V>,
    assignments: BTreeMap<V, T>,
    range_constraints: &impl RangeConstraintProvider<T, V>,
    bus_interaction_handler: &impl BusInteractionHandler<T>,
) -> Result<BTreeMap<V, RangeConstraint<T>>, ContradictingConstraintError> {
    let effects = constraint_system
        .constraints_referencing_variables(assignments.keys())
        .map(|constraint| match constraint {
            ConstraintRef::AlgebraicConstraint(identity) => {
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
            ConstraintRef::BusInteraction(bus_interaction) => {
                let mut bus_interaction = bus_interaction.clone();
                for (variable, value) in assignments.iter() {
                    bus_interaction
                        .fields_mut()
                        .for_each(|expr| expr.substitute_by_known(variable, value))
                }
                bus_interaction
                    .solve(bus_interaction_handler, range_constraints)
                    .map_err(|_| ContradictingConstraintError)
            }
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
