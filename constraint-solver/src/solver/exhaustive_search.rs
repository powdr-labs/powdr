use itertools::Itertools;
use powdr_number::ExpressionConvertible;
use powdr_number::FieldElement;
use powdr_number::LargeInt;

use crate::constraint_system::BusInteractionHandler;
use crate::constraint_system::ConstraintRef;
use crate::effect::Effect;
use crate::grouped_expression::RangeConstraintProvider;
use crate::indexed_constraint_system::IndexedConstraintSystem;
use crate::runtime_constant::ReferencedSymbols;
use crate::runtime_constant::RuntimeConstant;
use crate::runtime_constant::Substitutable;
use crate::utils::{get_all_possible_assignments, has_few_possible_assignments};

use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Display;
use std::hash::Hash;

use super::Error;

/// The maximum number of possible assignments to try when doing exhaustive search.
const MAX_SEARCH_WIDTH: u64 = 1 << 10;
/// The maximum range width of a variable to be considered for exhaustive search.
const MAX_VAR_RANGE_WIDTH: u64 = 5;

/// Goes through all possible assignments for the given variables and checks whether they satisfy
/// all constraints. If exactly one assignment satisfies the constraint system (and all others
/// lead to a contradiction), it returns that assignment.
/// If multiple assignments satisfy the constraint system, it returns `None`.
/// Returns an error if all assignments are contradictory.
pub fn find_unique_assignment_for_set<T, V: Clone + Hash + Ord + Eq + Display>(
    constraint_system: &IndexedConstraintSystem<T, V>,
    variables: &BTreeSet<V>,
    rc: impl RangeConstraintProvider<T::FieldType, V> + Clone,
    bus_interaction_handler: &impl BusInteractionHandler<T::FieldType>,
) -> Result<Option<BTreeMap<V, T::FieldType>>, Error>
where
    T: RuntimeConstant
        + ReferencedSymbols<V>
        + Substitutable<V>
        + ExpressionConvertible<T::FieldType, V>
        + Display,
{
    let mut assignments =
        get_all_possible_assignments(variables.iter().cloned(), &rc).filter_map(|assignments| {
            derive_more_assignments(constraint_system, assignments, &rc, bus_interaction_handler)
                .ok()
        });
    let Some(first_assignments) = assignments.next() else {
        // No assignment satisfied the constraint system.
        return Err(Error::ExhaustiveSearchError);
    };
    // Intersect all assignments.
    // A special case of this is that only one of the possible assignments satisfies the constraint system,
    // but even if there are multiple, they might agree on a subset of their assignments.
    Ok(assignments
        .try_fold(first_assignments, |mut acc, assignments| {
            acc.retain(|variable, value| assignments.get(variable) == Some(value));
            if acc.is_empty() {
                // Exiting early here is crucial for performance.
                return Err(());
            }
            Ok(acc)
        })
        .ok())
}

/// Returns all unique sets of variables that appear together in an identity
/// (either in an algebraic constraint or in the same field of a bus interaction),
/// IF the number of possible assignments is less than `MAX_SEARCH_WIDTH`.
pub fn get_brute_force_candidates<
    'a,
    T: RuntimeConstant + ReferencedSymbols<V>,
    V: Clone + Hash + Ord,
>(
    constraint_system: &'a IndexedConstraintSystem<T, V>,
    rc: impl RangeConstraintProvider<T::FieldType, V> + Clone + 'a,
) -> impl Iterator<Item = BTreeSet<V>> + 'a {
    constraint_system
        .expressions()
        .map(|expression| {
            expression
                .referenced_variables()
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

/// Given a list of assignments, tries to extend it with more assignments, based on the
/// constraints in the constraint system.
/// Fails if any of the assignments *directly* contradicts any of the constraints.
/// Note that getting an OK(_) here does not mean that there is no contradiction, as
/// this function only does one step of the derivation.
fn derive_more_assignments<T, V: Clone + Hash + Ord + Eq + Display>(
    constraint_system: &IndexedConstraintSystem<T, V>,
    assignments: BTreeMap<V, T::FieldType>,
    range_constraints: &impl RangeConstraintProvider<T::FieldType, V>,
    bus_interaction_handler: &impl BusInteractionHandler<T::FieldType>,
) -> Result<BTreeMap<V, T::FieldType>, ContradictingConstraintError>
where
    T: RuntimeConstant
        + Substitutable<V>
        + ReferencedSymbols<V>
        + ExpressionConvertible<<T as RuntimeConstant>::FieldType, V>
        + Display,
{
    let effects = constraint_system
        .constraints_referencing_variables(assignments.keys())
        .map(|constraint| match constraint {
            ConstraintRef::AlgebraicConstraint(identity) => {
                let mut identity = identity.cloned();
                for (variable, value) in assignments.iter() {
                    identity.substitute_by_known(variable, &T::from(*value));
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
                        .for_each(|expr| expr.substitute_by_known(variable, &T::from(*value)))
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
        .filter_map(|effect| {
            if let Effect::Assignment(variable, value) = effect {
                Some((variable, value.try_to_number()?))
            } else {
                None
            }
        })
        .chain(assignments)
        // Union of all unique assignments, but returning an error if there are any contradictions.
        .try_fold(BTreeMap::new(), |mut map, (variable, value)| {
            if let Some(existing) = map.insert(variable, value) {
                if existing != value {
                    // Duplicate assignment with different value.
                    return Err(ContradictingConstraintError);
                }
            }
            Ok(map)
        })
}
