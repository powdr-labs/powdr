use itertools::Itertools;
use powdr_number::FieldElement;
use powdr_number::LargeInt;

use crate::algebraic_constraint::AlgebraicConstraint;
use crate::constraint_system::{BusInteraction, BusInteractionHandler};
use crate::effect::Effect;
use crate::grouped_expression::GroupedExpression;
use crate::grouped_expression::RangeConstraintProvider;
use crate::indexed_constraint_system::IndexedConstraintSystem;
use crate::range_constraint::RangeConstraint;
use crate::utils::{get_all_possible_assignments, has_few_possible_assignments};

use std::collections::btree_map::Entry;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt::Display;
use std::hash::Hash;

use super::Error;

/// The maximum number of possible assignments to try when doing exhaustive search.
const MAX_SEARCH_WIDTH: u64 = 1 << 10;
/// The maximum range width of a variable to be considered for exhaustive search.
const MAX_VAR_RANGE_WIDTH: u64 = 5;

/// Result of simplifying constraints by exhaustive search.
pub struct ExhaustiveSearchResult<T: FieldElement, V> {
    /// Simplified algebraic constraints: (index, new_expression).
    pub algebraic: Vec<(usize, GroupedExpression<T, V>)>,
    /// Simplified bus interactions: (index, new_bus_interaction).
    pub bus_interactions: Vec<(usize, BusInteraction<GroupedExpression<T, V>>)>,
    /// Tightened range constraints for all variables (not just search variables).
    pub range_constraints: BTreeMap<V, RangeConstraint<T>>,
}

impl<T: FieldElement, V> ExhaustiveSearchResult<T, V> {
    pub fn is_empty(&self) -> bool {
        self.algebraic.is_empty()
            && self.bus_interactions.is_empty()
            && self.range_constraints.is_empty()
    }
}

/// Goes through all possible assignments for the given variables.
/// For each non-contradictory assignment:
/// - Substitutes into all constraints referencing the variables. If all assignments
///   produce the same simplified expression, the constraint is simplified.
/// - Computes the disjunction of derived range constraints for all variables.
///
/// Returns an error if all assignments are contradictory.
pub fn simplify_by_exhaustive_search<T: FieldElement, V: Clone + Hash + Ord + Eq + Display>(
    algebraic_constraints: &[AlgebraicConstraint<GroupedExpression<T, V>>],
    bus_interactions: &[BusInteraction<GroupedExpression<T, V>>],
    variables: &BTreeSet<V>,
    range_constraints: &impl RangeConstraintProvider<T, V>,
    bus_interaction_handler: &dyn BusInteractionHandler<T>,
) -> Result<ExhaustiveSearchResult<T, V>, Error> {
    // For each constraint, track the simplified expression across all valid assignments.
    // None means different assignments produced different expressions.
    let mut alg_results: HashMap<usize, Option<GroupedExpression<T, V>>> = HashMap::new();
    let mut bus_results: HashMap<usize, Option<BusInteraction<GroupedExpression<T, V>>>> =
        HashMap::new();
    // Track disjunction of derived range constraints across valid assignments.
    let mut derived_range_constraints: Option<BTreeMap<V, RangeConstraint<T>>> = None;
    let mut num_valid = 0usize;

    for assignments in get_all_possible_assignments(variables.iter().cloned(), range_constraints) {
        // Substitute into all affected algebraic constraints and check for contradictions.
        let mut simplified_alg = Vec::new();
        let mut contradictory = false;

        for (i, constraint) in algebraic_constraints.iter().enumerate() {
            if !constraint
                .expression
                .referenced_unknown_variables()
                .any(|v| variables.contains(v))
            {
                continue;
            }
            let mut expr = constraint.expression.clone();
            for (var, val) in &assignments {
                expr.substitute_by_known(var, val);
            }
            if let Some(k) = expr.try_to_known() {
                if !k.is_zero() {
                    contradictory = true;
                    break;
                }
            }
            simplified_alg.push((i, expr));
        }

        if contradictory {
            continue;
        }

        // Substitute into all affected bus interactions.
        let mut simplified_bus = Vec::new();
        for (i, bus) in bus_interactions.iter().enumerate() {
            if !bus.fields().any(|f| {
                f.referenced_unknown_variables()
                    .any(|v| variables.contains(v))
            }) {
                continue;
            }
            let mut bus = bus.clone();
            for (var, val) in &assignments {
                bus.fields_mut()
                    .for_each(|f| f.substitute_by_known(var, val));
            }
            simplified_bus.push((i, bus));
        }

        // Enhanced contradiction detection: solve each substituted constraint individually
        // and check for cross-constraint contradictions via range constraint intersection.
        // Also derives range constraints for all variables (not just search variables).
        let Some(new_constraints) = derive_range_constraints(
            &simplified_alg,
            &simplified_bus,
            &assignments,
            range_constraints,
            bus_interaction_handler,
        ) else {
            continue;
        };

        num_valid += 1;

        // Merge simplified constraints.
        for (i, expr) in simplified_alg {
            let entry = alg_results.entry(i).or_insert_with(|| Some(expr.clone()));
            if entry.as_ref() != Some(&expr) {
                *entry = None;
            }
        }
        for (i, bus) in simplified_bus {
            let entry = bus_results.entry(i).or_insert_with(|| Some(bus.clone()));
            if entry.as_ref() != Some(&bus) {
                *entry = None;
            }
        }

        // Merge range constraints (disjunction across valid assignments).
        derived_range_constraints = Some(match derived_range_constraints {
            None => new_constraints,
            Some(mut acc) => {
                for (var, rc) in &mut acc {
                    let other = new_constraints.get(var).cloned().unwrap_or_default();
                    *rc = rc.disjunction(&other);
                }
                acc
            }
        });
    }

    if num_valid == 0 {
        return Err(Error::ExhaustiveSearchError);
    }

    let algebraic = alg_results
        .into_iter()
        .filter_map(|(i, expr)| expr.map(|e| (i, e)))
        .filter(|(i, e)| *e != algebraic_constraints[*i].expression)
        .collect();

    let bus_interaction_simplifications = bus_results
        .into_iter()
        .filter_map(|(i, bus)| bus.map(|b| (i, b)))
        .filter(|(i, b)| *b != bus_interactions[*i])
        .collect();

    // Only keep range constraints that are tighter than what we already know.
    let mut new_range_constraints = derived_range_constraints.unwrap_or_default();
    new_range_constraints.retain(|v, rc| range_constraints.get(v) != *rc);

    Ok(ExhaustiveSearchResult {
        algebraic,
        bus_interactions: bus_interaction_simplifications,
        range_constraints: new_range_constraints,
    })
}

/// Tries to derive new range constraints from the given assignment by solving
/// each substituted constraint individually and intersecting derived range constraints.
/// Returns `None` if the assignment is contradictory (e.g., one constraint derives
/// `x = 1` and another derives `x = 8`).
fn derive_range_constraints<T: FieldElement, V: Clone + Hash + Ord + Eq + Display>(
    simplified_alg: &[(usize, GroupedExpression<T, V>)],
    simplified_bus: &[(usize, BusInteraction<GroupedExpression<T, V>>)],
    assignments: &BTreeMap<V, T>,
    range_constraints: &impl RangeConstraintProvider<T, V>,
    bus_interaction_handler: &dyn BusInteractionHandler<T>,
) -> Option<BTreeMap<V, RangeConstraint<T>>> {
    let mut derived: BTreeMap<V, RangeConstraint<T>> = BTreeMap::new();

    // Add concrete assignments as range constraints.
    for (var, val) in assignments {
        derived.insert(var.clone(), RangeConstraint::from_value(*val));
    }

    // Solve each algebraic constraint and collect derived range constraints.
    for (_, expr) in simplified_alg {
        let effects = AlgebraicConstraint::assert_zero(expr)
            .solve(range_constraints)
            .ok()?
            .effects;
        if intersect_effects(&mut derived, effects) {
            return None;
        }
    }

    // Solve each bus interaction and collect derived range constraints.
    for (_, bus) in simplified_bus {
        let effects = bus.solve(bus_interaction_handler, range_constraints).ok()?;
        if intersect_effects(&mut derived, effects) {
            return None;
        }
    }

    Some(derived)
}

/// Intersects effects (assignments and range constraints) into the accumulated map.
/// Returns true if a contradiction is detected (disjoint intersection).
fn intersect_effects<T: FieldElement, V: Clone + Ord>(
    acc: &mut BTreeMap<V, RangeConstraint<T>>,
    effects: impl IntoIterator<Item = Effect<T, V>>,
) -> bool {
    for effect in effects {
        let (var, rc) = match effect {
            Effect::Assignment(var, val) => (var, RangeConstraint::from_value(val)),
            Effect::RangeConstraint(var, rc) => (var, rc),
            _ => continue,
        };
        match acc.entry(var) {
            Entry::Vacant(entry) => {
                entry.insert(rc);
            }
            Entry::Occupied(mut entry) => {
                let existing = entry.get();
                if existing.is_disjoint(&rc) {
                    return true;
                }
                entry.insert(existing.conjunction(&rc));
            }
        }
    }
    false
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
