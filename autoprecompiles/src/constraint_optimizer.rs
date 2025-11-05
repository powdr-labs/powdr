use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    fmt::Display,
    hash::Hash,
    iter::once,
};

use itertools::Itertools;
use num_traits::Zero;
use powdr_constraint_solver::{
    constraint_system::{
        AlgebraicConstraint, BusInteractionHandler, ConstraintRef, ConstraintSystem,
    },
    grouped_expression::{GroupedExpression, GroupedExpressionComponent},
    indexed_constraint_system::IndexedConstraintSystem,
    inliner::DegreeBound,
    range_constraint::RangeConstraint,
    reachability::reachable_variables,
    solver::Solver,
};
use powdr_number::FieldElement;

use crate::{
    low_degree_bus_interaction_optimizer::LowDegreeBusInteractionOptimizer,
    memory_optimizer::{optimize_memory, MemoryBusInteraction},
    range_constraint_optimizer::RangeConstraintHandler,
    stats_logger::StatsLogger,
};

#[derive(Debug)]
pub enum Error {
    ConstraintSolverError(powdr_constraint_solver::solver::Error),
}

impl From<powdr_constraint_solver::solver::Error> for Error {
    fn from(err: powdr_constraint_solver::solver::Error) -> Self {
        Error::ConstraintSolverError(err)
    }
}

/// Simplifies the constraints as much as possible.
/// This function is similar to powdr_pilopt::qse_opt::run_qse_optimization, except it:
/// - Runs on the entire constraint system, including bus interactions.
/// - Panics if the solver fails.
/// - Removes trivial constraints (e.g. `0 = 0` or bus interaction with multiplicity `0`)
///   from the constraint system.
pub fn optimize_constraints<
    P: FieldElement,
    V: Ord + Clone + Eq + Hash + Display,
    M: MemoryBusInteraction<P, V>,
>(
    constraint_system: ConstraintSystem<P, V>,
    solver: &mut impl Solver<P, V>,
    bus_interaction_handler: impl BusInteractionHandler<P>
        + IsBusStateful<P>
        + RangeConstraintHandler<P>
        + Clone,
    stats_logger: &mut StatsLogger,
    memory_bus_id: Option<u64>,
    degree_bound: DegreeBound,
) -> Result<ConstraintSystem<P, V>, Error> {
    // Index the constraint system for the first time
    let constraint_system = IndexedConstraintSystem::from(constraint_system);

    let constraint_system = solver_based_optimization(constraint_system, solver)?;
    stats_logger.log("solver-based optimization", &constraint_system);

    let constraint_system = remove_trivial_constraints(constraint_system);
    stats_logger.log("removing trivial constraints", &constraint_system);

    let constraint_system =
        remove_free_variables(constraint_system, solver, bus_interaction_handler.clone());
    stats_logger.log("removing free variables", &constraint_system);

    let constraint_system =
        remove_disconnected_columns(constraint_system, solver, bus_interaction_handler.clone());
    stats_logger.log("removing disconnected columns", &constraint_system);

    let constraint_system = combine_free_variables(constraint_system, solver);
    stats_logger.log("combining free variables", &constraint_system);

    let constraint_system = trivial_simplifications(
        constraint_system,
        bus_interaction_handler.clone(),
        stats_logger,
    );

    // At this point, we throw away the index and only keep the constraint system, since the rest of the optimisations are defined on the system alone
    let constraint_system: ConstraintSystem<P, V> = constraint_system.into();

    let constraint_system = optimize_memory::<_, _, M>(constraint_system, solver, memory_bus_id);
    stats_logger.log("memory optimization", &constraint_system);

    let constraint_system = LowDegreeBusInteractionOptimizer::new(
        solver,
        bus_interaction_handler.clone(),
        degree_bound,
    )
    .optimize(constraint_system);
    stats_logger.log(
        "low degree bus interaction optimization",
        &constraint_system,
    );

    Ok(constraint_system)
}

/// Performs some very easy simplifications that only remove constraints.
pub fn trivial_simplifications<P: FieldElement, V: Ord + Clone + Eq + Hash + Display>(
    constraint_system: IndexedConstraintSystem<P, V>,
    bus_interaction_handler: impl BusInteractionHandler<P>
        + IsBusStateful<P>
        + RangeConstraintHandler<P>
        + Clone,
    stats_logger: &mut StatsLogger,
) -> IndexedConstraintSystem<P, V> {
    let constraint_system = remove_trivial_constraints(constraint_system);
    stats_logger.log("removing trivial constraints", &constraint_system);

    let constraint_system =
        remove_equal_bus_interactions(constraint_system, bus_interaction_handler.clone());
    stats_logger.log("removing equal bus interactions", &constraint_system);

    let constraint_system = remove_redundant_constraints(constraint_system);
    stats_logger.log("removing redundant constraints", &constraint_system);

    let constraint_system = remove_unreferenced_derived_variables(constraint_system);
    stats_logger.log(
        "removing unreferenced derived variables",
        &constraint_system,
    );

    constraint_system
}

fn solver_based_optimization<T: FieldElement, V: Clone + Ord + Hash + Display>(
    mut constraint_system: IndexedConstraintSystem<T, V>,
    solver: &mut impl Solver<T, V>,
) -> Result<IndexedConstraintSystem<T, V>, Error> {
    let assignments = solver.solve()?;
    log::trace!("Solver figured out the following assignments:");
    if log::log_enabled!(log::Level::Trace) {
        for (var, value) in assignments.iter() {
            log::trace!("  {var} = {value}");
        }
    }
    // Assert that all substitutions are affine so that the degree
    // does not increase.
    assert!(assignments.iter().all(|(_, expr)| expr.is_affine()));
    constraint_system.apply_substitutions(assignments);

    // Now try to replace bus interaction fields that the solver knows to be constant
    let mut bus_interactions = vec![];
    let mut new_algebraic_constraints = vec![];
    // We remove all bus interactions because we do not want to change the order.
    constraint_system.retain_bus_interactions(|bus_interaction| {
        let mut modified = false;
        let replacement = bus_interaction
            .fields()
            .map(|field| {
                if let Some(n) = try_replace_by_number(field, solver) {
                    modified = true;
                    new_algebraic_constraints
                        .push(AlgebraicConstraint::assert_eq(n.clone(), field.clone()));
                    n
                } else {
                    field.clone()
                }
            })
            .collect();
        if modified {
            log::trace!("Replacing bus interaction {bus_interaction} with {replacement}");
        }
        bus_interactions.push(replacement);
        false
    });
    constraint_system.add_bus_interactions(bus_interactions);
    constraint_system.add_algebraic_constraints(new_algebraic_constraints);
    Ok(constraint_system)
}

/// Tries to find a number that is equivalent to the expression and returns it
/// as a GroupedExpression.
/// Returns None if it was unsuccessful or if the expression already is a number.
fn try_replace_by_number<T: FieldElement, V: Clone + Ord + Hash + Display>(
    expr: &GroupedExpression<T, V>,
    solver: &impl Solver<T, V>,
) -> Option<GroupedExpression<T, V>> {
    if expr.try_to_number().is_some() {
        return None;
    }
    Some(GroupedExpression::from_number(
        solver
            .range_constraint_for_expression(expr)
            .try_to_single_value()?,
    ))
}

/// Removes free variables from the constraint system, under some conditions.
///
/// Motivation: Suppose there is a constraint `2 * foo = bar` and `foo` only appears in this constraint.
/// Then, if we assume that all constraints are satisfiable, the prover would be able to satisfy it for
/// any value of `bar` by solving for `foo`. Therefore, the constraint can be removed.
/// The same would be true for a *stateless* bus interaction, e.g. `[foo * bar] in [BYTES]`.
///
/// This function removes *some* constraints like this (see TODOs below).
fn remove_free_variables<T: FieldElement, V: Clone + Ord + Eq + Hash + Display>(
    mut constraint_system: IndexedConstraintSystem<T, V>,
    solver: &mut impl Solver<T, V>,
    bus_interaction_handler: impl IsBusStateful<T> + Clone,
) -> IndexedConstraintSystem<T, V> {
    let all_variables = constraint_system
        .referenced_unknown_variables()
        .cloned()
        .collect::<HashSet<_>>();

    // Find variables that are referenced in exactly one constraint
    let variables_to_delete = single_occurrence_variables(&constraint_system)
        .filter(|(constraint, variable)| match constraint {
            // Remove the algebraic constraint if we can solve for the variable.
            ConstraintRef::AlgebraicConstraint(constr) => {
                can_always_be_satisfied_via_free_variable(*constr, variable)
            }
            ConstraintRef::BusInteraction(bus_interaction) => {
                let bus_id = bus_interaction.bus_id.try_to_number().unwrap();
                // Only stateless bus interactions can be removed.
                let is_stateless = !bus_interaction_handler.is_stateful(bus_id);
                // TODO: This is overly strict.
                // We assume that the bus interaction is satisfiable. Given that it is, there
                // will be at least one assignment of the payload fields that satisfies it.
                // If the prover has the freedom to choose each payload field, it can always find
                // a satisfying assignment.
                // This could be generalized to multiple unknown fields, but it would be more complicated,
                // because *each* field would need a *different* free variable.
                let has_one_unknown_field = bus_interaction
                    .payload
                    .iter()
                    .filter(|field| field.try_to_number().is_none())
                    .count()
                    == 1;
                // If the expression is linear in the free variable, the prover would be able to solve for it
                // to satisfy the constraint. Otherwise, this is not necessarily the case.
                // Note that if the above check is true, there will only be one field of degree > 0.
                let all_degrees_at_most_one = bus_interaction
                    .payload
                    .iter()
                    .all(|field| field.degree_of_variable(variable) <= 1);
                is_stateless && has_one_unknown_field && all_degrees_at_most_one
            }
        })
        .map(|(constraint, variable)| variable.clone())
        .collect::<HashSet<_>>();

    let variables_to_keep = all_variables
        .difference(&variables_to_delete)
        .cloned()
        .collect::<HashSet<_>>();

    solver.retain_variables(&variables_to_keep);

    constraint_system.retain_algebraic_constraints(|constraint| {
        constraint
            .referenced_unknown_variables()
            .all(|var| variables_to_keep.contains(var))
    });

    constraint_system.retain_bus_interactions(|bus_interaction| {
        let bus_id = bus_interaction.bus_id.try_to_number().unwrap();
        bus_interaction_handler.is_stateful(bus_id)
            || bus_interaction
                .referenced_unknown_variables()
                .all(|var| variables_to_keep.contains(var))
    });

    constraint_system
}

/// Returns pairs of constraints and variables such that the variable occurs only
/// in the given constraint.
fn single_occurrence_variables<T: FieldElement, V: Clone + Ord + Eq + Hash + Display>(
    constraint_system: &IndexedConstraintSystem<T, V>,
) -> impl Iterator<Item = (ConstraintRef<T, V>, V)> {
    constraint_system
        .referenced_unknown_variables()
        .unique()
        .filter_map(|variable| {
            constraint_system
                .constraints_referencing_variables(once(variable))
                .exactly_one()
                .ok()
                .map(|constraint| (constraint, variable.clone()))
        })
}

/// Returns true if the given constraint can always be made to be satisfied by setting the
/// free variable, regardless of the values of other variables.
fn can_always_be_satisfied_via_free_variable<
    T: FieldElement,
    V: Clone + Hash + Eq + Ord + Display,
>(
    constraint: AlgebraicConstraint<&GroupedExpression<T, V>>,
    free_variable: &V,
) -> bool {
    if constraint.try_solve_for(free_variable).is_some() {
        true
    } else if let Some((left, right)) = constraint.expression.try_as_single_product() {
        // If either `left` or `right` can be set to 0, the constraint is satisfied.
        can_always_be_satisfied_via_free_variable(AlgebraicConstraint::from(left), free_variable)
            || can_always_be_satisfied_via_free_variable(
                AlgebraicConstraint::from(right),
                free_variable,
            )
    } else {
        false
    }
}

/// Tries to combine multiple variables that only occur in the same algebraic
/// constraint.
///
/// The simplified pattern is `X * V1 + Y * V2 = C`, where `V1` and `V2` only occur
/// here and only once.
/// The only combination of values for `X`, `Y` and `C` where this is not satisfiable
/// is `X = 0`, `Y = 0`, `C != 0`. So the constraint is equivalent to the statement
/// `(X = 0 and Y = 0) -> C = 0`.
///
/// Considering the simpler case where both `X` and `Y` are non-negative such that
/// `X + Y` does not wrap.
/// Then `X = 0 and Y = 0` is equivalent to `X + Y = 0`. So we can replace the constraint
/// by `(X + Y) * V3 = C`, where `V3` is a new variable that only occurs here.
///
/// If e.g. `X` can be negative, we replace it by `X * X`, if that value is still small enough.
fn combine_free_variables<T: FieldElement, V: Clone + Ord + Eq + Hash + Display>(
    mut constraint_system: IndexedConstraintSystem<T, V>,
    solver: &mut impl Solver<T, V>,
) -> IndexedConstraintSystem<T, V> {
    // TODO tracegen needs to be modified
    let single_occurrence = single_occurrence_variables(&constraint_system)
        .into_group_map()
        .into_iter()
        .flat_map(|(c, v)| match c {
            ConstraintRef::AlgebraicConstraint(constr) => Some((constr.clone(), v)),
            ConstraintRef::BusInteraction(_bus_interaction) => None,
        })
        .filter_map(|(c, vars)| {
            // Keep only the variables that occur exactly once
            // and then filter out the constraints where less than two
            // variables are left.
            let vars = vars
                .iter()
                .filter(|var| {
                    c.referenced_unknown_variables()
                        .filter(|v| v == var)
                        .count()
                        == 1
                })
                .cloned()
                .collect_vec();
            if vars.len() <= 1 {
                None
            } else {
                Some((c, vars))
            }
        })
        .flat_map(|(c, v)| {
            FreeVariablePatternMatch::try_from_constraint(&c, v.iter().cloned().collect())
        })
        .collect_vec();
    let mut constraints_to_add = vec![];
    let mut vars_to_remove = BTreeSet::<V>::new();
    for pattern_match in single_occurrence {
        let mut grouped: GroupedExpression<T, V> = Zero::zero();
        let mut grouped_rc = RangeConstraint::<T>::from_value(0.into());
        let mut rest = pattern_match.rest;
        let mut vars_replaced = BTreeSet::new();
        // TODO could use fold here.
        for (v, f, rc) in pattern_match
            .variables
            .into_iter()
            .map(|(v, f)| {
                let rc = f.range_constraint(solver);
                (v, f, rc)
            })
            .sorted_by_key(|(_, _, rc)| rc.range_width())
        {
            // TODO we could actually also try to extract a factor from `f` to
            // reduce its RC.
            if rc.range().0 <= rc.range().1 {
                // TODO not sure if this is correct.
                // what we need ot check is that the only way for X + Y = 0
                // is that X = 0 and Y = 0.
                if rc.combine_sum(&grouped_rc).range_width() != T::modulus() {
                    grouped += f;
                    grouped_rc = rc.combine_sum(&grouped_rc);
                    vars_replaced.insert(v.clone());
                } else {
                    rest += f * GroupedExpression::from_unknown_variable(v);
                }
            } else {
                // TODO same here
                if rc.square().combine_sum(&grouped_rc).range_width() != T::modulus() {
                    grouped += f.clone() * f;
                    grouped_rc = rc.square().combine_sum(&grouped_rc);
                    vars_replaced.insert(v.clone());
                } else {
                    rest += f * GroupedExpression::from_unknown_variable(v);
                }
            }
        }
        if vars_replaced.len() >= 2 {
            // TODO use a new variable
            constraints_to_add.push(AlgebraicConstraint::assert_zero(
                grouped
                    * GroupedExpression::from_unknown_variable(
                        vars_replaced.iter().next().unwrap().clone(),
                    )
                    + rest,
            ));
            vars_to_remove.extend(vars_replaced);
        }
    }

    constraint_system.retain_algebraic_constraints(|constr| {
        !constr
            .referenced_unknown_variables()
            .any(|v| vars_to_remove.contains(v))
    });
    constraint_system.add_algebraic_constraints(constraints_to_add);

    constraint_system
}

/// This pattern match corresponds to the constraint
/// \sum_{(v, f) in variables} f * v + rest = 0
/// such that all variables are different single-occurrence variables.
struct FreeVariablePatternMatch<T, V> {
    variables: BTreeMap<V, GroupedExpression<T, V>>,
    rest: GroupedExpression<T, V>,
}

impl<T: FieldElement, V: Clone + Ord + Eq + Hash + Display> FreeVariablePatternMatch<T, V> {
    fn try_from_constraint(
        constraint: &AlgebraicConstraint<&GroupedExpression<T, V>>,
        single_occurrence_variables: BTreeSet<V>,
    ) -> Option<Self> {
        if single_occurrence_variables.len() < 2 {
            return None;
        }
        let mut variables = BTreeMap::new();
        let rest = constraint
            .expression
            .clone()
            .into_summands()
            .filter_map(|item| match item {
                GroupedExpressionComponent::Constant(c) => Some(GroupedExpression::from_number(c)),
                GroupedExpressionComponent::Linear(v, c) => {
                    Some(GroupedExpression::from_unknown_variable(v) * c)
                }
                GroupedExpressionComponent::Quadratic(l, r) => {
                    if let Some((c, v)) =
                        Self::try_expression_to_single_occurrence_variable_multiple(
                            &l,
                            &single_occurrence_variables,
                        )
                    {
                        variables.insert(v, r.clone() * c);
                        None
                    } else if let Some((c, v)) =
                        Self::try_expression_to_single_occurrence_variable_multiple(
                            &r,
                            &single_occurrence_variables,
                        )
                    {
                        variables.insert(v, l.clone() * c);
                        None
                    } else {
                        Some(l * r)
                    }
                }
            })
            .sum();
        if variables.len() >= 2 {
            Some(Self { variables, rest })
        } else {
            None
        }
    }

    /// If `expr` is of the form `c * v` where `v` is a single occurrence variable
    /// and `c` a constant expression, returns `Some((c, v))`.
    fn try_expression_to_single_occurrence_variable_multiple(
        expr: &GroupedExpression<T, V>,
        single_occurrence_variables: &BTreeSet<V>,
    ) -> Option<(T, V)> {
        if expr.is_affine() && expr.constant_offset().is_zero() {
            let (v, c) = expr.linear_components().exactly_one().ok()?;
            if single_occurrence_variables.contains(v) {
                return Some((*c, v.clone()));
            }
        }
        None
    }
}

/// Removes any columns that are not connected to *stateful* bus interactions (e.g. memory),
/// because those are the only way to interact with the rest of the zkVM (e.g. other
/// instructions).
/// We assume that the input constraint system is satisfiable. Because the removed constraints
/// are not connected to rest of the system, the prover can always satisfy them, so removing
/// them is safe.
/// Note that if there were unsatisfiable constraints, they might also be removed, which would
/// change the statement being proven.
fn remove_disconnected_columns<T: FieldElement, V: Clone + Ord + Eq + Hash + Display>(
    mut constraint_system: IndexedConstraintSystem<T, V>,
    solver: &mut impl Solver<T, V>,
    bus_interaction_handler: impl IsBusStateful<T> + Clone,
) -> IndexedConstraintSystem<T, V> {
    let initial_variables = variables_in_stateful_bus_interactions(
        constraint_system.system(),
        bus_interaction_handler.clone(),
    )
    .cloned();
    let variables_to_keep = reachable_variables(initial_variables, &constraint_system);

    solver.retain_variables(&variables_to_keep);

    constraint_system.retain_algebraic_constraints(|constraint| {
        constraint
            .referenced_unknown_variables()
            .any(|var| variables_to_keep.contains(var))
    });

    constraint_system.retain_bus_interactions(|bus_interaction| {
        let bus_id = bus_interaction.bus_id.try_to_number().unwrap();
        let has_vars_to_keep = bus_interaction
            .referenced_unknown_variables()
            .any(|var| variables_to_keep.contains(var));
        // has_vars_to_keep would also be false for bus interactions containing only
        // constants, so we also check again whether it is stateful.
        bus_interaction_handler.is_stateful(bus_id) || has_vars_to_keep
    });

    constraint_system
}

/// Returns an iterator over all variables that are referenced in stateful bus interactions.
fn variables_in_stateful_bus_interactions<'a, P: FieldElement, V: Ord + Clone + Eq + Hash>(
    constraint_system: &'a ConstraintSystem<P, V>,
    bus_interaction_handler: impl IsBusStateful<P> + 'a,
) -> impl Iterator<Item = &'a V> {
    constraint_system
        .bus_interactions
        .iter()
        .filter(move |bus_interaction| {
            let bus_id = bus_interaction.bus_id.try_to_number().unwrap();
            bus_interaction_handler.is_stateful(bus_id)
        })
        .flat_map(|bus_interaction| bus_interaction.referenced_unknown_variables())
}

fn remove_trivial_constraints<P: FieldElement, V: PartialEq + Clone + Hash + Ord>(
    mut constraint_system: IndexedConstraintSystem<P, V>,
) -> IndexedConstraintSystem<P, V> {
    constraint_system.retain_algebraic_constraints(|constraint| !constraint.is_redundant());
    constraint_system
        .retain_bus_interactions(|bus_interaction| !bus_interaction.multiplicity.is_zero());
    constraint_system
}

fn remove_equal_bus_interactions<P: FieldElement, V: Ord + Clone + Eq + Hash>(
    mut constraint_system: IndexedConstraintSystem<P, V>,
    bus_interaction_handler: impl IsBusStateful<P>,
) -> IndexedConstraintSystem<P, V> {
    let mut seen = HashSet::new();
    constraint_system.retain_bus_interactions(|interaction| {
        // We only touch interactions with non-stateful buses.
        if let Some(bus_id) = interaction.bus_id.try_to_number() {
            if !bus_interaction_handler.is_stateful(bus_id) && !seen.insert(interaction.clone()) {
                return false;
            }
        }
        true
    });
    constraint_system
}

pub trait IsBusStateful<T: FieldElement> {
    /// Returns true if the bus with the given ID is stateful, i.e., whether there is any
    /// interaction with the rest of the zkVM. Examples of stateful buses are memory and
    /// execution bridge. Examples of non-stateful buses are fixed lookups.
    fn is_stateful(&self, bus_id: T) -> bool;
}

/// Removes constraints that are factors of other constraints.
fn remove_redundant_constraints<P: FieldElement, V: Clone + Ord + Hash + Display>(
    constraint_system: IndexedConstraintSystem<P, V>,
) -> IndexedConstraintSystem<P, V> {
    // First, remove duplicate factors from the constraints.
    let mut constraint_system = remove_duplicate_factors(constraint_system);

    // Maps each factor to the set of constraints that contain it.
    let mut constraints_by_factor = HashMap::new();
    // Turns each constraint into a set of factors.
    let constraints_as_factors = constraint_system
        .algebraic_constraints()
        .iter()
        .enumerate()
        .map(|(i, c)| {
            let factors = c.expression.to_factors();
            assert!(!factors.is_empty());
            for f in &factors {
                constraints_by_factor
                    .entry(f.clone())
                    .or_insert_with(HashSet::new)
                    .insert(i);
            }
            factors
        })
        .collect_vec();

    let mut redundant_constraints = HashSet::<usize>::new();
    for (i, factors) in constraints_as_factors.iter().enumerate() {
        // Go through all factors `f` and compute the intersection of all
        // constraints in `constraints_by_factor[f]`. These constraints
        // are multiples of the current constraint, so they are redundant
        // if they are proper multiples, i.e. have at least one more factor.
        let mut redundant = factors
            .iter()
            .map(|f| constraints_by_factor[f].clone())
            .reduce(|a, b| a.intersection(&b).copied().collect())
            .unwrap();
        // Only remove constraints that have the same factors if their index
        // is larger than the current one.
        // Counting the factors is sufficient here.
        redundant.retain(|j| {
            let other_factors = &constraints_as_factors[*j];
            // This assertion can fail if `remove_duplicate_factors` is not called at the start of this function.
            assert!(other_factors.len() >= factors.len());
            other_factors.len() > factors.len() || *j > i
        });
        redundant_constraints.extend(redundant);
    }
    let mut counter = 0;
    constraint_system.retain_algebraic_constraints(|_| {
        let retain = !redundant_constraints.contains(&counter);
        counter += 1;
        retain
    });
    constraint_system
}

/// If a constraint contains the same factor multiple times removes the duplicate factors.
fn remove_duplicate_factors<P: FieldElement, V: Clone + Ord + Hash + Display>(
    mut constraint_system: IndexedConstraintSystem<P, V>,
) -> IndexedConstraintSystem<P, V> {
    let mut constraint_to_add = vec![];
    constraint_system.retain_algebraic_constraints(|constraint| {
        let factors = constraint.expression.to_factors();
        assert!(!factors.is_empty());
        let factor_count = factors.len();
        let unique_factors = factors.into_iter().unique().collect_vec();
        if unique_factors.len() < factor_count {
            constraint_to_add.push(AlgebraicConstraint::assert_zero(
                unique_factors
                    .into_iter()
                    .reduce(|acc, factor| acc * factor)
                    .unwrap(),
            ));
            false
        } else {
            true
        }
    });
    constraint_system.add_algebraic_constraints(constraint_to_add);
    constraint_system
}

fn remove_unreferenced_derived_variables<P: FieldElement, V: Clone + Ord + Hash + Display>(
    mut constraint_system: IndexedConstraintSystem<P, V>,
) -> IndexedConstraintSystem<P, V> {
    // Note that `referenced_unknown_variables` only returns variables referenced in constraints.
    let referenced_variables = constraint_system
        .referenced_unknown_variables()
        .cloned()
        .collect::<HashSet<_>>();

    constraint_system.retain_derived_variables(|derived_var| {
        referenced_variables.contains(&derived_var.variable)
    });
    constraint_system
}
