use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::hash::Hash;

use itertools::Itertools;
use powdr_number::FieldElement;

use crate::range_constraint::RangeConstraint;
use crate::{
    algebraic_constraint::AlgebraicConstraint,
    constraint_system::{
        BusInteraction, BusInteractionHandler, ComputationMethod, ConstraintSystem, DerivedVariable,
    },
    grouped_expression::{GroupedExpression, RangeConstraintProvider},
    indexed_constraint_system::IndexedConstraintSystem,
    inliner::DegreeBound,
    rule_based_optimizer::{
        environment::Environment,
        item_db::ItemDB,
        new_var_generator::NewVarGenerator,
        rules,
        types::{Action, Expr, Var},
    },
    runtime_constant::VarTransformable,
};

pub type VariableAssignment<T, V> = (V, GroupedExpression<T, V>);

/// Perform rule-based optimization on the given constraint system. Returns the modified
/// system and a list of variable assignments that were made during the optimization.
/// The rules can also alter algebraic constraints and bus interactions, those alterations
/// will not be visible in the list of substitutions.
///
/// If a degree bound is NOT given, then the degrees of the returned system will not increase.
/// If it is given, then the degrees may increase, but will stay within the bound.
///
/// The function `new_var` can be used to generate a fresh variable, each call should
/// return a fresh variable and the parameter can be used as a name suggestion.
pub fn rule_based_optimization<T: FieldElement, V: Hash + Eq + Ord + Clone + Display>(
    mut system: IndexedConstraintSystem<T, V>,
    range_constraints: impl RangeConstraintProvider<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T> + Clone,
    new_var: &mut impl FnMut(&str) -> V,
    degree_bound: Option<DegreeBound>,
) -> (IndexedConstraintSystem<T, V>, Vec<VariableAssignment<T, V>>) {
    let mut assignments = vec![];
    let mut var_mapper = system
        .referenced_unknown_variables()
        .cloned()
        // Sorting is important here so that the order for V translates
        // to the same order on Var.
        .sorted()
        .collect::<ItemDB<V, Var>>();

    // The expression database will be used to map expressions and their IDs.
    // New expressions are created during rule execution and thus new IDs need
    // to be allocated. Because of lifetime issues, we pass it into
    // `env` and extract it again after the rules have run.
    let mut expr_db = Some(ItemDB::<GroupedExpression<T, Var>, Expr>::default());

    let mut range_constraints_on_vars: HashMap<Var, RangeConstraint<T>> = system
        .referenced_unknown_variables()
        .map(|v| (var_mapper.id(v), range_constraints.get(v)))
        .filter(|(_, rc)| !rc.is_unconstrained())
        .collect();

    loop {
        // Transform the constraint system into a simpler representation
        // using IDs for variables and expressions.
        let (algebraic_constraints, bus_interactions) =
            transform_constraint_system(&system, &var_mapper, expr_db.as_mut().unwrap());

        let duplicate_vars = system
            .referenced_unknown_variables()
            .map(|v| var_mapper.id(v))
            .duplicates()
            .collect::<HashSet<_>>();
        let single_occurrence_vars = system
            .referenced_unknown_variables()
            .map(|v| var_mapper.id(v))
            .collect::<HashSet<_>>()
            .difference(&duplicate_vars)
            .copied()
            .collect::<HashSet<_>>();

        // Create the "environment" singleton that can be used by the rules
        // to query information from the outside world.
        let env = Environment::<T>::new(
            expr_db.take().unwrap(),
            var_mapper
                .iter()
                .map(|(id, var)| (id, var.to_string()))
                .collect(),
            single_occurrence_vars,
            // The NewVarGenerator will be used to generate fresh variables.
            // because of lifetime issuse, we pass the next ID that
            // the var_mapper would use here and then re-create the
            // variables in the same sequence further down.
            NewVarGenerator::new(var_mapper.next_free_id()),
        );

        // Create the rule system and populate it with the initial facts.
        let mut rt = rules::Crepe::default();

        // It would be better to handle bus interactions inside the rule system,
        // but it is difficult because of the vector and the combinatorial
        // explosion of the range constraints, so we just determine the range constraints
        // on the bus interaction fields now.
        rt.extend(
            system
                .bus_interactions()
                .iter()
                .zip(bus_interactions)
                .flat_map(|(bus_inter, bus_inter_transformed)| {
                    let updated_rcs = bus_interaction_handler
                        .handle_bus_interaction(bus_inter.to_range_constraints(&range_constraints))
                        .fields()
                        .cloned()
                        .collect_vec();
                    bus_inter_transformed
                        .fields()
                        .cloned()
                        .zip(updated_rcs)
                        .collect_vec()
                })
                .filter(|(_, rc)| !rc.is_unconstrained())
                .into_grouping_map()
                .reduce(|rc1, _, rc2| rc1.conjunction(&rc2))
                .into_iter()
                .map(|(e, rc)| rules::InitialRangeConstraintOnExpression(e, rc)),
        );
        rt.extend(
            range_constraints_on_vars
                .iter()
                .map(|(var, rc)| rules::RangeConstraintOnVar(*var, *rc)),
        );
        rt.extend(
            algebraic_constraints
                .iter()
                .copied()
                .map(rules::InitialAlgebraicConstraint),
        );
        rt.extend(std::iter::once(rules::Env(&env)));

        // Uncomment this to get a runtime profile of the individual
        // rules.
        let ((actions,), profile) = rt.run_with_profiling();
        profile.report();
        // let (actions,) = rt.run();
        let (expr_db_, new_var_generator) = env.terminate();

        // Re-create the variables that were created using the
        // new_var_generator inside the rule system using
        // the var mapper, ensuring that the same IDs are used.
        for (var, prefix) in new_var_generator.requests() {
            let v = new_var(prefix);
            assert_eq!(var_mapper.insert(&v), *var);
            let computation_method = undo_variable_transform_in_computation_method(
                new_var_generator.computation_method(var),
                &var_mapper,
            );
            system.extend(ConstraintSystem {
                derived_variables: vec![DerivedVariable {
                    variable: v.clone(),
                    computation_method,
                }],
                ..Default::default()
            });
        }

        let mut progress = false;
        // Try to execute the actions that were determined by the rules.
        // Since the rules are "non-deterministic", some actions might conflict
        // (imagine x := 7, x := y and y := 7, they are all consistent but
        // some will fail depending on the order in which they are applied).
        // We try to ensure that at least the outcome is deterministic by
        // sorting the actions.

        // Collect replacement actions to process them in batch
        let mut replacement_actions = Vec::new();

        for action in actions.into_iter().map(|a| a.0).sorted() {
            match action {
                Action::UpdateRangeConstraintOnVar(var, rc) => {
                    let existing_rc = range_constraints_on_vars
                        .get(&var)
                        .cloned()
                        .unwrap_or_default();
                    let new_rc = existing_rc.conjunction(&rc);
                    if new_rc != existing_rc {
                        if let Some(val) = new_rc.try_to_single_value() {
                            system.substitute_by_known(&var_mapper[var], &val);
                            assignments.push((
                                var_mapper[var].clone(),
                                GroupedExpression::from_number(val),
                            ));
                        } else {
                            range_constraints_on_vars.insert(var, new_rc);
                        }
                        progress = true;
                    }
                }
                Action::SubstituteVariableByConstant(var, val) => {
                    system.substitute_by_known(&var_mapper[var], &val);
                    assignments
                        .push((var_mapper[var].clone(), GroupedExpression::from_number(val)));
                    progress = true;
                }
                Action::SubstituteVariableByVariable(v1, v2) => {
                    assignments.push((
                        var_mapper[v1].clone(),
                        GroupedExpression::from_unknown_variable(var_mapper[v2].clone()),
                    ));
                    system.substitute_by_unknown(
                        &var_mapper[v1],
                        &GroupedExpression::from_unknown_variable(var_mapper[v2].clone()),
                    );
                    progress = true;
                }
                Action::ReplaceAlgebraicConstraintBy(e1, replacement) => {
                    replacement_actions.push(ReplacementAction::new(
                        [e1],
                        [replacement],
                        &expr_db_,
                        &var_mapper,
                    ));
                }
                Action::ReplacePairOfAlgebraicConstraintsBy(e1, e2, replacement) => {
                    replacement_actions.push(ReplacementAction::new(
                        [e1, e2],
                        [replacement],
                        &expr_db_,
                        &var_mapper,
                    ));
                }
            }
        }

        progress |=
            batch_replace_algebraic_constraints(&mut system, replacement_actions, degree_bound);

        if !progress {
            break;
        }
        expr_db = Some(expr_db_);
    }
    system.retain_algebraic_constraints(|c| !c.is_redundant());
    (system, assignments)
}

/// A single replacement operation: replace `replace` constraints with `replace_by` constraints.
pub(crate) struct ReplacementAction<T, V> {
    /// Constraints to be replaced.
    pub(crate) replace: Vec<GroupedExpression<T, V>>,
    /// Replacement constraints.
    pub(crate) replace_by: Vec<GroupedExpression<T, V>>,
}

impl<T: FieldElement, V: Hash + Eq + Ord + Clone + Display> ReplacementAction<T, V> {
    /// Creates a new ReplacementAction from expression IDs, performing variable transformation.
    fn new(
        replace: impl IntoIterator<Item = Expr>,
        replace_by: impl IntoIterator<Item = Expr>,
        expr_db: &ItemDB<GroupedExpression<T, Var>, Expr>,
        var_mapper: &ItemDB<V, Var>,
    ) -> Self {
        Self {
            replace: replace
                .into_iter()
                .map(|e| undo_variable_transform(&expr_db[e], var_mapper))
                .collect(),
            replace_by: replace_by
                .into_iter()
                .map(|e| undo_variable_transform(&expr_db[e], var_mapper))
                .collect(),
        }
    }
}

/// Checks if a replacement action satisfies the degree bound constraints.
/// Returns true if the replacement is allowed, false otherwise.
///
/// If degree_bound is None, the replacement is only allowed if the degree does not increase.
/// If degree_bound is Some(bound), the replacement is allowed if the new degree stays within the bound.
fn is_replacement_within_degree_bound<T: FieldElement, V: Hash + Eq + Ord + Clone + Display>(
    replacement: &ReplacementAction<T, V>,
    degree_bound: Option<DegreeBound>,
) -> bool {
    let max_old_degree = replacement
        .replace
        .iter()
        .map(|e| e.degree())
        .max()
        .unwrap_or(0);
    let max_new_degree = replacement
        .replace_by
        .iter()
        .map(|e| e.degree())
        .max()
        .unwrap_or(0);

    // Check if the degree increase is acceptable
    let degree_increase = max_new_degree > max_old_degree;
    match degree_bound {
        None => !degree_increase,
        Some(bound) => max_new_degree <= bound.identities,
    }
}

/// Batch replaces multiple sets of algebraic constraints in a single pass through the constraint system.
/// Returns true if at least one replacement was successful.
///
/// If degree_bound is None, replacements are only done if the degree does not increase.
/// If degree_bound is Some(bound), replacements are only done if the degree stays within the bound.
pub(crate) fn batch_replace_algebraic_constraints<
    T: FieldElement,
    V: Hash + Eq + Ord + Clone + Display,
>(
    system: &mut IndexedConstraintSystem<T, V>,
    replacements: Vec<ReplacementAction<T, V>>,
    degree_bound: Option<DegreeBound>,
) -> bool {
    // Filter out replacements that violate degree bounds
    // and also filter out duplicate left hand sides.
    let valid_replacements: Vec<_> = replacements
        .into_iter()
        .filter(|replacement| {
            let within_bound = is_replacement_within_degree_bound(replacement, degree_bound);
            if !within_bound {
                log::debug!(
                    "Skipping replacement of {} by {} due to degree constraints.",
                    replacement.replace.iter().format(", "),
                    replacement.replace_by.iter().format(", ")
                );
            }
            within_bound
        })
        .map(|replacement| ReplacementAction {
            replace: replacement.replace.into_iter().unique().collect(),
            replace_by: replacement.replace_by,
        })
        .collect();

    // Build a map from constraints to search for to their index in the replacement list.
    // Note that the same expression can be present in multiple lists!
    let replace_to_index: HashMap<&GroupedExpression<T, V>, Vec<usize>> = valid_replacements
        .iter()
        .enumerate()
        .flat_map(|(i, r)| r.replace.iter().map(move |e| (e, i)))
        .into_group_map();

    // Compute which of the expressions to search for have been found for each replacement action.
    let mut replacement_found: Vec<HashSet<&GroupedExpression<T, V>>> =
        vec![Default::default(); valid_replacements.len()];

    for constraint in system.algebraic_constraints() {
        if let Some(replacement_indices) = replace_to_index.get(&constraint.expression) {
            for &i in replacement_indices {
                replacement_found[i].insert(&constraint.expression);
            }
        }
    }

    let mut constraints_to_remove: HashSet<&GroupedExpression<T, V>> = HashSet::new();
    let mut replacement_constraints = Vec::new();

    for (index, replacement) in valid_replacements.iter().enumerate() {
        if replacement_found[index].len() != replacement.replace.len() {
            log::debug!(
                "Incomplete replacement: wanted to replace {} but found only {}/{} constraints in the system.",
                replacement.replace.iter().format(", "),
                replacement_found[index].len(),
                replacement.replace.len()
            );
            continue;
        }

        // Check if any of this replacement's constraints to replace have already been claimed
        let has_conflict = replacement
            .replace
            .iter()
            .any(|replace_expr| constraints_to_remove.contains(replace_expr));

        if has_conflict {
            log::debug!(
                "Skipping replacement of {} due to conflict with earlier replacement.",
                replacement.replace.iter().format(", ")
            );
        } else {
            // No conflict, this replacement can proceed
            constraints_to_remove.extend(replacement.replace.iter());
            replacement_constraints.extend(replacement.replace_by.iter().cloned());
        }
    }

    if constraints_to_remove.is_empty() {
        // All replacements were skipped due to conflicts
        return false;
    }

    // Remove old constraints and add new ones
    system.retain_algebraic_constraints(|c| !constraints_to_remove.contains(&c.expression));
    system.add_algebraic_constraints(
        replacement_constraints
            .into_iter()
            .map(AlgebraicConstraint::assert_zero),
    );

    true
}

/// Transform the constraint system such that variables and expressions are
/// assigned IDs.
fn transform_constraint_system<T: FieldElement, V: Hash + Eq + Ord + Clone + Display>(
    system: &IndexedConstraintSystem<T, V>,
    var_mapper: &ItemDB<V, Var>,
    expression_db: &mut ItemDB<GroupedExpression<T, Var>, Expr>,
) -> (Vec<Expr>, Vec<BusInteraction<Expr>>) {
    let algebraic_constraints = system
        .system()
        .algebraic_constraints
        .iter()
        .map(|c| transform_variables(&c.expression, var_mapper))
        .map(|e| expression_db.insert_owned(e))
        .collect_vec();
    let bus_interactions: Vec<BusInteraction<Expr>> = system
        .system()
        .bus_interactions
        .iter()
        .map(|bus_inter| {
            bus_inter
                .fields()
                .map(|f| transform_variables(f, var_mapper))
                .map(|e| expression_db.insert_owned(e))
                .collect()
        })
        .collect_vec();
    (algebraic_constraints, bus_interactions)
}

/// Transform the variable type in the expression to use `Var` instead of `V`.
fn transform_variables<T: FieldElement, V: Hash + Eq + Ord + Clone + Display>(
    expr: &GroupedExpression<T, V>,
    var_mapper: &ItemDB<V, Var>,
) -> GroupedExpression<T, Var> {
    expr.transform_var_type(&mut |v| var_mapper.id(v))
}

/// Undo the effect of `transform_variables`, transforming from `Var` back to `V`.
fn undo_variable_transform<T: FieldElement, V: Hash + Eq + Ord + Clone + Display>(
    expr: &GroupedExpression<T, Var>,
    var_mapper: &ItemDB<V, Var>,
) -> GroupedExpression<T, V> {
    expr.transform_var_type(&mut |v| var_mapper[*v].clone())
}

/// Undo the effect of `transform_variables` on a computation method.
fn undo_variable_transform_in_computation_method<
    T: FieldElement,
    V: Hash + Eq + Ord + Clone + Display,
>(
    method: &ComputationMethod<T, GroupedExpression<T, Var>>,
    var_mapper: &ItemDB<V, Var>,
) -> ComputationMethod<T, GroupedExpression<T, V>> {
    match method {
        ComputationMethod::Constant(c) => ComputationMethod::Constant(*c),
        ComputationMethod::QuotientOrZero(numerator, denominator) => {
            ComputationMethod::QuotientOrZero(
                undo_variable_transform(numerator, var_mapper),
                undo_variable_transform(denominator, var_mapper),
            )
        }
    }
}
