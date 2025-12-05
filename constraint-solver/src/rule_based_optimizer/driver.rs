use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;

use itertools::Itertools;
use powdr_number::FieldElement;

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

    loop {
        // Transform the constraint system into a simpler representation
        // using IDs for variables and expressions.
        let (algebraic_constraints, bus_interactions) =
            transform_constraint_system(&system, &var_mapper, expr_db.as_mut().unwrap());

        // Create the "environment" singleton that can be used by the rules
        // to query information from the outside world.
        let env = Environment::<T>::new(
            expr_db.take().unwrap(),
            var_mapper
                .iter()
                .map(|(id, var)| (id, var.to_string()))
                .collect(),
            system
                .single_occurrence_variables()
                .map(|v| var_mapper.id(v))
                .collect(),
            system
                .referenced_unknown_variables()
                .map(|v| (var_mapper.id(v), range_constraints.get(v)))
                .collect(),
            // The NewVarGenerator will be used to generate fresh variables.
            // because of lifetime issuse, we pass the next ID that
            // the var_mapper would use here and then re-create the
            // variables in the same sequence further down.
            NewVarGenerator::new(var_mapper.next_free_id()),
        );

        // Create the rule system and populate it with the initial facts.
        let mut rt = rules::Crepe::new();

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
                .map(|(e, rc)| rules::InitialRangeConstraintOnExpression(e, rc)),
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
        // let ((actions,), profile) = rt.run_with_profiling();
        // profile.report();
        let (actions,) = rt.run();
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
        for action in actions.into_iter().map(|a| a.0).sorted() {
            match action {
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
                    progress |= replace_algebraic_constraints(
                        &mut system,
                        [e1],
                        [replacement],
                        &expr_db_,
                        &var_mapper,
                        degree_bound,
                    )
                }
                Action::ReplacePairOfAlgebraicConstraintsBy(e1, e2, replacement) => {
                    progress |= replace_algebraic_constraints(
                        &mut system,
                        [e1, e2],
                        [replacement],
                        &expr_db_,
                        &var_mapper,
                        degree_bound,
                    )
                }
            }
        }
        if !progress {
            break;
        }
        expr_db = Some(expr_db_);
    }
    system.retain_algebraic_constraints(|c| !c.is_redundant());
    (system, assignments)
}

/// Replaces all of the algebraic constraints in `old` by the ones in `replacement`.
/// Returns true if the replacement was successful, i.e. all of the `old` constraints
/// were found and replaced.
///
/// If degree_bound is None, the replacement is only done if the degree does not increase.
/// If degree_bound is Some(bound), the replacement is only done if the degree
/// stays within the bound.
fn replace_algebraic_constraints<T: FieldElement, V: Hash + Eq + Ord + Clone + Display>(
    system: &mut IndexedConstraintSystem<T, V>,
    old: impl IntoIterator<Item = Expr>,
    replacement: impl IntoIterator<Item = Expr>,
    expr_db: &ItemDB<GroupedExpression<T, Var>, Expr>,
    var_mapper: &ItemDB<V, Var>,
    degree_bound: Option<DegreeBound>,
) -> bool {
    // Undo the variable transformation.
    let mut old_found = old
        .into_iter()
        .map(|e| undo_variable_transform(&expr_db[e], var_mapper))
        .map(|e| (e, false))
        .collect::<HashMap<_, _>>();
    let replacement = replacement
        .into_iter()
        .map(|e| undo_variable_transform(&expr_db[e], var_mapper))
        .collect_vec();

    let max_old_degree = old_found.keys().map(|e| e.degree()).max().unwrap_or(0);
    let max_new_degree = replacement.iter().map(|e| e.degree()).max().unwrap_or(0);
    // If the degree does not increase, we do it in any case. If the degree increases, we
    // only do it if a degree bound is given and it stays within the bound.
    if max_new_degree > max_old_degree
        && (degree_bound.is_none() || max_new_degree > degree_bound.unwrap().identities)
    {
        log::debug!(
            "Skipping replacement of {} by {} due to degree constraints.",
            old_found.keys().format(", "),
            replacement.iter().format(", ")
        );
        return false;
    }
    // TODO special case for a single element in old:
    // We could run retain_algebraic_constraints right away
    // and thus iterate only once.
    for c in system.algebraic_constraints() {
        if old_found.contains_key(&c.expression) {
            *old_found.get_mut(&c.expression).unwrap() = true;
        }
    }
    if old_found.values().any(|found| !*found) {
        log::warn!(
            "Was about to replace constraints {} but did not find all in the system.",
            old_found.keys().format(", ")
        );
        false
    } else {
        system.retain_algebraic_constraints(|c| !old_found.contains_key(&c.expression));
        system.add_algebraic_constraints(
            replacement
                .into_iter()
                .map(AlgebraicConstraint::assert_zero),
        );
        true
    }
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
