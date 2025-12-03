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

pub fn rule_based_optimization<T: FieldElement, V: Hash + Eq + Ord + Clone + Display>(
    mut system: IndexedConstraintSystem<T, V>,
    range_constraints: impl RangeConstraintProvider<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T> + Clone,
    new_var_outer: &mut impl FnMut(&str) -> V,
    degree_bound: Option<DegreeBound>,
) -> (IndexedConstraintSystem<T, V>, Vec<VariableAssignment<T, V>>) {
    let mut assignments = vec![];
    let mut var_mapper = system
        .referenced_unknown_variables()
        .cloned()
        .sorted()
        .collect::<ItemDB<V, Var>>();

    let mut expr_db = Some(ItemDB::<GroupedExpression<T, Var>, Expr>::default());

    loop {
        let (algebraic_constraints, bus_interactions) =
            transform_constraint_system(&system, &var_mapper, expr_db.as_mut().unwrap());
        // TODO it would be better to handle that inside the rule system,
        // but it is difficult because of the vector and the combinatorial
        // explosion of the range constraints.
        let rcs = system
            .bus_interactions()
            .iter()
            .zip(bus_interactions)
            .flat_map(|(bus_inter, bus_inter_transformed)| {
                let bi_rc = bus_inter
                    .fields()
                    .map(|e| e.range_constraint(&range_constraints))
                    .collect();
                let updated_rcs = bus_interaction_handler
                    .handle_bus_interaction(bi_rc)
                    .fields()
                    .cloned()
                    .collect_vec();
                bus_inter_transformed
                    .fields()
                    .cloned()
                    .zip(updated_rcs)
                    .collect_vec()
            })
            .map(|(e, rc)| rules::InitialRangeConstraintOnExpression(e, rc))
            .collect_vec();
        let mut rt = rules::Crepe::new();
        rt.extend(rcs);

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
            // TODO or we just clone the var mapper?
            NewVarGenerator::new(var_mapper.next_free_id()),
        );

        rt.extend(
            algebraic_constraints
                .iter()
                .copied()
                .map(rules::InitialAlgebraicConstraint),
        );
        rt.extend(std::iter::once(rules::Env(&env)));

        // let ((actions,), profile) = rt.run_with_profiling();
        // profile.report();
        let (actions,) = rt.run();
        let (expr_db_, new_var_generator) = env.terminate();

        // TODO we do not need all of those variables.
        for (var, prefix) in new_var_generator.requests() {
            let v = new_var_outer(prefix);
            assert_eq!(var_mapper.insert(&v), *var);
            let computation_method = untransform_computation_method(
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

        // TODO update RC on vars?
        let mut progress = false;
        for action in actions.into_iter().map(|a| a.0).sorted() {
            match action {
                Action::SubstituteVariableByConstant(var, val) => {
                    system.substitute_by_known(&var_mapper[var], &val);
                    assignments
                        .push((var_mapper[var].clone(), GroupedExpression::from_number(val)));
                    progress = true;
                }
                Action::SubstituteVariableByVariable(v1, v2) => {
                    let (v1, v2) = if var_mapper[v1] < var_mapper[v2] {
                        (v1, v2)
                    } else {
                        (v2, v1)
                    };
                    // We need to notify the solver of the equivalence.
                    assignments.push((
                        var_mapper[v2].clone(),
                        GroupedExpression::from_unknown_variable(var_mapper[v1].clone()),
                    ));
                    system.substitute_by_unknown(
                        &var_mapper[v2],
                        &GroupedExpression::from_unknown_variable(var_mapper[v1].clone()),
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

fn replace_algebraic_constraints<T: FieldElement, V: Hash + Eq + Ord + Clone + Display>(
    system: &mut IndexedConstraintSystem<T, V>,
    old: impl IntoIterator<Item = Expr>,
    replacement: impl IntoIterator<Item = Expr>,
    expr_db: &ItemDB<GroupedExpression<T, Var>, Expr>,
    var_mapper: &ItemDB<V, Var>,
    degree_bound: Option<DegreeBound>,
) -> bool {
    let mut old_found = old
        .into_iter()
        .map(|e| untransform_grouped_expression(&expr_db[e], var_mapper))
        .map(|e| (e, false))
        .collect::<HashMap<_, _>>();
    let replacement = replacement
        .into_iter()
        .map(|e| untransform_grouped_expression(&expr_db[e], var_mapper))
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

fn transform_constraint_system<T: FieldElement, V: Hash + Eq + Ord + Clone + Display>(
    system: &IndexedConstraintSystem<T, V>,
    var_mapper: &ItemDB<V, Var>,
    expression_db: &mut ItemDB<GroupedExpression<T, Var>, Expr>,
) -> (Vec<Expr>, Vec<BusInteraction<Expr>>) {
    let algebraic_constraints = system
        .system()
        .algebraic_constraints
        .iter()
        .map(|c| transform_grouped_expression(&c.expression, var_mapper))
        .map(|e| expression_db.insert_owned(e))
        .collect_vec();
    let bus_interactions: Vec<BusInteraction<Expr>> = system
        .system()
        .bus_interactions
        .iter()
        .map(|bus_inter| {
            bus_inter
                .fields()
                .map(|f| transform_grouped_expression(f, var_mapper))
                .map(|e| expression_db.insert_owned(e))
                .collect()
        })
        .collect_vec();
    (algebraic_constraints, bus_interactions)
}

fn transform_grouped_expression<T: FieldElement, V: Hash + Eq + Ord + Clone + Display>(
    expr: &GroupedExpression<T, V>,
    var_mapper: &ItemDB<V, Var>,
) -> GroupedExpression<T, Var> {
    expr.transform_var_type(&mut |v| var_mapper.id(v))
}

fn untransform_grouped_expression<T: FieldElement, V: Hash + Eq + Ord + Clone + Display>(
    expr: &GroupedExpression<T, Var>,
    var_mapper: &ItemDB<V, Var>,
) -> GroupedExpression<T, V> {
    expr.transform_var_type(&mut |v| var_mapper[*v].clone())
}

fn untransform_computation_method<T: FieldElement, V: Hash + Eq + Ord + Clone + Display>(
    method: &ComputationMethod<T, GroupedExpression<T, Var>>,
    var_mapper: &ItemDB<V, Var>,
) -> ComputationMethod<T, GroupedExpression<T, V>> {
    match method {
        ComputationMethod::Constant(c) => ComputationMethod::Constant(*c),
        ComputationMethod::QuotientOrZero(numerator, denominator) => {
            ComputationMethod::QuotientOrZero(
                untransform_grouped_expression(numerator, var_mapper),
                untransform_grouped_expression(denominator, var_mapper),
            )
        }
    }
}
