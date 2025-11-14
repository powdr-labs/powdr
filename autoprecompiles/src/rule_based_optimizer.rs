#![allow(clippy::iter_over_hash_type)]
#![allow(for_loops_over_fallibles)]
use std::{collections::HashMap, fmt::Display, hash::Hash};

use itertools::Itertools;
use powdr_constraint_solver::{
    constraint_system::{BusInteraction, BusInteractionHandler},
    grouped_expression::{GroupedExpression, GroupedExpressionComponent, NoRangeConstraints},
    indexed_constraint_system::IndexedConstraintSystem,
};
use powdr_number::{BabyBearField, FieldElement, LargeInt};

use crepe::crepe;

use crate::range_constraint_optimizer::RangeConstraintHandler;

type F = BabyBearField;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Var(u32);

crepe! {
    @input
    struct AlgebraicConstraint<'a>(&'a GroupedExpression<F, Var>);

    @input
    struct BusInteractionConstraint<'a>(&'a BusInteraction<GroupedExpression<F, Var>>);

    @input
    struct RangeConstraintOnExpression<'a>(&'a GroupedExpression<F, Var>, F, F);

    struct Expression<'a>(&'a GroupedExpression<F, Var>);
    Expression(e) <- AlgebraicConstraint(e);
    Expression(e) <- BusInteractionConstraint(bus_inter), for e in bus_inter.fields();

    struct IsSimpleVar<'a>(&'a GroupedExpression<F, Var>, Var);
    IsSimpleVar(e, v) <- Expression(e), for v in e.try_to_simple_unknown();

    @output
    struct RangeConstraint(Var, F, F);

    struct IsAffine<'a>(&'a GroupedExpression<F, Var>);
    IsAffine(e) <- Expression(e), (e.is_affine());

    struct ExprHasLinearComponent<'a>(&'a GroupedExpression<F, Var>, F, Var);
    ExprHasLinearComponent(e, coeff, var) <- IsAffine(e), for (coeff, var) in linear_components(e);

    struct LinearComponentCount<'a>(&'a GroupedExpression<F, Var>, usize);
    LinearComponentCount(e, e.linear_components().count()) <- IsAffine(e);

    struct AffineExpression<'a>(&'a GroupedExpression<F, Var>, F, Var, F);
    AffineExpression(e, coeff, var, *e.constant_offset()) <-
      IsAffine(e),
      LinearComponentCount(e, 1),
      ExprHasLinearComponent(e, coeff, var);

    @output
    struct Assignment(Var, F);
    Assignment(var, -offset / coeff) <-
      AlgebraicConstraint(e),
      AffineExpression(e, coeff, var, offset);
}

fn linear_components(expr: &GroupedExpression<F, Var>) -> Vec<(F, Var)> {
    expr.linear_components().map(|(v, c)| (*c, *v)).collect()
}

pub fn rule_based_optimization<T: FieldElement, V: Hash + Eq + Ord + Clone + Display>(
    mut system: IndexedConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T> + Clone,
) -> IndexedConstraintSystem<T, V> {
    if T::modulus().to_arbitrary_integer() != BabyBearField::modulus().to_arbitrary_integer() {
        return system;
    }
    let start = std::time::Instant::now();
    let mut rt = Crepe::new();

    let mut var_mapper = Default::default();
    let transformed_expressions = system
        .system()
        .algebraic_constraints
        .iter()
        .map(|c| transform_grouped_expression(&c.expression, &mut var_mapper))
        .collect_vec();
    let bus_interactions: Vec<BusInteraction<GroupedExpression<F, Var>>> = system
        .system()
        .bus_interactions
        .iter()
        .map(|bus_inter| {
            bus_inter
                .fields()
                .map(|f| transform_grouped_expression(f, &mut var_mapper))
                .collect()
        })
        .collect_vec();
    // TODO we should do that inside the system, but the generic range constraint
    // handler makes it difficult.
    let range_constraints = system
        .system()
        .bus_interactions
        .iter()
        .enumerate()
        .flat_map(|(i, bus_interaction)| {
            let range_constraints = bus_interaction_handler
                .handle_bus_interaction(bus_interaction.to_range_constraints(&NoRangeConstraints))
                .fields()
                .cloned()
                .collect_vec();
            bus_interactions[i]
                .fields()
                .zip_eq(range_constraints)
                .map(|(expr, rc)| {
                    let (min, max) = rc.range();
                    RangeConstraintOnExpression(
                        &expr,
                        BabyBearField::from(min.to_arbitrary_integer()),
                        BabyBearField::from(max.to_arbitrary_integer()),
                    )
                })
        })
        .collect_vec();
    let transform_end = std::time::Instant::now();

    rt.extend(transformed_expressions.iter().map(AlgebraicConstraint));
    rt.extend(bus_interactions.iter().map(BusInteractionConstraint));
    rt.extend(range_constraints);

    let insert_end = std::time::Instant::now();

    let (rcs, assignments) = rt.run();
    let run_end = std::time::Instant::now();
    for RangeConstraint(var, min, max) in rcs {
        log::info!(
            "Rule-based range constraint: {} in [{}, {}]",
            var_mapper.backward(&var),
            min,
            max
        );
    }
    for (var, value) in assignments
        .into_iter()
        .map(|Assignment(var, value)| {
            (
                var_mapper.backward(&var),
                T::from(value.to_arbitrary_integer()),
            )
        })
        .sorted()
    {
        log::info!("Rule-based assignment: {var} = {value}",);
        system.substitute_by_known(var, &value);
    }
    let substitution_end = std::time::Instant::now();

    log::info!(
        "Rule-based optimization timings:\n\
           Transform: {}\n\
           Insert: {}\n\
           Run: {}\n\
           Substitution: {}\n\
         Total: {}",
        (transform_end - start).as_secs_f32(),
        (insert_end - transform_end).as_secs_f32(),
        (run_end - insert_end).as_secs_f32(),
        (substitution_end - run_end).as_secs_f32(),
        (substitution_end - start).as_secs_f32(),
    );

    system
}

struct VarMapper<V> {
    forward: HashMap<V, Var>,
    backward: HashMap<Var, V>,
    next_id: u32,
}

impl<V> Default for VarMapper<V> {
    fn default() -> Self {
        Self {
            forward: HashMap::new(),
            backward: HashMap::new(),
            next_id: 0,
        }
    }
}

impl<V: Hash + Eq + Clone + Display> VarMapper<V> {
    fn forward(&mut self, v: &V) -> Var {
        if let Some(var) = self.forward.get(v) {
            *var
        } else {
            let var = Var(self.next_id);
            self.forward.insert(v.clone(), var);
            self.backward.insert(var, v.clone());
            self.next_id += 1;
            var
        }
    }

    fn backward(&self, var: &Var) -> &V {
        self.backward.get(var).unwrap()
    }
}

fn transform_grouped_expression<T: FieldElement, V: Hash + Eq + Ord + Clone + Display>(
    expr: &GroupedExpression<T, V>,
    var_mapper: &mut VarMapper<V>,
) -> GroupedExpression<BabyBearField, Var> {
    expr.clone()
        .into_summands()
        .map(|s| match s {
            GroupedExpressionComponent::Quadratic(l, r) => {
                transform_grouped_expression(&l, var_mapper)
                    * transform_grouped_expression(&r, var_mapper)
            }
            GroupedExpressionComponent::Linear(v, c) => {
                GroupedExpression::from_unknown_variable(var_mapper.forward(&v))
                    * BabyBearField::from(c.to_arbitrary_integer())
            }
            GroupedExpressionComponent::Constant(c) => {
                GroupedExpression::from_number(c.to_arbitrary_integer().into())
            }
        })
        .sum()
}
