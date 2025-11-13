use itertools::Itertools;
use powdr_constraint_solver::{
    constraint_system::ConstraintSystem,
    grouped_expression::{GroupedExpression, GroupedExpressionComponent},
    indexed_constraint_system::apply_substitutions,
};
use powdr_number::{BabyBearField, FieldElement, LargeInt};

use crepe::crepe;

type F = BabyBearField;

crepe! {
    @input
    struct Constraint<'a>(&'a GroupedExpression<F, String>);

    struct Var<'a>(&'a str);

    struct IsAffine<'a>(&'a GroupedExpression<F, String>);

    // TODO it should not be required that it is a constraint, but
    // we cannot get expression from anywhere else.
    IsAffine(e) <- Constraint(e), (e.is_affine());

    struct ExprHasLinearComponent<'a>(&'a GroupedExpression<F, String>, F, Var<'a>);
    ExprHasLinearComponent(e, coeff, var) <- IsAffine(e), for (coeff, var) in linear_components(e);

    struct LinearComponentCount<'a>(&'a GroupedExpression<F, String>, usize);
    LinearComponentCount(e, count) <- IsAffine(e), for count in std::iter::once(e.linear_components().count());

    struct AffineExpression<'a>(&'a GroupedExpression<F, String>, F, Var<'a>, F);

    AffineExpression(e, coeff, var, offset) <-
      IsAffine(e),
      LinearComponentCount(e, 1),
      ExprHasLinearComponent(e, coeff, var),
      for offset in std::iter::once(*e.constant_offset());

    @output
    struct Assignment<'a>(Var<'a>, F);
    Assignment(var, value) <-
      Constraint(e),
      AffineExpression(e, coeff, var, offset),
        for value in std::iter::once(-offset / coeff);
}

fn linear_components<'a>(expr: &'a GroupedExpression<F, String>) -> Vec<(F, Var<'a>)> {
    expr.linear_components()
        .into_iter()
        .map(|(v, c)| (*c, Var(v.as_str())))
        .collect()
}

pub fn rule_based_optimization<
    T: FieldElement,
    V: std::hash::Hash + Eq + Ord + Clone + std::fmt::Display,
>(
    system: ConstraintSystem<T, V>,
) -> ConstraintSystem<T, V> {
    if T::modulus().to_arbitrary_integer() != BabyBearField::modulus().to_arbitrary_integer() {
        return system;
    }
    let mut rt = Crepe::new();

    let transformed_expressions = system
        .algebraic_constraints
        .iter()
        .map(|c| transform_grouped_expression(&c.expression))
        .collect_vec();

    rt.extend(transformed_expressions.iter().map(|e| Constraint(e)));

    let (assignments,) = rt.run();
    for Assignment(var, value) in assignments {
        println!("Inferred assignment: {} = {}", var.0, value);
    }
    // apply_substitutions(
    //     system,
    //     assignments
    //         .into_iter()
    //         .map(|Assignment(var, value)| (var.0.to_string(), value)),
    // )
    system
}

fn transform_grouped_expression<
    T: FieldElement,
    V: std::hash::Hash + Eq + Ord + Clone + std::fmt::Display,
>(
    expr: &GroupedExpression<T, V>,
) -> GroupedExpression<BabyBearField, String> {
    expr.clone()
        .into_summands()
        .map(|s| match s {
            GroupedExpressionComponent::Quadratic(l, r) => {
                transform_grouped_expression(&l) * transform_grouped_expression(&r)
            }
            GroupedExpressionComponent::Linear(v, c) => {
                GroupedExpression::from_unknown_variable(v.to_string())
                    * BabyBearField::from(c.to_arbitrary_integer())
            }
            GroupedExpressionComponent::Constant(c) => {
                GroupedExpression::from_number(c.to_arbitrary_integer().into())
            }
        })
        .sum()
}
