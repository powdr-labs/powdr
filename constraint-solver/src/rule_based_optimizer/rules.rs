#![allow(clippy::iter_over_hash_type)]
// This is about a warning about interior mutability for the key
// `Env`. We need it and it is probably fine.
#![allow(clippy::mutable_key_type)]

use crepe::crepe;
use itertools::Itertools;
use powdr_number::FieldElement;

use crate::{
    grouped_expression::RangeConstraintProvider,
    range_constraint::RangeConstraint,
    rule_based_optimizer::{
        environment::Environment,
        types::{Action, Expr, Var},
    },
};

crepe! {
    @input
    pub struct Env<'a, T: FieldElement>(pub &'a Environment<T>);

    @input
    pub struct InitialAlgebraicConstraint(pub Expr);

    @input
    pub struct InitialRangeConstraintOnExpression<T: FieldElement>(pub Expr, pub RangeConstraint<T>);

    struct AlgebraicConstraint(Expr);
    AlgebraicConstraint(e) <- InitialAlgebraicConstraint(e);

    struct RangeConstraintOnExpression<T: FieldElement>(Expr, RangeConstraint<T>);

    struct Expression(Expr);
    Expression(e) <- AlgebraicConstraint(e);
    Expression(e) <- RangeConstraintOnExpression(e, _);

    RangeConstraintOnExpression(e, rc) <-
      InitialRangeConstraintOnExpression(e, rc);

    struct ContainsVariable(Expr, Var);
    ContainsVariable(e, v) <-
      Env(env),
      Expression(e),
      for v in env.on_expr(e, (), |e, _| e.referenced_unknown_variables().cloned().collect_vec());

    struct Product(Expr, Expr, Expr);
    Product(e, l, r) <-
      Expression(e),
      Env(env),
      let Some((l, r)) = env.try_as_single_product(e);
    Product(e, r, l) <- Product(e, l, r);

    struct AffineExpression<T: FieldElement>(Expr, T, Var, T);
    AffineExpression(e, coeff, var, offset) <-
      Expression(e),
      Env(env),
      let Some((coeff, var, offset)) = env.try_to_affine(e);

    struct RangeConstraintOnVar<T: FieldElement>(Var, RangeConstraint<T>);
    RangeConstraintOnVar(v, rc) <- Env(env), ContainsVariable(_, v), let rc = env.get(&v);
    // RC(coeff * var + offset) = rc <=>
    // coeff * RC(var) + offset = rc <=>
    // RC(var) = (rc - offset) / coeff
    RangeConstraintOnVar(v, rc.combine_sum(&RangeConstraint::from_value(-offset)).multiple(T::one() / coeff)) <-
      RangeConstraintOnExpression(e, rc),
      AffineExpression(e, coeff, v, offset),
      (coeff != T::zero());

    RangeConstraintOnVar(v, v_rc1.conjunction(&v_rc2)) <-
      RangeConstraintOnVar(v, v_rc1),
      RangeConstraintOnVar(v, v_rc2);

    struct ReplaceAlgebraicConstraintBy(Expr, Expr);

    struct Solvable<T: FieldElement>(Expr, Var, T);
    Solvable(e, var, -offset / coeff) <-
      AffineExpression(e, coeff, var, offset);

    struct Assignment<T: FieldElement>(Var, T);
    Assignment(var, v) <-
      AlgebraicConstraint(e),
      Solvable(e, var, v);

    struct Equivalence(Var, Var);

    //------- quadratic equivalence -----

    // (E, expr, offset) <-> E = (expr) * (expr + offset) is a constraint
    struct QuadraticEquivalenceCandidate<T: FieldElement>(Expr, Expr, T);
    QuadraticEquivalenceCandidate(e, r, offset) <-
       Env(env),
       AlgebraicConstraint(e),
       Product(e, l, r),
       ({env.affine_var_count(l).unwrap_or(0) > 1 && env.affine_var_count(r).unwrap_or(0) > 1}),
       let Some(offset) = env.constant_difference(l, r);

    struct QuadraticEquivalenceCandidatePair<T: FieldElement>(Expr, Expr, T, Var, Var);
    QuadraticEquivalenceCandidatePair(expr1, expr2, offset / coeff, v1, v2) <-
      Env(env),
      QuadraticEquivalenceCandidate(_, expr1, offset),
      QuadraticEquivalenceCandidate(_, expr2, offset),
      (expr1 < expr2),
      let Some((v1, v2, coeff)) = env.differ_in_exactly_one_variable(expr1, expr2);
    // what exactly is re-executed for an update?

    struct QuadraticEquivalence(Var, Var);
    QuadraticEquivalence(v1, v2) <-
      QuadraticEquivalenceCandidatePair(_, _, offset, v1, v2),
      RangeConstraintOnVar(v1, rc),
      RangeConstraintOnVar(v2, rc),
      (rc.is_disjoint(&rc.combine_sum(&RangeConstraint::from_value(offset))));


    Equivalence(v1, v2) <- QuadraticEquivalence(v1, v2);

    ReplaceAlgebraicConstraintBy(e, env.substitute_by_known(e, v, val)) <-
      Env(env),
      Assignment(v, val),
      ContainsVariable(e, v),
      AlgebraicConstraint(e);

    ReplaceAlgebraicConstraintBy(e, env.substitute_by_var(e, v, v2)) <-
       Env(env),
       AlgebraicConstraint(e),
       ContainsVariable(e, v),
       Equivalence(v, v2);

    AlgebraicConstraint(e) <-
      ReplaceAlgebraicConstraintBy(_, e);


    @output
    pub struct ActionRule<T>(pub Action<T>);
    ActionRule(Action::SubstituteVariableByConstant(v, val)) <-
      Assignment(v, val);
    ActionRule(Action::SubstituteVariableByVariable(v1, v2)) <-
      Equivalence(v1, v2);
}
