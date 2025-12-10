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

// This file contains the set of datalog rules executed on the constraint system.
// Facts/relations will be produced according to the rules from existing
// facts until a fixed point is reached.
// Facts marked by `@input` are provided as input to the rule engine,
// and cannot be derived/extended by the rules.
// Facts marked by `@output` are collected as output from the rules engine.
// The only output is a set of Action rules to be applied to the constraint system.
// Substitutions performed on constraints inside the rule system are not
// automatically reflected in the constraint system to be optimized.
//
// The conditions of the rules are looped over / checked in the order in which they are
// written. If all of them match, the "head" of the rule is executed and a new
// fact is inserted into the database.
// If non-trivial rust code is used as a condition, it is advisable to end the rule
// after that condition and create a new "intermediate" fact for performance reasons.
//
// Since all rules are executed as long as they match, it is not possible to restrict
// or somehow direct the fact derivation process. For example, if a variable replacement
// is derived, new algebraic constraints will be created, but this does not mean that
// the old constraints are removed. If we have a constraint that has many variables
// and all of them are determined to be constant by other constraints, then the
// derivation process will create all possible combinations of substitutions.
// The same is true for range constraints: If we have a rule that requires a
// range constraint for a variable, it will iterate over all range constraints
// that have been derived for that variable over the course of executing the rules,
// not just the most strict one.

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

    // This rule is important: Just because a rule "generates" an Expr it does not
    // mean that it automatically is an Expression. If we want to say something
    // about all Exprs, we have to make sure to "obtain" them from Expression.
    struct Expression(Expr);
    Expression(e) <- AlgebraicConstraint(e);
    Expression(e) <- RangeConstraintOnExpression(e, _);

    RangeConstraintOnExpression(e, rc) <-
      InitialRangeConstraintOnExpression(e, rc);

    // ContainsVariable(e, v) => v appears inside e.
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

    // AffineExpression(e, coeff, var, offset) => e = coeff * var + offset
    struct AffineExpression<T: FieldElement>(Expr, T, Var, T);
    AffineExpression(e, coeff, var, offset) <-
      Expression(e),
      Env(env),
      let Some((coeff, var, offset)) = env.try_to_affine(e);

    // RangeConstraintOnVar(v, rc) => variable v has range constraint rc.
    // Note that this range constraint is not necessarily the currently best known
    // one, but any range constraint that is derivable using the rules.
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

    // Solvable(e, var, value) => (e = 0 => var = value)
    // Note that e is not required to be a constraint here.
    struct Solvable<T: FieldElement>(Expr, Var, T);
    Solvable(e, var, -offset / coeff) <-
      AffineExpression(e, coeff, var, offset);

    struct Assignment<T: FieldElement>(Var, T);
    Assignment(var, v) <-
      AlgebraicConstraint(e),
      Solvable(e, var, v);

    struct Equivalence(Var, Var);

    //------- quadratic equivalence -----

    // QuadraticEquivalenceCandidate(E, expr, offset) =>
    //   E = ((expr) * (expr + offset) = 0) is a constraint and
    //   expr is affine with at least 2 variables.
    struct QuadraticEquivalenceCandidate<T: FieldElement>(Expr, Expr, T);
    QuadraticEquivalenceCandidate(e, r, offset) <-
       Env(env),
       AlgebraicConstraint(e),
       Product(e, l, r), // note that this will always produce two facts for (l, r) and (r, l)
       ({env.affine_var_count(l).unwrap_or(0) > 1 && env.affine_var_count(r).unwrap_or(0) > 1}),
       let Some(offset) = env.constant_difference(l, r);

    // QuadraticEquivalenceCandidatePair(expr1, expr2, offset1 / coeff, v1, v2) =>
    //  (expr1) * (expr1 + offset1) = 0 and (expr2) * (expr2 + offset2) = 0 are constraints,
    //  expr1 is affine with at least 2 variables and is obtained from
    //  expr2 * factor by substituting v2 by v1 (factor != 0),
    //  offset1 == offset2 * factor and coeff is the coefficient of v1 in expr1.
    //
    //  This means that v1 is always equal to (-expr1 / coeff) or equal to
    //  (-(expr1 + offset1) / coeff) = (-expr1 / coeff - offset1 / coeff).
    //  Because of the above, also v2 is equal to
    //  (-expr1 / coeff) or equal to (-(expr1 + offset1) / coeff) [Yes, expr1!].
    struct QuadraticEquivalenceCandidatePair<T: FieldElement>(Expr, Expr, T, Var, Var);
    QuadraticEquivalenceCandidatePair(expr1, expr2, offset1 / coeff, v1, v2) <-
      Env(env),
      QuadraticEquivalenceCandidate(_, expr1, offset1),
      QuadraticEquivalenceCandidate(_, expr2, offset2),
      (expr1 < expr2),
      let Some((v1, v2,factor)) = env.differ_in_exactly_one_variable(expr1, expr2),
      (offset1 == offset2 * factor),
      let coeff = env.on_expr(expr1, (), |e, _| *e.coefficient_of_variable_in_affine_part(&v1).unwrap());

    // QuadraticEquivalence(v1, v2) => v1 and v2 are equal in all satisfying assignments.
    // Because of QuadraticEquivalenceCandidatePair, v1 is equal to X or X + offset,
    // where X is some value that depends on other variables. Similarly, v2 is equal to X or X + offset.
    // Because of the range constraints of v1 and v2, these two "or"s are exclusive ors.
    // This means depending on the value of X, it is either X or X + offset.
    // Since this "decision" only depens on X, both v1 and v2 are either X or X + offset at the same time
    // and thus equal.
    struct QuadraticEquivalence(Var, Var);
    QuadraticEquivalence(v1, v2) <-
      QuadraticEquivalenceCandidatePair(_, _, offset, v1, v2),
      RangeConstraintOnVar(v1, rc),
      RangeConstraintOnVar(v2, rc),
      (rc.is_disjoint(&rc.combine_sum(&RangeConstraint::from_value(offset))));

    Equivalence(v1, v2) <- QuadraticEquivalence(v1, v2);


    //------- end quadratic equivalence -----


    //------- split constraint based on minimal range derivation -----

    // it doesn't need to be affine, but only consider this case now.
    // split the expression into coeff * var + rest_expr = 0
    struct MultiVarAffineExpression<T: FieldElement>(Expr, T);



    struct MinimalRangeDerivationCandidate<T:FieldElement>(Expr,Expr,Expr, RangeConstraint<T>,RangeConstraint<T>);
    MinimalRangeDerivationCandidate(expr, expr1, expr_rest, expr1_rc,expr_rest_rc) <-
      Env(env),
      AlgebraicConstraint(expr),
      AffineExpression(expr1, coeff, var, left_offset),
      (left_offset == T::zero()),
      RangeConstraintOnExpression(expr1, expr1_rc),
      RangeConstraintOnExpression(expr_rest, expr_rest_rc),
      (env.is_minimal_range_deducible(expr1,expr_rest,expr));
      



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
    // Substitute the larger variable by the smaller.
    ActionRule(Action::SubstituteVariableByVariable(v1, v2)) <-
      Equivalence(v1, v2), (v1 > v2);
    ActionRule(Action::SubstituteVariableByVariable(v2, v1)) <-
      Equivalence(v1, v2), (v2 > v1);
}
