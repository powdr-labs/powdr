#![allow(clippy::iter_over_hash_type)]
// This is about a warning about interior mutability for the key
// `Env`. We need it and it is probably fine.
#![allow(clippy::mutable_key_type)]

use crepe::crepe;
use itertools::Itertools;
use powdr_number::FieldElement;

use crate::{
    constraint_system::ComputationMethod,
    grouped_expression::{GroupedExpression, GroupedExpressionComponent, RangeConstraintProvider},
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

    // This rule is important: Just because a rule "generates" an Expr it does not
    // mean that it automatically is an Expression. If we want to say something
    // about all Exprs, we have to make sure to "obtain" them from Expression.
    struct Expression(Expr);
    Expression(e) <- AlgebraicConstraint(e);
    Expression(e) <- InitialRangeConstraintOnExpression(e, _);

    // ReplaceAlgebraicConstraintBy(old_expr, new_expr) => old_expr can equivalently
    // be replaced by new_expr (and new_expression is in some way "simpler").
    struct ReplaceAlgebraicConstraintBy(Expr, Expr);


    //////////////////// STRUCTURAL PROPERTIES OF EXPRESSIONS //////////////////////

    // ContainsVariable(e, v) => v appears inside e.
    struct ContainsVariable(Expr, Var);
    ContainsVariable(e, v) <-
      Env(env),
      Expression(e),
      for v in env.on_expr(e, (), |e, _| e.referenced_unknown_variables().cloned().collect_vec());

    // AffineExpression(e, coeff, var, offset) => e = coeff * var + offset
    struct AffineExpression<T: FieldElement>(Expr, T, Var, T);
    AffineExpression(e, coeff, var, offset) <-
      Expression(e),
      Env(env),
      let Some((coeff, var, offset)) = env.try_to_affine(e);

    // HasProductSummand(e, l, r) => e contains a summand of the form l * r
    struct HasProductSummand(Expr, Expr, Expr);
    HasProductSummand(e, l, r) <-
      Env(env),
      Expression(e),
      (!env.on_expr(e, (), |e, _| e.is_affine())),
      for (l, r) in env.extract(e).into_summands().filter_map(|s| {
          if let GroupedExpressionComponent::Quadratic(l, r) = s {
              Some((env.insert_owned(l), env.insert_owned(r)))
          } else {
              None
          }
      });
    HasProductSummand(e, r, l) <- HasProductSummand(e, l, r);
    Expression(l) <- HasProductSummand(_, l, _);
    Expression(r) <- HasProductSummand(_, _, r);

    // Product(e, l, r) => e = l * r
    struct Product(Expr, Expr, Expr);
    Product(e, l, r) <-
      Expression(e),
      Env(env),
      let Some((l, r)) = env.try_as_single_product(e);
    Product(e, r, l) <- Product(e, l, r);
    Expression(e) <- Product(_, e, _);
    Expression(e) <- Product(_, _, e);

    // ProductConstraint(e, l, r) => e is an algebraic constraint of the form l * r = 0
    struct ProductConstraint(Expr, Expr, Expr);
    ProductConstraint(e, l, r) <-
      AlgebraicConstraint(e),
      Product(e, l, r);


    //////////////////////// RANGE CONSTRAINTS //////////////////////////

    struct RangeConstraintOnExpression<T: FieldElement>(Expr, RangeConstraint<T>);
    RangeConstraintOnExpression(e, rc) <-
      InitialRangeConstraintOnExpression(e, rc);
    RangeConstraintOnExpression(e, rc) <-
      Env(env),
      Expression(e),
      let rc = env.on_expr(e, (), |expr, _| expr.range_constraint(env));

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


    //////////////////////// SINGLE-OCCURRENCE VARIABLES //////////////////////////

    // Combine multiple variables that only occur in the same algebraic constraint.
    //
    // Assume we have an algebraic constraint of the form `X * V1 + Y * V2 = R`,
    // where `V1` and `V2` only occur in this constraint and only once.
    // The only combination of values for `X`, `Y` and `R` where this is _not_ satisfiable
    // is `X = 0`, `Y = 0`, `R != 0`. So the constraint is equivalent to the statement
    // `(X = 0 and Y = 0) -> R = 0`.
    //
    // Consider the simpler case where both `X` and `Y` are non-negative such that
    // `X + Y` does not wrap.
    // Then `X = 0 and Y = 0` is equivalent to `X + Y = 0`. So we can replace the constraint
    // by `(X + Y) * V3 = C`, where `V3` is a new variable that only occurs here.
    //
    // For the general case, where e.g. `X` can be negative, we replace it by `X * X`,
    // if that value is still small enough.
    struct SingleOccurrenceVariable(Var);
    SingleOccurrenceVariable(v) <-
      Env(env),
      for v in env.single_occurrence_variables().cloned();
    // SingleOccurrenceVariable(e, v) => v occurs only once in e and e is the
    // only constraint in appears in.
    struct SingleOccurrenceVariableInExpr(Expr, Var);
    SingleOccurrenceVariableInExpr(e, v) <-
      SingleOccurrenceVariable(v),
      ContainsVariable(e, v),
      AlgebraicConstraint(e);

    // LargestSingleOccurrenceVariablePairInExpr(e, v1, v2) =>
    // v1 and v2 are different variables that only occur in e and only once,
    // and are the two largest variables with that property in e.
    struct LargestSingleOccurrenceVariablePairInExpr(Expr, Var, Var);
    LargestSingleOccurrenceVariablePairInExpr(e, v1, v2) <-
      Env(env),
      SingleOccurrenceVariableInExpr(e, v1),
      SingleOccurrenceVariableInExpr(e, v2),
      (v1 < v2),
      (env
        .single_occurrence_variables()
        .filter(|v3| env.on_expr(e, (), |e, _| {
            e.referenced_unknown_variables().any(|v| v == *v3)
        }))
        .all(|&v3| v3 == v1 || v3 == v2 || v3 < v1));

    // FreeVariableCombinationCandidate(e, coeff1, v1, coeff2, v2, x1, x2)
    // => e is the expression of an algebraic constraint and
    // e = coeff1 * v1 * x1 + coeff2 * v2 * x2 + ...
    // where v1 and v2 are different variables that only occur here and only once.
    struct FreeVariableCombinationCandidate<T: FieldElement>(Expr, T, Var, Expr, T, Var, Expr);
    FreeVariableCombinationCandidate(e, coeff1, v1, x1, coeff2, v2, x2) <-
      // If we only consider he largest variable pair we could miss optimization opportunities,
      // but at least the replacement becomes deterministic.
      LargestSingleOccurrenceVariablePairInExpr(e, v1, v2),
      AlgebraicConstraint(e),
      HasProductSummand(e, x1, v1_e),
      AffineExpression(v1_e, coeff1, v1, offset1),
      (offset1.is_zero()),
      HasProductSummand(e, x2, v2_e),
      (x2 != v1_e),
      (x1 != v2_e),
      AffineExpression(v2_e, coeff2, v2, offset2),
      (offset2.is_zero());

    ReplaceAlgebraicConstraintBy(e, replacement) <-
      Env(env),
      FreeVariableCombinationCandidate(e, coeff1, v1, x1, coeff2, v2, x2),
      // Here, we have e = coeff1 * v1 * x1 + coeff2 * v2 * x2 + ...
      RangeConstraintOnExpression(x1, rc1),
      RangeConstraintOnExpression(x2, rc2),
      let Some(replacement) = (|| {
        // If the expression is not known to be non-negative, we square it.
        let square_if_needed = |expr: Expr, rc: RangeConstraint<T>| {
            let expr = env.extract(expr);
            if rc.range().0 == T::zero() {
                (expr, rc)
            } else {
                (expr.clone() * expr, rc.square())
            }
        };
        let (x1, rc1) = square_if_needed(x1, rc1);
        let (x2, rc2) = square_if_needed(x2, rc2);
        if !rc1.range().0.is_zero() || !rc2.range().0.is_zero() {
            return None;
        }
        let sum_rc = rc1.multiple(coeff1).combine_sum(&rc2.multiple(coeff2));
        if !(sum_rc.range().0.is_zero() && sum_rc.range().1 < T::from(-1)) {
            return None;
        }
        // Remove the summands with v1 and v2 from the expression.
        let r = env.extract(e).into_summands().filter(|s|{
            if let GroupedExpressionComponent::Quadratic(l, r) = s {
                let mut vars = l.referenced_unknown_variables().chain(r.referenced_unknown_variables());
                if vars.any(|v| v == &v1 || v == &v2) {
                    return false;
                }
            };
            true
        }).map(GroupedExpression::from).sum::<GroupedExpression<T, Var>>();
        let factor = x1 * coeff1 + x2 * coeff2;
        let combined_var = env.new_var("free_var", ComputationMethod::QuotientOrZero(-r.clone(), factor.clone()));
        let replacement = r + GroupedExpression::from_unknown_variable(combined_var) * factor;
        Some(env.insert_owned(replacement))
      })();

    //////////////////////// AFFINE SOLVING //////////////////////////

    // Solvable(e, var, value) => (e = 0 => var = value)
    // Note that e is not required to be a constraint here.
    struct Solvable<T: FieldElement>(Expr, Var, T);
    Solvable(e, var, -offset / coeff) <-
      AffineExpression(e, coeff, var, offset);

    // Assignment(var, v) => any satisfying assignment has var = v.
    struct Assignment<T: FieldElement>(Var, T);
    Assignment(var, v) <-
      AlgebraicConstraint(e),
      Solvable(e, var, v);

    ///////////////////////////////// OUTPUT ACTIONS //////////////////////////

    struct Equivalence(Var, Var);

    @output
    pub struct ActionRule<T>(pub Action<T>);
    ActionRule(Action::SubstituteVariableByConstant(v, val)) <-
      Assignment(v, val);
    // Substitute the larger variable by the smaller.
    ActionRule(Action::SubstituteVariableByVariable(v1, v2)) <-
      Equivalence(v1, v2), (v1 > v2);
    ActionRule(Action::SubstituteVariableByVariable(v2, v1)) <-
      Equivalence(v1, v2), (v2 > v1);
    ActionRule(Action::ReplaceAlgebraicConstraintBy(e1, e2)) <-
      ReplaceAlgebraicConstraintBy(e1, e2);
}
