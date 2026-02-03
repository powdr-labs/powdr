#![allow(clippy::iter_over_hash_type)]
// This is about a warning about interior mutability for the key
// `Env`. We need it and it is probably fine.
#![allow(clippy::mutable_key_type)]

use crepe::crepe;
use itertools::Itertools;
use powdr_number::FieldElement;

use crate::{
    constraint_system::ComputationMethod,
    grouped_expression::{GroupedExpression, GroupedExpressionComponent},
    range_constraint::RangeConstraint,
    rule_based_optimizer::{
        environment::Environment,
        types::{Action, Expr, ReplaceConstraintsAction, Var},
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

    @input
    pub struct RangeConstraintOnVar<T: FieldElement>(pub Var, pub RangeConstraint<T>);

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

    // ReplaceAlgebraicConstraintsBy(e1, e2) =>
    // the system that does not have the constraints in `e1` but has
    // the new constraints in `e2` is equivalent.
    struct ReplaceAlgebraicConstraintsBy([Option<Expr>; 10], [Option<Expr>; 5]);


    //////////////////// BASIC SEMANTIC PROPERTIES OF EXRESSIONS //////////////////////


    // EqualZero(e) => e = 0 for all satisfying assignments.
    struct EqualZero(Expr);
    EqualZero(e) <- AlgebraicConstraint(e);

    //////////////////// STRUCTURAL PROPERTIES OF EXPRESSIONS //////////////////////

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
    Expression(e) <- Product(_, e, _);
    Expression(e) <- Product(_, _, e);

    // AffineExpression(e, coeff, var, offset) => e = coeff * var + offset
    struct AffineExpression<T: FieldElement>(Expr, T, Var, T);
    AffineExpression(e, coeff, var, offset) <-
      Expression(e),
      Env(env),
      let Some((coeff, var, offset)) = env.try_to_affine(e);

    struct LinearExpression<T: FieldElement>(Expr, Var, T);
    LinearExpression(e, var, coeff) <-
      AffineExpression(e, coeff, var, T::zero());

    struct Constant<T: FieldElement>(Expr, T);
    Constant(e, value) <-
      Expression(e),
      Env(env),
      let Some(value) = env.try_to_number(e);

    // Split the expression into head and tail
    // ExpressionSumHeadTail(e, h, t) => e = h + t
    struct ExpressionSumHeadTail(Expr, Expr, Expr);
    ExpressionSumHeadTail(e, head, tail) <-
      Env(env),
      Expression(e),
      let Some((head, tail)) = env.try_split_into_head_tail(e);
    Expression(head) <- ExpressionSumHeadTail(_, head, _);
    Expression(tail) <- ExpressionSumHeadTail(_, _, tail);

    // IsAffine(e) => e is an affine expression, i.e. does not have super-linear parts.
    struct IsAffine(Expr);
    IsAffine(e) <-
      Constant(e, _);
    IsAffine(e) <-
      ExpressionSumHeadTail(e, head, tail),
      LinearExpression(head, _, _),
      IsAffine(tail);

    // HasSummand(e, summand) => summand is one of the summands of e.
    struct HasSummand(Expr, Expr);
    HasSummand(e, summand) <- ExpressionSumHeadTail(e, summand, _);
    HasSummand(e, summand) <-
      ExpressionSumHeadTail(e, _, tail),
      HasSummand(tail, summand);

    // DifferBySummand(e1, e2, s) => e1 = e2 + s and `s` is not a sum.
    // Note that `e1` and `e2` must "pre-exist" as expressions, i.e.
    // this rule cannot be used to split out a linear summand
    // from an expression but only to "compare" two expressions.
    struct DifferBySummand(Expr, Expr, Expr);
    DifferBySummand(e1, e2, s) <-
      ExpressionSumHeadTail(e1, s, e2);
    DifferBySummand(e1, e2, s) <-
      DifferBySummand(tail1, tail2, s),
      ExpressionSumHeadTail(e1, head, tail1),
      ExpressionSumHeadTail(e2, head, tail2);

    // AffinelyRelated(e1, f, e2, c) => e1 = f * e2 + c
    // Note this is currently only implemented for affine e1 and e2.
    // This only works if e1 and e2 have at least one variable
    // and both e1 and e2 have to "pre-exist" as expressions.
    // This means this rule cannot be used to subtract constants
    // or multiply/divide by constants alone.
    struct AffinelyRelated<T: FieldElement>(Expr, T, Expr, T);
    AffinelyRelated(e1, f, e2, o1 - o2 * f) <-
      AffineExpression(e1, f1, v, o1), // e1 = f1 * v + o1
      AffineExpression(e2, f2, v, o2),
      // Optimization: Compute f1 / f2 only once.
      let f = f1 / f2;
      // e2 = f2 * v + o2
      // e1 = f1 * (e2 - o2) / f2 + o1 = e2 * (f1 / f2) + (o1 - o2 * f1 / f2)

    AffinelyRelated(e1, f, e2, o) <-
      AffinelyRelated(tail1, f, tail2, o),
      // The swapped case and the equal will be computed by other rules.
      ExpressionSumHeadTail(e1, head1, tail1),
      LinearExpression(head1, v, f1),
      ExpressionSumHeadTail(e2, head2, tail2),
      LinearExpression(head2, v, f1 / f);

    // HasProductSummand(e, l, r) => e contains a summand of the form l * r
    struct HasProductSummand(Expr, Expr, Expr);
    HasProductSummand(e, l, r) <-
      HasSummand(e, summand),
      Product(summand, l, r);
    HasProductSummand(e, r, l) <- HasProductSummand(e, l, r);

    // ProductConstraint(e, l, r) => e is an algebraic constraint of the form l * r = 0
    struct ProductConstraint(Expr, Expr, Expr);
    ProductConstraint(e, l, r) <-
      AlgebraicConstraint(e),
      Product(e, l, r);


    //////////////////////// RANGE CONSTRAINTS //////////////////////////

    // Range constraints are tricky because they can easily lead to exponential behaviour.
    // Because of that, we should never update a range constraint on a variable
    // and only compute range constraints on expressions from smaller expressions.

    struct RangeConstraintOnExpression<T: FieldElement>(Expr, RangeConstraint<T>);
    RangeConstraintOnExpression(e, rc) <-
      InitialRangeConstraintOnExpression(e, rc);
    RangeConstraintOnExpression(e, rc.square()) <-
      Product(e, l, r),
      (l == r),
      RangeConstraintOnExpression(l, rc);
    RangeConstraintOnExpression(e, l_rc.combine_product(&r_rc)) <-
      Product(e, l, r),
      (l < r),
      RangeConstraintOnExpression(l, l_rc),
      RangeConstraintOnExpression(r, r_rc);
    RangeConstraintOnExpression(e, v_rc.multiple(coeff)) <-
      LinearExpression(e, v, coeff),
      RangeConstraintOnVar(v, v_rc);
    RangeConstraintOnExpression(e, head_rc.combine_sum(&tail_rc)) <-
      ExpressionSumHeadTail(e, head, tail),
      RangeConstraintOnExpression(head, head_rc),
      RangeConstraintOnExpression(tail, tail_rc);
    RangeConstraintOnExpression(e, RangeConstraint::from_value(value)) <-
      Constant(e, value);

    // UpdateRangeConstraintOnVar(v, rc) => rc is a valid range constraint for variable v
    // This is an output predicate and might cause the rule system to re-run if
    // the range constraint is better than the currently best known.
    // Please avoid deriving new range constraints directly since this can easily
    // lead to exponential behaviour.
    struct UpdateRangeConstraintOnVar<T: FieldElement>(Var, RangeConstraint<T>);
    // RC(coeff * var + offset) = rc <=>
    // coeff * RC(var) + offset = rc <=>
    // RC(var) = (rc - offset) / coeff
    UpdateRangeConstraintOnVar(v, rc.combine_sum(&RangeConstraint::from_value(-offset)).multiple(T::one() / coeff)) <-
      RangeConstraintOnExpression(e, rc),
      AffineExpression(e, coeff, v, offset),
      (coeff != T::zero());

    // This derives boolean constraints on variables from `v * (v - 1) = 0`,
    // but also works with `v * (v - 8) = 0` or similar.
    UpdateRangeConstraintOnVar(v, RangeConstraint::from_value(c1).disjunction(&RangeConstraint::from_value(c2))) <-
      ProductConstraint(_, l, r),
      (l < r),
      Solvable(l, v, c1),
      Solvable(r, v, c2);

    //////////////////////// SINGLE-OCCURRENCE VARIABLES //////////////////////////

    // Combine multiple variables that only occur in the same algebraic constraint.
    //
    // The use-case here is for "diff_inv_marker_..." variables that each are the
    // inverse of certain variables only if those variables are non-zero
    // (and arbitrary otherwise).
    // If the "diff_inv_marker_..." variables only occur once, they are essentially
    // "free" variables and under some conditions, we can combine them into a single
    // free variable and thus reduce the number of variables.
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
    // only constraint it appears in.
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
      // If we only consider the largest variable pair we could miss optimization opportunities,
      // but at least the replacement becomes deterministic.
      LargestSingleOccurrenceVariablePairInExpr(e, v1, v2),
      AlgebraicConstraint(e),
      HasProductSummand(e, x1, v1_e),
      LinearExpression(v1_e, v1, coeff1),
      HasProductSummand(e, x2, v2_e),
      (x2 != v1_e),
      (x1 != v2_e),
      LinearExpression(v2_e, v2, coeff2);

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
        let factor = x1.clone() * coeff1 + x2.clone() * coeff2;
        let combined_var = env.new_var("free_var", ComputationMethod::QuotientOrZero(-r.clone(), factor.clone()));
        let replacement = r + GroupedExpression::from_unknown_variable(combined_var) * factor;
        Some(env.insert_owned(replacement))
      })();

    //////////////// COMBINE CONSTRAINTS WITH NON-NEGATIVE FACTORS /////////////////////

    // If we have `x * a = 0` and `x * b = 0` and `a` and `b` are
    // both non-negative and their sum is constrained, then we can replace
    // both constraints by `x * (a + b) = 0`.
    // The same applies to three constraints.

    struct ReplaceEightOfAlgebraicConstraintsBy(Expr, Expr, Expr,Expr, Expr, Expr, Expr, Expr, Expr);
    ReplaceEightOfAlgebraicConstraintsBy(e0, e1, e2, e3, e4, e5, e6, e7, replacement) <-
      Env(env),
      ProductConstraint(e0, x, a0),
      ProductConstraint(e1, x, a1),
      (e0 < e1),
      ProductConstraint(e2, x, a2),
      (e1 < e2),
      ProductConstraint(e3, x, a3),
      (e2 < e3),
      ProductConstraint(e4, x, a4),
      (e3 < e4),
      ProductConstraint(e5, x, a5),
      (e4 < e5),
      ProductConstraint(e6, x, a6),
      (e5 < e6),
      ProductConstraint(e7, x, a7),
      (e6 < e7),
      RangeConstraintOnExpression(a0, rc_a0),
      RangeConstraintOnExpression(a1, rc_a1),
      RangeConstraintOnExpression(a2, rc_a2),
      RangeConstraintOnExpression(a3, rc_a3),
      RangeConstraintOnExpression(a4, rc_a4),
      RangeConstraintOnExpression(a5, rc_a5),
      RangeConstraintOnExpression(a6, rc_a6),
      RangeConstraintOnExpression(a7, rc_a7),
      (rc_a0.range().0 == T::zero()
        && rc_a1.range().0 == T::zero()
        && rc_a2.range().0 == T::zero()
        && rc_a3.range().0 == T::zero()
        && rc_a4.range().0 == T::zero()
        && rc_a5.range().0 == T::zero()
        && rc_a6.range().0 == T::zero()
        && rc_a7.range().0 == T::zero()
        && !rc_a0.combine_sum(&rc_a1).combine_sum(&rc_a2.combine_sum(&rc_a3)).combine_sum(&rc_a4).combine_sum(&rc_a5).combine_sum(&rc_a6).combine_sum(&rc_a7).is_unconstrained()),
      let replacement = env.insert_owned(env.extract(x) * (env.extract(a0) + env.extract(a1) + env.extract(a2) + env.extract(a3)+ env.extract(a4) + env.extract(a5) + env.extract(a6) + env.extract(a7)));

    struct ReplacedExpressionInEightSet(Expr);
    ReplacedExpressionInEightSet(e) <-
      ReplaceEightOfAlgebraicConstraintsBy(e, _, _, _, _, _, _, _, _);
    ReplacedExpressionInEightSet(e) <-
      ReplaceEightOfAlgebraicConstraintsBy(_, e, _, _, _, _, _, _, _);
    ReplacedExpressionInEightSet(e) <-
      ReplaceEightOfAlgebraicConstraintsBy(_, _, e, _, _, _, _, _, _);
    ReplacedExpressionInEightSet(e) <-
      ReplaceEightOfAlgebraicConstraintsBy(_, _, _, e, _, _, _, _, _);
    ReplacedExpressionInEightSet(e) <-
      ReplaceEightOfAlgebraicConstraintsBy(_, _, _, _, e, _, _, _, _);
    ReplacedExpressionInEightSet(e) <-
      ReplaceEightOfAlgebraicConstraintsBy(_, _, _, _, _, e, _, _, _);
    ReplacedExpressionInEightSet(e) <-
      ReplaceEightOfAlgebraicConstraintsBy(_, _, _, _, _, _, e, _, _);
    ReplacedExpressionInEightSet(e) <-
      ReplaceEightOfAlgebraicConstraintsBy(_, _, _, _, _, _, _, e, _);

    struct ReplaceFourOfAlgebraicConstraintsBy(Expr, Expr, Expr, Expr, Expr);
    ReplaceFourOfAlgebraicConstraintsBy(e0, e1, e2, e3, replacement) <-
      Env(env),
      ProductConstraint(e0, x, a0),
      !ReplacedExpressionInEightSet(e0),
      ProductConstraint(e1, x, a1),
      !ReplacedExpressionInEightSet(e1),
      (e0 < e1),
      ProductConstraint(e2, x, a2),
      !ReplacedExpressionInEightSet(e2),
      (e1 < e2),
      ProductConstraint(e3, x, a3),
      !ReplacedExpressionInEightSet(e3),
      (e2 < e3),
      RangeConstraintOnExpression(a0, rc_a0),
      RangeConstraintOnExpression(a1, rc_a1),
      RangeConstraintOnExpression(a2, rc_a2),
      RangeConstraintOnExpression(a3, rc_a3),
      (rc_a0.range().0 == T::zero()
        && rc_a1.range().0 == T::zero()
        && rc_a2.range().0 == T::zero()
        && rc_a3.range().0 == T::zero()
        && !rc_a0.combine_sum(&rc_a1).combine_sum(&rc_a2.combine_sum(&rc_a3)).is_unconstrained()),
      let replacement = env.insert_owned(env.extract(x) * (env.extract(a0) + env.extract(a1) + env.extract(a2) + env.extract(a3)));

    struct ReplacedExpressionInFourSet(Expr);
    ReplacedExpressionInFourSet(e) <-
      ReplaceFourOfAlgebraicConstraintsBy(e, _, _, _, _);
    ReplacedExpressionInFourSet(e) <-
      ReplaceFourOfAlgebraicConstraintsBy(_, e, _, _, _);
    ReplacedExpressionInFourSet(e) <-
      ReplaceFourOfAlgebraicConstraintsBy(_, _, e, _, _);
    ReplacedExpressionInFourSet(e) <-
      ReplaceFourOfAlgebraicConstraintsBy(_, _, _, e, _);

    struct ReplaceTripleOfAlgebraicConstraintsBy(Expr, Expr, Expr, Expr);
    ReplaceTripleOfAlgebraicConstraintsBy(e1, e2, e3, replacement) <-
      Env(env),
      ProductConstraint(e1, x, a1),
      ProductConstraint(e2, x, a2),
      ProductConstraint(e3, x, a3),
      !ReplacedExpressionInEightSet(e1),
      !ReplacedExpressionInEightSet(e2),
      !ReplacedExpressionInEightSet(e3),
      !ReplacedExpressionInFourSet(e1),
      !ReplacedExpressionInFourSet(e2),
      !ReplacedExpressionInFourSet(e3),
      (e1 < e2),
      (e2 < e3),
      RangeConstraintOnExpression(a1, rc_a1),
      RangeConstraintOnExpression(a2, rc_a2),
      RangeConstraintOnExpression(a3, rc_a3),
      (rc_a1.range().0 == T::zero()
        && rc_a2.range().0 == T::zero()
        && rc_a3.range().0 == T::zero()
        && !rc_a1.combine_sum(&rc_a2).combine_sum(&rc_a3).is_unconstrained()),
      let replacement = env.insert_owned(env.extract(x) * (env.extract(a1) + env.extract(a2) + env.extract(a3)));

    struct ReplacedExpressionInTripleSet(Expr);
    ReplacedExpressionInTripleSet(e) <- ReplaceTripleOfAlgebraicConstraintsBy(e, _, _, _);
    ReplacedExpressionInTripleSet(e) <- ReplaceTripleOfAlgebraicConstraintsBy(_, e, _, _);
    ReplacedExpressionInTripleSet(e) <- ReplaceTripleOfAlgebraicConstraintsBy(_, _, e, _);


    ReplaceAlgebraicConstraintsBy(extend_by_none([e1, e2]), replacement) <-
      Env(env),
      ProductConstraint(e1, x, a),
      ProductConstraint(e2, x, b),
      !ReplacedExpressionInEightSet(e1),
      !ReplacedExpressionInEightSet(e2),
      !ReplacedExpressionInFourSet(e1),
      !ReplacedExpressionInFourSet(e2),
      !ReplacedExpressionInTripleSet(e1),
      !ReplacedExpressionInTripleSet(e2),
      (e1 < e2),
      RangeConstraintOnExpression(a, rc_a),
      RangeConstraintOnExpression(b, rc_b),
      (rc_a.range().0 == T::zero()
        && rc_b.range().0 == T::zero() && !rc_a.combine_sum(&rc_b).is_unconstrained()),
      let replacement = extend_by_none([env.insert_owned(env.extract(x) * (env.extract(a) + env.extract(b)))]);

    //////////////////////// AFFINE SOLVING //////////////////////////

    // Solvable(e, var, value) => (e = 0 => var = value)
    // Note that e is not required to be a constraint here.
    struct Solvable<T: FieldElement>(Expr, Var, T);
    Solvable(e, var, -offset / coeff) <-
      AffineExpression(e, coeff, var, offset);

    // Assignment(var, v) => any satisfying assignment has var = v.
    struct Assignment<T: FieldElement>(Var, T);
    Assignment(var, v) <-
      EqualZero(e),
      Solvable(e, var, v);

    ///////////////////////////////// NO-WRAP ZERO SUM //////////////////////////

    // If an algebraic constraint head + tail = 0 has the following properties:
    // 1. the range constraint of head is [0, a] with a < P - 1,
    // 2. the range constraint of tail is [0, b] with b < P - 1,
    // 3. a + b (as integers) < P - 1,
    // then both head and tail must be zero.

    // EntailsZeroHeadAndTail(e1, e2) => e1 = 0 and e2 = 0
    struct EntailsZeroHeadAndTail(Expr, Expr);
    EntailsZeroHeadAndTail(head, tail) <-
      EqualZero(e),
      ExpressionSumHeadTail(e, head, tail),
      RangeConstraintOnExpression(head, rc_head),
      RangeConstraintOnExpression(tail, rc_tail),
      (rc_head.range().0 == T::from(0)),
      (rc_tail.range().0 == T::from(0)),
      (rc_head.range().1.to_integer() + rc_tail.range().1.to_integer() < T::from(-1).to_integer());

    EqualZero(head) <- EntailsZeroHeadAndTail(head,_);
    EqualZero(tail) <- EntailsZeroHeadAndTail(_, tail);

    ///////////////////////////////// OUTPUT ACTIONS //////////////////////////

    struct Equivalence(Var, Var);

    //------- quadratic equivalence -----

    // QuadraticEquivalenceCandidate(E, expr, offset) =>
    //   E = (expr * (expr + offset) = 0) is a constraint and
    //   expr is affine with at least 2 variables.
    struct QuadraticEquivalenceCandidate<T: FieldElement>(Expr, Expr, T);
    QuadraticEquivalenceCandidate(e, r, o / f) <-
       Env(env),
       ProductConstraint(e, l, r),
       AffinelyRelated(l, f, r, o), // l = f * r + o
       IsAffine(l),
       ({env.affine_var_count(l).unwrap_or(0) > 1});

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
      let Some((v1, v2, factor)) = env.differ_in_exactly_one_variable(expr1, expr2),
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

    @output
    pub struct ActionRule<T: FieldElement>(pub Action<T>);
    ActionRule(Action::UpdateRangeConstraintOnVar(v, rc)) <-
      UpdateRangeConstraintOnVar(v, rc);
    ActionRule(Action::SubstituteVariableByConstant(v, val)) <-
      Assignment(v, val);
    // Substitute the larger variable by the smaller.
    ActionRule(Action::SubstituteVariableByVariable(v1, v2)) <-
      Equivalence(v1, v2), (v1 > v2);
    ActionRule(Action::SubstituteVariableByVariable(v2, v1)) <-
      Equivalence(v1, v2), (v2 > v1);
    ActionRule(Action::ReplaceAlgebraicConstraintBy(e1, e2)) <-
      ReplaceAlgebraicConstraintBy(e1, e2);

    @output
    pub struct ReplaceConstraintsActionRule(pub ReplaceConstraintsAction);
    ReplaceConstraintsActionRule(ReplaceConstraintsAction{ to_replace, replace_by }) <-
      ReplaceAlgebraicConstraintsBy(to_replace, replace_by);
}

fn extend_by_none<const N1: usize, const N2: usize>(items: [Expr; N1]) -> [Option<Expr>; N2] {
    let mut output = [None; N2];
    for (i, item) in items.iter().enumerate() {
        output[i] = Some(*item);
    }
    output
}
