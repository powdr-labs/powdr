#![allow(clippy::iter_over_hash_type)]
// This is about a warning about interior mutability for the key
// `Env`. We need it and it is probably fine.
#![allow(clippy::mutable_key_type)]

use crepe::crepe;
use itertools::Itertools;
use num_traits::One;
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

    // DifferBySummand(e1, e2, s) => e1 = e2 + s and `s` is not a sum
    // and not a constant.
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

    // BooleanVar(v) => v is 0 or 1
    struct BooleanVar(Var);
    BooleanVar(v) <- RangeConstraintOnVar(v, RangeConstraint::from_mask(1));

    // BooleanExpressionConstraint(constr, e) => if constr is satisfied then e = 1 or e = 0
    struct BooleanExpressionConstraint(Expr, Expr);
    BooleanExpressionConstraint(constr, r) <-
      ProductConstraint(constr, l, r),
      // l = f * r + c, i.e. constr = (f * r + c) * r = 0
      // <=> (r + c / f) * r = 0
      // i.e. c / f = -1 <=> c = -f
      AffinelyRelated(l, f, r, c),
      (c == -f);

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

    //////////////////// EQUAL ZERO TEST ////////////////////////

    // PlusMinusResult(e, e1, v2) =>
    //   e = e1 * (2 * v2 - 1)
    struct PlusMinusResult(Expr, Expr, Var);
    PlusMinusResult(e, e1, v2) <-
      Product(e, e1, r),
      AffineExpression(r, coeff, v2, offset),
        (coeff == T::from(2)),
        (offset == T::from(-1));

    // DiffMarkerConstraint(e, diff_marker, e2, cmp_result, diff_val) =>
    //   e = diff_marker * (e2 * (2 * cmp_result - 1) + diff_val)
    // (up to a factor)
    struct DiffMarkerConstraint(Expr, Var, Expr, Var, Var);
    DiffMarkerConstraint(e, diff_marker, e2, cmp_result, diff_val) <-
      ProductConstraint(e, l, r),
      LinearExpression(l, diff_marker, _),
      // Note: the quadratic part has to be the head
      ExpressionSumHeadTail(r, r1, r2),
        PlusMinusResult(r1, e2, cmp_result),
        LinearExpression(r2, diff_val, _);

    // NegatedDiffMarkerConstraint(e, diff_marker, diff_expr, v, result, n) =>
    //   e is the constraint diff_marker_expr * (v * (2 * result - 1)) = 0
    //   and diff_marker_expr is of the form `1 - diff_marker1 - diff_marker2 - ...`
    //   such that we have n variables and there is another
    //   NegatedDiffMarkerConstraint with n-1 variables used to derive this one.
    struct NegatedDiffMarkerConstraint(Expr, Var, Expr, Var, Var, u32);
    NegatedDiffMarkerConstraint(e, diff_marker, l, v, result, 0) <-
      ProductConstraint(e, l, r),
      AffineExpression(l, T::from(-1), diff_marker, T::from(1)),
      PlusMinusResult(r, r2, result),
      LinearExpression(r2, v, T::from(-1));
    NegatedDiffMarkerConstraint(e, diff_marker, l, v, result, n + 1) <-
      ProductConstraint(e, l, r),
        NegatedDiffMarkerConstraint(_, _, diff_marker_expr2, _, result, n),
        DifferBySummand(l, diff_marker_expr2, diff_marker_e),
          LinearExpression(diff_marker_e, diff_marker, T::from(-1)),
      PlusMinusResult(r, r2, result),
      LinearExpression(r2, v, T::from(-1));

    // NegatedDiffMarkerConstraintFinal(e, diff_marker, l, result, n) =>
    //   e is the constraint diff_marker_expr * (result) = 0
    //   and diff_marker_expr is of the form `1 - diff_marker1 - diff_marker2 - ...`
    //   such that we have n variables and there is another
    //   NegatedDiffMarkerConstraint with n-1 variables used to derive this one.
    struct NegatedDiffMarkerConstraintFinal(Expr, Var, Expr, Var, u32);
    NegatedDiffMarkerConstraintFinal(e, diff_marker, l, result, n + 1) <-
      ProductConstraint(e, l, r),
        NegatedDiffMarkerConstraint(_, _, diff_marker_expr2, _, result, n),
        DifferBySummand(l, diff_marker_expr2, diff_marker_e),
          LinearExpression(diff_marker_e, diff_marker, T::from(-1)),
      LinearExpression(r, result, T::from(1));

    struct NegatedDiffMarkerConstraintFinalNegated(Expr, Var, Var, Var, u32);
    NegatedDiffMarkerConstraintFinalNegated(e, diff_marker, v, result, n + 1) <-
      ProductConstraint(e, l, r),
        NegatedDiffMarkerConstraint(_, _, diff_marker_expr2, _, result, n),
        DifferBySummand(l, diff_marker_expr2, diff_marker_e),
          LinearExpression(diff_marker_e, diff_marker, T::from(-1)),
      PlusMinusResult(r, r2, result),
      AffineExpression(r2, T::from(-1), v, T::from(1));

    // EqualZeroCheck(constrs, result, vars) =>
    //   constrsexprs can be equivalently replaced by a constraint that models
    //   result = 1 if all vars are zero, and result = 0 otherwise.
    struct EqualZeroCheck([Expr; 10], Var, [Var; 4]);
    EqualZeroCheck(constrs, result, vars) <-
      // (1 - diff_marker__3_0) * (a__3_0 * (2 * cmp_result_0 - 1)) = 0
      NegatedDiffMarkerConstraint(constr_0, diff_marker_3, _, a_3, result, 0),
      // (1 - (diff_marker__2_0 + diff_marker__3_0)) * (a__2_0 * (2 * cmp_result_0 - 1)) = 0
      NegatedDiffMarkerConstraint(constr_1, diff_marker_2, _, a_2, result, 1),
      // (1 - (diff_marker__1_0 + diff_marker__2_0 + diff_marker__3_0)) * (a__1_0 * (2 * cmp_result_0 - 1)) = 0
      NegatedDiffMarkerConstraint(constr_2, diff_marker_1, _, a_1, result, 2),
      // (1 - (diff_marker__0_0 + diff_marker__1_0 + diff_marker__2_0 + diff_marker__3_0)) * cmp_result_0 = 0
      NegatedDiffMarkerConstraintFinal(constr_3, diff_marker_0, one_minus_diff_marker_sum, result, 3),
      // (1 - (diff_marker__0_0 + diff_marker__1_0 + diff_marker__2_0 + diff_marker__3_0)) * ((1 - a__0_0) * (2 * cmp_result_0 - 1)) = 0
      NegatedDiffMarkerConstraintFinalNegated(constr_4, diff_marker_0, a_0, result, 3),
      // diff_marker__0_0 * ((a__0_0 - 1) * (2 * cmp_result_0 - 1) + diff_val_0) = 0
      DiffMarkerConstraint(constr_5, diff_marker_0, a_0_e, result, diff_val),
        AffineExpression(a_0_e, a_0_e_coeff, a_0, a_0_e_offset), (a_0_e_coeff == T::from(1)), (a_0_e_offset == T::from(-1)),
      // diff_marker__1_0 * (a__1_0 * (2 * cmp_result_0 - 1) + diff_val_0) = 0
      DiffMarkerConstraint(constr_6, diff_marker_1, a_1_e, result, diff_val),
        LinearExpression(a_1_e, a_1, T::from(1)),
      // diff_marker__2_0 * (a__2_0 * (2 * cmp_result_0 - 1) + diff_val_0) = 0
      DiffMarkerConstraint(constr_7, diff_marker_2, a_2_e, result, diff_val),
        LinearExpression(a_2_e, a_2, T::from(1)),
      // diff_marker__3_0 * (a__3_0 * (2 * cmp_result_0 - 1) + diff_val_0) = 0
      DiffMarkerConstraint(constr_8, diff_marker_3, a_3_e, result, diff_val),
        LinearExpression(a_3_e, a_3, T::from(1)),
      BooleanVar(result),
      BooleanVar(diff_marker_0),
      BooleanVar(diff_marker_1),
      BooleanVar(diff_marker_2),
      BooleanVar(diff_marker_3),
      RangeConstraintOnVar(a_0, rc_a0),
      RangeConstraintOnVar(a_1, rc_a1),
      RangeConstraintOnVar(a_2, rc_a2),
      RangeConstraintOnVar(a_3, rc_a3),
      // The next is needed so that the constraint `result + sum_inv_var * sum_of_vars - 1 = 0`
      // works. If there is a way to get the sum to be zero but not all variables are zero,
      // then this constraint cannot be satisfied.
      ( rc_a0.range().0 == T::zero() && rc_a1.range().0 == T::zero()
        && rc_a2.range().0 == T::zero() && rc_a3.range().0 == T::zero()
        && rc_a0.combine_sum(&rc_a1).combine_sum(&rc_a2).combine_sum(&rc_a3).range().1 < T::from(-1)),
      // (diff_marker__0_0 + diff_marker__1_0 + diff_marker__2_0 + diff_marker__3_0) * (diff_marker__0_0 + diff_marker__1_0 + diff_marker__2_0 + diff_marker__3_0 - 1) = 0
      BooleanExpressionConstraint(constr_9, diff_marker_sum),
      AffinelyRelated(diff_marker_sum, T::from(-1), one_minus_diff_marker_sum, T::from(1)),
      let constrs = [constr_0, constr_1, constr_2, constr_3, constr_4, constr_5, constr_6, constr_7, constr_8, constr_9],
      let vars = [a_0, a_1, a_2, a_3];

    ReplaceAlgebraicConstraintsBy(extend_by_none(constrs), extend_by_none(replacement)) <-
      Env(env),
      EqualZeroCheck(constrs, result, vars),
      let replacement = {
        let result = GroupedExpression::from_unknown_variable(result);
        assert!(vars.len() == 4);
        let vars = vars.into_iter().map(|v| GroupedExpression::from_unknown_variable(v)).collect_vec();
        let sum_of_vars = vars.iter().cloned().sum::<GroupedExpression<_, _>>();
        let sum_inv_var = GroupedExpression::from_unknown_variable(
          env.new_var("inv_of_sum", ComputationMethod::QuotientOrZero(One::one(), sum_of_vars.clone()))
        );
        [
          env.insert_owned(result.clone() * sum_of_vars.clone()),
          env.insert_owned(result + sum_inv_var * sum_of_vars - One::one()),
        ]
      };

    //////////////// COMBINE CONSTRAINTS WITH NON-NEGATIVE FACTORS /////////////////////

    // If we have `x * a = 0` and `x * b = 0` and `a` and `b` are
    // both non-negative and their sum is constrained, then we can replace
    // both constraints by `x * (a + b) = 0`.
    ReplaceAlgebraicConstraintsBy(extend_by_none([e1, e2]), replacement) <-
      Env(env),
      ProductConstraint(e1, x, a),
      ProductConstraint(e2, x, b),
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
