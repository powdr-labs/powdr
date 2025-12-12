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
    // --------------- beginning of "minimal" set, keep this identical to minimal.rs -----------------------------

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

    // --------------- end of "minimal" set -----------------------------

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
}
