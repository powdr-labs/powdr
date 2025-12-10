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
    @include!("minimal_rules.crepe");

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
