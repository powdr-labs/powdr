#![allow(clippy::iter_over_hash_type)]
// This is about a warning about interior mutability for the key
// `Env`. We need it and it is probably fine.
#![allow(clippy::mutable_key_type)]

use powdr_number::FieldElement;

use crate::{
    range_constraint::RangeConstraint,
    rule_based_optimizer::{
        environment::Environment,
        types::{Action, Expr},
    },
};

// The files in the `rules` directory contain the set of datalog rules executed on
// the constraint system.
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
//
// We split the rules into a "minimal" (run an gigantic systems) and a "complete" set
// (run on smaller systems). The minimal set is a sub-set of the complete set and
// they have the same inputs and outputs.

pub mod minimal;

pub mod complete;

const SHOW_PROFILE: bool = false;

pub fn run_minimal_rules<T: FieldElement>(
    env: Environment<T>,
    algebraic_constraints: impl IntoIterator<Item = Expr>,
    rcs_on_exprs: impl IntoIterator<Item = (Expr, RangeConstraint<T>)>,
) -> (Vec<Action<T>>, Environment<T>) {
    let mut rt = minimal::Crepe::default();
    rt.extend([minimal::Env(&env)]);
    rt.extend(
        algebraic_constraints
            .into_iter()
            .map(minimal::InitialAlgebraicConstraint),
    );
    rt.extend(
        rcs_on_exprs
            .into_iter()
            .map(|(v, rc)| minimal::InitialRangeConstraintOnExpression(v, rc)),
    );
    let actions = if SHOW_PROFILE {
        let ((actions,), profile) = rt.run_with_profiling();
        profile.report();
        actions
    } else {
        let (actions,) = rt.run();
        actions
    };
    (actions.into_iter().map(|a| a.0).collect(), env)
}

pub fn run_complete_rules<T: FieldElement>(
    env: Environment<T>,
    algebraic_constraints: impl IntoIterator<Item = Expr>,
    rcs_on_exprs: impl IntoIterator<Item = (Expr, RangeConstraint<T>)>,
) -> (Vec<Action<T>>, Environment<T>) {
    let mut rt = complete::Crepe::default();
    rt.extend([complete::Env(&env)]);
    rt.extend(
        algebraic_constraints
            .into_iter()
            .map(complete::InitialAlgebraicConstraint),
    );
    rt.extend(
        rcs_on_exprs
            .into_iter()
            .map(|(v, rc)| complete::InitialRangeConstraintOnExpression(v, rc)),
    );
    let actions = if SHOW_PROFILE {
        let ((actions,), profile) = rt.run_with_profiling();
        profile.report();
        actions
    } else {
        let (actions,) = rt.run();
        actions
    };
    (actions.into_iter().map(|a| a.0).collect(), env)
}
