use std::fmt::Display;

use derive_more::{From, Into};
use powdr_number::FieldElement;

use crate::range_constraint::RangeConstraint;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, From, Into)]
pub struct Var(usize);

impl Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "v_{}", self.0)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, From, Into)]
pub struct Expr(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Action<T: FieldElement> {
    UpdateRangeConstraintOnVar(Var, RangeConstraint<T>),
    SubstituteVariableByConstant(Var, T),
    /// Substitute the first variable by the second.
    SubstituteVariableByVariable(Var, Var),
    #[allow(dead_code)]
    /// Replace one algebraic constraint by another.
    ReplaceAlgebraicConstraintBy(Expr, Expr),
    #[allow(dead_code)]
    /// Replace a pair of algebraic constraints (the first two) by
    /// another (the third).
    ReplacePairOfAlgebraicConstraintsBy(Expr, Expr, Expr),
    /// Replace a triple of algebraic constraints (the first three) by
    /// another (the fourth).
    ReplaceTripleOfAlgebraicConstraintsBy(Expr, Expr, Expr, Expr),
    /// Replace four algebraic constraints (the first four) by
    /// another (the fifth).
    ReplaceFourOfAlgebraicConstraintsBy(Expr, Expr, Expr, Expr, Expr),
    /// Replace eight algebraic constraints (the first eight) by
    /// another (the ninth).
    ReplaceEightOfAlgebraicConstraintsBy(Expr, Expr, Expr, Expr, Expr, Expr, Expr, Expr, Expr),
}
