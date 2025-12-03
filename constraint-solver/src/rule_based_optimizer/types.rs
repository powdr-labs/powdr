use std::fmt::Display;

use derive_more::{From, Into};

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
pub enum Action<T> {
    SubstituteVariableByConstant(Var, T),
    SubstituteVariableByVariable(Var, Var),
    #[allow(dead_code)]
    ReplaceAlgebraicConstraintBy(Expr, Expr),
    #[allow(dead_code)]
    ReplacePairOfAlgebraicConstraintsBy(Expr, Expr, Expr),
}
