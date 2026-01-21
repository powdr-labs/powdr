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

use std::str::FromStr;
impl FromStr for Var {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // expects "v_<number>"
        let id = s
            .strip_prefix("v_")
            .and_then(|n| n.parse::<usize>().ok())
            .ok_or(())?;
        Ok(Var(id))
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
    ReplaceEightOfAlgebraicConstraintsBy(Expr, Expr, Expr, Expr, Expr, Expr, Expr, Expr, Expr),
    ReplaceFourOfAlgebraicConstraintsBy(Expr, Expr, Expr, Expr, Expr),
}
