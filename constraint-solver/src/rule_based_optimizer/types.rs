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
    /// Replace one algebraic constraint by another.
    ReplaceAlgebraicConstraintBy(Expr, Expr),
}

/// Replace a list of algebraic constraints by another list of
/// algebraic constraints. We use an array of Option instead of
/// a Vec because this type needs to be `Copy`.
/// This is a separate type from `Action` because it is much larger.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReplaceConstraintsAction {
    pub to_replace: [Option<Expr>; 10],
    pub replace_by: [Option<Expr>; 10],
}
