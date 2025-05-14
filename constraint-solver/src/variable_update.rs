use powdr_number::FieldElement;

use super::range_constraint::RangeConstraint;

/// An update representing new information about a variable.
#[derive(Debug, Clone)]
pub struct VariableUpdate<T: FieldElement, V, R> {
    pub variable: V,
    pub update: UpdateKind<T, R>,
}

#[derive(Debug, Clone)]
pub enum UpdateKind<T: FieldElement, R> {
    /// We have updated range constraints for the variable.
    RangeConstraintUpdate(RangeConstraint<T>),
    /// The variable is to be replaced by a different expression.
    Replace(R),
}
