use powdr_number::FieldElement;

use crate::witgen::range_constraints::RangeConstraint;

/// An update representing new information about a variable.
#[derive(Debug, Clone)]
pub struct VariableUpdate<T: FieldElement, V> {
    pub variable: V,
    /// If true, the variable is symbolically or concretely known.
    pub known: bool,
    /// The current range constraint of the variable. It can be a single number.
    pub range_constraint: RangeConstraint<T>,
}
