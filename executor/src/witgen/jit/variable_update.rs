use powdr_number::FieldElement;

use crate::witgen::range_constraints::RangeConstraint;

pub struct VariableUpdate<T: FieldElement, V> {
    variable: V,
    /// If true, the variable is symbolically or concretely known.
    known: bool,
    /// The current range constraint of the variable. It can be a single number.
    range_constraint: RangeConstraint<T>,
}
