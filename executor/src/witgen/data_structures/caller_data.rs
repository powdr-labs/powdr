use itertools::Itertools;
use powdr_number::FieldElement;

use crate::witgen::{
    global_constraints::RangeConstraintSet, machines::LookupCell, processor::OuterQuery,
    AffineExpression, AlgebraicVariable, EvalError, EvalResult, EvalValue,
};

/// A representation of the caller's data.
///
/// Useful for implementing [Machine::process_plookup] in terms of [Machine::process_lookup_direct].
pub struct CallerData<'a, 'b, T> {
    /// The raw data of the caller. Unknown values should be ignored.
    data: Vec<T>,
    /// The affine expressions of the caller.
    arguments: &'b [AffineExpression<AlgebraicVariable<'a>, T>],
    /// Range constraints coming from the caller.
    range_constraints: &'b dyn RangeConstraintSet<AlgebraicVariable<'a>, T>,
}

impl<'a, 'b, T: FieldElement> From<&'b OuterQuery<'a, '_, T>> for CallerData<'a, 'b, T> {
    /// Builds a `CallerData` from an `OuterQuery`.
    fn from(outer_query: &'b OuterQuery<'a, '_, T>) -> Self {
        let data = outer_query
            .arguments
            .iter()
            .map(|l| l.constant_value().unwrap_or_default())
            .collect();
        Self {
            data,
            arguments: &outer_query.arguments,
            range_constraints: outer_query.range_constraints,
        }
    }
}

impl<'a, 'b, T: FieldElement> CallerData<'a, 'b, T> {
    pub fn new(
        arguments: &'b [AffineExpression<AlgebraicVariable<'a>, T>],
        range_constraints: &'b dyn RangeConstraintSet<AlgebraicVariable<'a>, T>,
    ) -> Self {
        let data = arguments
            .iter()
            .map(|l| l.constant_value().unwrap_or_default())
            .collect();
        Self {
            data,
            arguments,
            range_constraints,
        }
    }
}

impl<T: FieldElement> CallerData<'_, '_, T> {
    /// Returns the data as a list of `LookupCell`s, as expected by `Machine::process_lookup_direct`.
    pub fn as_lookup_cells(&mut self) -> Vec<LookupCell<'_, T>> {
        self.data
            .iter_mut()
            .zip_eq(self.arguments.iter())
            .map(|(value, left)| match left.constant_value().is_some() {
                true => LookupCell::Input(value),
                false => LookupCell::Output(value),
            })
            .collect()
    }
}

impl<'a, 'b, T: FieldElement> From<CallerData<'a, 'b, T>> for EvalResult<'a, T> {
    /// Turns the result of a direct lookup into an `EvalResult`, as used by `Machine::process_plookup`.
    ///
    /// Note that this function assumes that the lookup was successful and complete.
    fn from(data: CallerData<'a, 'b, T>) -> EvalResult<'a, T> {
        let mut result = EvalValue::complete(vec![]);
        for (l, v) in data.arguments.iter().zip_eq(data.data.iter()) {
            if !l.is_constant() {
                let evaluated = l.clone() - (*v).into();
                match evaluated.solve_with_range_constraints(data.range_constraints) {
                    Ok(constraints) => {
                        result.combine(constraints);
                    }
                    Err(_) => {
                        // Fail the whole lookup
                        return Err(EvalError::ConstraintUnsatisfiable(format!(
                            "Constraint is invalid ({l} != {v}).",
                        )));
                    }
                }
            }
        }
        Ok(result)
    }
}
