use itertools::Itertools;
use powdr_number::FieldElement;

use crate::witgen::{
    machines::LookupCell,
    processor::{Arguments, OuterQuery},
    EvalError, EvalResult, EvalValue,
};

/// A representation of the caller's data.
///
/// Useful for implementing [Machine::process_plookup] in terms of [Machine::process_lookup_direct].
pub struct CallerData<'a, 'b, T> {
    /// The raw data of the caller. Unknown values should be ignored.
    data: Vec<T>,
    /// The affine expressions of the caller.
    arguments: &'b Arguments<'a, T>,
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
                match evaluated.solve() {
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
