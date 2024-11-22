pub use powdr_executor_utils::{HasMultipleSizesError, VariablySizedColumn};

/// Returns all columns with their unique sizes. Fails if any column has multiple sizes.
pub fn get_uniquely_sized<F>(
    column: &[(String, VariablySizedColumn<F>)],
) -> Result<Vec<(String, &Vec<F>)>, HasMultipleSizesError> {
    column
        .iter()
        .map(|(name, column)| Ok((name.clone(), column.get_uniquely_sized()?)))
        .collect()
}

pub fn get_uniquely_sized_cloned<F: Clone>(
    column: &[(String, VariablySizedColumn<F>)],
) -> Result<Vec<(String, Vec<F>)>, HasMultipleSizesError> {
    get_uniquely_sized(column).map(|column| {
        column
            .into_iter()
            .map(|(name, column)| (name, column.clone()))
            .collect()
    })
}
