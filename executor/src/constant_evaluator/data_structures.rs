use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Serialize, Deserialize)]
pub struct VariablySizedColumn<F> {
    column_by_size: BTreeMap<usize, Vec<F>>,
}

#[derive(Debug)]
pub struct HasMultipleSizesError;

impl<F> VariablySizedColumn<F> {
    /// Create a view where each column has a single size. Fails if any column has multiple sizes.
    pub fn get_uniquely_sized(&self) -> Result<&Vec<F>, HasMultipleSizesError> {
        if self.column_by_size.len() != 1 {
            return Err(HasMultipleSizesError);
        }
        Ok(self.column_by_size.values().next().unwrap())
    }

    /// Returns the set of available sizes.
    pub fn available_sizes(&self) -> BTreeSet<usize> {
        self.column_by_size.keys().cloned().collect()
    }

    /// Clones and returns the column with the given size.
    pub fn get_by_size(&self, size: usize) -> Option<&[F]> {
        self.column_by_size
            .get(&size)
            .map(|column| column.as_slice())
    }
}

/// Returns all columns with their unique sizes. Fails if any column has multiple sizes.
pub fn get_uniquely_sized<F>(
    column: &[(String, VariablySizedColumn<F>)],
) -> Result<Vec<(String, &Vec<F>)>, HasMultipleSizesError> {
    column
        .iter()
        .map(|(name, column)| Ok((name.clone(), column.get_uniquely_sized()?)))
        .collect()
}

/// Returns all columns with their maximum sizes.
pub fn get_max_sized<F>(column: &[(String, VariablySizedColumn<F>)]) -> Vec<(String, &Vec<F>)> {
    column
        .iter()
        .map(|(name, column)| {
            let max_size = column.column_by_size.keys().max().unwrap();
            (name.clone(), &column.column_by_size[max_size])
        })
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

impl<F> From<Vec<F>> for VariablySizedColumn<F> {
    fn from(column: Vec<F>) -> Self {
        VariablySizedColumn {
            column_by_size: [(column.len(), column)].into_iter().collect(),
        }
    }
}

impl<F> From<Vec<Vec<F>>> for VariablySizedColumn<F> {
    fn from(columns: Vec<Vec<F>>) -> Self {
        VariablySizedColumn {
            column_by_size: columns
                .into_iter()
                .map(|column| (column.len(), column))
                .collect(),
        }
    }
}
