use itertools::Itertools;
use powdr_ast::analyzed::Analyzed;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

use std::sync::Arc;

use powdr_number::{DegreeType, FieldElement};

/// A callback that computes an updated witness, given:
/// - The PIL for the current machine.
/// - The current witness.
/// - The challenges sampled so far.
pub type WitgenCallbackFn<T> = Arc<
    dyn Fn(&Analyzed<T>, &[(String, Vec<T>)], BTreeMap<u64, T>, u8) -> Vec<(String, Vec<T>)>
        + Send
        + Sync,
>;

#[derive(Clone)]
pub struct WitgenCallback<T>(WitgenCallbackFn<T>);

impl<T: FieldElement> WitgenCallback<T> {
    pub fn new(f: WitgenCallbackFn<T>) -> Self {
        WitgenCallback(f)
    }

    /// Computes the next-stage witness, given the current witness and challenges.
    pub fn next_stage_witness(
        &self,
        pil: &Analyzed<T>,
        current_witness: &[(String, Vec<T>)],
        challenges: BTreeMap<u64, T>,
        stage: u8,
    ) -> Vec<(String, Vec<T>)> {
        (self.0)(pil, current_witness, challenges, stage)
    }
}

#[derive(Serialize, Deserialize)]
pub struct VariablySizedColumn<F> {
    column_by_size: BTreeMap<DegreeType, Vec<F>>,
    /// If this is Some(x), then all sizes of this column have this value
    /// in all rows except the first and the last.
    constant_inner_value: Option<F>,
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
    pub fn available_sizes(&self) -> BTreeSet<DegreeType> {
        self.column_by_size.keys().cloned().collect()
    }

    /// Clones and returns the column with the given size.
    pub fn get_by_size(&self, size: DegreeType) -> Option<&[F]> {
        self.column_by_size
            .get(&size)
            .map(|column| column.as_slice())
    }

    /// If this returns Some(x), then all sizes of this column have this value
    /// in all rows except the first and the last.
    pub fn has_constant_inner_value(&self) -> &Option<F> {
        &self.constant_inner_value
    }
}

impl<F: PartialEq + Copy> From<Vec<F>> for VariablySizedColumn<F> {
    fn from(column: Vec<F>) -> Self {
        let constant_inner_value = constant_inner_value(&column);
        VariablySizedColumn {
            column_by_size: [(column.len() as DegreeType, column)].into_iter().collect(),
            constant_inner_value,
        }
    }
}

impl<F: PartialEq + Copy> From<Vec<Vec<F>>> for VariablySizedColumn<F> {
    fn from(columns: Vec<Vec<F>>) -> Self {
        let constant_inner_values = columns
            .iter()
            .map(|v| constant_inner_value(v))
            .collect::<Option<Vec<_>>>();
        let constant_inner_value =
            constant_inner_values.and_then(|v| v.iter().all_equal_value().ok().cloned());
        VariablySizedColumn {
            column_by_size: columns
                .into_iter()
                .map(|column| (column.len() as DegreeType, column))
                .collect(),
            constant_inner_value,
        }
    }
}

fn constant_inner_value<F: PartialEq + Copy>(column: &[F]) -> Option<F> {
    column[1..column.len() - 1]
        .iter()
        .all_equal_value()
        .ok()
        .cloned()
}
