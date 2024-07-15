use std::{collections::HashMap, iter::once, sync::Arc};

use itertools::Itertools;

use serde::{Deserialize, Serialize};

use crate::Columns;

/// A collection of column values for different sizes. This is acceptable to clone.
#[derive(Clone, Serialize, Deserialize)]
pub struct VariablySizedColumns<T> {
    inner: HashMap<u64, Arc<Columns<T>>>,
}

#[derive(Debug)]
pub struct NotUniquelySizedError;

impl<T> VariablySizedColumns<T> {
    pub fn new(value: impl IntoIterator<Item = (u64, Vec<(String, Vec<T>)>)>) -> Self {
        Self {
            inner: value
                .into_iter()
                .map(|(size, values)| (size, Arc::new(values)))
                .collect(),
        }
    }

    pub fn to_uniquely_sized(&self) -> Result<Arc<Columns<T>>, NotUniquelySizedError> {
        self.inner
            .iter()
            .exactly_one()
            .map(|v| v.1.clone())
            .map_err(|_| NotUniquelySizedError)
    }

    pub fn width(&self) -> usize {
        self.inner
            .values()
            .map(|v| v.len())
            .unique()
            .exactly_one()
            .map_err(|_| ())
            .unwrap()
    }
}

impl<T> From<Vec<(String, Vec<T>)>> for VariablySizedColumns<T> {
    fn from(value: Vec<(String, Vec<T>)>) -> Self {
        let len = value
            .iter()
            .map(|(_, v)| v.len())
            .unique()
            .exactly_one()
            .map_err(|_| ())
            .unwrap() as u64;
        Self::new(once((len, value)))
    }
}
