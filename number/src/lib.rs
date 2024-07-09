//! Numerical types used across powdr

#![deny(clippy::print_stdout)]

#[macro_use]
mod macros;
mod bn254;
mod goldilocks;
mod serialize;
mod traits;

use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};
pub use serialize::{
    buffered_write_file, read_polys_csv_file, write_polys_csv_file, CsvRenderMode, ReadWrite,
};

pub use bn254::Bn254Field;
pub use goldilocks::GoldilocksField;
pub use traits::KnownField;

pub use ibig::{IBig as BigInt, UBig as BigUint};
pub use traits::{FieldElement, LargeInt};
/// An arbitrary precision big integer, to be used as a last recourse

/// The type of polynomial degrees and indices into columns.
pub type DegreeType = u64;

/// Returns Some(i) if n == 2**i and None otherwise.
pub fn log2_exact(n: BigUint) -> Option<usize> {
    n.trailing_zeros()
        .filter(|zeros| n == (BigUint::from(1u32) << zeros))
}

#[derive(Serialize, Deserialize)]
pub struct VariablySizedColumn<F> {
    column_by_size: BTreeMap<usize, Vec<F>>,
}

#[derive(Debug)]
pub struct HasMultipleSizesError;

impl<F> VariablySizedColumn<F> {
    /// Create a view where each column has a single size. Fails if any column has multiple sizes.
    pub fn get_only_size(&self) -> Result<&Vec<F>, HasMultipleSizesError> {
        if self.column_by_size.len() != 1 {
            return Err(HasMultipleSizesError);
        }
        Ok(self.column_by_size.values().next().unwrap())
    }
}

pub fn get_only_size<F>(
    column: &[(String, VariablySizedColumn<F>)],
) -> Result<Vec<(String, &Vec<F>)>, HasMultipleSizesError> {
    column
        .iter()
        .map(|(name, column)| Ok((name.clone(), column.get_only_size()?)))
        .collect()
}

pub fn get_only_size_cloned<F: Clone>(
    column: &[(String, VariablySizedColumn<F>)],
) -> Result<Vec<(String, Vec<F>)>, HasMultipleSizesError> {
    get_only_size(column).map(|column| {
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

#[cfg(test)]
mod test {
    use super::*;
    use test_log::test;

    #[test]
    fn log2_exact_function() {
        assert_eq!(log2_exact(0u32.into()), None);
        assert_eq!(log2_exact(1u32.into()), Some(0));
        assert_eq!(log2_exact(2u32.into()), Some(1));
        assert_eq!(log2_exact(4u32.into()), Some(2));
        assert_eq!(log2_exact(BigUint::from(1u32) << 300), Some(300));
        assert_eq!(log2_exact(17u32.into()), None);
    }
}
