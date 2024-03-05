//! Numerical types used across powdr

#![deny(clippy::print_stdout)]

#[macro_use]
mod macros;
mod bn254;
mod goldilocks;
mod serialize;
mod traits;

pub use serialize::{
    read_polys_csv_file, read_polys_file, write_polys_csv_file, write_polys_file, CsvRenderMode,
};

pub use bn254::Bn254Field;
pub use goldilocks::GoldilocksField;
pub use traits::KnownField;

pub use num_bigint::BigUint;
pub use traits::{FieldElement, LargeInt};
/// An arbitrary precision big integer, to be used as a last recourse

/// The type of polynomial degrees and indices into columns.
pub type DegreeType = u64;

/// Returns Some(i) if n == 2**i and None otherwise.
pub fn log2_exact(n: BigUint) -> Option<u64> {
    n.trailing_zeros()
        .filter(|zeros| n == BigUint::from(1u32) << zeros)
}

#[cfg(test)]
mod test {
    use super::*;
    use test_log::test;

    #[test]
    fn test_log2_exact() {
        assert_eq!(log2_exact(0u32.into()), None);
        assert_eq!(log2_exact(1u32.into()), Some(0));
        assert_eq!(log2_exact(2u32.into()), Some(1));
        assert_eq!(log2_exact(4u32.into()), Some(2));
        assert_eq!(log2_exact(BigUint::from(1u32) << 300), Some(300));
        assert_eq!(log2_exact(17u32.into()), None);
    }
}
