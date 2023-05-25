//! Numerical types used across powdr

#[macro_use]
mod macros;
mod bn254;
mod goldilocks;
mod serialize;
mod traits;

pub use serialize::{read_polys_file, write_polys_file};

pub use bn254::Bn254Field;
pub use goldilocks::GoldilocksField;

use num_bigint::BigUint;
pub use traits::{BigInt, FieldElement};
/// An arbitrary precision big integer, to be used as a last recourse
pub type AbstractNumberType = BigUint;

/// The type of polynomial degrees and indices into columns.
pub type DegreeType = u64;

/// Returns Some(i) if n == 2**i and None otherwise.
pub fn log2_exact(n: AbstractNumberType) -> Option<u64> {
    n.trailing_zeros().and_then(|zeros| {
        if n == AbstractNumberType::from(1u32) << zeros {
            Some(zeros)
        } else {
            None
        }
    })
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_log2_exact() {
        assert_eq!(log2_exact(0u32.into()), None);
        assert_eq!(log2_exact(1u32.into()), Some(0));
        assert_eq!(log2_exact(2u32.into()), Some(1));
        assert_eq!(log2_exact(4u32.into()), Some(2));
        assert_eq!(log2_exact(AbstractNumberType::from(1u32) << 300), Some(300));
        assert_eq!(log2_exact(17u32.into()), None);
    }
}
