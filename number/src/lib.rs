//! Numerical types used across powdr

#[macro_use]
mod macros;
mod bn254;
mod goldilocks;
mod traits;

pub use bn254::Bn254Field;
pub use goldilocks::GoldilocksField;

use num_bigint::BigUint;
pub use traits::{BigInt, FieldElement};
/// An arbitrary precision big integer, to be used as a last recourse
pub type AbstractNumberType = BigUint;

/// The type of polynomial degrees and indices into columns.
pub type DegreeType = u64;
