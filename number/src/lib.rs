//! Numerical types used across powdr

#[macro_use]
mod macros;
mod bn254;
mod goldilocks;
mod traits;

pub use goldilocks::GoldilocksField as FieldElement;
use num_bigint::BigUint;
pub use traits::FieldElementTrait;
/// The abstract type of numbers to be computed with.
pub type AbstractNumberType = BigUint;

/// The type of polynomial degrees and indices into columns.
pub type DegreeType = u64;
