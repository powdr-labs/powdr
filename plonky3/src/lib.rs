// #![no_std]

extern crate alloc;

mod folder;
mod proof;
mod prover;
mod symbolic_builder;
mod traits;
mod verifier;

use folder::*;
use proof::*;
use prover::*;
use traits::*;
use verifier::*;

#[cfg(debug_assertions)]
mod check_constraints;

mod circuit_builder;
mod params;
mod stark;
pub use params::{baby_bear, Commitment, FieldElementMap, ProverData};
pub use stark::Plonky3Prover;
