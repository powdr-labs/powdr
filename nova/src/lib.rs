#![feature(box_patterns)]
#![allow(dead_code)]
pub(crate) mod circuit;
pub(crate) mod circuit_builder;
pub(crate) mod nonnative;
pub(crate) mod prover;
pub(crate) mod utils;

pub use prover::*;

/// LIMB WITHD as base, represent 2^16
pub(crate) const LIMB_WIDTH: usize = 16;
