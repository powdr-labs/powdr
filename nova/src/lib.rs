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
pub(crate) const FREE_INPUT_INSTR_NAME: &str = "free_input_instr";
pub(crate) const FREE_INPUT_TY: &str = "free_input_ty";
pub(crate) const FREE_INPUT_DUMMY_REG: &str = "free_input_dummy_reg";
