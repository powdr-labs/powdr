mod software_impl;

pub use software_impl::*;

use lazy_static::lazy_static;
use p3_goldilocks::Goldilocks;
use p3_poseidon2::poseidon2_round_numbers_128;

// From: https://github.com/Plonky3/Plonky3/blob/64e79fe28c51ab35b509c68242256f253b61d612/poseidon2/benches/poseidon2.rs#L31
pub const D: u64 = 7;
pub const WIDTH: usize = 8;

lazy_static! {
    static ref ROUNDS: (usize, usize) = poseidon2_round_numbers_128::<Goldilocks>(WIDTH, D);
    pub static ref ROUNDS_F: usize = ROUNDS.0;
    pub static ref ROUNDS_P: usize = ROUNDS.1;
}
