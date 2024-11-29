use p3_field::{AbstractField, PrimeField64};
use p3_symmetric::Permutation;

pub fn poseidon2_gl(input: &[u64; 8]) -> [u64; 8] {
    let mut state = input.map(p3_goldilocks::Goldilocks::from_canonical_u64);
    powdr_plonky3::poseidon2::goldilocks::PERM.permute_mut(&mut state);
    state.map(|v| v.as_canonical_u64())
}
