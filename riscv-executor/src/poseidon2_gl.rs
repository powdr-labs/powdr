use p3_field::{AbstractField, PrimeField64};
use p3_goldilocks::Goldilocks;
use p3_symmetric::Permutation;
use powdr_number::{FieldElement, LargeInt};

pub fn poseidon2_gl<F: FieldElement>(input: &[F; 8]) -> [F; 8] {
    let mut state = input.map(|v| {
        let v: u64 = v.to_integer().try_into_u64().unwrap();
        Goldilocks::from_wrapped_u64(v)
    });
    powdr_plonky3::poseidon2::goldilocks::PERM.permute_mut(&mut state);
    state.map(|v| F::from(v.as_canonical_u64()))
}
