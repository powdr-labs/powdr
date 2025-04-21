use lazy_static::lazy_static;
use p3_field::AbstractField;
use p3_goldilocks::Goldilocks;
use p3_symmetric::CryptographicPermutation;
use powdr_riscv_runtime::{
    goldilocks::{extract_opaque_vec, Goldilocks as PowdrGoldilocks, OpaqueGoldilocks},
    hash::poseidon2_gl_inplace,
};

#[derive(Clone, Copy, Debug)]
pub struct Permutation;

impl p3_symmetric::Permutation<[Goldilocks; 8]> for Permutation {
    fn permute(&self, input: [Goldilocks; 8]) -> [Goldilocks; 8] {
        // TODO: as of this writing, I am temporarily introducing a performance regression to convert
        // p3_goldilocks::Goldilocks to and from OpaqueGoldilocks in software. To get an improvement,
        // we must patch the p3's Merkle Tree implementation to use OpaqueGoldilocks directly and do
        // the conversion only once, in the output.

        // Both Goldilocks and PowdrGoldilocks are repr(transparent), and both use
        // canonical representation internally, so it is safe to cast between their
        // array's pointers.
        let input = unsafe { &*(&input as *const _ as *const [PowdrGoldilocks; 8]) };
        let mut state = input.map(|x| OpaqueGoldilocks::from(x));
        poseidon2_gl_inplace(&mut state);

        extract_opaque_vec::<8>(&state).map(|x| Goldilocks::from_canonical_u64(x))
    }

    fn permute_mut(&self, data: &mut [Goldilocks; 8]) {
        let data = unsafe { &mut *(data as *mut _ as *mut [OpaqueGoldilocks; 8]) };
        poseidon2_gl_inplace(data);
    }
}
impl CryptographicPermutation<[Goldilocks; 8]> for Permutation {}

lazy_static! {
    pub static ref PERM: Permutation = Permutation;
}
