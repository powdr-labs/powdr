use lazy_static::lazy_static;
use p3_field::AbstractField;
use p3_goldilocks::Goldilocks;
use p3_symmetric::CryptographicPermutation;
use powdr_riscv_runtime::{
    goldilocks::Goldilocks as PowdrGoldilocks,
    hash::{poseidon2_gl, poseidon2_gl_inplace},
};

#[derive(Clone, Copy, Debug)]
pub struct Permutation;

impl p3_symmetric::Permutation<[Goldilocks; 8]> for Permutation {
    // Both Goldilocks and PowdrGoldilocks are repr(transparent), and both use
    // canonical representation internally, so it is safe to cast between their
    // array's references.
    //
    // TODO: We are relying on implementation detail. So, ideally, we should
    // static assert that std::mem::transmute(Goldilocks::one()) == 1u64.

    fn permute(&self, input: [Goldilocks; 8]) -> [Goldilocks; 8] {
        let input = unsafe { &*(&input as *const _ as *const [PowdrGoldilocks; 8]) };
        let output = poseidon2_gl(input);
        // Let's hope the compiler optimizes this into a no-op.
        output.map(|x| Goldilocks::from_canonical_u64(u64::from(x)))
    }

    fn permute_mut(&self, data: &mut [Goldilocks; 8]) {
        let data = unsafe { &mut *(data as *mut _ as *mut [PowdrGoldilocks; 8]) };
        poseidon2_gl_inplace(data);
    }
}
impl CryptographicPermutation<[Goldilocks; 8]> for Permutation {}

lazy_static! {
    pub static ref PERM: Permutation = Permutation;
}
