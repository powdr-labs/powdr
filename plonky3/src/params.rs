use p3_baby_bear::BabyBearParameters;
use p3_field::{AbstractField, PrimeField};
use p3_fri::{FriConfig, TwoAdicFriPcs};
use p3_goldilocks::Goldilocks;
use p3_symmetric::{CryptographicPermutation, Permutation};
use p3_uni_stark::StarkConfig;
use powdr_number::FieldElement;

pub(crate) trait FieldElementMap: Clone + Send + Sync {
    type P3Field: PrimeField;
    type MdsMatrix;
    type PermObject: Clone;
    type Perm: Permutation<Self::PermObject> + CryptographicPermutation<Self::PermObject>;
    type Hash;
    type Compress;
    type Challenge;
    type Challenger;
    type Dft;
    type ValMmcs;
    type ChallengeMmcs;
    type MyPcs;
    type Config;

    const DEGREE: usize;
    const WIDTH: usize;
    const RATE: usize;
    const OUT: usize;
    const N: usize;
    const CHUNK: usize;
    const DIGEST_ELEMS: usize;

    const FRI_LOG_BLOWUP: usize = 1;
    const FRI_NUM_QUERIES: usize = 100;
    const FRI_PROOF_OF_WORK_BITS: usize = 16;

    fn to_p3_field<T: FieldElement>(elt: T) -> Self::P3Field;

    fn get_challenger() -> Self::Challenger;

    fn get_config() -> Self::Config;
}
