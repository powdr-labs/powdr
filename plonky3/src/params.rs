// use lazy_static::lazy_static;

use p3_challenger::DuplexChallenger;
use p3_commit::ExtensionMmcs;
use p3_dft::Radix2DitParallel;
use p3_field::{extension::BinomialExtensionField, AbstractField, PrimeField};
use p3_fri::{FriConfig, TwoAdicFriPcs};
use p3_symmetric::{compression::TruncatedPermutation, CryptographicPermutation, Permutation};
use p3_uni_stark::StarkConfig;

type Dft = Radix2DitParallel;

pub trait FieldElementMap: Clone + Send + Sync {
    type P3Field: PrimeField;
    type MdsMatrix;
    type Perm: Permutation<[AbstractField; WIDTH]>
        + CryptographicPermutation<[AbstractField; WIDTH]>;

    type Hash = PaddingFreeSponge<Perm, WIDTH, RATE, OUT>;
    type Compress = TruncatedPermutation<Perm, N, CHUNK, WIDTH>;
    type Challenge = BinomialExtensionField<P3Field, DEGREE>;
    type Challenger = DuplexChallenger<P3Field, Perm>;
    type ChallengeMmcs = ExtensionMmcs<P3Field, Challenge, ValMmcs>;
    type MyPcs = TwoAdicFriPcs<P3Field, Dft, ValMmcs, ChallengeMmcs>;
    type Config = StarkConfig<MyPcs, Challenge, Challenger>;

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

    pub(crate) fn to_p3_field(&self) -> Self::P3Field;

    pub(crate) fn get_challenger() -> Challenger;

    pub(crate) fn get_config() -> Config;
}
