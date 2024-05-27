use p3_challenger::DuplexChallenger;
use p3_commit::ExtensionMmcs;
use p3_dft::Radix2DitParallel;
use p3_field::{extension::BinomialExtensionField, Field};
use p3_fri::{FriConfig, TwoAdicFriPcs};
use p3_goldilocks::MdsMatrixGoldilocks;
use p3_merkle_tree::FieldMerkleTreeMmcs;
use p3_poseidon::Poseidon;
use p3_symmetric::{PaddingFreeSponge, TruncatedPermutation};
use p3_uni_stark::StarkConfig;
use p3_util::log2_ceil_usize;

use crate::prover::Val;

const D: usize = 2;
type Challenge = BinomialExtensionField<Val, D>;
pub const WIDTH: usize = 8;
const ALPHA: u64 = 7;
type Perm = Poseidon<Val, MdsMatrixGoldilocks, WIDTH, ALPHA>;

const RATE: usize = 4;
const OUT: usize = 4;
type MyHash = PaddingFreeSponge<Perm, WIDTH, RATE, OUT>;

const N: usize = 2;
const CHUNK: usize = 4;
type MyCompress = TruncatedPermutation<Perm, N, CHUNK, WIDTH>;

const DIGEST_ELEMS: usize = 4;
type ValMmcs = FieldMerkleTreeMmcs<
    <Val as Field>::Packing,
    <Val as Field>::Packing,
    MyHash,
    MyCompress,
    DIGEST_ELEMS,
>;
pub type Challenger = DuplexChallenger<Val, Perm, WIDTH>;
type ChallengeMmcs = ExtensionMmcs<Val, Challenge, ValMmcs>;
type Dft = Radix2DitParallel;
type Pcs = TwoAdicFriPcs<Val, Dft, ValMmcs, ChallengeMmcs>;
pub type MyConfig = StarkConfig<Pcs, Challenge, Challenger>;

pub const HALF_NUM_FULL_ROUNDS: usize = 4;
pub const NUM_PARTIAL_ROUNDS: usize = 22;

#[derive(Clone, Debug)]
pub struct Constants {
    pub values: Vec<Val>,
}

impl Constants {
    pub fn new(values: Vec<Val>) -> Self {
        Self { values }
    }

    pub fn to_config_and_perm(
        &self,
        degree: u64,
    ) -> (StarkConfig<Pcs, Challenge, Challenger>, Perm) {
        let perm = Perm::new(
            HALF_NUM_FULL_ROUNDS,
            NUM_PARTIAL_ROUNDS,
            self.values.clone(),
            MdsMatrixGoldilocks,
        );

        let hash = MyHash::new(perm.clone());

        let compress = MyCompress::new(perm.clone());

        let val_mmcs = ValMmcs::new(hash, compress);

        let challenge_mmcs = ChallengeMmcs::new(val_mmcs.clone());

        let dft = Dft {};

        let fri_config = FriConfig {
            log_blowup: 1,
            num_queries: 100,
            proof_of_work_bits: 16,
            mmcs: challenge_mmcs,
        };

        let pcs = Pcs::new(log2_ceil_usize(degree as usize), dft, val_mmcs, fri_config);

        let config = MyConfig::new(pcs);

        (config, perm)
    }
}
