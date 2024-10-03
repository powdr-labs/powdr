//! The concrete parameters used in the prover
//! Inspired from [this example](https://github.com/Plonky3/Plonky3/blob/6a1b0710fdf85136d0fdd645b92933615867740a/keccak-air/examples/prove_goldilocks_keccak.rs#L57)

use lazy_static::lazy_static;

use crate::params::{Challenger, FieldElementMap, Plonky3Field};
use p3_challenger::DuplexChallenger;
use p3_commit::ExtensionMmcs;
use p3_dft::Radix2DitParallel;
use p3_field::{extension::BinomialExtensionField, AbstractField, Field, PrimeField64};
use p3_fri::{FriConfig, TwoAdicFriPcs};
use p3_goldilocks::{Goldilocks, MdsMatrixGoldilocks};
use p3_merkle_tree::MerkleTreeMmcs;
use p3_poseidon::Poseidon;
use p3_symmetric::{PaddingFreeSponge, TruncatedPermutation};
use p3_uni_stark::StarkConfig;
use powdr_number::{FieldElement, GoldilocksField, LargeInt};
use rand::{distributions::Standard, Rng, SeedableRng};

const DEGREE: usize = 2;
type FriChallenge = BinomialExtensionField<Goldilocks, DEGREE>;
const WIDTH: usize = 8;
const ALPHA: u64 = 7;
type Perm = Poseidon<Goldilocks, MdsMatrixGoldilocks, WIDTH, ALPHA>;

const RATE: usize = 4;
const OUT: usize = 4;
type Hash = PaddingFreeSponge<Perm, WIDTH, RATE, OUT>;

const N: usize = 2;
const CHUNK: usize = 4;
type Compress = TruncatedPermutation<Perm, N, CHUNK, WIDTH>;

const DIGEST_ELEMS: usize = 4;
type ValMmcs = MerkleTreeMmcs<
    <Goldilocks as Field>::Packing,
    <Goldilocks as Field>::Packing,
    Hash,
    Compress,
    DIGEST_ELEMS,
>;

pub type FriChallenger = DuplexChallenger<Goldilocks, Perm, WIDTH, RATE>;
type ChallengeMmcs = ExtensionMmcs<Goldilocks, FriChallenge, ValMmcs>;
type Dft = Radix2DitParallel;
type MyPcs = TwoAdicFriPcs<Goldilocks, Dft, ValMmcs, ChallengeMmcs>;

const HALF_NUM_FULL_ROUNDS: usize = 4;
const NUM_PARTIAL_ROUNDS: usize = 22;

const FRI_LOG_BLOWUP: usize = 1;
const FRI_NUM_QUERIES: usize = 100;
const FRI_PROOF_OF_WORK_BITS: usize = 16;

const NUM_ROUNDS: usize = 2 * HALF_NUM_FULL_ROUNDS + NUM_PARTIAL_ROUNDS;
const NUM_CONSTANTS: usize = WIDTH * NUM_ROUNDS;

const RNG_SEED: u64 = 42;

lazy_static! {
    static ref PERM_GL: Perm = Perm::new(
        HALF_NUM_FULL_ROUNDS,
        NUM_PARTIAL_ROUNDS,
        rand_chacha::ChaCha8Rng::seed_from_u64(RNG_SEED)
            .sample_iter(Standard)
            .take(NUM_CONSTANTS)
            .collect(),
        MdsMatrixGoldilocks,
    );
}

impl FieldElementMap for GoldilocksField {
    type Config = StarkConfig<MyPcs, FriChallenge, FriChallenger>;

    fn into_p3_field(self) -> Plonky3Field<Self> {
        Goldilocks::from_canonical_u64(self.to_integer().try_into_u64().unwrap())
    }

    fn from_p3_field(e: Plonky3Field<Self>) -> Self {
        Self::from(e.as_canonical_u64())
    }

    fn get_challenger() -> Challenger<Self> {
        FriChallenger::new(PERM_GL.clone())
    }

    fn get_config() -> Self::Config {
        let hash = Hash::new(PERM_GL.clone());

        let compress = Compress::new(PERM_GL.clone());

        let val_mmcs = ValMmcs::new(hash, compress);

        let challenge_mmcs = ChallengeMmcs::new(val_mmcs.clone());

        let dft = Dft {};

        let fri_config = FriConfig {
            log_blowup: FRI_LOG_BLOWUP,
            num_queries: FRI_NUM_QUERIES,
            proof_of_work_bits: FRI_PROOF_OF_WORK_BITS,
            mmcs: challenge_mmcs,
        };

        let pcs = MyPcs::new(dft, val_mmcs, fri_config);

        Self::Config::new(pcs)
    }
}
