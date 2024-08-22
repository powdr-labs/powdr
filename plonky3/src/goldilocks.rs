//! The concrete parameters used in the prover
//! Inspired from [this example](https://github.com/Plonky3/Plonky3/blob/6a1b0710fdf85136d0fdd645b92933615867740a/keccak-air/examples/prove_goldilocks_keccak.rs#L57)

use lazy_static::lazy_static;

use p3_challenger::DuplexChallenger;
use p3_commit::ExtensionMmcs;
use p3_dft::Radix2DitParallel;
use p3_field::{extension::BinomialExtensionField, AbstractField, Field};
use p3_fri::{FriConfig, TwoAdicFriPcs};
use p3_goldilocks::{Goldilocks, MdsMatrixGoldilocks};
use p3_merkle_tree::FieldMerkleTreeMmcs;
use p3_poseidon::Poseidon;
use p3_symmetric::{PaddingFreeSponge, TruncatedPermutation};
use p3_uni_stark::StarkConfig;
use powdr_number::{FieldElement, LargeInt};
use rand::{distributions::Standard, Rng, SeedableRng};

const GL_DEGREE: usize = 2;
type GoldilocksChallenge = BinomialExtensionField<Goldilocks, GL_DEGREE>;
const GL_WIDTH: usize = 8;
const GL_ALPHA: u64 = 7;
type GoldilocksPerm = Poseidon<Goldilocks, MdsMatrixGoldilocks, GL_WIDTH, GL_ALPHA>;

const GL_RATE: usize = 4;
const GL_OUT: usize = 4;
type GoldilocksHash = PaddingFreeSponge<GoldilocksPerm, GL_WIDTH, GL_RATE, GL_OUT>;

const GL_N: usize = 2;
const GL_CHUNK: usize = 4;
type GoldilocksCompress = TruncatedPermutation<GoldilocksPerm, GL_N, GL_CHUNK, GL_WIDTH>;

const GL_DIGEST_ELEMS: usize = 4;
type GoldilocksValMmcs = FieldMerkleTreeMmcs<
    <Goldilocks as Field>::Packing,
    <Goldilocks as Field>::Packing,
    GoldilocksHash,
    GoldilocksCompress,
    GL_DIGEST_ELEMS,
>;

pub type GoldilocksChallenger = DuplexChallenger<Goldilocks, GoldilocksPerm, GL_WIDTH, GL_RATE>;
type GoldilocksChallengeMmcs = ExtensionMmcs<Goldilocks, GoldilocksChallenge, GoldilocksValMmcs>;
type Dft = Radix2DitParallel;
type GoldilocksMyPcs = TwoAdicFriPcs<Goldilocks, Dft, GoldilocksValMmcs, GoldilocksChallengeMmcs>;

const GL_HALF_NUM_FULL_ROUNDS: usize = 4;
const GL_NUM_PARTIAL_ROUNDS: usize = 22;

const GL_FRI_LOG_BLOWUP: usize = 1;
const GL_FRI_NUM_QUERIES: usize = 100;
const GL_FRI_PROOF_OF_WORK_BITS: usize = 16;

const GL_NUM_ROUNDS: usize = 2 * GL_HALF_NUM_FULL_ROUNDS + GL_NUM_PARTIAL_ROUNDS;
const GL_NUM_CONSTANTS: usize = GL_WIDTH * GL_NUM_ROUNDS;

const GL_RNG_SEED: u64 = 42;

lazy_static! {
    static ref PERM_GL: GoldilocksPerm = GoldilocksPerm::new(
        GL_HALF_NUM_FULL_ROUNDS,
        GL_NUM_PARTIAL_ROUNDS,
        rand_chacha::ChaCha8Rng::seed_from_u64(GL_RNG_SEED)
            .sample_iter(Standard)
            .take(GL_NUM_CONSTANTS)
            .collect(),
        MdsMatrixGoldilocks,
    );
}

pub type GoldilocksConfig = StarkConfig<GoldilocksMyPcs, GoldilocksChallenge, GoldilocksChallenger>;

pub fn gl_get_challenger() -> GoldilocksChallenger {
    GoldilocksChallenger::new(PERM_GL.clone())
}

pub fn gl_get_config() -> GoldilocksConfig {
    let hash = GoldilocksHash::new(PERM_GL.clone());

    let compress = GoldilocksCompress::new(PERM_GL.clone());

    let val_mmcs = GoldilocksValMmcs::new(hash, compress);

    let challenge_mmcs = GoldilocksChallengeMmcs::new(val_mmcs.clone());

    let dft = Dft {};

    let fri_config = FriConfig {
        log_blowup: GL_FRI_LOG_BLOWUP,
        num_queries: GL_FRI_NUM_QUERIES,
        proof_of_work_bits: GL_FRI_PROOF_OF_WORK_BITS,
        mmcs: challenge_mmcs,
    };

    let pcs = GoldilocksMyPcs::new(dft, val_mmcs, fri_config);

    GoldilocksConfig::new(pcs)
}
