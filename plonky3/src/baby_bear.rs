//! The concrete parameters used in the prover
//! Inspired from [this example](https://github.com/Plonky3/Plonky3/blob/6a1b0710fdf85136d0fdd645b92933615867740a/keccak-air/examples/prove_baby_bear_poseidon2.rs)

use lazy_static::lazy_static;

use p3_baby_bear::{BabyBear, DiffusionMatrixBabyBear};
use p3_challenger::DuplexChallenger;
use p3_commit::ExtensionMmcs;
use p3_dft::Radix2DitParallel;
use p3_field::{extension::BinomialExtensionField, AbstractField, Field};
use p3_fri::{FriConfig, TwoAdicFriPcs};
use p3_merkle_tree::FieldMerkleTreeMmcs;
use p3_poseidon2::{Poseidon2, Poseidon2ExternalMatrixGeneral};
use p3_symmetric::{PaddingFreeSponge, TruncatedPermutation};
use p3_uni_stark::StarkConfig;

use rand::{distributions::Standard, Rng, SeedableRng};

use powdr_number::{FieldElement, LargeInt};

const BB_D: u64 = 7;
// params directly taken from plonky3's poseidon2_round_numbers_128 function
// to guarentee 128-bit security.
const BB_ROUNDS_F: usize = 8;
const BB_ROUNDS_P: usize = 13;
const BB_WIDTH: usize = 16;
type BabyBearPerm =
    Poseidon2<BabyBear, Poseidon2ExternalMatrixGeneral, DiffusionMatrixBabyBear, BB_WIDTH, BB_D>;

const BB_DEGREE: usize = 4;
pub type BabyBearChallenge = BinomialExtensionField<BabyBear, BB_DEGREE>;

const BB_RATE: usize = 8;
const BB_OUT: usize = 8;
pub type BabyBearChallenger = DuplexChallenger<BabyBear, BabyBearPerm, BB_WIDTH, BB_RATE>;
type BabyBearHash = PaddingFreeSponge<BabyBearPerm, BB_WIDTH, BB_RATE, BB_OUT>;

const BB_N: usize = 2;
const BB_CHUNK: usize = 8;
type BabyBearCompress = TruncatedPermutation<BabyBearPerm, BB_N, BB_CHUNK, BB_WIDTH>;
const BB_DIGEST_ELEMS: usize = 8;
type BabyBearValMmcs = FieldMerkleTreeMmcs<
    <BabyBear as Field>::Packing,
    <BabyBear as Field>::Packing,
    BabyBearHash,
    BabyBearCompress,
    BB_DIGEST_ELEMS,
>;

type BabyBearChallengeMmcs = ExtensionMmcs<BabyBear, BabyBearChallenge, BabyBearValMmcs>;
type Dft = Radix2DitParallel;
type BabyBearMyPcs = TwoAdicFriPcs<BabyBear, Dft, BabyBearValMmcs, BabyBearChallengeMmcs>;

const BB_FRI_LOG_BLOWUP: usize = 1;
const BB_FRI_NUM_QUERIES: usize = 100;
const BB_FRI_PROOF_OF_WORK_BITS: usize = 16;

const BB_RNG_SEED: u64 = 42;

lazy_static! {
    static ref PERM_BB: BabyBearPerm = BabyBearPerm::new(
        BB_ROUNDS_F,
        rand_chacha::ChaCha8Rng::seed_from_u64(BB_RNG_SEED)
            .sample_iter(Standard)
            .take(BB_ROUNDS_F)
            .collect::<Vec<[BabyBear; BB_WIDTH]>>(),
        Poseidon2ExternalMatrixGeneral,
        BB_ROUNDS_P,
        rand_chacha::ChaCha8Rng::seed_from_u64(BB_RNG_SEED)
            .sample_iter(Standard)
            .take(BB_ROUNDS_P)
            .collect(),
        DiffusionMatrixBabyBear::default()
    );
}

pub type BabyBearConfig = StarkConfig<BabyBearMyPcs, BabyBearChallenge, BabyBearChallenger>;

pub fn baby_bear_get_challenger() -> BabyBearChallenger {
    BabyBearChallenger::new(PERM_BB.clone())
}

pub fn baby_bear_get_config() -> BabyBearConfig {
    let hash = BabyBearHash::new(PERM_BB.clone());

    let compress = BabyBearCompress::new(PERM_BB.clone());

    let val_mmcs = BabyBearValMmcs::new(hash, compress);

    let challenge_mmcs = BabyBearChallengeMmcs::new(val_mmcs.clone());

    let dft = Dft {};

    let fri_config = FriConfig {
        log_blowup: BB_FRI_LOG_BLOWUP,
        num_queries: BB_FRI_NUM_QUERIES,
        proof_of_work_bits: BB_FRI_PROOF_OF_WORK_BITS,
        mmcs: challenge_mmcs,
    };

    let pcs = BabyBearMyPcs::new(dft, val_mmcs, fri_config);

    BabyBearConfig::new(pcs)
}
