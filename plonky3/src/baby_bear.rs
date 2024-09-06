//! The concrete parameters used in the prover
//! Inspired from [this example](https://github.com/Plonky3/Plonky3/blob/6a1b0710fdf85136d0fdd645b92933615867740a/keccak-air/examples/prove_baby_bear_poseidon2.rs)

use lazy_static::lazy_static;

use crate::params::{Challenger, FieldElementMap, Plonky3Field};
use p3_baby_bear::{BabyBear, DiffusionMatrixBabyBear};
use p3_challenger::DuplexChallenger;
use p3_commit::ExtensionMmcs;
use p3_dft::Radix2DitParallel;
use p3_field::{extension::BinomialExtensionField, Field};
use p3_fri::{FriConfig, TwoAdicFriPcs};
use p3_merkle_tree::FieldMerkleTreeMmcs;
use p3_poseidon2::{Poseidon2, Poseidon2ExternalMatrixGeneral};
use p3_symmetric::{PaddingFreeSponge, TruncatedPermutation};
use p3_uni_stark::StarkConfig;

use rand::{distributions::Standard, Rng, SeedableRng};

use powdr_number::BabyBearField;

const D: u64 = 7;
// params directly taken from plonky3's poseidon2_round_numbers_128 function
// to guarentee 128-bit security.
const ROUNDS_F: usize = 8;
const ROUNDS_P: usize = 13;
const WIDTH: usize = 16;
type Perm = Poseidon2<BabyBear, Poseidon2ExternalMatrixGeneral, DiffusionMatrixBabyBear, WIDTH, D>;

const DEGREE: usize = 4;
type FriChallenge = BinomialExtensionField<BabyBear, DEGREE>;

const RATE: usize = 8;
const OUT: usize = 8;
type FriChallenger = DuplexChallenger<BabyBear, Perm, WIDTH, RATE>;
type Hash = PaddingFreeSponge<Perm, WIDTH, RATE, OUT>;

const N: usize = 2;
const CHUNK: usize = 8;
type Compress = TruncatedPermutation<Perm, N, CHUNK, WIDTH>;
const DIGEST_ELEMS: usize = 8;
type ValMmcs = FieldMerkleTreeMmcs<
    <BabyBear as Field>::Packing,
    <BabyBear as Field>::Packing,
    Hash,
    Compress,
    DIGEST_ELEMS,
>;

type ChallengeMmcs = ExtensionMmcs<BabyBear, FriChallenge, ValMmcs>;
type Dft = Radix2DitParallel;
type MyPcs = TwoAdicFriPcs<BabyBear, Dft, ValMmcs, ChallengeMmcs>;

const FRI_LOG_BLOWUP: usize = 1;
const FRI_NUM_QUERIES: usize = 100;
const FRI_PROOF_OF_WORK_BITS: usize = 16;

const RNG_SEED: u64 = 42;

lazy_static! {
    static ref PERM_BB: Perm = Perm::new(
        ROUNDS_F,
        rand_chacha::ChaCha8Rng::seed_from_u64(RNG_SEED)
            .sample_iter(Standard)
            .take(ROUNDS_F)
            .collect::<Vec<[BabyBear; WIDTH]>>(),
        Poseidon2ExternalMatrixGeneral,
        ROUNDS_P,
        rand_chacha::ChaCha8Rng::seed_from_u64(RNG_SEED)
            .sample_iter(Standard)
            .take(ROUNDS_P)
            .collect(),
        DiffusionMatrixBabyBear::default()
    );
}

impl FieldElementMap for BabyBearField {
    type Config = StarkConfig<MyPcs, FriChallenge, FriChallenger>;
    fn into_p3_field(self) -> Plonky3Field<Self> {
        self.0
    }

    fn get_challenger() -> Challenger<Self> {
        FriChallenger::new(PERM_BB.clone())
    }

    fn get_config() -> Self::Config {
        let hash = Hash::new(PERM_BB.clone());

        let compress = Compress::new(PERM_BB.clone());

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
