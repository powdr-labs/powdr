//! The concrete parameters used in the prover
//! Inspired from [this example](https://github.com/Plonky3/Plonky3/blob/6a1b0710fdf85136d0fdd645b92933615867740a/keccak-air/examples/prove_goldilocks_keccak.rs#L57)

use lazy_static::lazy_static;

use crate::params::FieldElementMap;
use p3_baby_bear::{BabyBear, DiffusionMatrixBabyBear, MdsMatrixBabyBear};
use p3_challenger::DuplexChallenger;
use p3_commit::ExtensionMmcs;
use p3_dft::Radix2DitParallel;
use p3_field::{extension::BinomialExtensionField, Field}, AbstractField;
use p3_fri::{FriConfig, TwoAdicFriPcs};
use p3_merkle_tree::FieldMerkleTreeMmcs;
use p3_poseidon2::{DiffusionPermutation, Poseidon2, Poseidon2ExternalMatrixGeneral};
use p3_symmetric::{PaddingFreeSponge, TruncatedPermutation};
use p3_uni_stark::StarkConfig;

use rand::{distributions::Standard, Rng, SeedableRng};

use powdr_number::{BabyBearField, FieldElement};

pub trait Poseidon2Compatible {
    const D: u64;
    const ROUNDS_F: usize;
    const ROUNDS_P: usize;
    const PERM_WIDTH: usize;
    const RNG_SEED: u64 = 42;

    type Poseidon2Perm: Poseidon2<
        PrimeField,
        Poseidon2ExternalMatrixGeneral,
        DiffusionPermutation<AbstractField, PERM_WIDTH>,
        PERM_WIDTH,
        D,
    >;
}

impl Poseidon2Compatible for BabyBearField {
    const D: u64 = 7;
    // params directly taken from plonky3's poseidon2_round_numbers_128 function
    // to guarentee 128-bit security.
    const ROUNDS_F: usize = 8;
    const ROUNDS_P: usize = 13;
    const PERM_WIDTH: usize = 16;

    type Poseidon2Perm =
        Poseidon2<BabyBear, Poseidon2ExternalMatrixGeneral, DiffusionMatrixBabyBear, PERM_WIDTH, D>;
}

lazy_static! {
    static ref PERM_BB: Poseidon2Compatible::Poseidon2Perm = BabyBearField::Poseidon2Perm::new(
        BabyBearField::ROUNDS_F,
        rand_chacha::ChaCha8Rng::seed_from_u64(BabyBearField::RNG_SEED)
            .sample_iter(Standard)
            .take(BabyBearField::ROUNDS_F)
            .collect::<Vec<[BabyBear; BabyBearField::PERM_WIDTH]>>(),
        Poseidon2ExternalMatrixGeneral,
        BabyBearField::ROUNDS_P,
        rand_chacha::ChaCha8Rng::seed_from_u64(BabyBearField::RNG_SEED)
            .sample_iter(Standard)
            .take(BabyBearField::ROUNDS_P)
            .collect(),
        DiffusionMatrixBabyBear::default()
    );
}

impl FieldElementMap for BabyBearField {
    type P3Field = BabyBear;
    type MdsMatrix = MdsMatrixBabyBear;

    const DEGREE: usize = 2;
    const WIDTH: usize = BabyBearField::PERM_WIDTH;
    const RATE: usize = 8;
    const OUT: usize = 8;
    const N: usize = 2;
    const CHUNK: usize = 8;
    const DIGEST_ELEMS: usize = 8;

    type Perm = BabyBearField::Poseidon2Perm;

    fn to_p3_field(&self) -> Self::P3Field {
        BabyBear::from_canonical_u32(self.to_integer().try_into_u32().unwrap())
    }

    fn get_challenger() -> Challenger {
        Challenger::new(PERM_BB.clone())
    }

    fn get_config() -> Config {
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

        Config::new(pcs)
    }
}
