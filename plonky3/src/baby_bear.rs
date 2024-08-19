//! The concrete parameters used in the prover
//! Inspired from [this example](https://github.com/Plonky3/Plonky3/blob/6a1b0710fdf85136d0fdd645b92933615867740a/keccak-air/examples/prove_baby_bear_poseidon2.rs)

use lazy_static::lazy_static;

use crate::params::FieldElementMap;
use p3_baby_bear::{BabyBear, DiffusionMatrixBabyBear, MdsMatrixBabyBear};
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

use powdr_number::{BabyBearField, FieldElement, LargeInt};

pub trait Poseidon2Compatible {
    const D: u64;
    const ROUNDS_F: usize;
    const ROUNDS_P: usize;
    const PERM_WIDTH: usize;
    const RNG_SEED: u64 = 42;

    type PermObject;
    type Poseidon2Perm;
}

impl Poseidon2Compatible for BabyBearField {
    const D: u64 = 7;
    // params directly taken from plonky3's poseidon2_round_numbers_128 function
    // to guarentee 128-bit security.
    const ROUNDS_F: usize = 8;
    const ROUNDS_P: usize = 13;
    const PERM_WIDTH: usize = 16;

    type PermObject = [BabyBear; Self::PERM_WIDTH];
    type Poseidon2Perm = Poseidon2<
        BabyBear,
        Poseidon2ExternalMatrixGeneral,
        DiffusionMatrixBabyBear,
        { Self::PERM_WIDTH },
        { Self::D },
    >;
}

lazy_static! {
    static ref PERM_BB: <BabyBearField as Poseidon2Compatible>::Poseidon2Perm =
        <BabyBearField as Poseidon2Compatible>::Poseidon2Perm::new(
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
    type PermObject = <BabyBearField as Poseidon2Compatible>::PermObject;
    type Perm = <BabyBearField as Poseidon2Compatible>::Poseidon2Perm;

    type Hash = PaddingFreeSponge<Self::Perm, { Self::WIDTH }, { Self::RATE }, { Self::OUT }>;
    type Compress = TruncatedPermutation<Self::Perm, { Self::N }, { Self::CHUNK }, { Self::WIDTH }>;
    type Challenge = BinomialExtensionField<Self::P3Field, { Self::DEGREE }>;
    type Challenger = DuplexChallenger<Self::P3Field, Self::Perm, { Self::WIDTH }, { Self::RATE }>;
    type Dft = Radix2DitParallel;
    type ValMmcs = FieldMerkleTreeMmcs<
        <Self::P3Field as Field>::Packing,
        <Self::P3Field as Field>::Packing,
        Self::Hash,
        Self::Compress,
        { Self::DIGEST_ELEMS },
    >;
    type ChallengeMmcs = ExtensionMmcs<Self::P3Field, Self::Challenge, Self::ValMmcs>;
    type MyPcs = TwoAdicFriPcs<Self::P3Field, Self::Dft, Self::ValMmcs, Self::ChallengeMmcs>;
    type Config = StarkConfig<Self::MyPcs, Self::Challenge, Self::Challenger>;

    const DEGREE: usize = 4;
    const WIDTH: usize = BabyBearField::PERM_WIDTH;
    const RATE: usize = 8;
    const OUT: usize = 8;
    const N: usize = 2;
    const CHUNK: usize = 8;
    const DIGEST_ELEMS: usize = 8;

    fn to_p3_field<T: FieldElement>(elt: T) -> Self::P3Field {
        BabyBear::from_canonical_u32(elt.to_integer().try_into_u32().unwrap())
    }

    fn get_challenger() -> Self::Challenger {
        Self::Challenger::new(PERM_BB.clone())
    }

    fn get_config() -> Self::Config {
        let hash = Self::Hash::new(PERM_BB.clone());

        let compress = Self::Compress::new(PERM_BB.clone());

        let val_mmcs = Self::ValMmcs::new(hash, compress);

        let challenge_mmcs = Self::ChallengeMmcs::new(val_mmcs.clone());

        let dft = Self::Dft {};

        let fri_config = FriConfig {
            log_blowup: Self::FRI_LOG_BLOWUP,
            num_queries: Self::FRI_NUM_QUERIES,
            proof_of_work_bits: Self::FRI_PROOF_OF_WORK_BITS,
            mmcs: challenge_mmcs,
        };

        let pcs = Self::MyPcs::new(dft, val_mmcs, fri_config);

        Self::Config::new(pcs)
    }
}
