//! The concrete parameters used in the prover
//! Inspired from [this example](https://github.com/Plonky3/Plonky3/blob/6a1b0710fdf85136d0fdd645b92933615867740a/keccak-air/examples/prove_goldilocks_keccak.rs#L57)

use lazy_static::lazy_static;

use crate::params::FieldElementMap;
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
use powdr_number::{FieldElement, GoldilocksField, LargeInt};
use rand::distributions::Standard;

pub trait PoseidonCompatible {
    const PERM_WIDTH: usize;
    const ALPHA: u64;
    const HALF_NUM_FULL_ROUNDS: usize;
    const NUM_PARTIAL_ROUNDS: usize;
    const NUM_ROUNDS: usize = 2 * Self::HALF_NUM_FULL_ROUNDS + Self::NUM_PARTIAL_ROUNDS;
    const NUM_CONSTANTS: usize = Self::PERM_WIDTH * Self::NUM_ROUNDS;
    const RNG_SEED: u64 = 42;

    type PermObject;
    type PoseidonPerm;
}

impl PoseidonCompatible for GoldilocksField {
    const PERM_WIDTH: usize = 8;
    const ALPHA: u64 = 7;
    const HALF_NUM_FULL_ROUNDS: usize = 4;
    const NUM_PARTIAL_ROUNDS: usize = 22;

    type PermObject = [Goldilocks; Self::PERM_WIDTH];
    type PoseidonPerm =
        Poseidon<Goldilocks, MdsMatrixGoldilocks, { Self::PERM_WIDTH }, { Self::ALPHA }>;
}

lazy_static! {
    static ref PERM_GL: <GoldilocksField as PoseidonCompatible>::PoseidonPerm =
        GoldilocksField::PoseidonPerm::new(
            GoldilocksField::HALF_NUM_FULL_ROUNDS,
            GoldilocksField::NUM_PARTIAL_ROUNDS,
            rand_chacha::ChaCha8Rng::seed_from_u64(GoldilocksField::RNG_SEED)
                .sample_iter(Standard)
                .take(GoldilocksField::NUM_CONSTANTS)
                .collect(),
            MdsMatrixGoldilocks,
        );
}

impl FieldElementMap for GoldilocksField {
    type P3Field = Goldilocks;
    type MdsMatrix = MdsMatrixGoldilocks;
    type PermObject = <GoldilocksField as PoseidonCompatible>::PermObject;
    type Perm = <GoldilocksField as PoseidonCompatible>::PoseidonPerm;

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

    const DEGREE: usize = 2;
    const WIDTH: usize = GoldilocksField::PERM_WIDTH;
    const RATE: usize = 4;
    const OUT: usize = 4;
    const N: usize = 2;
    const CHUNK: usize = 4;
    const DIGEST_ELEMS: usize = 4;

    fn to_p3_field<T: FieldElement>(elt: T) -> Self::P3Field {
        Goldilocks::from_canonical_u64(elt.to_integer().try_into_u64().unwrap())
    }

    fn get_challenger() -> Self::Challenger {
        Self::Challenger::new(PERM_GL.clone())
    }

    fn get_config() -> Self::Config {
        let hash = Self::Hash::new(PERM_GL.clone());

        let compress = Self::Compress::new(PERM_GL.clone());

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
