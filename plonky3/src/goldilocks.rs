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
    const NUM_ROUNDS: usize = 2 * HALF_NUM_FULL_ROUNDS + NUM_PARTIAL_ROUNDS;
    const NUM_CONSTANTS: usize = WIDTH * NUM_ROUNDS;
    const RNG_SEED: u64 = 42;

    type PoseidonPerm: Poseidon<
        PrimeField,
        MdsPermutation<AbstractField, PERM_WIDTH>,
        PERM_WIDTH,
        ALPHA,
    >;
}

impl PoseidonCompatible for GoldilocksField {
    const PERM_WIDTH: usize = 8;
    const ALPHA: u64 = 7;
    const HALF_NUM_FULL_ROUNDS: usize = 4;
    const NUM_PARTIAL_ROUNDS: usize = 22;

    type PoseidonPerm = Poseidon<Goldilocks, MdsMatrixGoldilocks, PERM_WIDTH, ALPHA>;
}

lazy_static! {
    static ref PERM_GL: PoseidonCompatible::PoseidonPerm = GoldilocksField::PoseidonPerm::new(
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
    type Perm = GoldilocksField::PoseidonPerm;

    const DEGREE: usize = 2;
    const WIDTH: usize = GoldilocksField::PERM_WIDTH;
    const RATE: usize = 4;
    const OUT: usize = 4;
    const N: usize = 2;
    const CHUNK: usize = 4;
    const DIGEST_ELEMS: usize = 4;

    fn to_p3_field(&self) -> Self::P3Field {
        Goldilocks::from_canonical_u64(self.to_integer().try_into_u64().unwrap())
    }

    fn get_challenger() -> Challenger {
        Challenger::new(PERM_GL.clone())
    }

    fn get_config() -> Config {
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

        Config::new(pcs)
    }
}
