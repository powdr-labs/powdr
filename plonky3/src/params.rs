//! The concrete parameters used in the prover
//! Inspired from [this example](https://github.com/Plonky3/Plonky3/blob/6a1b0710fdf85136d0fdd645b92933615867740a/keccak-air/examples/prove_goldilocks_keccak.rs#L57)

use lazy_static::lazy_static;

use p3_baby_bear::MdsMatrixBabyBear;
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

use rand::{distributions::Standard, Rng, SeedableRng};

use crate::circuit_builder::FieldElementMap;
use powdr_number::{BabyBearField, GoldilocksField};

const D: usize = 2;
type Challenge<T: FieldElementMap> = BinomialExtensionField<T::P3Field, D>;
const WIDTH: usize = 8;
const ALPHA: u64 = 7;
type Perm<T: FieldElementMap> = Poseidon<T::P3Field, T::MdsMatrix, WIDTH, ALPHA>;

const RATE: usize = 4;
const OUT: usize = 4;
type Hash<T: FieldElementMap> = PaddingFreeSponge<Perm<T>, WIDTH, RATE, OUT>;

const N: usize = 2;
const CHUNK: usize = 4;
type Compress<T: FieldElementMap> = TruncatedPermutation<Perm<T>, N, CHUNK, WIDTH>;

const DIGEST_ELEMS: usize = 4;
type ValMmcs<T: FieldElementMap> = FieldMerkleTreeMmcs<
    <T::P3Field as Field>::Packing,
    <T::P3Field as Field>::Packing,
    Hash<T>,
    Compress<T>,
    DIGEST_ELEMS,
>;
pub type Challenger<T: FieldElementMap> = DuplexChallenger<T::P3Field, Perm<T>, WIDTH, RATE>;
type ChallengeMmcs<T: FieldElementMap> = ExtensionMmcs<T::P3Field, Challenge<T>, ValMmcs<T>>;
type Dft = Radix2DitParallel;
type MyPcs<T: FieldElementMap> = TwoAdicFriPcs<T::P3Field, Dft, ValMmcs<T>, ChallengeMmcs<T>>;
pub type Config<T: FieldElementMap> = StarkConfig<MyPcs<T>, Challenge<T>, Challenger<T>>;

const HALF_NUM_FULL_ROUNDS: usize = 4;
const NUM_PARTIAL_ROUNDS: usize = 22;

const FRI_LOG_BLOWUP: usize = 1;
const FRI_NUM_QUERIES: usize = 100;
const FRI_PROOF_OF_WORK_BITS: usize = 16;

const NUM_ROUNDS: usize = 2 * HALF_NUM_FULL_ROUNDS + NUM_PARTIAL_ROUNDS;
const NUM_CONSTANTS: usize = WIDTH * NUM_ROUNDS;

const RNG_SEED: u64 = 42;

lazy_static! {
    static ref PERM_GL: Perm<GoldilocksField> = Perm::new(
        HALF_NUM_FULL_ROUNDS,
        NUM_PARTIAL_ROUNDS,
        rand_chacha::ChaCha8Rng::seed_from_u64(RNG_SEED)
            .sample_iter(Standard)
            .take(NUM_CONSTANTS)
            .collect(),
        MdsMatrixGoldilocks,
    );
}

lazy_static! {
    static ref PERM_BB: Perm<BabyBearField> = Perm::new(
        HALF_NUM_FULL_ROUNDS,
        NUM_PARTIAL_ROUNDS,
        rand_chacha::ChaCha8Rng::seed_from_u64(RNG_SEED)
            .sample_iter(Standard)
            .take(NUM_CONSTANTS)
            .collect(),
        MdsMatrixBabyBear::new(),
    );
}

pub trait Permutation {
    fn get_challenger<T: FieldElementMap>() -> Challenger<T>;
}

impl Permutation for GoldilocksField {
    fn get_challenger<T>() -> Challenger<GoldilocksField> {
        Challenger::new(PERM_GL.clone())
    }
}

pub fn get_config<T: FieldElementMap>() -> StarkConfig<MyPcs<T>, Challenge<T>, Challenger<T>> {
    let hash = Hash::<GoldilocksField>::new(PERM_GL.clone());

    let compress = Compress::<T>::new(PERM_GL.clone());

    let val_mmcs = ValMmcs::<T>::new(hash, compress);

    let challenge_mmcs = ChallengeMmcs::<T>::new(val_mmcs.clone());

    let dft = Dft {};

    let fri_config = FriConfig {
        log_blowup: FRI_LOG_BLOWUP,
        num_queries: FRI_NUM_QUERIES,
        proof_of_work_bits: FRI_PROOF_OF_WORK_BITS,
        mmcs: challenge_mmcs,
    };

    let pcs = MyPcs::<T>::new(dft, val_mmcs, fri_config);

    Config::new(pcs)
}
