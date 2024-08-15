//! The concrete parameters used in the prover
//! Inspired from [this example](https://github.com/Plonky3/Plonky3/blob/6a1b0710fdf85136d0fdd645b92933615867740a/keccak-air/examples/prove_goldilocks_keccak.rs#L57)

use lazy_static::lazy_static;

use crate::params::FieldElementMap;
use p3_baby_bear::{BabyBear, DiffusionMatrixBabyBear, MdsMatrixBabyBear};
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

use powdr_number::{BabyBearField, FieldElement};

const DEGREE: usize = 2;
type Challenge<T: FieldElementMap> = BinomialExtensionField<T::P3Field, DEGREE>;

const WIDTH: usize = 16;
const D: u64 = 7;
type Perm = Poseidon2<BabyBear, Poseidon2ExternalMatrixGeneral, DiffusionMatrixBabyBear, WIDTH, D>;

// const ROUNDS_F: usize = poseidon2_round_numbers_128::<dyn BabyBear>(WIDTH, D).0;
// const ROUNDS_P: usize = poseidon2_round_numbers_128::<dyn BabyBear>(WIDTH, D).1;

// params directly taken from plonky3's poseidon2_round_numbers_128 function
// to guarentee 128-bit security.

const ROUNDS_F: usize = 8;
const ROUNDS_P: usize = 13;

const RATE: usize = 8;
const OUT: usize = 8;
type Hash<T: FieldElementMap> = PaddingFreeSponge<Perm, WIDTH, RATE, OUT>;

const N: usize = 2;
const CHUNK: usize = 8;
type Compress<T: FieldElementMap> = TruncatedPermutation<Perm, N, CHUNK, WIDTH>;

const DIGEST_ELEMS: usize = 8;
type ValMmcs<T: FieldElementMap> = FieldMerkleTreeMmcs<
    <T::P3Field as Field>::Packing,
    <T::P3Field as Field>::Packing,
    Hash<T>,
    Compress<T>,
    DIGEST_ELEMS,
>;

pub type Challenger<T: FieldElementMap> = DuplexChallenger<T::P3Field, Perm, WIDTH, RATE>;
type ChallengeMmcs<T: FieldElementMap> = ExtensionMmcs<T::P3Field, Challenge<T>, ValMmcs<T>>;
type Dft = Radix2DitParallel;
type MyPcs<T: FieldElementMap> = TwoAdicFriPcs<T::P3Field, Dft, ValMmcs<T>, ChallengeMmcs<T>>;
pub type Config<T: FieldElementMap> = StarkConfig<MyPcs<T>, Challenge<T>, Challenger<T>>;

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
    type P3Field = BabyBear;
    type MdsMatrix = MdsMatrixBabyBear;

    fn to_p3_field(&self) -> Self::P3Field {
        BabyBear::from_canonical_u32(self.to_integer().try_into_u32().unwrap())
    }
}

pub(crate) fn get_challenger_baby_bear<T: FieldElementMap>() -> Challenger<T>
where
    p3_monty_31::poseidon2::DiffusionMatrixMontyField31<BabyBearDiffusionMatrixParameters>:
        DiffusionPermutation<<T as FieldElementMap>::P3Field, 16>,
{
    Challenger::new(PERM_BB.clone())
}

pub(crate) fn get_config_baby_bear<T: FieldElementMap>(
) -> StarkConfig<MyPcs<T>, Challenge<T>, Challenger<T>> {
    let hash = Hash::<T>::new(PERM_BB.clone());

    let compress = Compress::<T>::new(PERM_BB.clone());

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
