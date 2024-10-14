//! The concrete parameters used in the prover for Mersenne-31
//! Inspired from [this example](https://github.com/Plonky3/Plonky3/blob/7c5deb0eab7191a97f7bb088637c1a68b2e6eb68/keccak-air/examples/prove_m31_poseidon2.rs)

use std::marker::PhantomData;

use lazy_static::lazy_static;

use crate::params::{
    poseidon2::{poseidon2_external_constants, poseidon2_internal_constants},
    Challenger, FieldElementMap, Plonky3Field,
};
use p3_challenger::DuplexChallenger;
use p3_circle::CirclePcs;
use p3_commit::ExtensionMmcs;
use p3_field::{extension::BinomialExtensionField, Field};
use p3_fri::FriConfig;
use p3_merkle_tree::MerkleTreeMmcs;
use p3_mersenne_31::{DiffusionMatrixMersenne31, Mersenne31};
use p3_poseidon2::{poseidon2_round_numbers_128, Poseidon2, Poseidon2ExternalMatrixGeneral};
use p3_symmetric::{PaddingFreeSponge, TruncatedPermutation};
use p3_uni_stark::StarkConfig;

use powdr_number::Mersenne31Field;

const D: u64 = 5;
const WIDTH: usize = 16;
type Perm =
    Poseidon2<Mersenne31, Poseidon2ExternalMatrixGeneral, DiffusionMatrixMersenne31, WIDTH, D>;

const DEGREE: usize = 3;
type FriChallenge = BinomialExtensionField<Mersenne31, DEGREE>;

const RATE: usize = 8;
const OUT: usize = 8;
type FriChallenger = DuplexChallenger<Mersenne31, Perm, WIDTH, RATE>;
type Hash = PaddingFreeSponge<Perm, WIDTH, RATE, OUT>;

const N: usize = 2;
const CHUNK: usize = 8;
type Compress = TruncatedPermutation<Perm, N, CHUNK, WIDTH>;
const DIGEST_ELEMS: usize = 8;
type ValMmcs = MerkleTreeMmcs<
    <Mersenne31 as Field>::Packing,
    <Mersenne31 as Field>::Packing,
    Hash,
    Compress,
    DIGEST_ELEMS,
>;

type ChallengeMmcs = ExtensionMmcs<Mersenne31, FriChallenge, ValMmcs>;
type Pcs = CirclePcs<Mersenne31, ValMmcs, ChallengeMmcs>;

const FRI_LOG_BLOWUP: usize = 1;
const FRI_NUM_QUERIES: usize = 100;
const FRI_PROOF_OF_WORK_BITS: usize = 16;

lazy_static! {
    static ref ROUNDS: (usize, usize) = poseidon2_round_numbers_128::<Mersenne31>(WIDTH, D);
    static ref ROUNDS_F: usize = ROUNDS.0;
    static ref ROUNDS_P: usize = ROUNDS.1;
    static ref PERM_M31: Perm = Perm::new(
        *ROUNDS_F,
        poseidon2_external_constants(*ROUNDS_F),
        Poseidon2ExternalMatrixGeneral,
        *ROUNDS_P,
        poseidon2_internal_constants(*ROUNDS_P),
        DiffusionMatrixMersenne31
    );
}

impl FieldElementMap for Mersenne31Field {
    type Config = StarkConfig<Pcs, FriChallenge, FriChallenger>;
    fn into_p3_field(self) -> Plonky3Field<Self> {
        self.into_inner()
    }

    fn from_p3_field(e: Plonky3Field<Self>) -> Self {
        Self::from_inner(e)
    }

    fn get_challenger() -> Challenger<Self> {
        FriChallenger::new(PERM_M31.clone())
    }

    fn get_config() -> Self::Config {
        let hash = Hash::new(PERM_M31.clone());

        let compress = Compress::new(PERM_M31.clone());

        let val_mmcs = ValMmcs::new(hash, compress);

        let challenge_mmcs = ChallengeMmcs::new(val_mmcs.clone());

        let fri_config = FriConfig {
            log_blowup: FRI_LOG_BLOWUP,
            num_queries: FRI_NUM_QUERIES,
            proof_of_work_bits: FRI_PROOF_OF_WORK_BITS,
            mmcs: challenge_mmcs,
        };

        let pcs = Pcs {
            mmcs: val_mmcs,
            fri_config,
            _phantom: PhantomData,
        };

        Self::Config::new(pcs)
    }
}
