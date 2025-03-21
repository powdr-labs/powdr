//! The concrete parameters used in the prover
//!
//! Inspired from [this example](https://github.com/Plonky3/Plonky3/blob/6a1b0710fdf85136d0fdd645b92933615867740a/keccak-air/examples/prove_goldilocks_keccak.rs#L57)
//! (But using Poseidon2 instead of Poseidon)

use crate::{
    params::{Challenger, FieldElementMap, Plonky3Field},
    poseidon2::goldilocks::{Permutation, PERM, WIDTH},
};
use p3_challenger::DuplexChallenger;
use p3_commit::ExtensionMmcs;
use p3_dft::Radix2DitParallel;
use p3_field::{extension::BinomialExtensionField, AbstractField, Field, PrimeField64};
use p3_fri::{FriConfig, TwoAdicFriPcs};
use p3_goldilocks::Goldilocks;
use p3_merkle_tree::MerkleTreeMmcs;
use p3_symmetric::{PaddingFreeSponge, TruncatedPermutation};
use p3_uni_stark::StarkConfig;
use powdr_number::{FieldElement, GoldilocksField, LargeInt};

const DEGREE: usize = 2;
type FriChallenge = BinomialExtensionField<Goldilocks, DEGREE>;

const RATE: usize = 4;
const OUT: usize = 4;
type Hash = PaddingFreeSponge<Permutation, WIDTH, RATE, OUT>;

const N: usize = 2;
const CHUNK: usize = 4;
type Compress = TruncatedPermutation<Permutation, N, CHUNK, WIDTH>;

const DIGEST_ELEMS: usize = 4;
type ValMmcs = MerkleTreeMmcs<
    <Goldilocks as Field>::Packing,
    <Goldilocks as Field>::Packing,
    Hash,
    Compress,
    DIGEST_ELEMS,
>;

pub type FriChallenger = DuplexChallenger<Goldilocks, Permutation, WIDTH, RATE>;
type ChallengeMmcs = ExtensionMmcs<Goldilocks, FriChallenge, ValMmcs>;
type Dft = Radix2DitParallel<Goldilocks>;
type MyPcs = TwoAdicFriPcs<Goldilocks, Dft, ValMmcs, ChallengeMmcs>;

const FRI_LOG_BLOWUP: usize = 1;
const FRI_NUM_QUERIES: usize = 100;
const FRI_PROOF_OF_WORK_BITS: usize = 16;

impl FieldElementMap for GoldilocksField {
    type Config = StarkConfig<MyPcs, FriChallenge, FriChallenger>;

    fn into_p3_field(self) -> Plonky3Field<Self> {
        Goldilocks::from_canonical_u64(self.to_integer().try_into_u64().unwrap())
    }

    fn from_p3_field(e: Plonky3Field<Self>) -> Self {
        Self::from(e.as_canonical_u64())
    }

    fn get_challenger() -> Challenger<Self> {
        FriChallenger::new(PERM.clone())
    }

    fn get_config() -> Self::Config {
        let hash = Hash::new(PERM.clone());

        let compress = Compress::new(PERM.clone());

        let val_mmcs = ValMmcs::new(hash, compress);

        let challenge_mmcs = ChallengeMmcs::new(val_mmcs.clone());

        let dft = Dft::default();

        let (log_blowup, num_queries, proof_of_work_bits) = Self::get_fri_parameters();

        let fri_config = FriConfig {
            log_blowup,
            num_queries,
            proof_of_work_bits,
            mmcs: challenge_mmcs,
        };

        let pcs = MyPcs::new(dft, val_mmcs, fri_config);

        Self::Config::new(pcs)
    }

    fn get_fri_parameters() -> (usize,usize,usize) {
        (FRI_LOG_BLOWUP, FRI_NUM_QUERIES, FRI_PROOF_OF_WORK_BITS)
    }

    fn degree_bound() -> usize {
        // Currently, Plonky3 can't compute evaluations other than those already computed for the
        // FRI commitment. This introduces the following dependency between the blowup factor and
        // the degree bound:
        (1 << FRI_LOG_BLOWUP) + 1
    }
}
