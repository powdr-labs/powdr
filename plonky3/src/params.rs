// use lazy_static::lazy_static;

// use p3_baby_bear::MdsMatrixBabyBear;
// use p3_challenger::DuplexChallenger;
// use p3_commit::ExtensionMmcs;
// use p3_dft::Radix2DitParallel;
use p3_field::PrimeField;
// use p3_fri::{FriConfig, TwoAdicFriPcs};
// use p3_goldilocks::{Goldilocks, MdsMatrixGoldilocks};
// use p3_merkle_tree::FieldMerkleTreeMmcs;
// use p3_poseidon::Poseidon;
// use p3_symmetric::{PaddingFreeSponge, TruncatedPermutation};
// use p3_uni_stark::StarkConfig;

// use rand::{distributions::Standard, Rng, SeedableRng};

// use crate::data_traits::FieldElementMap;
// use powdr_number::{BabyBearField, GoldilocksField};

pub trait FieldElementMap: Clone + Send + Sync {
    type P3Field: PrimeField;
    type MdsMatrix;

    fn to_p3_field(&self) -> Self::P3Field;
}
// pub trait Permutation {
//     fn get_challenger<T: FieldElementMap>() -> Challenger<T>;
// }

// pub trait Configure {
//     pub fn get_config<T: FieldElementMap>() -> StarkConfig<MyPcs<T>, Challenge<T>, Challenger<T>>;
// }
