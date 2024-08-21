use p3_challenger::{CanObserve, CanSample, FieldChallenger};
use p3_commit::{Pcs, PolynomialSpace, Val};
use p3_field::{ExtensionField, PrimeField};
use p3_symmetric::{CryptographicPermutation, Permutation};
use p3_uni_stark::{StarkConfig, StarkGenericConfig};
use powdr_number::FieldElement;

pub type Plonky3Field<T> =
    <<MyPcs<T> as Pcs<Challenge<T>, Challenger<T>>>::Domain as PolynomialSpace>::Val;
pub type MyPcs<T> = <<T as FieldElementMap>::Config as StarkGenericConfig>::Pcs;
pub type Challenge<T> = <<T as FieldElementMap>::Config as StarkGenericConfig>::Challenge;
pub type Challenger<T> = <<T as FieldElementMap>::Config as StarkGenericConfig>::Challenger;

pub(crate) trait FieldElementMap: FieldElement {
    type P3Field: PrimeField;
    // type MdsMatrix;
    // type PermObject: Clone;
    // type Perm: Permutation<Self::PermObject> + CryptographicPermutation<Self::PermObject>;
    // // type Hash;
    // // type Compress;
    // type Challenge: ExtensionField<
    //     Val<<Self::MyPcs as Pcs<Self::Challenge, Self::Challenger>>::Domain>,
    // >;
    // // type Dft;
    // // type ValMmcs;
    // // type ChallengeMmcs;
    // type MyPcs: Pcs<Self::Challenge, Self::Challenger>;
    type Config: StarkGenericConfig;

    // :FieldChallenger<Self::P3Field>
    //     + CanObserve<<Self::MyPcs as Pcs<Self::Challenge, Self::Challenger>>::Commitment>
    //     + CanSample<Self::Challenge>;

    const DEGREE: usize;
    const WIDTH: usize;
    const RATE: usize;
    const OUT: usize;
    const N: usize;
    const CHUNK: usize;
    const DIGEST_ELEMS: usize;

    const FRI_LOG_BLOWUP: usize = 1;
    const FRI_NUM_QUERIES: usize = 100;
    const FRI_PROOF_OF_WORK_BITS: usize = 16;

    fn to_p3_field(&self) -> Plonky3Field<Self>;

    fn get_challenger() -> Challenger<Self>;

    fn get_config() -> Self::Config;
}
