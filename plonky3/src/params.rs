use p3_challenger::{CanObserve, CanSample, FieldChallenger};
use p3_commit::{Pcs, Val};
use p3_field::{ExtensionField, PrimeField};
use p3_symmetric::{CryptographicPermutation, Permutation};
use p3_uni_stark::{StarkConfig, StarkGenericConfig};
use powdr_number::FieldElement;

pub(crate) trait FieldElementMap: FieldElement {
    type P3Field: PrimeField;
    type MdsMatrix;
    type PermObject: Clone;
    type Perm: Permutation<Self::PermObject> + CryptographicPermutation<Self::PermObject>;
    type Hash;
    type Compress;
    type Challenge: ExtensionField<
        Val<<Self::MyPcs as Pcs<Self::Challenge, Self::Challenger>>::Domain>,
    >;
    type Dft;
    type ValMmcs;
    type ChallengeMmcs;
    type MyPcs: Pcs<Self::Challenge, Self::Challenger>;
    type Config: StarkGenericConfig;
    type Challenger: FieldChallenger<<
        <<Self::MyPcs as p3_commit::Pcs<
            Self::Challenge,
            Self>::Challenger,
        >>::Domain as p3_commit::PolynomialSpace>::Val>;

        // FieldChallenger<Self::P3Field>
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

    fn to_p3_field<T: FieldElement>(elt: T) -> Self::P3Field;

    fn get_challenger() -> Self::Challenger;

    fn get_config() -> StarkConfig<Self::MyPcs, Self::Challenge, Self::Challenger>;
}
