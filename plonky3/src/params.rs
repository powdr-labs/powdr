//! The concrete parameters used in the prover
//! Inspired from [this example](https://github.com/Plonky3/Plonky3/blob/6a1b0710fdf85136d0fdd645b92933615867740a/keccak-air/examples/prove_goldilocks_poseidon.rs)

use p3_commit::PolynomialSpace;
use p3_uni_stark::StarkGenericConfig;
use powdr_number::FieldElement;

pub type Plonky3Field<T> =
    <<Pcs<T> as p3_commit::Pcs<Challenge<T>, Challenger<T>>>::Domain as PolynomialSpace>::Val;
pub type Pcs<T> = <<T as FieldElementMap>::Config as StarkGenericConfig>::Pcs;
pub type Challenge<T> = <<T as FieldElementMap>::Config as StarkGenericConfig>::Challenge;
pub type Challenger<T> = <<T as FieldElementMap>::Config as StarkGenericConfig>::Challenger;

pub type ProverData<F> = <Pcs<F> as p3_commit::Pcs<Challenge<F>, Challenger<F>>>::ProverData;
pub type Commitment<F> = <Pcs<F> as p3_commit::Pcs<Challenge<F>, Challenger<F>>>::Commitment;

pub trait FieldElementMap: FieldElement
where
    ProverData<Self>: Send,
    Commitment<Self>: Send,
{
    type Config: StarkGenericConfig;

    fn into_p3_field(self) -> Plonky3Field<Self>;

    fn get_challenger() -> Challenger<Self>;

    fn get_config() -> Self::Config;
}
