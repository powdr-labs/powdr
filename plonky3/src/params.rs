use std::io::Chain;

use p3_commit::PolynomialSpace;
use p3_uni_stark::StarkGenericConfig;
use powdr_number::FieldElement;

pub type Plonky3Field<T> =
    <<Pcs<T> as p3_commit::Pcs<Challenge<T>, Challenger<T>>>::Domain as PolynomialSpace>::Val;
pub type Pcs<T> = <<T as FieldElementMap>::Config as StarkGenericConfig>::Pcs;
pub type Challenge<T> = <<T as FieldElementMap>::Config as StarkGenericConfig>::Challenge;
pub type Challenger<T> = <<T as FieldElementMap>::Config as StarkGenericConfig>::Challenger;

type Config<F> = <F as FieldElementMap>::Config;
pub type ProverData<F> = <Pcs<F> as p3_commit::Pcs<Challenge<F>, Challenger<F>>>::ProverData;
pub type Commitment<F> = <Pcs<F> as p3_commit::Pcs<Challenge<F>, Challenger<F>>>::Commitment;

pub trait FieldElementMap: FieldElement
where
    ProverData<Self>: Send,
    Commitment<Self>: Send,
{
    type Config: StarkGenericConfig;

    fn to_p3_field(&self) -> Plonky3Field<Self>;

    fn get_challenger() -> Challenger<Self>;

    fn get_config() -> Self::Config;
}
