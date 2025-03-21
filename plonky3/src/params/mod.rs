pub mod baby_bear;
pub mod goldilocks;
pub mod koala_bear;
pub mod mersenne_31;
pub mod poseidon2;

use p3_uni_stark::StarkGenericConfig;
use powdr_number::FieldElement;

use p3_commit::PolynomialSpace;

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

    fn from_p3_field(_: Plonky3Field<Self>) -> Self;

    fn get_challenger() -> Challenger<Self>;

    fn get_config() -> Self::Config;

    fn degree_bound() -> usize;
    
    // get fri parameters: log_blowup, num_queries, proof_of_work_bits
    fn get_fri_parameters() -> (usize,usize,usize);
}
