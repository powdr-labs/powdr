use p3_commit::{Pcs, PolynomialSpace};
use p3_uni_stark::StarkGenericConfig;
use powdr_number::FieldElement;

pub type Plonky3Field<T> =
    <<MyPcs<T> as Pcs<Challenge<T>, Challenger<T>>>::Domain as PolynomialSpace>::Val;
pub type MyPcs<T> = <<T as FieldElementMap>::Config as StarkGenericConfig>::Pcs;
pub type Challenge<T> = <<T as FieldElementMap>::Config as StarkGenericConfig>::Challenge;
pub type Challenger<T> = <<T as FieldElementMap>::Config as StarkGenericConfig>::Challenger;

pub trait FieldElementMap: FieldElement {
    type Config: StarkGenericConfig + Sync;

    fn to_p3_field(&self) -> Plonky3Field<Self>;

    fn get_challenger() -> Challenger<Self>;

    fn get_config() -> Self::Config;
}
