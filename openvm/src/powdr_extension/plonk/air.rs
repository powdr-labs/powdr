use openvm_stark_backend::{
    interaction::InteractionBuilder,
    p3_air::{Air, BaseAir},
    p3_field::PrimeField32,
    rap::{BaseAirWithPublicValues, ColumnsAir, PartitionedBaseAir},
};

pub struct PlonkAir<F> {
    pub _marker: std::marker::PhantomData<F>,
}

impl<F: PrimeField32> ColumnsAir<F> for PlonkAir<F> {
    fn columns(&self) -> Option<Vec<String>> {
        todo!()
    }
}

impl<F: PrimeField32> BaseAir<F> for PlonkAir<F> {
    fn width(&self) -> usize {
        todo!()
    }
}
impl<F: PrimeField32> BaseAirWithPublicValues<F> for PlonkAir<F> {}

impl<AB: InteractionBuilder> Air<AB> for PlonkAir<AB::F>
where
    AB::F: PrimeField32,
{
    fn eval(&self, _builder: &mut AB) {
        todo!()
    }
}

impl<F: PrimeField32> PartitionedBaseAir<F> for PlonkAir<F> {}
