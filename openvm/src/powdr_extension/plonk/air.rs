use openvm_circuit_primitives::AlignedBorrow;
use openvm_stark_backend::{
    interaction::InteractionBuilder,
    p3_air::{Air, BaseAir},
    p3_field::PrimeField32,
    p3_matrix::Matrix,
    rap::{BaseAirWithPublicValues, ColumnsAir, PartitionedBaseAir},
};
use std::borrow::Borrow;
use struct_reflection::StructReflection;
use struct_reflection::StructReflectionHelper;

#[repr(C)]
#[derive(AlignedBorrow, StructReflection)]
pub struct PlonkColumns<T> {
    q_l: T,
    q_r: T,
    q_o: T,
    q_mul: T,
    q_const: T,

    a: T,
    b: T,
    c: T,
}

pub struct PlonkAir<F> {
    pub _marker: std::marker::PhantomData<F>,
}

impl<F: PrimeField32> ColumnsAir<F> for PlonkAir<F> {
    fn columns(&self) -> Option<Vec<String>> {
        PlonkColumns::<F>::struct_reflection()
    }
}

impl<F: PrimeField32> BaseAir<F> for PlonkAir<F> {
    fn width(&self) -> usize {
        PlonkColumns::<F>::width()
    }
}
impl<F: PrimeField32> BaseAirWithPublicValues<F> for PlonkAir<F> {}

impl<AB: InteractionBuilder> Air<AB> for PlonkAir<AB::F>
where
    AB::F: PrimeField32,
{
    fn eval(&self, builder: &mut AB) {
        let main = builder.main();
        let local = main.row_slice(0);

        let PlonkColumns {
            q_l,
            q_r,
            q_o,
            q_mul,
            q_const,
            a,
            b,
            c,
        } = (*local).borrow();

        builder.assert_zero(
            q_l.clone() * a.clone()
                + q_r.clone() * b.clone()
                + q_o.clone() * c.clone()
                + q_mul.clone() * (a.clone() * b.clone())
                + q_const.clone(),
        );
    }
}

impl<F: PrimeField32> PartitionedBaseAir<F> for PlonkAir<F> {}
