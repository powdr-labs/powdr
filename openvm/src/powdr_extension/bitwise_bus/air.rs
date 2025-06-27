use openvm_circuit_primitives::xor::XorLookupAir;
use openvm_stark_backend::{
    interaction::InteractionBuilder,
    p3_air::{Air, BaseAir},
    p3_field::PrimeField32,
    p3_matrix::Matrix,
    rap::{BaseAirWithPublicValues, ColumnsAir, PartitionedBaseAir},
};
use struct_reflection::{StructReflection,StructReflectionHelper};




// This BitwiseLookupAir capture the bitwise bus interaction with structure: 
// bus_interaction(BITWISE_LOOKUP, [A, B, C, <const>], D * <const>);
#[derive(StructReflection)]
pub struct BitwiseLookupColumns<T> {
    q_const_arg: T,
    q_const_mult: T,

    a: T,
    b: T,
    c: T,
    d: T,
}






pub struct BitwiseLookupAir<F> {
    pub _marker: std::marker::PhantomData<F>,
}

impl<F: PrimeField32> ColumnsAir<F> for BitwiseLookupAir<F> {
    fn columns(&self) -> Option<Vec<String>> {
        BitwiseLookupColumns::<F>::struct_reflection()
    }
}

impl<F: PrimeField32> BaseAir<F> for BitwiseLookupAir<F> {
    fn width(&self) -> usize {
        BitwiseLookupColumns::<F>::width()
    }
}

impl<F: PrimeField32> BaseAirWithPublicValues<F> for BitwiseLookupAir<F> {}



impl<AB: InteractionBuilder> Air<AB> for BitwiseLookupAir<AB::F>
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

        builder.assert_zero(*q_l * *a + *q_r * *b + *q_o * *c + *q_mul * (*a * *b) + *q_const);
    }
}

impl<F: PrimeField32> PartitionedBaseAir<F> for BitwiseLookupAir<F> {}