use openvm_circuit_primitives::AlignedBorrow;
use openvm_stark_backend::{
    interaction::InteractionBuilder,
    p3_air::{Air, BaseAir},
    p3_field::PrimeField32,
    p3_matrix::Matrix,
    rap::{BaseAirWithPublicValues, ColumnsAir, PartitionedBaseAir},
};
use powdr_autoprecompiles::bus_map::{BusMap, BusType};
use std::borrow::Borrow;
use struct_reflection::{StructReflection, StructReflectionHelper};

// This BitwiseLookupAir captures the bitwise bus interaction with structure:
// bus_interaction(BITWISE_LOOKUP, [A, B, C, <const>], D * <const>);
#[repr(C)]
#[derive(AlignedBorrow, StructReflection)]
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
    bus_map: BusMap,
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

        let BitwiseLookupColumns {
            q_const_arg,
            q_const_mult,
            a,
            b,
            c,
            d,
        } = (*local).borrow();

        builder.push_interaction(
            self.bus_map
                .get_bus_id(&BusType::BitwiseLookup)
                .expect("BusType::BitwiseLookup not found in bus_map") as u16,
            vec![*a, *b, *c, *q_const_arg],
            *d * *q_const_mult,
            1,
        );
    }
}

impl<F: PrimeField32> PartitionedBaseAir<F> for BitwiseLookupAir<F> {}
