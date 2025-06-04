use openvm_circuit_primitives::AlignedBorrow;
use openvm_stark_backend::{
    interaction::InteractionBuilder,
    p3_air::{Air, BaseAir},
    p3_field::PrimeField32,
    p3_matrix::Matrix,
    rap::{BaseAirWithPublicValues, ColumnsAir, PartitionedBaseAir},
};
use crate::bus_interaction_handler::{DEFAULT_BITWISE_LOOKUP, DEFAULT_EXECUTION_BRIDGE, DEFAULT_MEMORY, DEFAULT_PC_LOOKUP, DEFAULT_TUPLE_RANGE_CHECKER, DEFAULT_VARIABLE_RANGE_CHECKER};
use std::borrow::Borrow;
use struct_reflection::StructReflection;
use struct_reflection::StructReflectionHelper;


#[repr(C)]
#[derive(AlignedBorrow, StructReflection)]
pub struct PlonkColumns<T> {
    pub q_l: T,
    pub q_r: T,
    pub q_o: T,
    pub q_mul: T,
    pub q_const: T,

    pub q_bitwise: T,
    pub q_memory: T,
    pub q_range_check: T,
    pub q_execution: T,
    pub q_pc: T,
    pub q_range_tuple: T,

    pub a: T,
    pub b: T,
    pub c: T,
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
    AB::Var: std::fmt::Debug,
{
    fn eval(&self, builder: &mut AB) {
        let main = builder.main();
        let local = main.row_slice(0);
        let local_next = main.row_slice(1);
        let local_next_next = main.row_slice(2);


        let PlonkColumns {
            q_l,
            q_r,
            q_o,
            q_mul,
            q_const,
            q_bitwise,
            q_memory,
            q_range_check,
            q_execution,
            q_pc,
            q_range_tuple,
            a,
            b,
            c,
        } = (*local).borrow();

        

        

        let PlonkColumns {
            q_l: q_l_next,
            q_r: q_r_next,
            q_o: q_o_next,
            q_mul: q_mul_next,
            q_const: q_const_next,
            q_bitwise: q_bitwise_next,
            q_memory: q_memory_next,
            q_range_check: q_range_check_next,
            q_execution: q_execution_next,
            q_pc: q_pc_next,
            q_range_tuple: q_range_tuple_next,
            a: a_next,
            b: b_next,
            c: c_next,
        } = (*local_next).borrow();

        let PlonkColumns {
            q_l: q_l_next_next,
            q_r: q_r_next_next,
            q_o: q_o_next_next,
            q_mul: q_mul_next_next,
            q_const: q_const_next_next,
            q_bitwise: q_bitwise_next_next,
            q_memory: q_memory_next_next,
            q_range_check: q_range_check_next_next,
            q_execution: q_execution_next_next,
            q_pc: q_pc_next_next,
            q_range_tuple: q_range_tuple_next_next,
            a: a_next_next,
            b: b_next_next,
            c: c_next_next,
        } = (*local_next_next).borrow();
        

        builder.assert_zero(*q_l * *a + *q_r * *b + *q_o * *c + *q_mul * (*a * *b) + *q_const);
        
    
        builder.push_interaction(
            DEFAULT_EXECUTION_BRIDGE as u16,
            vec![*a, *b],
            *c * *q_execution,
            1,
        );

        builder.push_interaction(
            DEFAULT_PC_LOOKUP as u16,
            vec![*a],
            *b * *q_pc,
            1,
        );

        builder.push_interaction(
            DEFAULT_VARIABLE_RANGE_CHECKER as u16,
            vec![*a, *b],
            *c * *q_range_check,
            1,
        );

        builder.push_interaction(
            DEFAULT_BITWISE_LOOKUP as u16,
            vec![*a, *b, *c, *a_next],
            *b_next * *q_bitwise,
            1,
        );

        builder.push_interaction(
            DEFAULT_MEMORY as u16,
            vec![*a, *b, *c, *a_next, *b_next, *c_next, *a_next_next],
            *b_next_next * *q_memory,
            1,
        );


    }
}

impl<F: PrimeField32> PartitionedBaseAir<F> for PlonkAir<F> {}
