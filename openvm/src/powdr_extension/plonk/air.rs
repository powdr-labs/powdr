use crate::{BusMap, BusType};
use openvm_circuit_primitives::AlignedBorrow;
use openvm_sdk::F;
use openvm_stark_backend::{
    interaction::InteractionBuilder,
    p3_air::{Air, AirBuilder, BaseAir},
    p3_field::PrimeField32,
    p3_matrix::Matrix,
    rap::{BaseAirWithPublicValues, ColumnsAir, PartitionedBaseAir},
};
use std::{borrow::Borrow, ops::Neg};
use struct_reflection::StructReflection;
use struct_reflection::StructReflectionHelper;
use openvm_stark_backend::p3_field::FieldAlgebra;
use openvm_stark_backend::air_builders::symbolic::symbolic_expression::SymbolicExpression;

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

    pub a_id:T,
    pub b_id: T,
    pub c_id: T,
    pub d_id: T,
    pub e_id: T,

    pub a_perm: T,
    pub b_perm: T,
    pub c_perm: T,
    pub d_perm: T,
    pub e_perm: T,  

    pub a: T,
    pub b: T,
    pub c: T,
    pub d: T,
    pub e: T,
}

pub struct PlonkAir<F> {
    pub bus_map: BusMap,
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
        let local_next = main.row_slice(1);

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
            q_range_tuple: _,
            a_id,
            b_id,
            c_id,
            d_id,
            e_id,
            a_perm,
            b_perm,
            c_perm,
            d_perm,
            e_perm,
            a,
            b,
            c,
            d,
            e,
        } = (*local).borrow();

        let PlonkColumns {
            q_l: _,
            q_r: _,
            q_o: _,
            q_mul: _,
            q_const: _,
            q_bitwise: _,
            q_memory: _,
            q_range_check: _,
            q_execution: _,
            q_pc: _,
            q_range_tuple: _,
            a_id: _,
            b_id: _,
            c_id: _,
            d_id: _,    
            e_id: _,
            a_perm: _,
            b_perm: _,
            c_perm: _,
            d_perm: _,
            e_perm: _,
            a: a_next,
            b: b_next,
            c: c_next,
            d: d_next,
            e: e_next,
        } = (*local_next).borrow();

        builder.assert_zero(*q_l * *a + *q_r * *b + *q_o * *c + *q_mul * (*a * *b) + *q_const);

        // OpenVM bus interactions
        builder.push_interaction(
            self.bus_map
                .get_bus_id(&BusType::ExecutionBridge)
                .expect("BusType::ExecutionBridge not found in bus_map") as u16,
            vec![*a, *b],
            *c * *q_execution,
            1,
        );

        builder.push_interaction(
            self.bus_map
                .get_bus_id(&BusType::VariableRangeChecker)
                .expect("BusType::VariableRangeChecker not found in bus_map") as u16,
            vec![*a, *b],
            *c * *q_range_check,
            1,
        );

        builder.push_interaction(
            self.bus_map
                .get_bus_id(&BusType::BitwiseLookup)
                .expect("BusType::BitwiseLookup not found in bus_map") as u16,
            vec![*a, *b, *c, *d],
            *e * *q_bitwise,
            1,
        );

        builder.push_interaction(
            self.bus_map
                .get_bus_id(&BusType::PcLookup)
                .expect("BusType::PcLookup not found in bus_map") as u16,
            vec![*a, *b, *c, *d, *e, *a_next, *b_next, *c_next, *d_next],
            *q_pc * *e_next,
            1,
        );

        builder.push_interaction(
            self.bus_map
                .get_bus_id(&BusType::Memory)
                .expect("BusType::PcLookup not found in bus_map") as u16,
            vec![*a, *b, *c, *d, *e, *a_next, *b_next],
            *q_memory * *c_next,
            1,
        );
        // copy constraints are active in every row
        let q_copy_constraint: AB::Expr=AB::F::from_canonical_u64(1 << 32).into();

        // Copy constraints 
        // find a proper way to define bus index.
        builder.push_interaction(10, vec![*a,*a_id], q_copy_constraint.clone(), 1);
        builder.push_interaction(10, vec![*b,*b_id], q_copy_constraint.clone(), 1);
        builder.push_interaction(10, vec![*c,*c_id], q_copy_constraint.clone(), 1);
        builder.push_interaction(10, vec![*d,*d_id], q_copy_constraint.clone(), 1);
        builder.push_interaction(10, vec![*e,*e_id], q_copy_constraint.clone(), 1);

        builder.push_interaction(10, vec![*a,*a_perm], q_copy_constraint.clone(), 1);
        builder.push_interaction(10, vec![*b,*b_perm], q_copy_constraint.clone(), 1);
        builder.push_interaction(10, vec![*c,*c_perm], q_copy_constraint.clone(), 1);
        builder.push_interaction(10, vec![*d,*d_perm], q_copy_constraint.clone(), 1);
        builder.push_interaction(10, vec![*e,*e_perm], q_copy_constraint, 1);

    }
}

impl<F: PrimeField32> PartitionedBaseAir<F> for PlonkAir<F> {}
