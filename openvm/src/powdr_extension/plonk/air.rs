use crate::{BusMap, BusType};
use itertools::Itertools;
use openvm_circuit_primitives::AlignedBorrow;
use openvm_stark_backend::{
    interaction::InteractionBuilder,
    p3_air::{Air, BaseAir},
    p3_field::{FieldAlgebra, PrimeField32},
    p3_matrix::Matrix,
    rap::{BaseAirWithPublicValues, ColumnsAir, PartitionedBaseAir},
};
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

    pub a_id: T,
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
    pub copy_constraint_bus_id: u16,
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
            q_range_tuple,
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
            a: a_next,
            b: b_next,
            c: c_next,
            d: d_next,
            e: e_next,
            ..
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
                .get_bus_id(&BusType::TupleRangeChecker)
                .expect("BusType::VariableRangeChecker not found in bus_map") as u16,
            vec![*a, *b],
            *c * *q_range_tuple,
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
        // copy constraints are active in every row, so the multiplicity is always 1 or -1.
        let q_copy_constraint: AB::Expr = AB::F::from_canonical_u64(1).into();

        let witness_columns = [*a, *b, *c, *d, *e];
        let witness_ids = [*a_id, *b_id, *c_id, *d_id, *e_id];
        let witness_perms = [*a_perm, *b_perm, *c_perm, *d_perm, *e_perm];

        for ((column, id), perm) in witness_columns
            .iter()
            .zip_eq(witness_ids.iter())
            .zip_eq(witness_perms.iter())
        {
            builder.push_interaction(
                self.copy_constraint_bus_id,
                vec![*column, *id],
                q_copy_constraint.clone(),
                1,
            );
            builder.push_interaction(
                self.copy_constraint_bus_id,
                vec![*column, *perm],
                -q_copy_constraint.clone(),
                1,
            );
        }
    }
}

impl<F: PrimeField32> PartitionedBaseAir<F> for PlonkAir<F> {}
