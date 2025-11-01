use crate::{
    bus_map::{BusMap, OpenVmBusType},
    extraction_utils::get_air_metrics,
    AirMetrics, BusType, Instr,
};
use itertools::Itertools;
use openvm_circuit_primitives::AlignedBorrow;
use openvm_stark_backend::{
    interaction::InteractionBuilder,
    p3_air::{Air, BaseAir},
    p3_field::{FieldAlgebra, PrimeField32},
    p3_matrix::Matrix,
    rap::{BaseAirWithPublicValues, ColumnsAir, PartitionedBaseAir},
};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::{adapter::PowdrArithmetization, Apc};
use std::{borrow::Borrow, sync::Arc};
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
    pub execution_bus_id: u16,
    pub memory_bus_id: u16,
    pub pc_lookup_bus_id: u16,
    pub variable_range_checker_bus_id: u16,
    pub tuple_range_checker_bus_id: u16,
    pub bitwise_lookup_bus_id: u16,
    pub _marker: std::marker::PhantomData<F>,
}

impl<F> PlonkAir<F> {
    /// create a dummy plonk air, when we only care about the shape (number of columns and constraints)
    fn dummy() -> Self {
        Self {
            copy_constraint_bus_id: 0,
            execution_bus_id: 1,
            memory_bus_id: 2,
            pc_lookup_bus_id: 3,
            variable_range_checker_bus_id: 4,
            tuple_range_checker_bus_id: 5,
            bitwise_lookup_bus_id: 6,
            _marker: std::marker::PhantomData,
        }
    }

    pub fn new(copy_constraint_bus_id: u16, bus_map: &BusMap) -> Self {
        Self {
            copy_constraint_bus_id,
            execution_bus_id: bus_map.get_bus_id(&BusType::ExecutionBridge).unwrap() as u16,
            memory_bus_id: bus_map.get_bus_id(&BusType::Memory).unwrap() as u16,
            pc_lookup_bus_id: bus_map.get_bus_id(&BusType::PcLookup).unwrap() as u16,
            variable_range_checker_bus_id: bus_map
                .get_bus_id(&BusType::Other(OpenVmBusType::VariableRangeChecker))
                .unwrap() as u16,
            tuple_range_checker_bus_id: bus_map
                .get_bus_id(&BusType::Other(OpenVmBusType::TupleRangeChecker))
                .unwrap() as u16,
            bitwise_lookup_bus_id: bus_map
                .get_bus_id(&BusType::Other(OpenVmBusType::BitwiseLookup))
                .unwrap() as u16,
            _marker: std::marker::PhantomData,
        }
    }
}

impl PowdrArithmetization<BabyBear, Instr<BabyBear>, AirMetrics> for PlonkAir<BabyBear> {
    fn get_apc_metrics(
        _: Arc<Apc<BabyBear, Instr<BabyBear>>>,
        max_constraint_degree: usize,
    ) -> AirMetrics {
        get_air_metrics(Arc::new(PlonkAir::dummy()), max_constraint_degree)
    }
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
        builder.push_interaction(self.execution_bus_id, vec![*a, *b], *c * *q_execution, 1);

        builder.push_interaction(
            self.variable_range_checker_bus_id,
            vec![*a, *b],
            *c * *q_range_check,
            1,
        );

        builder.push_interaction(
            self.tuple_range_checker_bus_id,
            vec![*a, *b],
            *c * *q_range_tuple,
            1,
        );

        builder.push_interaction(
            self.bitwise_lookup_bus_id,
            vec![*a, *b, *c, *d],
            *e * *q_bitwise,
            1,
        );

        builder.push_interaction(
            self.pc_lookup_bus_id,
            vec![*a, *b, *c, *d, *e, *a_next, *b_next, *c_next, *d_next],
            *q_pc * *e_next,
            1,
        );

        builder.push_interaction(
            self.memory_bus_id,
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
