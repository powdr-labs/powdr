use itertools::Itertools;
use powdr_constraint_solver::constraint_system::ComputationMethod;
use rayon::prelude::*;
use std::collections::{BTreeMap, HashMap};
use std::fmt::Display;
use std::{cmp::Eq, hash::Hash};

use crate::expression::{AlgebraicExpression, AlgebraicReference};
use crate::{Apc, InstructionHandler};

pub struct OriginalRowReference<'a, D, I> {
    pub air_id: &'a I,
    pub row_index: usize,
    pub data: &'a D,
    pub length: usize,
}

impl<'a, D, I> OriginalRowReference<'a, D, I> {
    pub fn start(&self) -> usize {
        self.row_index * self.length
    }
}

pub struct TraceData<'a, F, D, I> {
    /// For each call of the apc, the values of each original instruction's dummy trace.
    pub dummy_values: Vec<Vec<OriginalRowReference<'a, D, I>>>,
    /// The mapping from dummy trace index to APC index for each instruction.
    pub dummy_trace_index_to_apc_index_by_instruction: Vec<Vec<(usize, usize)>>,
    /// The mapping from poly_id to the index in the list of apc columns.
    /// The values are always unique and contiguous.
    pub apc_poly_id_to_index: BTreeMap<u64, usize>,
    /// Indices of columns to compute and the way to compute them
    /// (from other values).
    pub columns_to_compute: &'a [(
        AlgebraicReference,
        ComputationMethod<F, AlgebraicExpression<F>>,
    )],
}

pub trait TraceTrait<F>: Send + Sync {
    type Values: Send + Sync;

    fn width(&self) -> usize;

    fn values(&self) -> &Self::Values;
}

pub fn generate_trace<'a, IH, M: TraceTrait<IH::Field>>(
    air_id_to_dummy_trace: &'a HashMap<IH::AirId, M>,
    instruction_handler: &'a IH,
    apc_call_count: usize,
    apc: &'a Apc<IH::Field, IH::Instruction>,
) -> TraceData<'a, IH::Field, M::Values, IH::AirId>
where
    IH: InstructionHandler,
    IH::Field: Display + Clone + Send + Sync,
    IH::AirId: Eq + Hash + Send + Sync,
{
    // Returns a vector with the same length as original instructions
    let original_instruction_air_ids = apc
        .instructions()
        .iter()
        .map(|instruction| {
            instruction_handler
                .get_instruction_air_and_id(instruction)
                .0
        })
        .collect::<Vec<_>>();

    let air_id_occurrences = original_instruction_air_ids.iter().counts();

    let apc_poly_id_to_index: BTreeMap<u64, usize> = apc
        .machine
        .main_columns()
        .enumerate()
        .map(|(index, c)| (c.id, index))
        .collect();

    let original_instruction_table_offsets = original_instruction_air_ids
        .iter()
        .scan(
            HashMap::default(),
            |counts: &mut HashMap<&IH::AirId, usize>, air_id| {
                let count = counts.entry(air_id).or_default();
                let current_count = *count;
                *count += 1;
                Some(current_count)
            },
        )
        .collect::<Vec<_>>();

    let dummy_trace_index_to_apc_index_by_instruction = apc
        .subs
        .iter()
        .map(|subs| {
            subs.iter()
                .map(|substitution| {
                    (
                        substitution.original_poly_index,
                        apc_poly_id_to_index[&substitution.apc_poly_id],
                    )
                })
                .collect_vec()
        })
        .collect();

    let dummy_values = (0..apc_call_count)
        .into_par_iter()
        .map(|trace_row| {
            original_instruction_air_ids
                .iter()
                .zip_eq(original_instruction_table_offsets.iter())
                .map(|(air_id, dummy_table_offset)| {
                    let (air_id, trace) = air_id_to_dummy_trace.get_key_value(air_id).unwrap();
                    let values = trace.values();
                    let width = trace.width();
                    let occurrences_per_record = air_id_occurrences.get(air_id).unwrap();
                    let row_index = trace_row * occurrences_per_record + dummy_table_offset;
                    OriginalRowReference {
                        data: values,
                        length: width,
                        air_id,
                        row_index,
                    }
                })
                .collect_vec()
        })
        .collect();

    let columns_to_compute = &apc.machine.derived_columns;

    TraceData {
        dummy_values,
        dummy_trace_index_to_apc_index_by_instruction,
        apc_poly_id_to_index,
        columns_to_compute,
    }
}
