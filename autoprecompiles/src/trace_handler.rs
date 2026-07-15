use itertools::Itertools;
use powdr_constraint_solver::constraint_system::DerivedVariable;
use rayon::prelude::*;
use std::collections::{BTreeMap, HashMap};
use std::fmt::Display;
use std::{cmp::Eq, hash::Hash};

use crate::blocks::PcStep;
use crate::expression::{AlgebraicExpression, AlgebraicReference};
use crate::{Apc, InstructionHandler};

pub struct OriginalRowReference<'a, D> {
    pub data: &'a D,
    pub start: usize,
    pub length: usize,
}

pub struct TraceData<'a, F, D> {
    /// For each call of the apc, the values of each original instruction's dummy trace.
    pub dummy_values: Vec<Vec<OriginalRowReference<'a, D>>>,
    /// The mapping from dummy trace index to APC index for each instruction.
    pub dummy_trace_index_to_apc_index_by_instruction: Vec<Vec<(usize, usize)>>,
    /// The mapping from a column's poly_id to its location `(instruction, index)` in the per-call
    /// dummy trace. It covers every substituted column: both those surviving in the APC and those
    /// removed from it but still referenced by a derived column's computation method. Witgen uses it
    /// to read a derived column's inputs directly from the original instruction trace, so removed
    /// columns need no separate plumbing. Since derived columns never depend on other derived
    /// columns, every column referenced by a computation method is backed by the dummy trace and is
    /// therefore present here.
    pub apc_poly_id_to_dummy_index: BTreeMap<u64, (usize, usize)>,
    /// The mapping from poly_id to the index in the list of apc columns.
    /// The values are always unique and contiguous.
    pub apc_poly_id_to_index: BTreeMap<u64, usize>,
    /// Indices of columns to compute and the way to compute them
    /// (from other values).
    pub columns_to_compute: &'a [DerivedVariable<F, AlgebraicReference, AlgebraicExpression<F>>],
}

pub trait TraceTrait<F>: Send + Sync {
    type Values: Send + Sync;

    fn width(&self) -> usize;

    fn values(&self) -> &Self::Values;
}

// TODO: refactor `Apc` so we don't have to pass A, V here
pub fn generate_trace<'a, IH, M: TraceTrait<IH::Field>, A, V>(
    air_id_to_dummy_trace: &'a HashMap<IH::AirId, M>,
    instruction_handler: &'a IH,
    apc_call_count: usize,
    apc: &'a Apc<IH::Field, IH::Instruction, A, V>,
) -> TraceData<'a, IH::Field, M::Values>
where
    IH: InstructionHandler,
    IH::Field: Display + Clone + Send + Sync,
    IH::AirId: Eq + Hash + Send + Sync,
    IH::Instruction: PcStep,
{
    // Keep only instructions that produce dummy records
    let instructions_with_subs = apc
        .instructions()
        .zip_eq(apc.subs.iter())
        .filter(|(_, subs)| !subs.is_empty());
    let instructions_with_subs = instructions_with_subs.collect::<Vec<_>>();

    let original_instruction_air_ids = instructions_with_subs
        .iter()
        .map(|(instruction, _)| {
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

    // For each instruction, map each substitution's dummy-trace index to its APC column index (for
    // surviving columns, which are copied directly into the APC row), and record every substituted
    // column's location in the dummy trace keyed by poly_id. Columns removed from the APC but still
    // referenced by a derived column have no APC index; witgen recovers their values from the dummy
    // trace via `apc_poly_id_to_dummy_index`. In the native path every substitution targets a
    // surviving column, so the surviving mapping is identical to the previous unconditional mapping.
    let mut dummy_trace_index_to_apc_index_by_instruction: Vec<Vec<(usize, usize)>> =
        Vec::with_capacity(instructions_with_subs.len());
    let mut apc_poly_id_to_dummy_index: BTreeMap<u64, (usize, usize)> = BTreeMap::new();
    for (instruction_index, (_, subs)) in instructions_with_subs.iter().enumerate() {
        let mut surviving = Vec::new();
        for substitution in subs.iter() {
            apc_poly_id_to_dummy_index.insert(
                substitution.apc_poly_id,
                (instruction_index, substitution.original_poly_index),
            );
            if let Some(index) = apc_poly_id_to_index.get(&substitution.apc_poly_id) {
                surviving.push((substitution.original_poly_index, *index));
            }
        }
        dummy_trace_index_to_apc_index_by_instruction.push(surviving);
    }

    let dummy_values = (0..apc_call_count)
        .into_par_iter()
        .map(|trace_row| {
            original_instruction_air_ids
                .iter()
                .zip_eq(original_instruction_table_offsets.iter())
                .map(|(air_id, dummy_table_offset)| {
                    let trace = air_id_to_dummy_trace.get(air_id).unwrap();
                    let values = trace.values();
                    let width = trace.width();
                    let occurrences_per_record = air_id_occurrences.get(air_id).unwrap();
                    let start = (trace_row * occurrences_per_record + dummy_table_offset) * width;
                    OriginalRowReference {
                        data: values,
                        start,
                        length: width,
                    }
                })
                .collect_vec()
        })
        .collect();

    let columns_to_compute = &apc.machine.derived_columns;

    TraceData {
        dummy_values,
        dummy_trace_index_to_apc_index_by_instruction,
        apc_poly_id_to_dummy_index,
        apc_poly_id_to_index,
        columns_to_compute,
    }
}
