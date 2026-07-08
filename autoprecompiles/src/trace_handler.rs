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
    /// For each instruction, the mapping from dummy trace index to the poly_id of a column that was
    /// substituted out of the APC (so it has no index in `apc_poly_id_to_index`) but is still
    /// referenced by a derived column's computation method. Witgen reads these values from the
    /// original instruction trace to evaluate the derived columns. Empty in the native optimizer
    /// path, where derived columns only reference surviving (indexed) columns.
    pub removed_column_dummy_index_by_instruction: Vec<Vec<(usize, u64)>>,
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

    // Partition each instruction's substitutions into surviving columns (those with an index in
    // `apc_poly_id_to_index`, copied directly into the APC row) and removed-but-referenced columns
    // (substituted out of the APC yet referenced by a derived column, whose values witgen reads from
    // the original trace to evaluate derived columns). In the native path every substitution targets
    // a surviving column, so the removed partition is empty and the surviving partition is identical
    // to the previous unconditional mapping.
    let mut dummy_trace_index_to_apc_index_by_instruction: Vec<Vec<(usize, usize)>> =
        Vec::with_capacity(instructions_with_subs.len());
    let mut removed_column_dummy_index_by_instruction: Vec<Vec<(usize, u64)>> =
        Vec::with_capacity(instructions_with_subs.len());
    for (_, subs) in &instructions_with_subs {
        let mut surviving = Vec::new();
        let mut removed = Vec::new();
        for substitution in subs.iter() {
            match apc_poly_id_to_index.get(&substitution.apc_poly_id) {
                Some(index) => surviving.push((substitution.original_poly_index, *index)),
                None => removed.push((substitution.original_poly_index, substitution.apc_poly_id)),
            }
        }
        dummy_trace_index_to_apc_index_by_instruction.push(surviving);
        removed_column_dummy_index_by_instruction.push(removed);
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
        removed_column_dummy_index_by_instruction,
        apc_poly_id_to_index,
        columns_to_compute,
    }
}
