use itertools::Itertools;
use rayon::prelude::*;
use std::collections::{BTreeMap, HashMap};
use std::{cmp::Eq, hash::Hash};

use crate::{Apc, InstructionHandler};

/// Returns data needed for constructing the APC trace.
pub struct TraceData<'a, F> {
    /// The dummy trace values for each instruction.
    pub dummy_values: Vec<Vec<&'a [F]>>,
    /// The mapping from dummy trace index to APC index for each instruction.
    pub dummy_trace_index_to_apc_index_by_instruction: Vec<HashMap<usize, usize>>,
    /// The mapping from poly_id to the index in the list of apc columns.
    /// The values are always unique and contiguous.
    pub apc_poly_id_to_index: BTreeMap<u64, usize>,
    /// The index of is_valid column in the list of apc columns.
    pub is_valid_index: usize,
}

pub struct Trace<F> {
    pub values: Vec<F>,
    pub width: usize,
}

impl<F> Trace<F> {
    pub fn new(values: Vec<F>, width: usize) -> Self {
        Self { values, width }
    }
}

pub fn generate_trace<'a, ID, F, I, IH>(
    air_id_to_dummy_trace: &'a HashMap<ID, Trace<F>>,
    instruction_handler: &'a IH,
    apc_call_count: usize,
    apc: &Apc<F, I>,
) -> TraceData<'a, F>
where
    F: Send + Sync,
    IH: InstructionHandler<F, I, ID>,
    ID: Eq + Hash + Send + Sync,
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

    let is_valid_index = apc_poly_id_to_index[&apc.is_valid_poly_id()];

    let original_instruction_table_offsets = original_instruction_air_ids
        .iter()
        .scan(
            HashMap::default(),
            |counts: &mut HashMap<&ID, usize>, air_id| {
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
            let mut dummy_trace_index_to_apc_index = HashMap::new();
            for (dummy_index, poly_id) in subs.iter().enumerate() {
                if let Some(apc_index) = apc_poly_id_to_index.get(poly_id) {
                    dummy_trace_index_to_apc_index.insert(dummy_index, *apc_index);
                }
            }
            dummy_trace_index_to_apc_index
        })
        .collect::<Vec<_>>();

    let dummy_values = (0..apc_call_count)
        .into_par_iter()
        .map(|trace_row| {
            original_instruction_air_ids
                .iter()
                .zip_eq(original_instruction_table_offsets.iter())
                .map(|(air_id, dummy_table_offset)| {
                    let Trace { values, width } = air_id_to_dummy_trace.get(air_id).unwrap();
                    let occurrences_per_record = air_id_occurrences.get(air_id).unwrap();
                    let start = (trace_row * occurrences_per_record + dummy_table_offset) * width;
                    let end = start + width;
                    &values[start..end]
                })
                .collect_vec()
        })
        .collect();

    TraceData {
        dummy_values,
        dummy_trace_index_to_apc_index_by_instruction,
        apc_poly_id_to_index,
        is_valid_index,
    }
}
