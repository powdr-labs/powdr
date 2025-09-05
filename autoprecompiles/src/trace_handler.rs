use itertools::Itertools;
use rayon::prelude::*;
use std::cmp::Eq;
use std::collections::{BTreeMap, HashMap};
use std::hash::Hash;

/// Returns data needed for constructing the APC trace.
pub struct TraceHandlerData<'a, F> {
    /// The dummy trace values for each instruction.
    pub dummy_values: Vec<Vec<&'a [F]>>,
    /// The mapping from dummy trace index to APC index for each instruction.
    pub dummy_trace_index_to_apc_index_by_instruction: Vec<HashMap<usize, usize>>,
}

pub trait TraceHandler {
    type AirId: Hash + Eq + Sync;
    type Field: Sync;

    /// Returns a vector with the same length as original instructions
    fn original_instruction_air_ids(&self) -> Vec<Self::AirId>;

    fn original_instruction_subs(&self) -> Vec<Vec<u64>>;

    fn apc_poly_id_to_index(&self) -> &BTreeMap<u64, usize>;

    fn apc_call_count(&self) -> usize;

    fn air_id_to_dummy_trace_and_width(&self) -> &HashMap<Self::AirId, (Vec<Self::Field>, usize)>;

    fn data<'a>(&'a self) -> TraceHandlerData<'a, Self::Field> {
        let air_id_to_dummy_trace_and_width = self.air_id_to_dummy_trace_and_width();

        let original_instruction_air_ids = self.original_instruction_air_ids();

        let air_id_occurrences = original_instruction_air_ids.iter().counts();

        let apc_poly_id_to_index = self.apc_poly_id_to_index();

        let original_instruction_table_offsets = original_instruction_air_ids
            .iter()
            .scan(
                HashMap::default(),
                |counts: &mut HashMap<&Self::AirId, usize>, air_id| {
                    let count = counts.entry(air_id).or_default();
                    let current_count = *count;
                    *count += 1;
                    Some(current_count)
                },
            )
            .collect::<Vec<_>>();

        let dummy_trace_index_to_apc_index_by_instruction = self
            .original_instruction_subs()
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

        let dummy_values = (0..self.apc_call_count())
            .into_par_iter()
            .map(|trace_row| {
                original_instruction_air_ids
                    .iter()
                    .zip_eq(original_instruction_table_offsets.iter())
                    .map(|(air_id, dummy_table_offset)| {
                        let (dummy_table, width) =
                            air_id_to_dummy_trace_and_width.get(air_id).unwrap();
                        let occurrences_per_record = air_id_occurrences.get(air_id).unwrap();
                        let start =
                            (trace_row * occurrences_per_record + dummy_table_offset) * width;
                        let end = start + width;
                        &dummy_table[start..end]
                    })
                    .collect_vec()
            })
            .collect();

        TraceHandlerData {
            dummy_values,
            dummy_trace_index_to_apc_index_by_instruction,
        }
    }
}
