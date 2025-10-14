use itertools::Itertools;
use powdr_constraint_solver::constraint_system::ComputationMethod;
use rayon::prelude::*;
use std::collections::{BTreeMap, HashMap};
use std::fmt::Display;
use std::{cmp::Eq, hash::Hash};

use crate::expression::{AlgebraicExpression, AlgebraicReference};
use crate::{Apc, InstructionHandler};

pub struct TraceData<'a, F> {
    /// For each call of the apc, the values of each original instruction's dummy trace.
    pub dummy_values: Vec<Vec<&'a [F]>>,
    /// The mapping from dummy trace index to APC index for each instruction.
    pub dummy_trace_index_to_apc_index_by_instruction: Vec<Vec<(usize, usize)>>,
    /// The mapping from poly_id to the index in the list of apc columns.
    /// The values are always unique and contiguous.
    pub apc_poly_id_to_index: BTreeMap<u64, usize>,
    /// Indices of columns to compute and the way to compute them
    /// (from other values).
    pub columns_to_compute:
        BTreeMap<AlgebraicReference, ComputationMethod<F, AlgebraicExpression<F>>>,
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

pub fn generate_trace<'a, IH>(
    air_id_to_dummy_trace: &'a HashMap<IH::AirId, Trace<IH::Field>>,
    instruction_handler: &'a IH,
    apc_call_count: usize,
    apc: &Apc<IH::Field, IH::Instruction>,
    field_unity: IH::Field,
) -> TraceData<'a, IH::Field>
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
                .enumerate()
                .filter_map(|(dummy_index, poly_id)| {
                    // Check if this dummy column is present in the final apc row
                    apc_poly_id_to_index
                        .get(poly_id)
                        // If it is, map the dummy index to the apc index
                        .map(|apc_index| (dummy_index, *apc_index))
                })
                .collect()
        })
        .collect();

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

    // The "is_valid" column
    let is_valid_column = AlgebraicReference {
        name: "is_valid".to_string().into(),
        id: apc.is_valid_poly_id(),
    };

    let columns_to_compute = [(is_valid_column, ComputationMethod::Constant(field_unity))]
        .into_iter()
        .chain(apc.machine.derived_columns.iter().cloned())
        .collect();

    TraceData {
        dummy_values,
        dummy_trace_index_to_apc_index_by_instruction,
        apc_poly_id_to_index,
        columns_to_compute,
    }
}
