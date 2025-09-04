use crate::powdr_extension::vm::OriginalInstruction;
use openvm_stark_backend::p3_field::PrimeField32;
use powdr_autoprecompiles::trace_handler::TraceHandler;
use std::collections::{BTreeMap, HashMap};

pub struct OpenVmTraceHandler<'a, F: PrimeField32> {
    pub original_instructions: &'a Vec<OriginalInstruction<F>>,
    pub column_index_by_poly_id: &'a BTreeMap<u64, usize>,
    pub air_id_to_dummy_trace_and_width: &'a HashMap<String, (Vec<F>, usize)>,
    pub original_instruction_air_ids: Vec<String>,
    pub num_trace_rows: usize,
}

impl<'a, F: PrimeField32> OpenVmTraceHandler<'a, F> {
    pub fn new(
        original_instructions: &'a Vec<OriginalInstruction<F>>,
        column_index_by_poly_id: &'a BTreeMap<u64, usize>,
        air_id_to_dummy_trace_and_width: &'a HashMap<String, (Vec<F>, usize)>,
        original_instruction_air_ids: Vec<String>,
        num_trace_rows: usize,
    ) -> Self {
        Self {
            original_instructions,
            column_index_by_poly_id,
            air_id_to_dummy_trace_and_width,
            original_instruction_air_ids,
            num_trace_rows,
        }
    }
}

impl<'a, F: PrimeField32> TraceHandler for OpenVmTraceHandler<'a, F> {
    type AirId = String;
    type Field = F;

    fn original_instruction_air_ids(&self) -> Vec<Self::AirId> {
        self.original_instruction_air_ids.clone()
    }

    fn original_instruction_subs(&self) -> Vec<Vec<u64>> {
        self.original_instructions
            .iter()
            .map(|instruction| instruction.subs.clone())
            .collect()
    }

    fn apc_poly_id_to_index(&self) -> &'a BTreeMap<u64, usize> {
        self.column_index_by_poly_id
    }

    fn num_trace_rows(&self) -> usize {
        self.num_trace_rows
    }

    fn air_id_to_dummy_trace_and_width(
        &self,
    ) -> &'a HashMap<Self::AirId, (Vec<Self::Field>, usize)> {
        self.air_id_to_dummy_trace_and_width
    }
}
