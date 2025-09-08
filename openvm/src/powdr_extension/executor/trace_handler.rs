use crate::Instr;
use openvm_stark_backend::p3_field::PrimeField32;
use powdr_autoprecompiles::{trace_handler::TraceHandler, Apc};
use std::{
    collections::{BTreeMap, HashMap},
    sync::Arc,
};

pub struct OpenVmTraceHandler<'a, F: PrimeField32> {
    pub column_index_by_poly_id: &'a BTreeMap<u64, usize>,
    pub air_id_to_dummy_trace_and_width: &'a HashMap<String, (Vec<F>, usize)>,
    pub original_instruction_air_ids: Vec<String>,
    pub apc_call_count: usize,
    pub apc: Arc<Apc<F, Instr<F>>>,
}

impl<'a, F: PrimeField32> OpenVmTraceHandler<'a, F> {
    pub fn new(
        column_index_by_poly_id: &'a BTreeMap<u64, usize>,
        air_id_to_dummy_trace_and_width: &'a HashMap<String, (Vec<F>, usize)>,
        original_instruction_air_ids: Vec<String>,
        apc_call_count: usize,
        apc: Arc<Apc<F, Instr<F>>>,
    ) -> Self {
        Self {
            column_index_by_poly_id,
            air_id_to_dummy_trace_and_width,
            original_instruction_air_ids,
            apc_call_count,
            apc,
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
        self.apc.subs.clone()
    }

    fn apc_poly_id_to_index(&self) -> &'a BTreeMap<u64, usize> {
        self.column_index_by_poly_id
    }

    fn apc_call_count(&self) -> usize {
        self.apc_call_count
    }

    fn air_id_to_dummy_trace_and_width(
        &self,
    ) -> &'a HashMap<Self::AirId, (Vec<Self::Field>, usize)> {
        self.air_id_to_dummy_trace_and_width
    }
}
