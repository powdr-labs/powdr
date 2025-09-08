use crate::Instr;
use openvm_stark_backend::p3_field::PrimeField32;
use powdr_autoprecompiles::trace_handler::{DummyTrace, TraceHandler};
use std::collections::HashMap;

pub struct OpenVmTraceHandler<'a, F: PrimeField32> {
    pub air_id_to_dummy_trace_and_width: &'a HashMap<String, DummyTrace<F>>,
    pub original_instruction_air_ids: Vec<String>,
    pub apc_call_count: usize,
}

impl<'a, F: PrimeField32> OpenVmTraceHandler<'a, F> {
    pub fn new(
        air_id_to_dummy_trace_and_width: &'a HashMap<String, DummyTrace<F>>,
        original_instruction_air_ids: Vec<String>,
        apc_call_count: usize,
    ) -> Self {
        Self {
            air_id_to_dummy_trace_and_width,
            original_instruction_air_ids,
            apc_call_count,
        }
    }
}

impl<'a, F: PrimeField32> TraceHandler for OpenVmTraceHandler<'a, F> {
    type AirId = String;
    type Field = F;
    type Instruction = Instr<F>;

    fn original_instruction_air_ids(&self) -> Vec<Self::AirId> {
        self.original_instruction_air_ids.clone()
    }

    fn apc_call_count(&self) -> usize {
        self.apc_call_count
    }

    fn air_id_to_dummy_trace_and_width(&self) -> &'a HashMap<Self::AirId, DummyTrace<Self::Field>> {
        self.air_id_to_dummy_trace_and_width
    }
}
