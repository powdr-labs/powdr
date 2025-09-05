use crate::{extraction_utils::OriginalAirs, powdr_extension::vm::OriginalInstruction, customize_exe::{BabyBearOpenVmApcAdapter, Instr}};
use openvm_stark_backend::p3_field::PrimeField32;
use powdr_autoprecompiles::adapter::Adapter;
use powdr_autoprecompiles::trace_handler::TraceHandler;
use std::collections::{BTreeMap, HashMap};

pub struct OpenVmTraceHandler<'a, F: PrimeField32> {
    pub original_instructions: &'a Vec<OriginalInstruction<F>>,
    pub column_index_by_poly_id: &'a BTreeMap<u64, usize>,
    pub air_id_to_dummy_trace_and_width: &'a HashMap<String, (Vec<F>, usize)>,
    pub instruction_handler: &'a OriginalAirs<F>,
    pub apc_call_count: usize,
}

impl<'a, F: PrimeField32> OpenVmTraceHandler<'a, F> {
    pub fn new(
        original_instructions: &'a Vec<OriginalInstruction<F>>,
        column_index_by_poly_id: &'a BTreeMap<u64, usize>,
        air_id_to_dummy_trace_and_width: &'a HashMap<String, (Vec<F>, usize)>,
        instruction_handler: &'a OriginalAirs<F>,
        apc_call_count: usize,
    ) -> Self {
        Self {
            original_instructions,
            column_index_by_poly_id,
            air_id_to_dummy_trace_and_width,
            instruction_handler,
            apc_call_count,
        }
    }
}

impl<'a, F: PrimeField32> TraceHandler<BabyBearOpenVmApcAdapter<'a>> for OpenVmTraceHandler<'a, F> {

    fn original_instructions(&self) -> Vec<<BabyBearOpenVmApcAdapter<'a> as Adapter>::Instruction> {
        self.original_instructions.iter().map(|instruction| Instr(instruction.instruction.clone())).collect()
    }

    fn instruction_handler(&self) -> &<BabyBearOpenVmApcAdapter<'a> as Adapter>::InstructionHandler {
        &self.instruction_handler
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

    fn apc_call_count(&self) -> usize {
        self.apc_call_count
    }

    fn air_id_to_dummy_trace_and_width(
        &self,
    ) -> &'a HashMap<<BabyBearOpenVmApcAdapter as Adapter>::AirId, (Vec<<BabyBearOpenVmApcAdapter as Adapter>::Field>, usize)> {
        self.air_id_to_dummy_trace_and_width
    }
}
