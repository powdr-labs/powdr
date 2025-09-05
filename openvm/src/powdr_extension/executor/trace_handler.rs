use crate::{extraction_utils::OriginalAirs, powdr_extension::vm::OriginalInstruction, customize_exe::Instr};
use powdr_autoprecompiles::adapter::Adapter;
use powdr_autoprecompiles::trace_handler::TraceHandler;
use std::collections::{BTreeMap, HashMap};

pub struct OpenVmTraceHandler<'a, A: Adapter> {
    pub original_instructions: &'a Vec<A::Instruction>,
    pub column_index_by_poly_id: &'a BTreeMap<u64, usize>,
    pub air_id_to_dummy_trace_and_width: &'a HashMap<A::AirId, (Vec<A::Field>, usize)>,
    pub instruction_handler: &'a A::InstructionHandler,
    pub apc_call_count: usize,
    pub subs: Vec<Vec<u64>>,
}

impl<'a, A: Adapter> OpenVmTraceHandler<'a, A> {
    pub fn new(
        original_instructions: &'a Vec<A::Instruction>,
        column_index_by_poly_id: &'a BTreeMap<u64, usize>,
        air_id_to_dummy_trace_and_width: &'a HashMap<A::AirId, (Vec<A::Field>, usize)>,
        instruction_handler: &'a A::InstructionHandler,
        apc_call_count: usize,
        subs: Vec<Vec<u64>>,
    ) -> Self {
        Self {
            original_instructions,
            column_index_by_poly_id,
            air_id_to_dummy_trace_and_width,
            instruction_handler,
            apc_call_count,
            subs,
        }
    }
}

impl<'a, A: Adapter> TraceHandler<A> for OpenVmTraceHandler<'a, A> {

    fn original_instructions(&self) -> Vec<A::Instruction> {
        self.original_instructions.clone()
    }

    fn instruction_handler(&self) -> &A::InstructionHandler {
        self.instruction_handler
    }

    fn original_instruction_subs(&self) -> Vec<Vec<u64>> {
        self.subs.clone()
    }

    fn apc_poly_id_to_index(&self) -> &'a BTreeMap<u64, usize> {
        self.column_index_by_poly_id
    }

    fn apc_call_count(&self) -> usize {
        self.apc_call_count
    }

    fn air_id_to_dummy_trace_and_width(
        &self,
    ) -> &'a HashMap<A::AirId, (Vec<A::Field>, usize)> {
        self.air_id_to_dummy_trace_and_width
    }
}
