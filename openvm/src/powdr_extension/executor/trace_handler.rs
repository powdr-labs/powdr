use powdr_autoprecompiles::trace_handler::TraceHandler;
use powdr_autoprecompiles::{adapter::Adapter, trace_handler::DummyTrace};
use std::collections::HashMap;

pub struct OpenVmTraceHandler<'a, A: Adapter> {
    pub original_instructions: &'a Vec<A::Instruction>,
    pub air_id_to_dummy_trace_and_width: &'a HashMap<A::AirId, DummyTrace<A::Field>>,
    pub instruction_handler: &'a A::InstructionHandler,
    pub apc_call_count: usize,
}

impl<'a, A: Adapter> OpenVmTraceHandler<'a, A> {
    pub fn new(
        original_instructions: &'a Vec<A::Instruction>,
        air_id_to_dummy_trace_and_width: &'a HashMap<A::AirId, DummyTrace<A::Field>>,
        instruction_handler: &'a A::InstructionHandler,
        apc_call_count: usize,
    ) -> Self {
        Self {
            original_instructions,
            air_id_to_dummy_trace_and_width,
            instruction_handler,
            apc_call_count,
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

    fn apc_call_count(&self) -> usize {
        self.apc_call_count
    }

    fn air_id_to_dummy_trace_and_width(&self) -> &'a HashMap<A::AirId, DummyTrace<A::Field>> {
        self.air_id_to_dummy_trace_and_width
    }
}
