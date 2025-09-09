use std::collections::HashMap;

use powdr_autoprecompiles::adapter::Adapter;
use powdr_autoprecompiles::trace_handler::{Trace, TraceHandler};

pub struct OpenVmTraceHandler<'a, A: Adapter> {
    pub air_id_to_dummy_trace: &'a HashMap<A::AirId, Trace<A::Field>>,
    pub instruction_handler: &'a A::InstructionHandler,
    pub apc_call_count: usize,
}

impl<'a, A: Adapter> OpenVmTraceHandler<'a, A> {
    pub fn new(
        air_id_to_dummy_trace: &'a HashMap<A::AirId, Trace<A::Field>>,
        instruction_handler: &'a A::InstructionHandler,
        apc_call_count: usize,
    ) -> Self {
        Self {
            air_id_to_dummy_trace,
            instruction_handler,
            apc_call_count,
        }
    }
}

impl<'a, A: Adapter> TraceHandler<A> for OpenVmTraceHandler<'a, A> {
    fn instruction_handler(&self) -> &A::InstructionHandler {
        self.instruction_handler
    }

    fn apc_call_count(&self) -> usize {
        self.apc_call_count
    }

    fn air_id_to_dummy_trace(&self) -> &'a HashMap<A::AirId, Trace<A::Field>> {
        self.air_id_to_dummy_trace
    }
}
