use std::sync::Arc;

use crate::traits::OpenVmField;
use openvm_circuit::{
    arch::{ExecutionState, InstructionExecutor, Result as ExecutionResult},
    system::memory::MemoryController,
};

use super::air::BitwiseLookupAir;
use crate::IntoOpenVm;
use openvm_instructions::instruction::Instruction;
use openvm_stark_backend::{
    config::{StarkGenericConfig, Val},
    p3_field::PrimeField32,
    prover::types::AirProofInput,
    rap::AnyRap,
    Chip, ChipUsageGetter,
};

pub struct BitwiseLookupChip<P: IntoOpenVm> {
    air: Arc<BitwiseLookupAir<OpenVmField<P>>>,
}

impl<P: IntoOpenVm> InstructionExecutor<OpenVmField<P>> for BitwiseLookupChip<P> {
    fn execute(
        &mut self,
        _memory: &mut MemoryController<OpenVmField<P>>,
        _instruction: &Instruction<OpenVmField<P>>,
        _from_state: ExecutionState<u32>,
    ) -> ExecutionResult<ExecutionState<u32>> {
        todo!()
    }

    fn get_opcode_name(&self, _opcode: usize) -> String {
        todo!()
    }
}

impl<P: IntoOpenVm> ChipUsageGetter for BitwiseLookupChip<P> {
    fn air_name(&self) -> String {
        todo!()
    }
    fn current_trace_height(&self) -> usize {
        todo!()
    }

    fn trace_width(&self) -> usize {
        todo!()
    }
}

impl<SC: StarkGenericConfig, P: IntoOpenVm<Field = Val<SC>>> Chip<SC> for BitwiseLookupChip<P>
where
    Val<SC>: PrimeField32,
{
    fn air(&self) -> Arc<dyn AnyRap<SC>> {
        self.air.clone()
    }

    fn generate_air_proof_input(self) -> AirProofInput<SC> {
        todo!()
    }
}
