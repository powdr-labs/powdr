use std::sync::Arc;

use openvm_circuit::{
    arch::{ExecutionState, InstructionExecutor, Result as ExecutionResult},
    system::memory::MemoryController,
};
use openvm_instructions::instruction::Instruction;
use openvm_stark_backend::{
    config::{StarkGenericConfig, Val},
    p3_field::PrimeField32,
    prover::types::AirProofInput,
    rap::AnyRap,
    Chip, ChipUsageGetter,
};

use super::air::PlonkAir;

pub struct PlonkChip<F: PrimeField32> {
    air: Arc<PlonkAir<F>>,
}

impl<F: PrimeField32> Default for PlonkChip<F> {
    fn default() -> Self {
        let air = Arc::new(PlonkAir {
            _marker: std::marker::PhantomData,
        });
        Self { air }
    }
}

impl<F: PrimeField32> InstructionExecutor<F> for PlonkChip<F> {
    fn execute(
        &mut self,
        _memory: &mut MemoryController<F>,
        _instruction: &Instruction<F>,
        _from_state: ExecutionState<u32>,
    ) -> ExecutionResult<ExecutionState<u32>> {
        todo!()
    }

    fn get_opcode_name(&self, _opcode: usize) -> String {
        todo!()
    }
}

impl<F: PrimeField32> ChipUsageGetter for PlonkChip<F> {
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

impl<SC: StarkGenericConfig> Chip<SC> for PlonkChip<Val<SC>>
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
