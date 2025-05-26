use std::sync::{Arc, Mutex};

use crate::powdr_extension::{
    chip::SharedChips, executor::PowdrExecutor, PowdrOpcode, PowdrPrecompile,
};
use openvm_circuit::{
    arch::{ExecutionState, InstructionExecutor, Result as ExecutionResult},
    system::memory::{MemoryController, OfflineMemory},
};
use openvm_instructions::instruction::Instruction;
use openvm_instructions::LocalOpcode;
use openvm_sdk::config::SdkVmConfig;
use openvm_stark_backend::p3_air::BaseAir;
use openvm_stark_backend::{
    config::{StarkGenericConfig, Val},
    p3_field::PrimeField32,
    prover::types::AirProofInput,
    rap::AnyRap,
    Chip, ChipUsageGetter,
};

use super::air::PlonkAir;

pub struct PlonkChip<F: PrimeField32> {
    name: String,
    opcode: PowdrOpcode,
    air: Arc<PlonkAir<F>>,
    executor: PowdrExecutor<F>,
}

impl<F: PrimeField32> PlonkChip<F> {
    #[allow(dead_code)]
    pub(crate) fn new(
        precompile: PowdrPrecompile<F>,
        memory: Arc<Mutex<OfflineMemory<F>>>,
        base_config: SdkVmConfig,
        periphery: SharedChips,
    ) -> Self {
        let PowdrPrecompile {
            original_instructions,
            original_airs,
            is_valid_column,
            name,
            opcode,
            ..
        } = precompile;
        let air = PlonkAir {
            _marker: std::marker::PhantomData,
        };
        let executor = PowdrExecutor::new(
            original_instructions,
            original_airs,
            is_valid_column,
            memory,
            base_config,
            periphery,
        );

        Self {
            name,
            opcode,
            air: Arc::new(air),
            executor,
        }
    }
}

impl<F: PrimeField32> InstructionExecutor<F> for PlonkChip<F> {
    fn execute(
        &mut self,
        memory: &mut MemoryController<F>,
        instruction: &Instruction<F>,
        from_state: ExecutionState<u32>,
    ) -> ExecutionResult<ExecutionState<u32>> {
        let &Instruction { opcode, .. } = instruction;
        assert_eq!(opcode.as_usize(), self.opcode.global_opcode().as_usize());

        let execution_state = self.executor.execute(memory, from_state)?;

        Ok(execution_state)
    }

    fn get_opcode_name(&self, _opcode: usize) -> String {
        self.name.clone()
    }
}

impl<F: PrimeField32> ChipUsageGetter for PlonkChip<F> {
    fn air_name(&self) -> String {
        format!("powdr_plonk_air_for_opcode_{}", self.opcode.global_opcode()).to_string()
    }
    fn current_trace_height(&self) -> usize {
        self.executor.number_of_calls()
    }

    fn trace_width(&self) -> usize {
        self.air.width()
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
