use std::{
    collections::BTreeMap,
    sync::{Arc, Mutex},
};

use super::{
    chip::SymbolicMachine,
    vm::{OriginalInstruction, SdkVmInventory},
};
use openvm_circuit::{arch::VmConfig, system::memory::MemoryController};
use openvm_circuit::{
    arch::{
        ExecutionState, InstructionExecutor, Result as ExecutionResult, VmChipComplex,
        VmInventoryError,
    },
    system::memory::OfflineMemory,
};
use openvm_circuit_primitives::var_range::SharedVariableRangeCheckerChip;
use openvm_native_circuit::CastFExtension;
use openvm_sdk::config::{SdkVmConfig, SdkVmConfigExecutor, SdkVmConfigPeriphery};

use openvm_stark_backend::p3_field::PrimeField32;
use powdr_autoprecompiles::powdr::Column;

/// A struct which holds the state of the execution based on the original instructions in this block and a dummy inventory.
pub struct PowdrExecutor<F: PrimeField32> {
    pub instructions: Vec<OriginalInstruction<F>>,
    pub air_by_opcode_id: BTreeMap<usize, SymbolicMachine<F>>,
    pub is_valid_poly_id: u64,
    pub inventory: SdkVmInventory<F>,
    pub current_trace_height: usize,
}

impl<F: PrimeField32> PowdrExecutor<F> {
    pub fn new(
        instructions: Vec<OriginalInstruction<F>>,
        air_by_opcode_id: BTreeMap<usize, SymbolicMachine<F>>,
        is_valid_column: Column,
        memory: Arc<Mutex<OfflineMemory<F>>>,
        range_checker: &SharedVariableRangeCheckerChip,
        base_config: SdkVmConfig,
    ) -> Self {
        Self {
            instructions,
            air_by_opcode_id,
            is_valid_poly_id: is_valid_column.id.id,
            inventory: create_chip_complex_with_memory(
                memory,
                range_checker.clone(),
                base_config.clone(),
            )
            .unwrap()
            .inventory,
            current_trace_height: 0,
        }
    }

    pub fn execute(
        &mut self,
        memory: &mut MemoryController<F>,
        from_state: ExecutionState<u32>,
    ) -> ExecutionResult<ExecutionState<u32>> {
        // execute the original instructions one by one
        let res = self
            .instructions
            .iter()
            .try_fold(from_state, |execution_state, instruction| {
                let executor = self
                    .inventory
                    .get_mut_executor(&instruction.opcode())
                    .unwrap();
                executor.execute(memory, instruction.as_ref(), execution_state)
            });

        self.current_trace_height += 1;

        res
    }
}

// Extracted from openvm, extended to create an inventory with the correct memory
fn create_chip_complex_with_memory<F: PrimeField32>(
    memory: Arc<Mutex<OfflineMemory<F>>>,
    range_checker: SharedVariableRangeCheckerChip,
    base_config: SdkVmConfig,
) -> std::result::Result<
    VmChipComplex<F, SdkVmConfigExecutor<F>, SdkVmConfigPeriphery<F>>,
    VmInventoryError,
> {
    use openvm_keccak256_circuit::Keccak256;
    use openvm_native_circuit::Native;
    use openvm_rv32im_circuit::{Rv32I, Rv32Io};
    use openvm_sha256_circuit::Sha256;

    let this = base_config;
    let mut complex = this.system.config.create_chip_complex()?.transmute();

    // CHANGE: inject the correct memory here to be passed to the chips, to be accessible in their get_proof_input
    complex.base.memory_controller.offline_memory = memory.clone();
    complex.base.range_checker_chip = range_checker;
    // END CHANGE

    if this.rv32i.is_some() {
        complex = complex.extend(&Rv32I)?;
    }
    if this.io.is_some() {
        complex = complex.extend(&Rv32Io)?;
    }
    if this.keccak.is_some() {
        complex = complex.extend(&Keccak256)?;
    }
    if this.sha256.is_some() {
        complex = complex.extend(&Sha256)?;
    }
    if this.native.is_some() {
        complex = complex.extend(&Native)?;
    }
    if this.castf.is_some() {
        complex = complex.extend(&CastFExtension)?;
    }

    if let Some(rv32m) = this.rv32m {
        let mut rv32m = rv32m;
        if let Some(ref bigint) = this.bigint {
            rv32m.range_tuple_checker_sizes[0] =
                rv32m.range_tuple_checker_sizes[0].max(bigint.range_tuple_checker_sizes[0]);
            rv32m.range_tuple_checker_sizes[1] =
                rv32m.range_tuple_checker_sizes[1].max(bigint.range_tuple_checker_sizes[1]);
        }
        complex = complex.extend(&rv32m)?;
    }
    if let Some(bigint) = this.bigint {
        let mut bigint = bigint;
        if let Some(ref rv32m) = this.rv32m {
            bigint.range_tuple_checker_sizes[0] =
                rv32m.range_tuple_checker_sizes[0].max(bigint.range_tuple_checker_sizes[0]);
            bigint.range_tuple_checker_sizes[1] =
                rv32m.range_tuple_checker_sizes[1].max(bigint.range_tuple_checker_sizes[1]);
        }
        complex = complex.extend(&bigint)?;
    }
    if let Some(ref modular) = this.modular {
        complex = complex.extend(modular)?;
    }
    if let Some(ref fp2) = this.fp2 {
        complex = complex.extend(fp2)?;
    }
    if let Some(ref pairing) = this.pairing {
        complex = complex.extend(pairing)?;
    }
    if let Some(ref ecc) = this.ecc {
        complex = complex.extend(ecc)?;
    }

    Ok(complex)
}
