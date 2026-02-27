use std::collections::HashSet;

use openvm_circuit::arch::{AirInventory, ChipInventoryError, VmBuilder};
use openvm_instructions::{instruction::Instruction, program::DEFAULT_PC_STEP, VmOpcode};
use openvm_sdk::config::SdkVmConfig;
use openvm_stark_backend::p3_field::PrimeField32;
use openvm_stark_sdk::config::baby_bear_poseidon2::BabyBearPoseidon2Engine;

use crate::{
    instruction_sets::{
        riscv::opcode::{branch_opcodes_set, instruction_allowlist},
        OpenVmISA, OriginalCpuChipComplex,
    },
    BabyBearSC, ExtendedVmConfig, ExtendedVmConfigCpuBuilder,
};

pub mod customize_exe;
pub mod instruction_formatter;
pub mod opcode;
pub mod program;
pub mod symbolic_instruction_builder;

// Clone should not be required
#[derive(Clone)]
pub struct RiscvISA;

impl OpenVmISA for RiscvISA {
    const DEFAULT_PC_STEP: u32 = DEFAULT_PC_STEP;

    type DummyConfig = SdkVmConfig;
    type OriginalConfig = ExtendedVmConfig;

    fn is_allowed(opcode: VmOpcode) -> bool {
        instruction_allowlist().contains(&opcode)
    }

    fn is_branching(opcode: VmOpcode) -> bool {
        branch_opcodes_set().contains(&opcode)
    }

    fn format<F: PrimeField32>(instruction: &Instruction<F>) -> String {
        instruction_formatter::openvm_instruction_formatter(instruction)
    }

    fn instruction_allowlist() -> HashSet<VmOpcode> {
        instruction_allowlist()
    }

    fn lower(original: Self::OriginalConfig) -> Self::DummyConfig {
        original.sdk
    }

    fn create_original_chip_complex(
        config: &Self::OriginalConfig,
        airs: AirInventory<BabyBearSC>,
    ) -> Result<OriginalCpuChipComplex, ChipInventoryError> {
        <ExtendedVmConfigCpuBuilder as VmBuilder<BabyBearPoseidon2Engine>>::create_chip_complex(
            &ExtendedVmConfigCpuBuilder,
            config,
            airs,
        )
    }
}
