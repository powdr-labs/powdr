use std::collections::HashSet;

use openvm_instructions::{VmOpcode, instruction::Instruction, program::DEFAULT_PC_STEP};
use openvm_sdk::config::SdkVmConfig;
use openvm_stark_backend::p3_field::PrimeField32;

use crate::{ExtendedVmConfig, instruction_sets::{OpenVmISA, riscv::opcode::{branch_opcodes_set, instruction_allowlist}}};

pub mod opcode;
pub mod instruction_formatter;
pub mod symbolic_instruction_builder;
pub mod program;
pub mod customize_exe;

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
}