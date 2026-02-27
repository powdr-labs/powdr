use std::collections::HashSet;

use openvm_circuit::arch::{AnyEnum, VmConfig, VmExecutionConfig};
use openvm_instructions::{VmOpcode, instruction::Instruction};
use openvm_stark_backend::{config::StarkGenericConfig, p3_field::PrimeField32};
use openvm_stark_sdk::p3_baby_bear::BabyBear;

use crate::BabyBearSC;

pub mod riscv;

// TODO: Send + Sync + Clone actually not needed
pub trait OpenVmISA: Send + Sync + Clone {
    const DEFAULT_PC_STEP: u32;

    type DummyConfig: VmConfig<BabyBearSC> + VmExecutionConfig<BabyBear>;
    type OriginalConfig: VmConfig<BabyBearSC> + VmExecutionConfig<BabyBear>;

    fn lower(original: Self::OriginalConfig) -> Self::DummyConfig;

    fn is_allowed(opcode: VmOpcode) -> bool;

    fn is_branching(opcode: VmOpcode) -> bool;

    fn instruction_allowlist() -> HashSet<VmOpcode>;

    fn format<F: PrimeField32>(instruction: &Instruction<F>) -> String;
}