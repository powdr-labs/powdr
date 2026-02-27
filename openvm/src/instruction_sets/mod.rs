use std::collections::HashSet;

use openvm_circuit::arch::{
    AirInventory, ChipInventoryError, MatrixRecordArena, VmChipComplex, VmConfig, VmExecutionConfig,
};
use openvm_circuit::system::SystemChipInventory;
use openvm_instructions::{instruction::Instruction, VmOpcode};
use openvm_stark_backend::{config::Val, p3_field::PrimeField32, prover::cpu::CpuBackend};
use openvm_stark_sdk::p3_baby_bear::BabyBear;

use crate::BabyBearSC;

pub mod riscv;

pub type OriginalCpuChipComplex = VmChipComplex<
    BabyBearSC,
    MatrixRecordArena<Val<BabyBearSC>>,
    CpuBackend<BabyBearSC>,
    SystemChipInventory<BabyBearSC>,
>;

// TODO: Send + Sync + Clone actually not needed
pub trait OpenVmISA: Send + Sync + Clone {
    const DEFAULT_PC_STEP: u32;

    type DummyConfig: VmConfig<BabyBearSC> + VmExecutionConfig<BabyBear>;
    type OriginalConfig: VmConfig<BabyBearSC> + VmExecutionConfig<BabyBear>;

    fn lower(original: Self::OriginalConfig) -> Self::DummyConfig;
    fn create_original_chip_complex(
        config: &Self::OriginalConfig,
        airs: AirInventory<BabyBearSC>,
    ) -> Result<OriginalCpuChipComplex, ChipInventoryError>;

    fn is_allowed(opcode: VmOpcode) -> bool;

    fn is_branching(opcode: VmOpcode) -> bool;

    fn instruction_allowlist() -> HashSet<VmOpcode>;

    fn format<F: PrimeField32>(instruction: &Instruction<F>) -> String;
}
