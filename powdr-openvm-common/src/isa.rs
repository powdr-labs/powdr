use std::collections::HashSet;

use openvm_circuit::arch::{
    AirInventory, AnyEnum, ChipInventory, ChipInventoryError, DenseRecordArena,
    InterpreterExecutor, MatrixRecordArena, PreflightExecutor, VmChipComplex, VmConfig,
    VmExecutionConfig,
};
use openvm_circuit::system::SystemChipInventory;
use openvm_instructions::{instruction::Instruction, VmOpcode};
use openvm_sdk::config::TranspilerConfig;
use openvm_stark_backend::{config::Val, p3_field::PrimeField32, prover::cpu::CpuBackend};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use serde::{Deserialize, Serialize};

use crate::vm::PowdrExtensionExecutor;
use crate::{BabyBearSC, PeripheryBusIds};

pub type OriginalCpuChipComplex = VmChipComplex<
    BabyBearSC,
    MatrixRecordArena<Val<BabyBearSC>>,
    CpuBackend<BabyBearSC>,
    SystemChipInventory<BabyBearSC>,
>;
pub type OriginalCpuChipInventory =
    ChipInventory<BabyBearSC, MatrixRecordArena<Val<BabyBearSC>>, CpuBackend<BabyBearSC>>;

pub trait OpenVmISA: Send + Sync + Clone + 'static {
    const DEFAULT_PC_STEP: u32;
    type RegisterAddress: PartialEq
        + Eq
        + std::hash::Hash
        + Clone
        + Copy
        + std::fmt::Debug
        + Serialize
        + for<'a> Deserialize<'a>
        + Send
        + Sync;

    type DummyExecutor: InterpreterExecutor<BabyBear>
        + PreflightExecutor<BabyBear, MatrixRecordArena<BabyBear>>
        + PreflightExecutor<BabyBear, DenseRecordArena>;
    type DummyConfig: VmConfig<BabyBearSC>
        + VmExecutionConfig<BabyBear, Executor = Self::DummyExecutor>;
    type DummyInventoryContext: Clone;
    type Executor: AnyEnum
        + From<<Self::OriginalConfig as VmExecutionConfig<BabyBear>>::Executor>
        + From<PowdrExtensionExecutor<Self>>;
    type OriginalConfig: VmConfig<BabyBearSC>
        + VmExecutionConfig<BabyBear>
        + TranspilerConfig<BabyBear>;

    fn lower(original: Self::OriginalConfig) -> Self::DummyConfig;

    fn create_original_chip_complex(
        config: &Self::OriginalConfig,
        airs: AirInventory<BabyBearSC>,
    ) -> Result<OriginalCpuChipComplex, ChipInventoryError>;

    fn create_dummy_inventory(
        config: &Self::OriginalConfig,
        context: Self::DummyInventoryContext,
    ) -> OriginalCpuChipInventory;

    fn is_allowed(opcode: VmOpcode) -> bool;

    fn is_branching(opcode: VmOpcode) -> bool;

    fn instruction_allowlist() -> HashSet<VmOpcode>;

    fn extra_targets() -> HashSet<VmOpcode>;

    fn get_register_value(register: &Self::RegisterAddress) -> u32;

    fn value_limb(value: u32, limb_index: usize) -> u32;

    fn format<F: PrimeField32>(instruction: &Instruction<F>) -> String;

    fn apply_interaction(
        periphery: &Self::DummyInventoryContext,
        bus_id: u16,
        mult: u32,
        args: impl Iterator<Item = u32>,
        periphery_bus_ids: &PeripheryBusIds,
    );
}
