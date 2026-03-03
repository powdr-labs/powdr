use std::collections::{BTreeMap, HashSet};

use openvm_circuit::arch::{
    AirInventory, AnyEnum, ChipInventory, ChipInventoryError, DenseRecordArena, Executor,
    InterpreterExecutor, MatrixRecordArena, MeteredExecutor, PreflightExecutor, VmBuilder,
    VmChipComplex, VmConfig, VmExecutionConfig,
};
use openvm_circuit::system::SystemChipInventory;
use openvm_circuit_derive::{
    AnyEnum, AotExecutor, AotMeteredExecutor, Executor, MeteredExecutor, PreflightExecutor,
};
use openvm_circuit_primitives::Chip;
use openvm_instructions::{instruction::Instruction, VmOpcode};
use openvm_sdk::config::TranspilerConfig;
use openvm_stark_backend::{config::Val, p3_field::PrimeField32, prover::cpu::CpuBackend};
use openvm_stark_sdk::config::baby_bear_poseidon2::BabyBearPoseidon2Engine;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use serde::{Deserialize, Serialize};

use crate::program::OriginalCompiledProgram;
use crate::trace_generator::cpu::periphery::SharedPeripheryChipsCpu;
use crate::vm::PowdrExtensionExecutor;
use crate::BabyBearSC;

pub type OriginalCpuChipComplex = VmChipComplex<
    BabyBearSC,
    MatrixRecordArena<Val<BabyBearSC>>,
    CpuBackend<BabyBearSC>,
    SystemChipInventory<BabyBearSC>,
>;
pub type OriginalCpuChipInventory =
    ChipInventory<BabyBearSC, MatrixRecordArena<Val<BabyBearSC>>, CpuBackend<BabyBearSC>>;

#[allow(clippy::large_enum_variant)]
#[derive(
    AnyEnum, Chip, Executor, MeteredExecutor, AotExecutor, AotMeteredExecutor, PreflightExecutor,
)]
pub enum SpecializedExecutor<F: PrimeField32, ISA: OpenVmISA> {
    #[any_enum]
    OriginalExecutor(ISA::OriginalExecutor<F>),
    #[any_enum]
    PowdrExecutor(PowdrExtensionExecutor<ISA>),
}

impl<F: PrimeField32, ISA: OpenVmISA> From<PowdrExtensionExecutor<ISA>>
    for SpecializedExecutor<F, ISA>
{
    fn from(value: PowdrExtensionExecutor<ISA>) -> Self {
        Self::PowdrExecutor(value)
    }
}

pub trait OpenVmISA: Send + Sync + Clone + 'static + Default {
    /// The original program, for example, an elf for riscv. It must allow recovering the jump destinations / labels.
    type Program<'a>;

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

    type OriginalExecutor<F: PrimeField32>: AnyEnum
        + InterpreterExecutor<F>
        + Executor<F>
        + MeteredExecutor<F>
        + PreflightExecutor<F, MatrixRecordArena<F>>
        + PreflightExecutor<F, DenseRecordArena>
        + Into<SpecializedExecutor<F, Self>>;

    type OriginalConfig: VmConfig<BabyBearSC>
        + VmExecutionConfig<BabyBear, Executor = Self::OriginalExecutor<BabyBear>>
        + TranspilerConfig<BabyBear>;

    type OriginalBuilder: Clone
        + Default
        + VmBuilder<
            BabyBearPoseidon2Engine,
            VmConfig = Self::OriginalConfig,
            SystemChipInventory = SystemChipInventory<BabyBearSC>,
            RecordArena = MatrixRecordArena<Val<BabyBearSC>>,
        >;

    fn create_original_chip_complex(
        config: &Self::OriginalConfig,
        airs: AirInventory<BabyBearSC>,
    ) -> Result<OriginalCpuChipComplex, ChipInventoryError>;

    /// Given a config of the vanilla VM and a shared periphery (non-instruction chips), create a dummy inventory
    fn create_dummy_inventory(
        config: &Self::OriginalConfig,
        context: SharedPeripheryChipsCpu<Self>,
    ) -> OriginalCpuChipInventory;

    /// Whether a given opcode is branching
    fn is_branching(opcode: VmOpcode) -> bool;

    /// The set of instructions which are allowed to be put into autoprecompiles
    fn instruction_allowlist() -> HashSet<VmOpcode>;

    /// Return the value of register `register` as an u32
    fn get_register_value(register: &Self::RegisterAddress) -> u32;

    /// Return the `limb_index`-th limb of `value` as a u32, where `value` is a memory value
    fn value_limb(value: u32, limb_index: usize) -> u32;

    /// Format an instruction of this ISA
    fn format<F: PrimeField32>(instruction: &Instruction<F>) -> String;

    /// Given an original program (elf + compiled exe), return the pcs which correspond to labels
    fn get_labels(original_program: &OriginalCompiledProgram<Self>) -> BTreeMap<u64, Vec<String>>;
}
