use std::collections::{BTreeMap, BTreeSet, HashSet};

use openvm_circuit::arch::{
    AirInventory, AirInventoryError, AnyEnum, ChipInventory, ChipInventoryError, DenseRecordArena,
    Executor, InterpreterExecutor, MatrixRecordArena, MeteredExecutor, PreflightExecutor,
    VmBuilder, VmChipComplex, VmCircuitExtension, VmConfig, VmExecutionConfig,
};
#[cfg(feature = "cuda")]
use openvm_circuit::system::cuda::SystemChipInventoryGPU;
use openvm_circuit::system::SystemChipInventory;
use openvm_circuit_derive::{
    AnyEnum, AotExecutor, AotMeteredExecutor, Executor, MeteredExecutor, PreflightExecutor,
};
use openvm_circuit_primitives::Chip;
#[cfg(feature = "cuda")]
use openvm_cuda_backend::engine::GpuBabyBearPoseidon2Engine;
#[cfg(feature = "cuda")]
use openvm_cuda_backend::prover_backend::GpuBackend;
use openvm_instructions::{instruction::Instruction, VmOpcode};
use openvm_sdk::config::TranspilerConfig;
use openvm_stark_backend::{config::Val, p3_field::PrimeField32, prover::cpu::CpuBackend};
use openvm_stark_sdk::config::baby_bear_poseidon2::BabyBearPoseidon2Engine;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use serde::{Deserialize, Serialize};

use crate::program::OriginalCompiledProgram;
use crate::trace_generator::cpu::periphery::SharedPeripheryChipsCpu;
#[cfg(feature = "cuda")]
use crate::trace_generator::cuda::periphery::SharedPeripheryChipsGpu;
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

#[cfg(feature = "cuda")]
pub type OriginalGpuChipComplex =
    VmChipComplex<BabyBearSC, DenseRecordArena, GpuBackend, SystemChipInventoryGPU>;
#[cfg(feature = "cuda")]
pub type OriginalGpuChipInventory = ChipInventory<BabyBearSC, DenseRecordArena, GpuBackend>;

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

    type OriginalBuilderCpu: Clone
        + Default
        + VmBuilder<
            BabyBearPoseidon2Engine,
            VmConfig = Self::OriginalConfig,
            SystemChipInventory = SystemChipInventory<BabyBearSC>,
            RecordArena = MatrixRecordArena<Val<BabyBearSC>>,
        >;

    #[cfg(feature = "cuda")]
    type OriginalBuilderGpu: Clone
        + Default
        + VmBuilder<
            GpuBabyBearPoseidon2Engine,
            VmConfig = Self::OriginalConfig,
            SystemChipInventory = SystemChipInventoryGPU,
            RecordArena = DenseRecordArena,
        >;

    fn create_dummy_airs<E: VmCircuitExtension<BabyBearSC>>(
        config: &Self::OriginalConfig,
        shared_chips: E,
    ) -> Result<AirInventory<BabyBearSC>, AirInventoryError>;

    fn create_original_chip_complex(
        config: &Self::OriginalConfig,
        airs: AirInventory<BabyBearSC>,
    ) -> Result<OriginalCpuChipComplex, ChipInventoryError>;

    fn create_dummy_chip_complex_cpu(
        config: &Self::OriginalConfig,
        circuit: AirInventory<BabyBearSC>,
        shared_chips: SharedPeripheryChipsCpu<Self>,
    ) -> Result<OriginalCpuChipComplex, ChipInventoryError>;

    #[cfg(feature = "cuda")]
    fn create_dummy_chip_complex_gpu(
        config: &Self::OriginalConfig,
        circuit: AirInventory<BabyBearSC>,
        shared_chips: SharedPeripheryChipsGpu<Self>,
    ) -> Result<OriginalGpuChipComplex, ChipInventoryError>;

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

    fn get_labels_debug<'a>(program: &Self::Program<'a>) -> BTreeMap<u64, Vec<String>>;

    /// Given an original program (elf + compiled exe), return the pcs which correspond to labels
    fn get_jump_destinations(original_program: &OriginalCompiledProgram<Self>) -> BTreeSet<u64>;
}
