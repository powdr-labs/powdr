use std::collections::{BTreeSet, HashSet};
use std::sync::Arc;

use openvm_circuit::arch::{
    AirInventory, AirInventoryError, AnyEnum, ChipInventory, ChipInventoryError, DenseRecordArena,
    Executor, InterpreterExecutor, MatrixRecordArena, MeteredExecutor, PreflightExecutor,
    VmBuilder, VmChipComplex, VmCircuitExtension, VmConfig, VmExecutionConfig, VmField,
};
#[cfg(feature = "cuda")]
use openvm_circuit::system::cuda::SystemChipInventoryGPU;
use openvm_circuit::system::SystemChipInventory;
#[cfg(feature = "cuda")]
use openvm_cuda_backend::engine::GpuBabyBearPoseidon2CpuEngine;
#[cfg(feature = "cuda")]
use openvm_cuda_backend::prover_backend::GpuBackend;
use openvm_instructions::{instruction::Instruction, VmOpcode};
use openvm_sdk_config::TranspilerConfig;
use openvm_stark_backend::{p3_field::PrimeField32, prover::CpuBackend, Val};
use openvm_stark_sdk::config::baby_bear_poseidon2::BabyBearPoseidon2CpuEngine;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_riscv_elf::debug_info::SymbolTable;

use crate::powdr_extension::trace_generator::cpu::SharedPeripheryChipsCpu;
#[cfg(feature = "cuda")]
use crate::powdr_extension::trace_generator::SharedPeripheryChipsGpu;
use crate::program::OriginalCompiledProgram;
use crate::{BabyBearSC, Instr, SpecializedExecutor};

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

pub type IsaApc<F, ISA> = Arc<powdr_autoprecompiles::Apc<F, Instr<F, ISA>, (), u32>>;

pub trait OpenVmISA: Send + Sync + Clone + 'static + Default {
    /// The original program, for example, an elf for riscv. It must allow recovering the jump destinations / labels.
    type Program<'a>;

    type Executor<F: VmField>: AnyEnum
        + InterpreterExecutor<F>
        + Executor<F>
        + MeteredExecutor<F>
        + PreflightExecutor<F, MatrixRecordArena<F>>
        + PreflightExecutor<F, DenseRecordArena>
        + Send
        + Sync
        + Into<SpecializedExecutor<F, Self>>;

    type Config: VmConfig<BabyBearSC>
        + VmExecutionConfig<BabyBear, Executor = Self::Executor<BabyBear>>
        + TranspilerConfig<BabyBear>;

    type CpuBuilder: Clone
        + Default
        + VmBuilder<
            BabyBearPoseidon2CpuEngine,
            VmConfig = Self::Config,
            SystemChipInventory = SystemChipInventory<BabyBearSC>,
            RecordArena = MatrixRecordArena<Val<BabyBearSC>>,
        >;

    #[cfg(feature = "cuda")]
    type GpuBuilder: Clone
        + Default
        + VmBuilder<
            GpuBabyBearPoseidon2CpuEngine,
            VmConfig = Self::Config,
            SystemChipInventory = SystemChipInventoryGPU,
            RecordArena = DenseRecordArena,
        >;

    fn create_dummy_airs<E: VmCircuitExtension<BabyBearSC>>(
        config: &Self::Config,
        shared_chips: E,
    ) -> Result<AirInventory<BabyBearSC>, AirInventoryError>;

    fn create_original_chip_complex(
        config: &Self::Config,
        airs: AirInventory<BabyBearSC>,
    ) -> Result<OriginalCpuChipComplex, ChipInventoryError>;

    fn create_dummy_chip_complex_cpu(
        config: &Self::Config,
        circuit: AirInventory<BabyBearSC>,
        shared_chips: SharedPeripheryChipsCpu<Self>,
    ) -> Result<OriginalCpuChipComplex, ChipInventoryError>;

    #[cfg(feature = "cuda")]
    fn create_dummy_chip_complex_gpu(
        config: &Self::Config,
        circuit: AirInventory<BabyBearSC>,
        shared_chips: SharedPeripheryChipsGpu<Self>,
    ) -> Result<OriginalGpuChipComplex, ChipInventoryError>;

    /// The set of branching opcodes
    fn branching_opcodes() -> HashSet<VmOpcode>;

    /// The set of opcodes which are allowed to be put into autoprecompiles
    fn allowed_opcodes() -> HashSet<VmOpcode>;

    /// Format an instruction of this ISA
    fn format<F: PrimeField32>(instruction: &Instruction<F>) -> String;

    fn get_symbol_table<'a>(program: &Self::Program<'a>) -> SymbolTable;

    /// Given an original program, return the pcs which correspond to jump destinations
    fn get_jump_destinations(original_program: &OriginalCompiledProgram<Self>) -> BTreeSet<u64>;
}
