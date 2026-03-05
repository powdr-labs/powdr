// Mostly taken from [this openvm extension](https://github.com/openvm-org/openvm/blob/1b76fd5a900a7d69850ee9173969f70ef79c4c76/extensions/rv32im/circuit/src/extension.rs#L185) and simplified to only handle a single opcode with its necessary dependencies

use std::cell::RefCell;
use std::iter::once;
use std::rc::Rc;

use derive_more::From;
use openvm_circuit::arch::{DenseRecordArena, MatrixRecordArena};
use openvm_instructions::instruction::Instruction;
use openvm_instructions::LocalOpcode;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_openvm_bus_interaction_handler::bus_map::BusMap;

use crate::{
    apc_air::PowdrAir,
    customize_exe::OvmApcStats,
    extraction_utils::{OriginalAirs, OriginalVmConfig},
    isa::OpenVmISA,
    powdr_extension::{
        executor::{OriginalArenas, PowdrExecutor},
        PowdrOpcode,
    },
    IsaApc,
};
use openvm_circuit::{
    arch::{AirInventory, AirInventoryError, VmCircuitExtension, VmExecutionExtension},
    circuit_derive::Chip,
};
use openvm_stark_backend::{
    config::{StarkGenericConfig, Val},
    p3_field::{Field, PrimeField32},
};
use serde::{Deserialize, Serialize};

#[derive(Clone, Deserialize, Serialize)]
#[serde(bound = "F: Field")]
pub struct PowdrExtension<F, ISA: OpenVmISA> {
    pub precompiles: Vec<PowdrPrecompile<F, ISA>>,
    pub base_config: OriginalVmConfig<ISA>,
    pub bus_map: BusMap,
    pub airs: OriginalAirs<F, ISA>,
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(bound = "F: Field")]
pub struct PowdrPrecompile<F, ISA: OpenVmISA> {
    pub name: String,
    pub opcode: PowdrOpcode,
    pub apc: IsaApc<F, ISA>,
    pub apc_stats: OvmApcStats,
    #[serde(skip)]
    pub apc_record_arena_cpu: Rc<RefCell<OriginalArenas<MatrixRecordArena<F>>>>,
    #[serde(skip)]
    pub apc_record_arena_gpu: Rc<RefCell<OriginalArenas<DenseRecordArena>>>,
}

impl<F, ISA: OpenVmISA> PowdrPrecompile<F, ISA> {
    pub fn new(
        name: String,
        opcode: PowdrOpcode,
        apc: IsaApc<F, ISA>,
        apc_stats: OvmApcStats,
    ) -> Self {
        Self {
            name,
            opcode,
            apc,
            apc_stats,
            // Initialize with empty Rc (default to OriginalArenas::Uninitialized) for each APC
            apc_record_arena_cpu: Default::default(),
            apc_record_arena_gpu: Default::default(),
        }
    }
}

impl<F, ISA: OpenVmISA> PowdrExtension<F, ISA> {
    pub fn new(
        precompiles: Vec<PowdrPrecompile<F, ISA>>,
        base_config: OriginalVmConfig<ISA>,
        bus_map: BusMap,
        airs: OriginalAirs<F, ISA>,
    ) -> Self {
        Self {
            precompiles,
            base_config,
            bus_map,
            airs,
        }
    }
}

#[derive(From, Chip)]
#[allow(clippy::large_enum_variant)]
pub enum PowdrExtensionExecutor<ISA: OpenVmISA> {
    Powdr(PowdrExecutor<ISA>),
}

impl<ISA: OpenVmISA> openvm_circuit::arch::AnyEnum for PowdrExtensionExecutor<ISA> {
    fn as_any_kind(&self) -> &dyn std::any::Any {
        match self {
            Self::Powdr(x) => x,
        }
    }

    fn as_any_kind_mut(&mut self) -> &mut dyn std::any::Any {
        match self {
            Self::Powdr(x) => x,
        }
    }
}

impl<ISA: OpenVmISA> openvm_circuit::arch::InterpreterExecutor<BabyBear>
    for PowdrExtensionExecutor<ISA>
{
    fn pre_compute_size(&self) -> usize {
        match self {
            Self::Powdr(x) => x.pre_compute_size(),
        }
    }

    #[cfg(not(feature = "tco"))]
    fn pre_compute<Ctx>(
        &self,
        pc: u32,
        inst: &Instruction<BabyBear>,
        data: &mut [u8],
    ) -> Result<
        openvm_circuit::arch::ExecuteFunc<BabyBear, Ctx>,
        openvm_circuit::arch::StaticProgramError,
    >
    where
        Ctx: openvm_circuit::arch::execution_mode::ExecutionCtxTrait,
    {
        match self {
            Self::Powdr(x) => x.pre_compute(pc, inst, data),
        }
    }

    #[cfg(feature = "tco")]
    fn handler<Ctx>(
        &self,
        pc: u32,
        inst: &Instruction<BabyBear>,
        data: &mut [u8],
    ) -> Result<
        openvm_circuit::arch::Handler<BabyBear, Ctx>,
        openvm_circuit::arch::StaticProgramError,
    >
    where
        Ctx: openvm_circuit::arch::execution_mode::ExecutionCtxTrait,
    {
        match self {
            Self::Powdr(x) => x.handler(pc, inst, data),
        }
    }
}

impl<ISA: OpenVmISA> openvm_circuit::arch::InterpreterMeteredExecutor<BabyBear>
    for PowdrExtensionExecutor<ISA>
{
    fn metered_pre_compute_size(&self) -> usize {
        match self {
            Self::Powdr(x) => x.metered_pre_compute_size(),
        }
    }

    #[cfg(not(feature = "tco"))]
    fn metered_pre_compute<Ctx>(
        &self,
        chip_idx: usize,
        pc: u32,
        inst: &Instruction<BabyBear>,
        data: &mut [u8],
    ) -> Result<
        openvm_circuit::arch::ExecuteFunc<BabyBear, Ctx>,
        openvm_circuit::arch::StaticProgramError,
    >
    where
        Ctx: openvm_circuit::arch::execution_mode::MeteredExecutionCtxTrait,
    {
        match self {
            Self::Powdr(x) => x.metered_pre_compute(chip_idx, pc, inst, data),
        }
    }

    #[cfg(feature = "tco")]
    fn metered_handler<Ctx>(
        &self,
        chip_idx: usize,
        pc: u32,
        inst: &Instruction<BabyBear>,
        data: &mut [u8],
    ) -> Result<
        openvm_circuit::arch::Handler<BabyBear, Ctx>,
        openvm_circuit::arch::StaticProgramError,
    >
    where
        Ctx: openvm_circuit::arch::execution_mode::MeteredExecutionCtxTrait,
    {
        match self {
            Self::Powdr(x) => x.metered_handler(chip_idx, pc, inst, data),
        }
    }
}

#[cfg(feature = "aot")]
impl<ISA: OpenVmISA> openvm_circuit::arch::AotExecutor<BabyBear> for PowdrExtensionExecutor<ISA>
where
    PowdrExecutor<ISA>: openvm_circuit::arch::AotExecutor<BabyBear>,
{
    fn is_aot_supported(&self, inst: &Instruction<BabyBear>) -> bool {
        match self {
            Self::Powdr(x) => x.is_aot_supported(inst),
        }
    }

    fn generate_x86_asm(
        &self,
        inst: &Instruction<BabyBear>,
        pc: u32,
    ) -> Result<String, openvm_circuit::arch::AotError> {
        match self {
            Self::Powdr(x) => x.generate_x86_asm(inst, pc),
        }
    }
}

#[cfg(feature = "aot")]
impl<ISA: OpenVmISA> openvm_circuit::arch::AotMeteredExecutor<BabyBear>
    for PowdrExtensionExecutor<ISA>
where
    PowdrExecutor<ISA>: openvm_circuit::arch::AotMeteredExecutor<BabyBear>,
{
    fn is_aot_metered_supported(&self, inst: &Instruction<BabyBear>) -> bool {
        match self {
            Self::Powdr(x) => x.is_aot_metered_supported(inst),
        }
    }

    fn generate_x86_metered_asm(
        &self,
        inst: &Instruction<BabyBear>,
        pc: u32,
        chip_idx: usize,
        config: &openvm_circuit::arch::SystemConfig,
    ) -> Result<String, openvm_circuit::arch::AotError> {
        match self {
            Self::Powdr(x) => x.generate_x86_metered_asm(inst, pc, chip_idx, config),
        }
    }
}

impl<ISA: OpenVmISA, RA> openvm_circuit::arch::PreflightExecutor<BabyBear, RA>
    for PowdrExtensionExecutor<ISA>
where
    PowdrExecutor<ISA>: openvm_circuit::arch::PreflightExecutor<BabyBear, RA>,
{
    fn execute(
        &self,
        state: openvm_circuit::arch::VmStateMut<
            BabyBear,
            openvm_circuit::system::memory::online::TracingMemory,
            RA,
        >,
        instruction: &Instruction<BabyBear>,
    ) -> Result<(), openvm_circuit::arch::ExecutionError> {
        match self {
            Self::Powdr(x) => x.execute(state, instruction),
        }
    }

    fn get_opcode_name(&self, opcode: usize) -> String {
        match self {
            Self::Powdr(x) => <PowdrExecutor<ISA> as openvm_circuit::arch::PreflightExecutor<
                BabyBear,
                RA,
            >>::get_opcode_name(x, opcode),
        }
    }
}

impl<ISA: OpenVmISA> VmExecutionExtension<BabyBear> for PowdrExtension<BabyBear, ISA> {
    type Executor = PowdrExtensionExecutor<ISA>;

    fn extend_execution(
        &self,
        inventory: &mut openvm_circuit::arch::ExecutorInventoryBuilder<BabyBear, Self::Executor>,
    ) -> Result<(), openvm_circuit::arch::ExecutorInventoryError> {
        for precompile in &self.precompiles {
            // The apc chip uses a single row per call
            let height_change = 1;

            let powdr_executor = PowdrExtensionExecutor::Powdr(PowdrExecutor::new(
                self.airs.clone(),
                self.base_config.clone(),
                precompile.apc.clone(),
                precompile.apc_record_arena_cpu.clone(),
                precompile.apc_record_arena_gpu.clone(),
                height_change,
            ));
            inventory.add_executor(powdr_executor, once(precompile.opcode.global_opcode()))?;
        }

        Ok(())
    }
}

impl<SC, ISA: OpenVmISA> VmCircuitExtension<SC> for PowdrExtension<Val<SC>, ISA>
where
    SC: StarkGenericConfig,
    Val<SC>: PrimeField32,
{
    fn extend_circuit(&self, inventory: &mut AirInventory<SC>) -> Result<(), AirInventoryError> {
        for precompile in &self.precompiles {
            inventory.add_air(PowdrAir::new(precompile.apc.machine.clone()));
        }
        Ok(())
    }
}
