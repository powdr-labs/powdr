// Mostly taken from [this openvm extension](https://github.com/openvm-org/openvm/blob/1b76fd5a900a7d69850ee9173969f70ef79c4c76/extensions/rv32im/circuit/src/extension.rs#L185) and simplified to only handle a single opcode with its necessary dependencies

use std::cell::RefCell;
use std::iter::once;
use std::rc::Rc;
use std::sync::Arc;

use derive_more::From;
use openvm_circuit::arch::{DenseRecordArena, MatrixRecordArena};
use openvm_circuit_derive::{
    AotExecutor, AotMeteredExecutor, Executor, MeteredExecutor, PreflightExecutor,
};
use openvm_instructions::LocalOpcode;
use openvm_instructions::instruction::Instruction;
use openvm_stark_sdk::p3_baby_bear::BabyBear;

use crate::bus_map::BusMap;
use crate::customize_exe::{OpenVmRegisterAddress, OvmApcStats};
use crate::extraction_utils::{OriginalAirs, OriginalVmConfig};
use crate::instruction_sets::OpenVmISA;
use crate::powdr_extension::chip::PowdrAir;
use crate::powdr_extension::executor::{OriginalArenas, PowdrExecutor};
use openvm_circuit::{
    arch::{AirInventory, AirInventoryError, VmCircuitExtension, VmExecutionExtension},
    circuit_derive::Chip,
    derive::AnyEnum,
};
use openvm_stark_backend::{
    config::{StarkGenericConfig, Val},
    p3_field::{Field, PrimeField32},
};
use powdr_autoprecompiles::Apc;
use serde::{Deserialize, Serialize};

use crate::{Instr, RiscvISA};

use super::PowdrOpcode;

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
pub struct PowdrPrecompile<F, ISA> {
    pub name: String,
    pub opcode: PowdrOpcode,
    pub apc: Arc<Apc<F, Instr<F, ISA>, OpenVmRegisterAddress, u32>>,
    pub apc_stats: OvmApcStats,
    #[serde(skip)]
    pub apc_record_arena_cpu: Rc<RefCell<OriginalArenas<MatrixRecordArena<F>>>>,
    #[serde(skip)]
    pub apc_record_arena_gpu: Rc<RefCell<OriginalArenas<DenseRecordArena>>>,
}

impl<F, ISA> PowdrPrecompile<F, ISA> {
    pub fn new(
        name: String,
        opcode: PowdrOpcode,
        apc: Arc<Apc<F, Instr<F, ISA>, OpenVmRegisterAddress, u32>>,
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

#[derive(
    From,
    AnyEnum,
    PreflightExecutor,
    Executor,
    MeteredExecutor,
    AotExecutor,
    AotMeteredExecutor,
    Chip,
)]
#[allow(clippy::large_enum_variant)]
pub enum PowdrExtensionExecutor<ISA> {
    Powdr(PowdrExecutor<ISA>),
}

impl VmExecutionExtension<BabyBear> for PowdrExtension<BabyBear, RiscvISA> {
    type Executor = PowdrExtensionExecutor<RiscvISA>;

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
