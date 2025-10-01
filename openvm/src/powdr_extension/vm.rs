// Mostly taken from [this openvm extension](https://github.com/openvm-org/openvm/blob/1b76fd5a900a7d69850ee9173969f70ef79c4c76/extensions/rv32im/circuit/src/extension.rs#L185) and simplified to only handle a single opcode with its necessary dependencies

use std::cell::RefCell;
use std::iter::once;
use std::rc::Rc;
use std::sync::Arc;

use derive_more::From;
use itertools::Itertools;
use openvm_circuit_derive::{Executor, MeteredExecutor, PreflightExecutor};
use openvm_instructions::LocalOpcode;
use openvm_stark_sdk::p3_baby_bear::BabyBear;

use crate::bus_map::BusMap;
use crate::customize_exe::OvmApcStats;
use crate::extraction_utils::{OriginalAirs, OriginalVmConfig};
use crate::powdr_extension::chip::PowdrAir;
use crate::powdr_extension::executor::{OriginalArenas, PowdrExecutor};
use crate::powdr_extension::PlonkAir;
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

use crate::{Instr, PrecompileImplementation};

use super::PowdrOpcode;

#[derive(Clone, Deserialize, Serialize)]
#[serde(bound = "F: Field")]
pub struct PowdrExtension<F> {
    pub precompiles: Vec<PowdrPrecompile<F>>,
    pub base_config: OriginalVmConfig,
    pub implementation: PrecompileImplementation,
    pub bus_map: BusMap,
    pub airs: OriginalAirs<F>,
    #[serde(skip)]
    pub record_arena_by_air_name: Vec<Rc<RefCell<OriginalArenas>>>,
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(bound = "F: Field")]
pub struct PowdrPrecompile<F> {
    pub name: String,
    pub opcode: PowdrOpcode,
    pub apc: Arc<Apc<F, Instr<F>>>,
    pub apc_stats: Option<OvmApcStats>,
}

impl<F> PowdrPrecompile<F> {
    pub fn new(
        name: String,
        opcode: PowdrOpcode,
        apc: Arc<Apc<F, Instr<F>>>,
        apc_stats: Option<OvmApcStats>,
    ) -> Self {
        Self {
            name,
            opcode,
            apc,
            apc_stats,
        }
    }
}

impl<F> PowdrExtension<F> {
    pub fn new(
        precompiles: Vec<PowdrPrecompile<F>>,
        base_config: OriginalVmConfig,
        implementation: PrecompileImplementation,
        bus_map: BusMap,
        airs: OriginalAirs<F>,
    ) -> Self {
        // Initialize with empty Rc (default to OriginalArenas::Uninitialized), one for each APC
        let record_arena_by_air_name = (0..precompiles.len()).map(|_| Default::default()).collect();
        Self {
            precompiles,
            base_config,
            implementation,
            bus_map,
            airs,
            record_arena_by_air_name,
        }
    }
}

#[derive(From, AnyEnum, PreflightExecutor, Executor, MeteredExecutor, Chip)]
#[allow(clippy::large_enum_variant)]
pub enum PowdrExtensionExecutor {
    Powdr(PowdrExecutor),
}

impl VmExecutionExtension<BabyBear> for PowdrExtension<BabyBear> {
    type Executor = PowdrExtensionExecutor;

    // TODO: this part seems duplicated to `extend_prover`, so need to study the split of functionalities between them
    fn extend_execution(
        &self,
        inventory: &mut openvm_circuit::arch::ExecutorInventoryBuilder<BabyBear, Self::Executor>,
    ) -> Result<(), openvm_circuit::arch::ExecutorInventoryError> {
        for (precompile, record_arenas) in self
            .precompiles
            .iter()
            .zip_eq(self.record_arena_by_air_name.iter())
        {
            let powdr_executor = PowdrExtensionExecutor::Powdr(PowdrExecutor::new(
                self.airs.clone(),
                self.base_config.clone(),
                precompile.apc.clone(),
                record_arenas.clone(),
            ));
            inventory.add_executor(powdr_executor, once(precompile.opcode.global_opcode()))?;
        }

        Ok(())
    }
}

impl<SC> VmCircuitExtension<SC> for PowdrExtension<Val<SC>>
where
    SC: StarkGenericConfig,
    Val<SC>: PrimeField32,
{
    fn extend_circuit(&self, inventory: &mut AirInventory<SC>) -> Result<(), AirInventoryError> {
        for precompile in &self.precompiles {
            match self.implementation {
                PrecompileImplementation::SingleRowChip => {
                    inventory.add_air(PowdrAir::new(precompile.apc.clone()));
                }
                PrecompileImplementation::PlonkChip => {
                    let copy_constraint_bus_id = inventory.new_bus_idx();
                    let plonk_air = PlonkAir {
                        copy_constraint_bus_id,
                        bus_map: self.bus_map.clone(),
                        _marker: std::marker::PhantomData,
                    };
                    inventory.add_air(plonk_air);
                }
            }
        }
        Ok(())
    }
}
