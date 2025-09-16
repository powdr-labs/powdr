// Mostly taken from [this openvm extension](https://github.com/openvm-org/openvm/blob/1b76fd5a900a7d69850ee9173969f70ef79c4c76/extensions/rv32im/circuit/src/extension.rs#L185) and simplified to only handle a single opcode with its necessary dependencies

use std::iter::once;
use std::sync::Arc;

use derive_more::From;
use openvm_circuit_derive::PreflightExecutor;

use crate::bus_map::BusMap;
use crate::customize_exe::OvmApcStats;
use crate::extraction_utils::OriginalAirs;
use crate::powdr_extension::executor::PowdrPeripheryInstances;
use openvm_circuit::{
    arch::{AirInventory, AirInventoryError, ChipInventory, VmCircuitExtension},
    circuit_derive::{Chip, ChipUsageGetter},
    derive::AnyEnum,
    system::phantom::PhantomChip,
};
use openvm_circuit_primitives::bitwise_op_lookup::SharedBitwiseOperationLookupChip;
use openvm_circuit_primitives::range_tuple::SharedRangeTupleCheckerChip;
use openvm_circuit_primitives::var_range::SharedVariableRangeCheckerChip;
use openvm_instructions::LocalOpcode;
use openvm_stark_backend::{
    config::StarkGenericConfig,
    p3_field::{Field, PrimeField32},
    
};
use powdr_autoprecompiles::Apc;
use serde::{Deserialize, Serialize};

use crate::{ExtendedVmConfig, Instr, PrecompileImplementation};

use super::plonk::chip::PlonkChip;
use super::{chip::PowdrChip, PowdrOpcode};

#[derive(Clone, Deserialize, Serialize)]
#[serde(bound = "F: Field")]
pub struct PowdrExtension<F> {
    pub precompiles: Vec<PowdrPrecompile<F>>,
    pub base_config: ExtendedVmConfig,
    pub implementation: PrecompileImplementation,
    pub bus_map: BusMap,
    pub airs: OriginalAirs<F>,
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
        base_config: ExtendedVmConfig,
        implementation: PrecompileImplementation,
        bus_map: BusMap,
        airs: OriginalAirs<F>,
    ) -> Self {
        Self {
            precompiles,
            base_config,
            implementation,
            bus_map,
            airs,
        }
    }
}

#[derive( From, AnyEnum, PreflightExecutor, Chip)]
#[allow(clippy::large_enum_variant)]
pub enum PowdrExecutor<F: PrimeField32> {
    Powdr(PowdrChip<F>),
    Plonk(PlonkChip<F>),
}

impl<F: PrimeField32> PowdrExecutor<F> {
    pub fn air_name(&self) -> String {
        match self {
            PowdrExecutor::Powdr(powdr_chip) => powdr_chip.air_name(),
            PowdrExecutor::Plonk(plonk_chip) => plonk_chip.air_name(),
        }
    }
}

impl<SC: StarkGenericConfig> VmCircuitExtension<SC> for PowdrExtension<SC> {
    fn extend_circuit(&self, inventory: &mut AirInventory<SC>) -> Result<(), AirInventoryError> {
        let offline_memory = inventory.system_base().offline_memory();

        // TODO: here we make assumptions about the existence of some chips in the periphery. Make this more flexible
        let bitwise_lookup = inventory
            .find_chip::<SharedBitwiseOperationLookupChip<8>>()
            .first()
            .cloned();
        let range_checker = *inventory
            .find_chip::<SharedVariableRangeCheckerChip>()
            .first()
            .unwrap();
        let tuple_range_checker = inventory
            .find_chip::<SharedRangeTupleCheckerChip<2>>()
            .first()
            .cloned();

        // Create the shared chips and the dummy shared chips
        let shared_chips_pair =
            PowdrPeripheryInstances::new(range_checker, bitwise_lookup, tuple_range_checker);

        for precompile in &self.precompiles {
            let powdr_chip: PowdrExecutor<_> = match self.implementation {
                PrecompileImplementation::SingleRowChip => PowdrChip::new(
                    precompile.clone(),
                    self.airs.clone(),
                    offline_memory.clone(),
                    self.base_config.clone(),
                    shared_chips_pair.clone(),
                )
                .into(),
                PrecompileImplementation::PlonkChip => {
                    let copy_constraint_bus_id = inventory.new_bus_idx();

                    PlonkChip::new(
                        precompile.clone(),
                        self.airs.clone(),
                        offline_memory.clone(),
                        self.base_config.clone(),
                        shared_chips_pair.clone(),
                        self.bus_map.clone(),
                        copy_constraint_bus_id,
                    )
                    .into()
                }
            };

            inventory.add_executor(powdr_chip, once(precompile.opcode.global_opcode()))?;
        }

        Ok(inventory)
    }
}
