// Mostly taken from [this openvm extension](https://github.com/openvm-org/openvm/blob/1b76fd5a900a7d69850ee9173969f70ef79c4c76/extensions/rv32im/circuit/src/extension.rs#L185) and simplified to only handle a single opcode with its necessary dependencies

use std::iter::once;
use std::sync::Arc;

use derive_more::From;
use openvm_circuit_derive::{Executor, MeteredExecutor, PreflightExecutor};
use openvm_instructions::LocalOpcode;
use openvm_sdk::SC;
use openvm_stark_sdk::{engine::StarkEngine, p3_baby_bear::BabyBear};

use crate::customize_exe::OvmApcStats;
use crate::extraction_utils::{OriginalAirs, OriginalVmConfig};
use crate::powdr_extension::chip::PowdrAir;
use crate::powdr_extension::executor::PowdrPeripheryInstances;
use crate::{bus_map::BusMap, BabyBearSC};
use openvm_circuit::{
    arch::{
        AirInventory, AirInventoryError, ChipInventory, RowMajorMatrixArena, VmCircuitExtension,
        VmExecutionExtension, VmProverExtension,
    },
    circuit_derive::{Chip, ChipUsageGetter},
    derive::AnyEnum,
};
use openvm_circuit_primitives::bitwise_op_lookup::SharedBitwiseOperationLookupChip;
use openvm_circuit_primitives::range_tuple::SharedRangeTupleCheckerChip;
use openvm_circuit_primitives::var_range::SharedVariableRangeCheckerChip;
use openvm_stark_backend::{
    config::{StarkGenericConfig, Val},
    p3_field::{Field, PrimeField32},
    prover::cpu::{CpuBackend, CpuDevice},
    ChipUsageGetter,
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
    pub base_config: OriginalVmConfig,
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
        base_config: OriginalVmConfig,
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

#[derive(ChipUsageGetter, From, AnyEnum, PreflightExecutor, Executor, MeteredExecutor, Chip)]
#[allow(clippy::large_enum_variant)]
pub enum PowdrExecutor {
    Powdr(PowdrChip),
    Plonk(PlonkChip),
}

impl PowdrExecutor {
    pub fn air_name(&self) -> String {
        match self {
            PowdrExecutor::Powdr(powdr_chip) => powdr_chip.air_name(),
            PowdrExecutor::Plonk(plonk_chip) => plonk_chip.air_name(),
        }
    }
}

impl VmExecutionExtension<BabyBear> for PowdrExtension<BabyBear> {
    type Executor = PowdrExecutor;

    // TODO: this part seems duplicated to `extend_prover`, so need to study the split of functionalities between them
    fn extend_execution(
        &self,
        inventory: &mut openvm_circuit::arch::ExecutorInventoryBuilder<BabyBear, Self::Executor>,
    ) -> Result<(), openvm_circuit::arch::ExecutorInventoryError> {
        let chip_complex = &self.base_config.chip_complex();

        let chip_inventory = &chip_complex.inventory;

        // TODO: here we make assumptions about the existence of some chips in the periphery. Make this more flexible
        let bitwise_lookup = chip_inventory
            .find_chip::<SharedBitwiseOperationLookupChip<8>>()
            .next()
            .cloned();
        let range_checker = chip_inventory
            .find_chip::<SharedVariableRangeCheckerChip>()
            .next()
            .unwrap();
        let tuple_range_checker = chip_inventory
            .find_chip::<SharedRangeTupleCheckerChip<2>>()
            .next()
            .cloned();

        // Create the shared chips and the dummy shared chips
        let shared_chips_pair = PowdrPeripheryInstances::new(
            range_checker.clone(),
            bitwise_lookup,
            tuple_range_checker,
        );

        for precompile in self.precompiles.iter() {
            let powdr_executor: PowdrExecutor = match self.implementation {
                PrecompileImplementation::SingleRowChip => PowdrChip::new(
                    precompile.clone(),
                    self.airs.clone(),
                    unimplemented!("no access to memory here"),
                    // offline_memory.clone(),
                    self.base_config.config().clone(),
                    shared_chips_pair.clone(),
                )
                .into(),
                PrecompileImplementation::PlonkChip => {
                    // let copy_constraint_bus_id = inventory.new_bus_idx();
                    let copy_constraint_bus_id = unimplemented!(
                        "cannot create a new bus id here, probably in VmCircuitExtension?"
                    );
                    PlonkChip::new(
                        precompile.clone(),
                        self.airs.clone(),
                        unimplemented!("no access to memory here"),
                        // offline_memory.clone(),
                        self.base_config.config().clone(),
                        shared_chips_pair.clone(),
                        self.bus_map.clone(),
                        copy_constraint_bus_id,
                    )
                    .into()
                }
            };
            inventory.add_executor(powdr_executor, once(precompile.opcode.global_opcode()))?;
        }

        Ok(())
    }
}

impl<E, RA> VmProverExtension<E, RA, PowdrExtension<BabyBear>> for PowdrExtension<BabyBear>
where
    E: StarkEngine<SC = BabyBearSC, PB = CpuBackend<BabyBearSC>, PD = CpuDevice<BabyBearSC>>,
    RA: RowMajorMatrixArena<BabyBear>,
{
    fn extend_prover(
        &self,
        extension: &PowdrExtension<Val<SC>>,
        inventory: &mut ChipInventory<E::SC, RA, E::PB>,
    ) -> Result<(), openvm_circuit::arch::ChipInventoryError> {
        let offline_memory: () = unimplemented!("no access to memory here");

        // TODO: here we make assumptions about the existence of some chips in the periphery. Make this more flexible
        let bitwise_lookup = inventory
            .find_chip::<SharedBitwiseOperationLookupChip<8>>()
            .next()
            .cloned();
        let range_checker = *inventory
            .find_chip::<SharedVariableRangeCheckerChip>()
            .next()
            .unwrap();
        let tuple_range_checker = inventory
            .find_chip::<SharedRangeTupleCheckerChip<2>>()
            .next()
            .cloned();

        // Create the shared chips and the dummy shared chips
        let shared_chips_pair =
            PowdrPeripheryInstances::new(range_checker, bitwise_lookup, tuple_range_checker);

        if let Some(precompile) = self.precompiles.first() {
            let powdr_chip: PowdrExecutor = match self.implementation {
                PrecompileImplementation::SingleRowChip => PowdrChip::new(
                    precompile.clone(),
                    self.airs.clone(),
                    unimplemented!("no access to memory here"),
                    // offline_memory.clone(),
                    self.base_config.config().clone(),
                    shared_chips_pair.clone(),
                )
                .into(),
                PrecompileImplementation::PlonkChip => {
                    // let copy_constraint_bus_id = inventory.new_bus_idx();
                    let copy_constraint_bus_id = unimplemented!(
                        "cannot create a new bus id here, probably in VmCircuitExtension?"
                    );

                    PlonkChip::new(
                        precompile.clone(),
                        self.airs.clone(),
                        unimplemented!("no access to memory here"),
                        // offline_memory.clone(),
                        self.base_config.config().clone(),
                        shared_chips_pair.clone(),
                        self.bus_map.clone(),
                        copy_constraint_bus_id,
                    )
                    .into()
                }
            };

            // TODO: no execution stuff is done here, maybe in VmExecutionExtension?
            // inventory.add_executor(powdr_chip, once(precompile.opcode.global_opcode()))?;
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
        for apc in self.precompiles.iter() {
            inventory.add_air(PowdrAir::new(apc.apc.clone()));
        }
        Ok(())
    }
}
