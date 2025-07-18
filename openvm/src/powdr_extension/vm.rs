// Mostly taken from [this openvm extension](https://github.com/openvm-org/openvm/blob/1b76fd5a900a7d69850ee9173969f70ef79c4c76/extensions/rv32im/circuit/src/extension.rs#L185) and simplified to only handle a single opcode with its necessary dependencies

use std::iter::once;

use derive_more::From;
use openvm_circuit_derive::InstructionExecutor;
use powdr_autoprecompiles::expression::AlgebraicReference;

use crate::customize_exe::OvmApcStats;
use crate::extraction_utils::OriginalAirs;
use crate::powdr_extension::executor::PowdrPeripheryInstances;
use openvm_circuit::arch::VmInventoryError;
use openvm_circuit::{
    arch::{VmExtension, VmInventory},
    circuit_derive::{Chip, ChipUsageGetter},
    derive::AnyEnum,
    system::phantom::PhantomChip,
};
use openvm_circuit_primitives::bitwise_op_lookup::SharedBitwiseOperationLookupChip;
use openvm_circuit_primitives::range_tuple::SharedRangeTupleCheckerChip;
use openvm_circuit_primitives::var_range::SharedVariableRangeCheckerChip;
use openvm_instructions::VmOpcode;
use openvm_instructions::{instruction::Instruction, LocalOpcode};
use openvm_sdk::config::{SdkVmConfig, SdkVmConfigPeriphery};
use openvm_stark_backend::{
    p3_field::{Field, PrimeField32},
    ChipUsageGetter,
};
use powdr_autoprecompiles::SymbolicMachine;
use serde::{Deserialize, Serialize};

use crate::{BusMap, PrecompileImplementation};

use super::plonk::chip::PlonkChip;
use super::{chip::PowdrChip, PowdrOpcode};

#[derive(Clone, Deserialize, Serialize)]
#[serde(bound = "F: Field")]
pub struct PowdrExtension<F> {
    pub precompiles: Vec<PowdrPrecompile<F>>,
    pub base_config: SdkVmConfig,
    pub implementation: PrecompileImplementation,
    pub bus_map: BusMap,
    pub airs: OriginalAirs<F>,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct OriginalInstruction<F> {
    pub instruction: Instruction<F>,
    /// The autoprecompile poly_ids that the instruction points to, in the same order as the corresponding original columns
    pub subs: Vec<u64>,
}

impl<F> OriginalInstruction<F> {
    pub fn new(instruction: Instruction<F>, subs: Vec<u64>) -> Self {
        Self { instruction, subs }
    }

    pub fn opcode(&self) -> VmOpcode {
        self.instruction.opcode
    }
}

impl<F> AsRef<Instruction<F>> for OriginalInstruction<F> {
    fn as_ref(&self) -> &Instruction<F> {
        &self.instruction
    }
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(bound = "F: Field")]
pub struct PowdrPrecompile<F> {
    pub name: String,
    pub opcode: PowdrOpcode,
    pub machine: SymbolicMachine<F>,
    pub original_instructions: Vec<OriginalInstruction<F>>,
    pub is_valid_column: AlgebraicReference,
    pub apc_stats: Option<OvmApcStats>,
}

impl<F> PowdrPrecompile<F> {
    pub fn new(
        name: String,
        opcode: PowdrOpcode,
        machine: SymbolicMachine<F>,
        original_instructions: Vec<OriginalInstruction<F>>,
        is_valid_column: AlgebraicReference,
        apc_stats: Option<OvmApcStats>,
    ) -> Self {
        Self {
            name,
            opcode,
            machine,
            original_instructions,
            is_valid_column,
            apc_stats,
        }
    }
}

impl<F> PowdrExtension<F> {
    pub fn new(
        precompiles: Vec<PowdrPrecompile<F>>,
        base_config: SdkVmConfig,
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

#[derive(ChipUsageGetter, From, AnyEnum, InstructionExecutor, Chip)]
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

#[derive(From, ChipUsageGetter, Chip, AnyEnum)]
pub enum PowdrPeriphery<F: PrimeField32> {
    Sdk(SdkVmConfigPeriphery<F>),
    Phantom(PhantomChip<F>),
}

impl<F: PrimeField32> VmExtension<F> for PowdrExtension<F> {
    type Executor = PowdrExecutor<F>;

    type Periphery = PowdrPeriphery<F>;

    fn build(
        &self,
        builder: &mut openvm_circuit::arch::VmInventoryBuilder<F>,
    ) -> Result<VmInventory<Self::Executor, Self::Periphery>, VmInventoryError> {
        let mut inventory = VmInventory::new();

        let offline_memory = builder.system_base().offline_memory();

        // TODO: here we make assumptions about the existence of some chips in the periphery. Make this more flexible
        let bitwise_lookup = builder
            .find_chip::<SharedBitwiseOperationLookupChip<8>>()
            .first()
            .cloned();
        let range_checker = *builder
            .find_chip::<SharedVariableRangeCheckerChip>()
            .first()
            .unwrap();
        let tuple_range_checker = builder
            .find_chip::<SharedRangeTupleCheckerChip<2>>()
            .first()
            .cloned();

        // Create the shared chips and the dummy shared chips
        let shared_chips_pair =
            PowdrPeripheryInstances::new(range_checker, bitwise_lookup, tuple_range_checker);

        for precompile in &self.precompiles {
            let powdr_chip: PowdrExecutor<F> = match self.implementation {
                PrecompileImplementation::SingleRowChip => PowdrChip::new(
                    precompile.clone(),
                    self.airs.clone(),
                    offline_memory.clone(),
                    self.base_config.clone(),
                    shared_chips_pair.clone(),
                )
                .into(),
                PrecompileImplementation::PlonkChip => {
                    let copy_constraint_bus_id = builder.new_bus_idx();

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
