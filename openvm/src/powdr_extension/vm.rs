// Mostly taken from [this openvm extension](https://github.com/openvm-org/openvm/blob/1b76fd5a900a7d69850ee9173969f70ef79c4c76/extensions/rv32im/circuit/src/extension.rs#L185) and simplified to only handle a single opcode with its necessary dependencies

use std::collections::BTreeMap;
use std::iter::once;

use derive_more::From;

use openvm_circuit::arch::{InstructionExecutor, VmInventoryError};
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
use openvm_stark_backend::config::StarkGenericConfig;
use openvm_stark_backend::{
    p3_field::{Field, PrimeField32},
    Chip,
};
use powdr_autoprecompiles::powdr::Column;
use powdr_autoprecompiles::SymbolicMachine;
use powdr_number::OpenVmField;
use serde::{Deserialize, Serialize};

use crate::PrecompileImplementation;
use crate::F;

use super::chip::SharedChips;
use super::plonk::chip::PlonkChip;
use super::{chip::PowdrChip, PowdrOpcode};

#[derive(Clone, Deserialize, Serialize)]
#[serde(bound = "P::OpenVmField: Field")]
pub struct PowdrExtension<P: OpenVmField> {
    pub precompiles: Vec<PowdrPrecompile<P>>,
    pub base_config: SdkVmConfig,
    pub implementation: PrecompileImplementation,
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
#[serde(bound = "P::OpenVmField: Field")]
pub struct PowdrPrecompile<P: OpenVmField> {
    pub name: String,
    pub opcode: PowdrOpcode,
    pub machine: SymbolicMachine<P>,
    pub original_instructions: Vec<OriginalInstruction<F<P>>>,
    pub original_airs: BTreeMap<usize, SymbolicMachine<P>>,
    pub is_valid_column: Column,
}

impl<P: OpenVmField> PowdrPrecompile<P> {
    pub fn new(
        name: String,
        opcode: PowdrOpcode,
        machine: SymbolicMachine<P>,
        original_instructions: Vec<OriginalInstruction<F<P>>>,
        original_airs: BTreeMap<usize, SymbolicMachine<P>>,
        is_valid_column: Column,
    ) -> Self {
        Self {
            name,
            opcode,
            machine,
            original_instructions,
            original_airs,
            is_valid_column,
        }
    }
}

impl<P: OpenVmField> PowdrExtension<P> {
    pub fn new(
        precompiles: Vec<PowdrPrecompile<P>>,
        base_config: SdkVmConfig,
        implementation: PrecompileImplementation,
    ) -> Self {
        Self {
            precompiles,
            base_config,
            implementation,
        }
    }
}

#[derive(ChipUsageGetter, From, AnyEnum)]
#[allow(clippy::large_enum_variant)]
pub enum PowdrExecutor<P: OpenVmField> {
    Powdr(PowdrChip<P>),
    Plonk(PlonkChip<P>),
}

impl<SC: StarkGenericConfig, P: OpenVmField> Chip<SC> for PowdrExecutor<P> {
    fn generate_air_proof_input(self) -> openvm_stark_backend::prover::types::AirProofInput<SC> {
        unimplemented!("PowdrExecutor does not support generating air proof input")
    }

    fn air(&self) -> std::sync::Arc<dyn openvm_stark_backend::rap::AnyRap<SC>> {
        unimplemented!("PowdrExecutor does not support air generation")
    }
}

impl<P: OpenVmField> InstructionExecutor<F<P>> for PowdrExecutor<P> {
    fn execute(
        &mut self,
        memory: &mut openvm_circuit::system::memory::MemoryController<F<P>>,
        instruction: &Instruction<F<P>>,
        from_state: openvm_circuit::arch::ExecutionState<u32>,
    ) -> openvm_circuit::arch::Result<openvm_circuit::arch::ExecutionState<u32>> {
        match self {
            PowdrExecutor::Powdr(powdr_chip) => powdr_chip.execute(memory, instruction, from_state),
            PowdrExecutor::Plonk(plonk_chip) => plonk_chip.execute(memory, instruction, from_state),
        }
    }

    fn get_opcode_name(&self, opcode: usize) -> String {
        match self {
            PowdrExecutor::Powdr(powdr_chip) => powdr_chip.get_opcode_name(opcode),
            PowdrExecutor::Plonk(plonk_chip) => plonk_chip.get_opcode_name(opcode),
        }
    }
}

#[derive(From, ChipUsageGetter, Chip, AnyEnum)]
pub enum PowdrPeriphery<F: PrimeField32> {
    Sdk(SdkVmConfigPeriphery<F>),
    Phantom(PhantomChip<F>),
}

impl<P: OpenVmField> VmExtension<F<P>> for PowdrExtension<P> {
    type Executor = PowdrExecutor<P>;

    type Periphery = PowdrPeriphery<F<P>>;

    fn build(
        &self,
        builder: &mut openvm_circuit::arch::VmInventoryBuilder<F<P>>,
    ) -> Result<VmInventory<Self::Executor, Self::Periphery>, VmInventoryError> {
        let mut inventory = VmInventory::new();

        let offline_memory = builder.system_base().offline_memory();

        // TODO: here we make assumptions about the existence of some chips in the periphery. Make this more flexible
        let bitwise_lookup = *builder
            .find_chip::<SharedBitwiseOperationLookupChip<8>>()
            .first()
            .unwrap();
        let range_checker = *builder
            .find_chip::<SharedVariableRangeCheckerChip>()
            .first()
            .unwrap();
        let tuple_range_checker = builder
            .find_chip::<SharedRangeTupleCheckerChip<2>>()
            .first()
            .cloned();

        for precompile in &self.precompiles {
            let powdr_chip: PowdrExecutor<P> = match self.implementation {
                PrecompileImplementation::SingleRowChip => PowdrChip::new(
                    precompile.clone(),
                    offline_memory.clone(),
                    self.base_config.clone(),
                    SharedChips::new(
                        bitwise_lookup.clone(),
                        range_checker.clone(),
                        tuple_range_checker.cloned(),
                    ),
                )
                .into(),
                PrecompileImplementation::PlonkChip => PlonkChip::new(
                    precompile.clone(),
                    offline_memory.clone(),
                    self.base_config.clone(),
                    SharedChips::new(
                        bitwise_lookup.clone(),
                        range_checker.clone(),
                        tuple_range_checker.cloned(),
                    ),
                )
                .into(),
            };

            inventory.add_executor(powdr_chip, once(precompile.opcode.global_opcode()))?;
        }

        Ok(inventory)
    }
}
