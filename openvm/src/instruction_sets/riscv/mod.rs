use std::collections::HashSet;

use openvm_circuit::arch::{AirInventory, ChipInventory, ChipInventoryError, VmBuilder};
use openvm_circuit_primitives::bitwise_op_lookup::SharedBitwiseOperationLookupChip;
use openvm_circuit_primitives::range_tuple::SharedRangeTupleCheckerChip;
use openvm_circuit_primitives::var_range::SharedVariableRangeCheckerChip;
use openvm_instructions::{instruction::Instruction, program::DEFAULT_PC_STEP, VmOpcode};
use openvm_sdk::config::{SdkVmConfig, SdkVmConfigExecutor};
use openvm_stark_backend::{
    config::StarkGenericConfig, p3_field::PrimeField32, prover::hal::ProverBackend,
};
use openvm_stark_sdk::config::baby_bear_poseidon2::BabyBearPoseidon2Engine;
use serde::{Deserialize, Serialize};

use crate::{
    get_periphery_bus_ids,
    instruction_sets::{
        riscv::opcode::{branch_opcodes_bigint_set, branch_opcodes_set, instruction_allowlist},
        OpenVmISA, OriginalCpuChipComplex, OriginalCpuChipInventory,
    },
    powdr_extension::trace_generator::{
        cpu::{create_dummy_chip_complex, new_periphery_instances, SharedPeripheryChipsCpu},
        create_dummy_airs,
    },
    BabyBearSC, ExtendedVmConfig, ExtendedVmConfigCpuBuilder,
};

use openvm_sdk::config::SdkVmCpuBuilder;

pub mod instruction_formatter;
pub mod opcode;
pub mod program;
pub mod symbolic_instruction_builder;

// Clone should not be required
#[derive(Clone, Default)]
pub struct RiscvISA;

/// A type to represent register addresses during execution
#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct OpenVmRegisterAddress(u8);

impl OpenVmISA for RiscvISA {
    const DEFAULT_PC_STEP: u32 = DEFAULT_PC_STEP;

    type DummyExecutor = SdkVmConfigExecutor<openvm_stark_sdk::p3_baby_bear::BabyBear>;
    type DummyConfig = SdkVmConfig;
    type DummyInventoryContext = SharedPeripheryChipsCpu;
    type DummyBuilder = SdkVmCpuBuilder;
    type Executor = crate::SpecializedExecutor;
    type OriginalConfig = ExtendedVmConfig;

    fn is_allowed(opcode: VmOpcode) -> bool {
        instruction_allowlist().contains(&opcode)
    }

    fn is_branching(opcode: VmOpcode) -> bool {
        branch_opcodes_set().contains(&opcode)
    }

    fn format<F: PrimeField32>(instruction: &Instruction<F>) -> String {
        instruction_formatter::openvm_instruction_formatter(instruction)
    }

    fn instruction_allowlist() -> HashSet<VmOpcode> {
        instruction_allowlist()
    }

    fn lower(original: Self::OriginalConfig) -> Self::DummyConfig {
        original.sdk
    }

    fn create_original_chip_complex(
        config: &Self::OriginalConfig,
        airs: AirInventory<BabyBearSC>,
    ) -> Result<OriginalCpuChipComplex, ChipInventoryError> {
        <ExtendedVmConfigCpuBuilder as VmBuilder<BabyBearPoseidon2Engine>>::create_chip_complex(
            &ExtendedVmConfigCpuBuilder,
            config,
            airs,
        )
    }
    fn create_dummy_inventory(
        config: &Self::OriginalConfig,
        shared_chips: Self::DummyInventoryContext,
    ) -> OriginalCpuChipInventory {
        let dummy_config = Self::lower(config.clone());
        let airs = create_dummy_airs(&dummy_config, shared_chips.clone())
            .expect("Failed to create dummy airs");

        create_dummy_chip_complex(&dummy_config, airs, shared_chips)
            .expect("Failed to create chip complex")
            .inventory
    }

    fn shared_chips_pair<SC, RA, PB>(
        inventory: &mut ChipInventory<SC, RA, PB>,
    ) -> powdr_openvm_common::trace_generator::cpu::PowdrPeripheryInstancesCpu<
        Self::DummyInventoryContext,
    >
    where
        SC: StarkGenericConfig,
        PB: ProverBackend,
    {
        let bitwise_lookup = inventory
            .find_chip::<SharedBitwiseOperationLookupChip<8>>()
            .next()
            .cloned();
        let range_checker = inventory
            .find_chip::<SharedVariableRangeCheckerChip>()
            .next()
            .unwrap();
        let tuple_range_checker = inventory
            .find_chip::<SharedRangeTupleCheckerChip<2>>()
            .next()
            .cloned();

        new_periphery_instances(
            range_checker.clone(),
            bitwise_lookup,
            tuple_range_checker,
            get_periphery_bus_ids(inventory),
        )
    }

    type RegisterAddress = OpenVmRegisterAddress;

    fn get_register_value(_register: &Self::RegisterAddress) -> u32 {
        todo!()
    }

    fn value_limb(_value: u32, _limb: usize) -> u32 {
        todo!()
    }

    fn apply_interaction(
        periphery: &Self::DummyInventoryContext,
        bus_id: u16,
        mult: u32,
        args: impl Iterator<Item = u32>,
        periphery_bus_ids: &powdr_openvm_common::PeripheryBusIds,
    ) {
        periphery.apply(bus_id, mult, args, periphery_bus_ids);
    }

    /// Besides the base RISCV-V branching instructions, the bigint extension adds two more branching
    /// instruction classes over BranchEqual and BranchLessThan.
    /// Those instructions have the form <INSTR rs0 rs1 target_offset ...>, where target_offset is the
    /// relative jump we're interested in.
    /// This means that for a given program address A containing the instruction above,
    /// we add A + target_offset as a target as well.
    fn extra_targets() -> HashSet<VmOpcode> {
        branch_opcodes_bigint_set()
    }
}
