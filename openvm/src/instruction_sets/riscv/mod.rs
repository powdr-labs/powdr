use std::collections::HashSet;

use openvm_circuit::arch::{AirInventory, ChipInventoryError, VmBuilder};
use openvm_instructions::{instruction::Instruction, program::DEFAULT_PC_STEP, VmOpcode};
use openvm_sdk::config::{SdkVmConfig, SdkVmConfigExecutor};
use openvm_stark_backend::p3_field::PrimeField32;
use openvm_stark_sdk::config::baby_bear_poseidon2::BabyBearPoseidon2Engine;
use serde::{Deserialize, Serialize};

use crate::{
    instruction_sets::{
        riscv::opcode::{branch_opcodes_set, instruction_allowlist},
        OpenVmISA, OriginalCpuChipComplex, OriginalCpuChipInventory,
    },
    powdr_extension::trace_generator::{
        cpu::{create_dummy_chip_complex, SharedPeripheryChipsCpu},
        create_dummy_airs,
    },
    BabyBearSC, ExtendedVmConfig, ExtendedVmConfigCpuBuilder,
};

pub mod customize_exe;
pub mod empirical_constraints;
pub mod instruction_formatter;
pub mod opcode;
pub mod program;
pub mod symbolic_instruction_builder;
pub mod trace_generation;

// Clone should not be required
#[derive(Clone)]
pub struct RiscvISA;

/// A type to represent register addresses during execution
#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct OpenVmRegisterAddress(u8);

impl OpenVmISA for RiscvISA {
    const DEFAULT_PC_STEP: u32 = DEFAULT_PC_STEP;

    type DummyExecutor = SdkVmConfigExecutor<openvm_stark_sdk::p3_baby_bear::BabyBear>;
    type DummyConfig = SdkVmConfig;
    type DummyInventoryContext = SharedPeripheryChipsCpu;
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
}
