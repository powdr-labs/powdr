use std::collections::{BTreeMap, BTreeSet, HashSet};

use openvm_circuit::arch::{AirInventory, ChipInventoryError, VmBuilder};
use openvm_instructions::{instruction::Instruction, program::DEFAULT_PC_STEP, VmOpcode};
use openvm_stark_backend::p3_field::PrimeField32;
use openvm_stark_sdk::config::baby_bear_poseidon2::BabyBearPoseidon2Engine;
use powdr_openvm_common::{
    isa::{OpenVmISA, OriginalCpuChipComplex, OriginalCpuChipInventory, SpecializedExecutor},
    program::OriginalCompiledProgram,
};
use powdr_riscv_elf::ElfProgram;
use serde::{Deserialize, Serialize};

use crate::{
    isa::{
        opcode::{branch_opcodes_bigint_set, branch_opcodes_set, instruction_allowlist},
        trace_generator::{
            cpu::{create_dummy_chip_complex, SharedPeripheryChipsCpu},
            create_dummy_airs,
        },
    },
    BabyBearSC, ExtendedVmConfig, ExtendedVmConfigCpuBuilder, ExtendedVmConfigExecutor,
};

/// The core logic of our extension
pub mod chip;
pub mod instruction_formatter;
pub mod opcode;
pub mod symbolic_instruction_builder;
/// The trace generator for the powdr instructions
pub mod trace_generator;

// Clone should not be required
#[derive(Clone, Default)]
pub struct RiscvISA;

/// A type to represent register addresses during execution
#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct OpenVmRegisterAddress(u8);

// This seems trivial but it's tricky to put into powdr-openvm-common because of some From implementation issues.
impl<F: PrimeField32> From<ExtendedVmConfigExecutor<F>> for SpecializedExecutor<F, RiscvISA> {
    fn from(value: ExtendedVmConfigExecutor<F>) -> Self {
        Self::OriginalExecutor(value)
    }
}

impl OpenVmISA for RiscvISA {
    type OriginalExecutor<F: PrimeField32> = ExtendedVmConfigExecutor<F>;
    type OriginalConfig = ExtendedVmConfig;
    type OriginalBuilder = ExtendedVmConfigCpuBuilder;

    fn is_branching(opcode: VmOpcode) -> bool {
        branch_opcodes_set().contains(&opcode)
    }

    fn format<F: PrimeField32>(instruction: &Instruction<F>) -> String {
        instruction_formatter::openvm_instruction_formatter(instruction)
    }

    fn instruction_allowlist() -> HashSet<VmOpcode> {
        instruction_allowlist()
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
        shared_chips: SharedPeripheryChipsCpu<Self>,
    ) -> OriginalCpuChipInventory {
        let airs =
            create_dummy_airs(config, shared_chips.clone()).expect("Failed to create dummy airs");

        create_dummy_chip_complex(config, airs, shared_chips)
            .expect("Failed to create chip complex")
            .inventory
    }

    type RegisterAddress = OpenVmRegisterAddress;

    fn get_register_value(_register: &Self::RegisterAddress) -> u32 {
        unimplemented!("execution constraints are currently unused")
    }

    fn value_limb(_value: u32, _limb: usize) -> u32 {
        unimplemented!("execution constraints are currently unused")
    }

    type Program<'a> = ElfProgram;

    fn get_labels(program: &OriginalCompiledProgram<Self>) -> BTreeMap<u64, Vec<String>> {
        let labels = program.elf.text_labels();
        add_extra_targets(program, labels.clone(), DEFAULT_PC_STEP);

        let debug_info = program.elf.debug_info();
        // tracing::info!(
        //     "Got {} basic blocks from `collect_basic_blocks`",
        //     blocks.len()
        // );
        // if tracing::enabled!(tracing::Level::DEBUG) {
        //     tracing::debug!("Basic blocks sorted by execution count (top 10):");
        //     for (count, block) in blocks
        //         .iter()
        //         .filter_map(|block| Some((pgo.pc_execution_count(block.start_pc)?, block)))
        //         .sorted_by_key(|(count, _)| *count)
        //         .rev()
        //         .take(10)
        //     {
        //         let name = debug_info
        //             .symbols
        //             .try_get_one_or_preceding(block.start_pc)
        //             .map(|(symbol, offset)| format!("{} + {offset}", rustc_demangle::demangle(symbol)))
        //             .unwrap_or_default();
        //         tracing::debug!("Basic block (executed {count} times), {name}:\n{block}",);
        //     }
        // }

        let labels = debug_info
            .symbols
            .table()
            .iter()
            .map(|(addr, names)| {
                (
                    *addr as u64,
                    names
                        .iter()
                        .map(|name| rustc_demangle::demangle(name).to_string())
                        .collect(),
                )
            })
            .collect();

        labels
    }
}

/// Besides the base RISCV-V branching instructions, the bigint extension adds two more branching
/// instruction classes over BranchEqual and BranchLessThan.
/// Those instructions have the form <INSTR rs0 rs1 target_offset ...>, where target_offset is the
/// relative jump we're interested in.
/// This means that for a given program address A containing the instruction above,
/// we add A + target_offset as a target as well.
fn add_extra_targets(
    compiled_program: &OriginalCompiledProgram<RiscvISA>,
    mut labels: BTreeSet<u32>,
    pc_step: u32,
) -> BTreeSet<u32> {
    let branch_opcodes_bigint = branch_opcodes_bigint_set();
    let program = &compiled_program.exe.program;
    let new_labels = program
        .instructions_and_debug_infos
        .iter()
        .enumerate()
        .filter_map(|(i, instr)| {
            let instr = instr.as_ref().unwrap().0.clone();
            let adjusted_pc = program.pc_base + (i as u32) * pc_step;
            let op = instr.opcode;
            branch_opcodes_bigint
                .contains(&op)
                .then_some(adjusted_pc + instr.c.as_canonical_u32())
        });
    labels.extend(new_labels);

    labels
}
