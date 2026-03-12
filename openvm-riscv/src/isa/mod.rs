use std::collections::{BTreeSet, HashSet};

use openvm_circuit::arch::{AirInventory, ChipInventoryError, VmBuilder};
use openvm_instructions::{instruction::Instruction, program::DEFAULT_PC_STEP, VmOpcode};
use openvm_stark_backend::p3_field::PrimeField32;
use openvm_stark_sdk::config::baby_bear_poseidon2::BabyBearPoseidon2Engine;
#[cfg(feature = "cuda")]
use powdr_openvm::{
    isa::OriginalGpuChipComplex, powdr_extension::trace_generator::SharedPeripheryChipsGpu,
};
use powdr_openvm::{
    isa::{OpenVmISA, OriginalCpuChipComplex},
    powdr_extension::trace_generator::cpu::SharedPeripheryChipsCpu,
    program::OriginalCompiledProgram,
    BabyBearSC, SpecializedExecutor,
};
use powdr_riscv_elf::{debug_info::SymbolTable, ElfProgram};
use serde::{Deserialize, Serialize};

#[cfg(feature = "cuda")]
use crate::ExtendedVmConfigGpuBuilder;
use crate::{
    isa::{
        opcode::{branch_opcodes_bigint_set, branch_opcodes_set, instruction_allowlist},
        trace_generator::{create_dummy_airs, create_dummy_chip_complex_cpu},
    },
    ExtendedVmConfig, ExtendedVmConfigCpuBuilder, ExtendedVmConfigExecutor,
};

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

// This seems trivial but it's tricky to put into powdr-openvm  because of some From implementation issues.
impl<F: PrimeField32> From<ExtendedVmConfigExecutor<F>> for SpecializedExecutor<F, RiscvISA> {
    fn from(value: ExtendedVmConfigExecutor<F>) -> Self {
        Self::OriginalExecutor(value)
    }
}

impl OpenVmISA for RiscvISA {
    type Executor<F: PrimeField32> = ExtendedVmConfigExecutor<F>;
    type Config = ExtendedVmConfig;
    type CpuBuilder = ExtendedVmConfigCpuBuilder;
    #[cfg(feature = "cuda")]
    type GpuBuilder = ExtendedVmConfigGpuBuilder;

    fn branching_opcodes() -> HashSet<VmOpcode> {
        branch_opcodes_set()
    }

    fn unconditional_jump_target<F: PrimeField32>(
        instruction: &Instruction<F>,
        pc: u64,
    ) -> Option<u64> {
        match instruction.opcode.as_usize() {
            opcode::OPCODE_JAL => Some(pc + instruction.c.as_canonical_u32() as u64),
            opcode::OPCODE_JALR => None,
            _ => None,
        }
    }

    fn format<F: PrimeField32>(instruction: &Instruction<F>) -> String {
        instruction_formatter::openvm_instruction_formatter(instruction)
    }

    fn allowed_opcodes() -> HashSet<VmOpcode> {
        instruction_allowlist()
    }

    fn create_original_chip_complex(
        config: &Self::Config,
        airs: AirInventory<BabyBearSC>,
    ) -> Result<OriginalCpuChipComplex, ChipInventoryError> {
        <ExtendedVmConfigCpuBuilder as VmBuilder<BabyBearPoseidon2Engine>>::create_chip_complex(
            &ExtendedVmConfigCpuBuilder,
            config,
            airs,
        )
    }

    type LinkedProgram<'a> = ElfProgram;

    fn get_symbol_table<'a>(program: &Self::LinkedProgram<'a>) -> SymbolTable {
        let debug_info = program.debug_info();
        let labels = SymbolTable::from_table(
            debug_info
                .symbols
                .table()
                .iter()
                .map(|(addr, names)| {
                    (
                        *addr,
                        names
                            .iter()
                            .map(|name| rustc_demangle::demangle(name).to_string())
                            .collect(),
                    )
                })
                .collect(),
        );

        labels
    }

    fn get_jump_destinations(program: &OriginalCompiledProgram<Self>) -> BTreeSet<u64> {
        let labels = program.linked_program.text_labels();

        let jump_dest = add_extra_targets(program, labels.clone(), DEFAULT_PC_STEP);

        jump_dest.into_iter().map(Into::into).collect()
    }

    fn create_dummy_airs<E: openvm_circuit::arch::VmCircuitExtension<powdr_openvm::BabyBearSC>>(
        config: &Self::Config,
        shared_chips: E,
    ) -> Result<AirInventory<powdr_openvm::BabyBearSC>, openvm_circuit::arch::AirInventoryError>
    {
        create_dummy_airs(config, shared_chips)
    }

    fn create_dummy_chip_complex_cpu(
        config: &Self::Config,
        circuit: AirInventory<powdr_openvm::BabyBearSC>,
        shared_chips: SharedPeripheryChipsCpu<Self>,
    ) -> Result<OriginalCpuChipComplex, ChipInventoryError> {
        create_dummy_chip_complex_cpu(config, circuit, shared_chips)
    }

    #[cfg(feature = "cuda")]
    fn create_dummy_chip_complex_gpu(
        config: &Self::Config,
        circuit: AirInventory<powdr_openvm::BabyBearSC>,
        shared_chips: SharedPeripheryChipsGpu<Self>,
    ) -> Result<OriginalGpuChipComplex, ChipInventoryError> {
        use crate::isa::trace_generator::create_dummy_chip_complex_gpu;

        create_dummy_chip_complex_gpu(config, circuit, shared_chips)
    }
}

/// Besides the base RISC-V branching instructions, the bigint extension adds two more branching
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
