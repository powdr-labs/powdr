use openvm_instructions::program::Program as OpenVmProgram;
use openvm_stark_backend::p3_field::PrimeField32;
use powdr_autoprecompiles::blocks::Program;

use crate::extraction_utils::OriginalVmConfig;
use crate::{instruction::Instr, isa::OpenVmISA};
use crate::{BabyBearOpenVmApcAdapter, SpecializedConfig};

/// A newtype wrapper around `OpenVmProgram` to implement the `Program` trait.
/// This is necessary because we cannot implement a foreign trait for a foreign type.
pub struct Prog<'a, F>(&'a OpenVmProgram<F>);

impl<'a, F> From<&'a OpenVmProgram<F>> for Prog<'a, F> {
    fn from(program: &'a OpenVmProgram<F>) -> Self {
        Prog(program)
    }
}

impl<'a, F: PrimeField32, ISA: OpenVmISA> Program<Instr<F, ISA>> for Prog<'a, F> {
    fn base_pc(&self) -> u64 {
        self.0.pc_base as u64
    }

    fn instructions(&self) -> Box<dyn Iterator<Item = Instr<F, ISA>> + '_> {
        Box::new(
            self.0
                .instructions_and_debug_infos
                .iter()
                .filter_map(|x| x.as_ref().map(|i| Instr::from(i.0.clone()))),
        )
    }

    fn length(&self) -> u32 {
        self.0.instructions_and_debug_infos.len() as u32
    }
}

use std::{collections::BTreeSet, sync::Arc};

use openvm_instructions::exe::VmExe;
use openvm_instructions::program::DEFAULT_PC_STEP;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::blocks::{collect_basic_blocks, BasicBlock};
use powdr_autoprecompiles::DegreeBound;
use powdr_riscv_elf::ElfProgram;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone)]
#[serde(bound = "")]
pub struct CompiledProgram<ISA: OpenVmISA> {
    pub exe: Arc<VmExe<BabyBear>>,
    pub vm_config: SpecializedConfig<ISA>,
}

// the original openvm program and config without powdr extension, along with the elf
pub struct OriginalCompiledProgram<ISA: OpenVmISA> {
    pub exe: Arc<VmExe<BabyBear>>,
    pub vm_config: OriginalVmConfig<ISA>,
    pub elf: ElfProgram,
}

impl<ISA: OpenVmISA> OriginalCompiledProgram<ISA> {
    /// Segments the program into basic blocks
    pub fn collect_basic_blocks(&self) -> Vec<BasicBlock<Instr<BabyBear, ISA>>> {
        let labels = self.elf.text_labels();

        let jumpdest_set = self.add_extra_targets(labels.clone(), DEFAULT_PC_STEP);

        let program = Prog::from(&self.exe.program);

        // Convert the jump destinations to u64 for compatibility with the `collect_basic_blocks` function.
        let jumpdest_set = jumpdest_set
            .iter()
            .map(|&x| x as u64)
            .collect::<BTreeSet<_>>();

        collect_basic_blocks::<BabyBearOpenVmApcAdapter<ISA>>(&program, &jumpdest_set)
    }

    fn add_extra_targets(&self, mut labels: BTreeSet<u32>, pc_step: u32) -> BTreeSet<u32> {
        let branch_opcodes_bigint = ISA::extra_targets();
        let program = &self.exe.program;
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

    /// Converts to a `CompiledProgram` with the original vm config (without autoprecompiles).
    pub fn compiled_program(&self, degree_bound: DegreeBound) -> CompiledProgram<ISA> {
        CompiledProgram {
            exe: self.exe.clone(),
            vm_config: SpecializedConfig::new(self.vm_config.clone(), Vec::new(), degree_bound),
        }
    }
}
