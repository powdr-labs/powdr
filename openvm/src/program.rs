use std::{collections::BTreeSet, sync::Arc};

use openvm_instructions::exe::VmExe;
use openvm_instructions::program::{Program as OpenVmProgram, DEFAULT_PC_STEP};
use openvm_stark_backend::p3_field::PrimeField32;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::blocks::{collect_basic_blocks, BasicBlock, Program};
use powdr_autoprecompiles::DegreeBound;
use powdr_riscv_elf::ElfProgram;
use serde::{Deserialize, Serialize};

use crate::extraction_utils::OriginalVmConfig;
use crate::opcode::branch_opcodes_bigint_set;
use crate::{BabyBearOpenVmApcAdapter, ExtendedVmConfig, Instr, SpecializedConfig};

#[derive(Serialize, Deserialize, Clone)]
pub struct CompiledProgram {
    pub exe: Arc<VmExe<BabyBear>>,
    pub vm_config: SpecializedConfig,
}

// the original openvm program and config without powdr extension, along with the elf
pub struct OriginalCompiledProgram {
    pub exe: Arc<VmExe<BabyBear>>,
    pub vm_config: ExtendedVmConfig,
    pub elf: ElfProgram,
}

impl OriginalCompiledProgram {
    /// Segments the program into basic blocks. The degree bound does not influence the result (see TODO below).
    pub fn collect_basic_blocks(
        &self,
        degree_bound: DegreeBound,
    ) -> Vec<BasicBlock<Instr<BabyBear>>> {
        // TODO: This only needs to build the airs so that `collect_basic_blocks` can call
        // `is_branching` and `is_allowed` on it. If we had a better way to do that, we could
        // remove the degree_bound parameter.
        let labels = self.elf.text_labels();
        let original_config = OriginalVmConfig::new(self.vm_config.clone());
        let airs = original_config.airs(degree_bound).expect("Failed to convert the AIR of an OpenVM instruction, even after filtering by the blacklist!");

        let jumpdest_set = self.add_extra_targets(labels.clone(), DEFAULT_PC_STEP);

        let program = Prog(&self.exe.program);

        // Convert the jump destinations to u64 for compatibility with the `collect_basic_blocks` function.
        let jumpdest_set = jumpdest_set
            .iter()
            .map(|&x| x as u64)
            .collect::<BTreeSet<_>>();

        collect_basic_blocks::<BabyBearOpenVmApcAdapter>(&program, &jumpdest_set, &airs)
    }

    /// Besides the base RISCV-V branching instructions, the bigint extension adds two more branching
    /// instruction classes over BranchEqual and BranchLessThan.
    /// Those instructions have the form <INSTR rs0 rs1 target_offset ...>, where target_offset is the
    /// relative jump we're interested in.
    /// This means that for a given program address A containing the instruction above,
    /// we add A + target_offset as a target as well.
    fn add_extra_targets(&self, mut labels: BTreeSet<u32>, pc_step: u32) -> BTreeSet<u32> {
        let branch_opcodes_bigint = branch_opcodes_bigint_set();
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
    pub fn compiled_program(&self, degree_bound: DegreeBound) -> CompiledProgram {
        CompiledProgram {
            exe: self.exe.clone(),
            vm_config: SpecializedConfig::new(
                OriginalVmConfig::new(self.vm_config.clone()),
                Vec::new(),
                degree_bound,
            ),
        }
    }
}

/// A newtype wrapper around `OpenVmProgram` to implement the `Program` trait.
/// This is necessary because we cannot implement a foreign trait for a foreign type.
pub struct Prog<'a, F>(&'a OpenVmProgram<F>);

impl<'a, F> From<&'a OpenVmProgram<F>> for Prog<'a, F> {
    fn from(program: &'a OpenVmProgram<F>) -> Self {
        Prog(program)
    }
}

impl<'a, F: PrimeField32> Program<Instr<F>> for Prog<'a, F> {
    fn base_pc(&self) -> u64 {
        self.0.pc_base as u64
    }

    fn instructions(&self) -> Box<dyn Iterator<Item = Instr<F>> + '_> {
        Box::new(
            self.0
                .instructions_and_debug_infos
                .iter()
                .filter_map(|x| x.as_ref().map(|i| Instr(i.0.clone()))),
        )
    }

    fn length(&self) -> u32 {
        self.0.instructions_and_debug_infos.len() as u32
    }
}
