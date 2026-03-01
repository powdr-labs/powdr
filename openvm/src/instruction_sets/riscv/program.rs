use std::{collections::BTreeSet, sync::Arc};

use openvm_instructions::exe::VmExe;
use openvm_instructions::program::DEFAULT_PC_STEP;
use openvm_stark_backend::p3_field::PrimeField32;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::blocks::{collect_basic_blocks, BasicBlock};
use powdr_autoprecompiles::DegreeBound;
use powdr_openvm_common::extraction_utils::OriginalVmConfig;
use powdr_openvm_common::{BabyBearOpenVmApcAdapter, SpecializedConfig};
use powdr_riscv_elf::ElfProgram;
use serde::{Deserialize, Serialize};

use crate::instruction_sets::riscv::opcode::branch_opcodes_bigint_set;
use crate::instruction_sets::OpenVmISA;
use crate::{Instr, Prog, RiscvISA};

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
    pub fn collect_basic_blocks(&self) -> Vec<BasicBlock<Instr<BabyBear, RiscvISA>>> {
        let labels = self.elf.text_labels();

        let jumpdest_set = self.add_extra_targets(labels.clone(), DEFAULT_PC_STEP);

        let program = Prog::from(&self.exe.program);

        // Convert the jump destinations to u64 for compatibility with the `collect_basic_blocks` function.
        let jumpdest_set = jumpdest_set
            .iter()
            .map(|&x| x as u64)
            .collect::<BTreeSet<_>>();

        collect_basic_blocks::<BabyBearOpenVmApcAdapter<RiscvISA>>(&program, &jumpdest_set)
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
    pub fn compiled_program(&self, degree_bound: DegreeBound) -> CompiledProgram<ISA> {
        CompiledProgram {
            exe: self.exe.clone(),
            vm_config: SpecializedConfig::new(self.vm_config.clone(), Vec::new(), degree_bound),
        }
    }
}
