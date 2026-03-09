use std::sync::Arc;

use openvm_instructions::exe::VmExe;
use openvm_instructions::program::Program as OpenVmProgram;
use openvm_stark_backend::p3_field::PrimeField32;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::blocks::{collect_basic_blocks, BasicBlock, Program};
use powdr_autoprecompiles::DegreeBound;
use serde::{Deserialize, Serialize};

use crate::customize_exe::Instr;
use crate::extraction_utils::OriginalVmConfig;
use crate::isa::OpenVmISA;
use crate::{BabyBearOpenVmApcAdapter, SpecializedConfig};
#[derive(Serialize, Deserialize, Clone)]
#[serde(bound = "")]
pub struct CompiledProgram<ISA: OpenVmISA> {
    pub exe: Arc<VmExe<BabyBear>>,
    pub vm_config: SpecializedConfig<ISA>,
}

// the original openvm program and config without powdr extension, along with the elf
pub struct OriginalCompiledProgram<'a, ISA: OpenVmISA> {
    pub exe: Arc<VmExe<BabyBear>>,
    pub vm_config: OriginalVmConfig<ISA>,
    pub linked_program: ISA::LinkedProgram<'a>,
}

impl<'a, ISA: OpenVmISA> OriginalCompiledProgram<'a, ISA> {
    pub fn new(
        exe: Arc<VmExe<BabyBear>>,
        vm_config: OriginalVmConfig<ISA>,
        linked_program: ISA::LinkedProgram<'a>,
    ) -> Self {
        Self {
            exe,
            vm_config,
            linked_program,
        }
    }

    /// Segments the program into basic blocks
    pub fn collect_basic_blocks(&self) -> Vec<BasicBlock<Instr<BabyBear, ISA>>> {
        let jumpdest_set = ISA::get_jump_destinations(self);

        let program = Prog::from(&self.exe.program);

        collect_basic_blocks::<BabyBearOpenVmApcAdapter<ISA>>(&program, &jumpdest_set)
    }

    /// Converts to a `CompiledProgram` with the original vm config (without autoprecompiles).
    pub fn compiled_program(&self, degree_bound: DegreeBound) -> CompiledProgram<ISA> {
        CompiledProgram {
            exe: self.exe.clone(),
            vm_config: SpecializedConfig::new(self.vm_config.clone(), Vec::new(), degree_bound),
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
