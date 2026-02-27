use openvm_instructions::program::{Program as OpenVmProgram};
use openvm_stark_backend::p3_field::PrimeField32;
use powdr_autoprecompiles::blocks::Program;

use crate::{Instr, instruction_sets::OpenVmISA};

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
