use std::collections::{HashSet, VecDeque};

use openvm_instructions::instruction::Instruction;
use openvm_instructions::LocalOpcode;
use openvm_stark_backend::p3_field::PrimeField32;

use crate::{
    customize_exe::{BasicBlock, OPENVM_INIT_PC},
    instruction_formatter::openvm_instruction_formatter,
};

pub enum PanicBehaviour {
    /// This basic block unconditionally leads to a panic.
    AlwaysLeadsToPanic,
    /// This basic block might lead to successful termination.
    MightNotLeadToPanic,
}

pub fn analyze_basic_blocks<'a, F: PrimeField32>(
    blocks: &'a [BasicBlock<F>],
    known_to_panic: impl IntoIterator<Item = &'a BasicBlock<F>>,
) /*-> impl Iterator<Item = (&'a BasicBlock<F>, PanicBehaviour)> */
{
    let known_to_panic: HashSet<BasicBlockIdentifier> =
        known_to_panic.into_iter().map(Into::into).collect();
    let mut queue = blocks.iter().collect::<VecDeque<_>>();
    while let Some(block) = queue.pop_front() {
        println!("{}", block.pretty_print(openvm_instruction_formatter));
    }
    todo!();
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct BasicBlockIdentifier(usize);

impl<F> From<&BasicBlock<F>> for BasicBlockIdentifier {
    fn from(block: &BasicBlock<F>) -> Self {
        BasicBlockIdentifier(block.start_idx)
    }
}

/// Returns the indices of the successors of this basic block or
/// an error if we cannot determine the successors for certain.
fn successors<F>(block: &BasicBlock<F>) -> Result<Vec<usize>, ()> {
    block
        .statements
        .iter()
        .flat_map(|instr| jump_destination(instr).transpose())
        .collect()
}

/// Returns `Ok(Some(offset))` where `offset` is the pc offset of the instruction this
/// instruction might jump to. Returns `Ok(None)` if this is not a jump instruction
/// and returns an error if we cannot determine the jump destination for certain.
fn jump_destination<F>(instruction: &Instruction<F>) -> Result<Option<usize>, ()> {
    let opcode = instruction.opcode.as_usize();
    if opcode
        == openvm_rv32im_transpiler::BranchEqualOpcode::BEQ
            .global_opcode()
            .as_usize()
    {
    } else if opcode
        == openvm_rv32im_transpiler::BranchEqualOpcode::BNE
            .global_opcode()
            .as_usize()
    {
        todo!()
    } else if opcode
        == openvm_rv32im_transpiler::BranchLessThanOpcode::BLT
            .global_opcode()
            .as_usize()
    {
        todo!()
    } else if opcode
        == openvm_rv32im_transpiler::BranchLessThanOpcode::BLTU
            .global_opcode()
            .as_usize()
    {
        todo!()
    } else if opcode
        == openvm_rv32im_transpiler::BranchLessThanOpcode::BGE
            .global_opcode()
            .as_usize()
    {
        todo!()
    } else if opcode
        == openvm_rv32im_transpiler::BranchLessThanOpcode::BGEU
            .global_opcode()
            .as_usize()
    {
        todo!()
    } else if opcode
        == openvm_rv32im_transpiler::Rv32JalLuiOpcode::JAL
            .global_opcode()
            .as_usize()
    {
        todo!()
    } else if opcode
        == openvm_rv32im_transpiler::Rv32JalLuiOpcode::LUI
            .global_opcode()
            .as_usize()
    {
        todo!()
    } else if opcode
        == openvm_rv32im_transpiler::Rv32JalrOpcode::JALR
            .global_opcode()
            .as_usize()
    {
        todo!()
    } else {
        todo!()
    };
    // TODO also handle those
    // // The instructions below are structs so we cannot call `global_opcode()` on them without
    // // an instnace, so we manually build the global opcodes.
    // Rv32BranchEqual256Opcode::CLASS_OFFSET
    //     + openvm_rv32im_transpiler::BranchEqualOpcode::BEQ.local_usize(),
    // Rv32BranchEqual256Opcode::CLASS_OFFSET
    //     + openvm_rv32im_transpiler::BranchEqualOpcode::BNE.local_usize(),
    // Rv32BranchLessThan256Opcode::CLASS_OFFSET
    //     + openvm_rv32im_transpiler::BranchLessThanOpcode::BLT.local_usize(),
    // Rv32BranchLessThan256Opcode::CLASS_OFFSET
    //     + openvm_rv32im_transpiler::BranchLessThanOpcode::BLTU.local_usize(),
    // Rv32BranchLessThan256Opcode::CLASS_OFFSET
    //     + openvm_rv32im_transpiler::BranchLessThanOpcode::BGE.local_usize(),
    // Rv32BranchLessThan256Opcode::CLASS_OFFSET
    //     + openvm_rv32im_transpiler::BranchLessThanOpcode::BGEU.local_usize(),
    todo!()
}

fn adjusted_pc(i: usize) -> u32 {
    OPENVM_INIT_PC + (i as u32) * 4
}
