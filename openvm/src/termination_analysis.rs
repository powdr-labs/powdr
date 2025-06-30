use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::{self, Display},
};

use itertools::Itertools;
use openvm_bigint_transpiler::{Rv32BranchEqual256Opcode, Rv32BranchLessThan256Opcode};
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
    let basic_blocks_by_identifier = blocks
        .iter()
        .map(|b| (BasicBlockIdentifier::from(b), b))
        .collect::<HashMap<_, _>>();
    println!("=============== blocks =================");
    for (id, block) in basic_blocks_by_identifier
        .iter()
        .sorted_by_key(|(id, _)| *id)
    {
        println!(
            "{id} (known {}to panic):\n{}",
            if known_to_panic.contains(id) {
                ""
            } else {
                "not "
            },
            block.pretty_print(openvm_instruction_formatter)
        )
    }
    println!("=============== =================");
    let mut queue = blocks.iter().collect::<VecDeque<_>>();
    while let Some(block) = queue.pop_front() {
        println!("{}", block.pretty_print(openvm_instruction_formatter));
        let successors = successors(block);
        match successors {
            Ok(successors) => {
                for s in &successors {
                    assert!(
                        basic_blocks_by_identifier.contains_key(s),
                        "Successor {s} not found in basic blocks"
                    );
                }

                println!(" -> {}", successors.iter().format(", "));
            }
            Err(_) => {
                println!("Unknown jump dest");
            }
        }
    }
    todo!();
}

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
struct BasicBlockIdentifier(usize);

impl<F> From<&BasicBlock<F>> for BasicBlockIdentifier {
    fn from(block: &BasicBlock<F>) -> Self {
        BasicBlockIdentifier(block.start_idx)
    }
}

impl BasicBlockIdentifier {
    /// Returns the identifier of the basic block that starts at the given address.
    pub fn from_address(address: u32) -> Self {
        assert!(address % 4 == 0, "Address must be a multiple of 4");
        BasicBlockIdentifier((address / 4) as usize)
    }

    pub fn to_address(&self) -> u64 {
        self.0 as u64 * 4
    }
}

impl Display for BasicBlockIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Returns the identifiers of successors of this basic block or
/// an error if we cannot determine the successors for certain.
fn successors<F: PrimeField32>(block: &BasicBlock<F>) -> Result<Vec<BasicBlockIdentifier>, ()> {
    // TODO assert that all statements except for the last one are "InstructionJumpBehaviour::ContinueNext"
    let last = block.statements.last().unwrap();
    let addr = F::from_canonical_u64(((block.start_idx + block.statements.len() - 1) * 4) as u64);
    let next_block = BasicBlockIdentifier(block.start_idx + block.statements.len());
    match jump_destination(addr, last) {
        InstructionJumpBehaviour::Unknown => Err(()),
        InstructionJumpBehaviour::ContinueNext => Ok(vec![next_block]),
        InstructionJumpBehaviour::UnconditionalJump(addr) => {
            Ok(vec![BasicBlockIdentifier::from_address(addr)])
        }
        InstructionJumpBehaviour::ConditionalJump(addr) => {
            Ok(vec![BasicBlockIdentifier::from_address(addr), next_block])
        }
    }
}

enum InstructionJumpBehaviour {
    /// Might go anywhere.
    Unknown,
    /// Only continues on the next instruction.
    ContinueNext,
    /// Only jumps to the given address.
    UnconditionalJump(u32),
    /// Might jump to the given address, but might also continue.
    ConditionalJump(u32),
    // TODO What about "jump and link" and "return"?
}

/// Returns `Ok(Some(offset))` where `offset` is the relative pc offset of the instruction this
/// instruction might jump to. Returns `Ok(None)` if this is not a jump instruction
/// and returns an error if we cannot determine the jump destination for certain.
fn jump_destination<F: PrimeField32>(
    address: F,
    instruction: &Instruction<F>,
) -> InstructionJumpBehaviour {
    let opcode = instruction.opcode.as_usize();

    // TODO group them
    if opcode
        == openvm_rv32im_transpiler::BranchEqualOpcode::BEQ
            .global_opcode()
            .as_usize()
    {
        InstructionJumpBehaviour::ConditionalJump((address + instruction.c).as_canonical_u32())
    } else if opcode
        == openvm_rv32im_transpiler::BranchEqualOpcode::BNE
            .global_opcode()
            .as_usize()
    {
        InstructionJumpBehaviour::ConditionalJump((address + instruction.c).as_canonical_u32())
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
        InstructionJumpBehaviour::ConditionalJump((address + instruction.c).as_canonical_u32())
    } else if opcode
        == openvm_rv32im_transpiler::BranchLessThanOpcode::BGE
            .global_opcode()
            .as_usize()
    {
        InstructionJumpBehaviour::ConditionalJump((address + instruction.c).as_canonical_u32())
    } else if opcode
        == openvm_rv32im_transpiler::BranchLessThanOpcode::BGEU
            .global_opcode()
            .as_usize()
    {
        InstructionJumpBehaviour::ConditionalJump((address + instruction.c).as_canonical_u32())
    } else if opcode
        == openvm_rv32im_transpiler::Rv32JalLuiOpcode::JAL
            .global_opcode()
            .as_usize()
    {
        // TODO we might treat JAL the same way as a conditional jump.
        // is this correct? If the destination always panics, then we do not continue here.

        InstructionJumpBehaviour::Unknown
    } else if opcode
        == openvm_rv32im_transpiler::Rv32JalrOpcode::JALR
            .global_opcode()
            .as_usize()
    {
        // dynamic jump, TODO but might be static if we use register 0?
        InstructionJumpBehaviour::Unknown
    } else if opcode
        == Rv32BranchEqual256Opcode::CLASS_OFFSET
            + openvm_rv32im_transpiler::BranchEqualOpcode::BEQ.local_usize()
    {
        todo!()
    } else if opcode
        == Rv32BranchEqual256Opcode::CLASS_OFFSET
            + openvm_rv32im_transpiler::BranchEqualOpcode::BNE.local_usize()
    {
        todo!()
    } else if opcode
        == Rv32BranchLessThan256Opcode::CLASS_OFFSET
            + openvm_rv32im_transpiler::BranchLessThanOpcode::BLT.local_usize()
    {
        todo!()
    } else if opcode
        == Rv32BranchLessThan256Opcode::CLASS_OFFSET
            + openvm_rv32im_transpiler::BranchLessThanOpcode::BLTU.local_usize()
    {
        todo!()
    } else if opcode
        == Rv32BranchLessThan256Opcode::CLASS_OFFSET
            + openvm_rv32im_transpiler::BranchLessThanOpcode::BGE.local_usize()
    {
        todo!()
    } else if opcode
        == Rv32BranchLessThan256Opcode::CLASS_OFFSET
            + openvm_rv32im_transpiler::BranchLessThanOpcode::BGEU.local_usize()
    {
        todo!()
    } else {
        InstructionJumpBehaviour::ContinueNext
    }
}

fn adjusted_pc(i: usize) -> u32 {
    OPENVM_INIT_PC + (i as u32) * 4
}
