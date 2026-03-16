use std::collections::{BTreeSet, HashMap, HashSet};

use crate::{
    adapter::Adapter,
    blocks::{BasicBlock, Program, SuperBlock},
};

/// Collects basic blocks from a program
pub fn collect_basic_blocks<A: Adapter>(
    program: &A::Program,
    jumpdest_set: &BTreeSet<u64>,
) -> Vec<SuperBlock<A::Instruction>> {
    let mut blocks = Vec::new();
    let mut curr_block = BasicBlock {
        start_pc: program.instruction_index_to_pc(0),
        instructions: Vec::new(),
    };
    for (i, instr) in program.instructions().enumerate() {
        let is_target = jumpdest_set.contains(&program.instruction_index_to_pc(i));
        let is_branching = A::is_branching(&instr);
        let is_allowed = A::is_allowed(&instr);

        // If this opcode cannot be in an apc, we make sure it's alone in a BB.
        if !is_allowed {
            // If not empty, push the current block.
            if !curr_block.instructions.is_empty() {
                blocks.push(curr_block);
            }
            // Push the instruction itself
            blocks.push(BasicBlock {
                start_pc: program.instruction_index_to_pc(i),
                instructions: vec![instr.clone()],
            });
            // Skip the instruction and start a new block from the next instruction.
            curr_block = BasicBlock {
                start_pc: program.instruction_index_to_pc(i + 1),
                instructions: Vec::new(),
            };
        } else {
            // If the instruction is a target, we need to close the previous block
            // as is if not empty and start a new block from this instruction.
            if is_target {
                if !curr_block.instructions.is_empty() {
                    blocks.push(curr_block);
                }
                curr_block = BasicBlock {
                    start_pc: program.instruction_index_to_pc(i),
                    instructions: Vec::new(),
                };
            }
            curr_block.instructions.push(instr.clone());
            // If the instruction is a branch, we need to close this block
            // with this instruction and start a new block from the next one.
            if is_branching {
                blocks.push(curr_block); // guaranteed to be non-empty because an instruction was just pushed
                curr_block = BasicBlock {
                    start_pc: program.instruction_index_to_pc(i + 1),
                    instructions: Vec::new(),
                };
            }
        }
    }

    if !curr_block.instructions.is_empty() {
        blocks.push(curr_block);
    }

    tracing::info!(
        "Got {} basic blocks from `collect_basic_blocks`",
        blocks.len()
    );

    expand_blocks::<A>(blocks)
}

fn expand_blocks<A: Adapter>(
    blocks: Vec<BasicBlock<A::Instruction>>,
) -> Vec<SuperBlock<A::Instruction>> {
    let mut expander = BasicBlockExpander::<A>::new(blocks.clone());
    blocks
        .into_iter()
        .map(|b| expander.expand(b.into(), &mut HashSet::default()))
        .collect()
}

struct BasicBlockExpander<A: Adapter> {
    start_pc_to_allowed_basic_block: HashMap<u64, SuperBlock<A::Instruction>>,
}

impl<A: Adapter> BasicBlockExpander<A> {
    fn new(basic_blocks: Vec<BasicBlock<A::Instruction>>) -> Self {
        Self {
            start_pc_to_allowed_basic_block: basic_blocks
                .into_iter()
                .filter(|b| b.instructions().all(|(_, i)| A::is_allowed(i)))
                .map(|b| (b.start_pc, b.into()))
                .collect(),
        }
    }

    fn expand(
        &mut self,
        mut block: SuperBlock<A::Instruction>,
        visited: &mut HashSet<u64>,
    ) -> SuperBlock<A::Instruction> {
        if visited.contains(&block.start_pc()) {
            panic!("cycle detected");
        } else {
            visited.insert(block.start_pc());
        }

        // We do not extend blocks which contain disallowed instructions
        if !block.instructions().all(|(_, i)| A::is_allowed(i)) {
            return block;
        }

        let (last, previous) = {
            let mut iter = block.instructions();
            let last = iter.next_back().unwrap();
            let previous = iter.next_back();
            (last, previous)
        };

        if let Some(target_pc) = A::static_target(last, previous) {
            if let Some(tail) = self
                .start_pc_to_allowed_basic_block
                .get(&target_pc)
                .cloned()
            {
                block.extend(self.expand(tail, visited));
            }
        }

        block
    }
}
