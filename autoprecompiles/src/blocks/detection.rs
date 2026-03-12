use std::collections::{BTreeSet, HashMap};

use crate::{
    adapter::Adapter,
    blocks::{BasicBlock, PcStep, Program},
};

fn merge_with_target_on_unconditional_jump_impl<I: Clone>(
    blocks: &[BasicBlock<I>],
    unconditional_jump_target: &impl Fn(&I, u64) -> Option<u64>,
) -> Vec<BasicBlock<I>> {
    let blocks_by_start_pc: HashMap<u64, &BasicBlock<I>> =
        blocks.iter().map(|b| (b.start_pc(), b)).collect();
    let mut merged = Vec::with_capacity(blocks.len());

    for curr in blocks {
        let maybe_target_pc = curr
            .instructions
            .last()
            .and_then(|(pc, instr)| unconditional_jump_target(instr, *pc));

        let Some(target_pc) = maybe_target_pc else {
            merged.push(curr.clone());
            continue;
        };

        if target_pc == curr.start_pc() {
            // Avoid duplicating self-looping blocks.
            merged.push(curr.clone());
            continue;
        }

        if let Some(target) = blocks_by_start_pc.get(&target_pc) {
            let mut instructions = curr.instructions.clone();
            instructions.extend(target.instructions.clone());
            merged.push(BasicBlock { instructions });
        } else {
            merged.push(curr.clone());
        }
    }

    merged
}

/// Collects basic blocks from a program
pub fn collect_basic_blocks<A: Adapter>(
    program: &A::Program,
    jumpdest_set: &BTreeSet<u64>,
) -> Vec<BasicBlock<A::Instruction>> {
    collect_basic_blocks_impl(
        program,
        jumpdest_set,
        &A::is_branching,
        &A::is_allowed,
        &A::unconditional_jump_target,
    )
}

fn collect_basic_blocks_impl<I: Clone + PcStep, P: Program<I>>(
    program: &P,
    jumpdest_set: &BTreeSet<u64>,
    is_branching: &impl Fn(&I) -> bool,
    is_allowed: &impl Fn(&I) -> bool,
    unconditional_jump_target: &impl Fn(&I, u64) -> Option<u64>,
) -> Vec<BasicBlock<I>> {
    let mut blocks = Vec::new();
    let mut curr_block = BasicBlock {
        instructions: Vec::new(),
    };

    for (i, instr) in program.instructions().enumerate() {
        let pc = program.instruction_index_to_pc(i);
        let is_target = jumpdest_set.contains(&pc);
        let is_branching = is_branching(&instr);
        let is_allowed = is_allowed(&instr);

        // If this opcode cannot be in an APC, make sure it's alone in a basic block.
        if !is_allowed {
            if !curr_block.instructions.is_empty() {
                blocks.push(curr_block);
                curr_block = BasicBlock {
                    instructions: Vec::new(),
                };
            }
            blocks.push(BasicBlock {
                instructions: vec![(pc, instr.clone())],
            });
            continue;
        }

        // If the instruction is a target, we need to close the previous block
        // as-is (if non-empty) and start a new block from this instruction.
        if is_target && !curr_block.instructions.is_empty() {
            blocks.push(curr_block);
            curr_block = BasicBlock {
                instructions: Vec::new(),
            };
        }

        curr_block.instructions.push((pc, instr.clone()));

        // If the instruction is a branch, close this block with this instruction.
        if is_branching {
            blocks.push(curr_block);
            curr_block = BasicBlock {
                instructions: Vec::new(),
            };
        }
    }

    if !curr_block.instructions.is_empty() {
        blocks.push(curr_block);
    }

    let blocks =
        merge_with_target_on_unconditional_jump_impl::<I>(&blocks, unconditional_jump_target);
    tracing::info!("Got {} basic blocks from `collect_basic_blocks`", blocks.len());
    blocks
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use crate::blocks::{PcStep, Program};

    use super::collect_basic_blocks_impl;

    #[derive(Clone, Debug, Eq, PartialEq)]
    enum TestInstr {
        A,
        BJumpToA,
    }

    impl PcStep for TestInstr {
        fn pc_step() -> u32 {
            4
        }
    }

    struct TestProgram {
        instrs: Vec<TestInstr>,
    }

    impl Program<TestInstr> for TestProgram {
        fn base_pc(&self) -> u64 {
            0
        }

        fn instructions(&self) -> Box<dyn Iterator<Item = TestInstr> + '_> {
            Box::new(self.instrs.iter().cloned())
        }

        fn length(&self) -> u32 {
            self.instrs.len() as u32
        }
    }

    #[test]
    fn detects_overlapping_blocks_for_unconditional_jump_target() {
        // Program order is A then B, and B unconditionally jumps to A.
        let program = TestProgram {
            instrs: vec![TestInstr::A, TestInstr::BJumpToA],
        };
        // Force a block split at B's PC so we have two original blocks A and B.
        let jumpdest_set = BTreeSet::from([4u64]);

        let blocks = collect_basic_blocks_impl(
            &program,
            &jumpdest_set,
            &|instr| matches!(instr, TestInstr::BJumpToA),
            &|_| true,
            &|instr, _pc| match instr {
                TestInstr::BJumpToA => Some(0),
                _ => None,
            },
        );

        assert_eq!(blocks.len(), 2);
        let block_instrs = blocks
            .iter()
            .map(|b| b.instructions.iter().map(|(_, i)| i).collect::<Vec<_>>())
            .collect::<Vec<_>>();
        assert!(block_instrs.contains(&vec![&TestInstr::A]));
        assert!(block_instrs.contains(&vec![&TestInstr::BJumpToA, &TestInstr::A]));
    }
}
