use std::collections::{BTreeSet, HashMap};

use crate::{
    adapter::Adapter,
    blocks::{BasicBlock, PcStep, Program},
};

/// Given a set of basic blocks, return a new set where if A unconditionally leads to B, then A replaced by AB in the set. B is left in the set.
fn merge_with_target_on_unconditional_jump_impl<I: Clone>(
    blocks: &[BasicBlock<I>],
    is_allowed: &impl Fn(&I) -> bool,
    unconditional_jump_target: &impl Fn(&(u64, I), Option<&(u64, I)>) -> Option<u64>,
) -> Vec<BasicBlock<I>> {
    let blocks_by_start_pc: HashMap<u64, &BasicBlock<I>> =
        blocks.iter().map(|b| (b.start_pc(), b)).collect();
    assert_eq!(
        blocks.len(),
        blocks_by_start_pc.len(),
        "two basic blocks start at the same pc"
    );

    // A cache of the merged blocks found so far
    let mut tails: HashMap<u64, Vec<(u64, I)>> = HashMap::new();

    // Recursively expand one basic block
    fn expanded_block_instructions<I: Clone>(
        start_pc: u64,
        blocks_by_start_pc: &HashMap<u64, &BasicBlock<I>>,
        is_allowed: &impl Fn(&I) -> bool,
        unconditional_jump_target: &impl Fn(&(u64, I), Option<&(u64, I)>) -> Option<u64>,
        tails: &mut HashMap<u64, Vec<(u64, I)>>,
        visiting: &mut BTreeSet<u64>,
    ) -> Vec<(u64, I)> {
        if let Some(cached) = tails.get(&start_pc) {
            return cached.clone();
        }
        let curr = blocks_by_start_pc[&start_pc];
        let mut instructions = curr.instructions.clone();

        if !curr.instructions.iter().all(|(_, instr)| is_allowed(instr)) {
            tails.insert(start_pc, instructions.clone());
            return instructions;
        }

        if !visiting.insert(start_pc) {
            return instructions;
        }

        // Get the last instruction of the block, and optionally the one before that
        let (last, rest) = curr.instructions.split_last().unwrap();
        let previous = rest.last();

        // Check if it jumps to some static target
        if let Some(target_pc) = unconditional_jump_target(last, previous) {
            assert!(target_pc != start_pc, "infinite basic block loop");
            assert!(
                blocks_by_start_pc.contains_key(&target_pc),
                "unconditional jump target is not a jumpdest"
            );
            let target = blocks_by_start_pc[&target_pc];
            if target
                .instructions
                .iter()
                .all(|(_, instr)| is_allowed(instr))
            {
                // Recursively expand the target
                let target_instructions = expanded_block_instructions(
                    target_pc,
                    blocks_by_start_pc,
                    is_allowed,
                    unconditional_jump_target,
                    tails,
                    visiting,
                );
                // Merge the current block with the expansion of its target block
                instructions.extend(target_instructions);
            }
        }

        visiting.remove(&start_pc);
        tails.insert(start_pc, instructions.clone());
        instructions
    }

    // Go through all block and expand them
    blocks
        .iter()
        .map(|b| {
            let mut visiting = BTreeSet::new();
            BasicBlock {
                instructions: expanded_block_instructions(
                    b.start_pc(),
                    &blocks_by_start_pc,
                    is_allowed,
                    unconditional_jump_target,
                    &mut tails,
                    &mut visiting,
                ),
            }
        })
        .collect()
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
        &A::static_jump_target,
    )
}

fn collect_basic_blocks_impl<I: Clone + PcStep, P: Program<I>>(
    program: &P,
    jumpdest_set: &BTreeSet<u64>,
    is_branching: &impl Fn(&I) -> bool,
    is_allowed: &impl Fn(&I) -> bool,
    unconditional_jump_target: &impl Fn(&(u64, I), Option<&(u64, I)>) -> Option<u64>,
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

        // If this opcode cannot be in an apc, we make sure it's alone in a BB.
        if !is_allowed {
            // If not empty, push the current block.
            if !curr_block.instructions.is_empty() {
                blocks.push(curr_block);
                curr_block = BasicBlock::default();
            }
            // Push the instruction itself
            blocks.push(BasicBlock {
                instructions: vec![(pc, instr.clone())],
            });
            continue;
        }

        // If the instruction is a target, we need to close the previous block
        // as is if not empty and start a new block from this instruction.
        if is_target && !curr_block.instructions.is_empty() {
            blocks.push(curr_block);
            curr_block = BasicBlock::default();
        }

        curr_block.instructions.push((pc, instr.clone()));

        // If the instruction is a branch, close this block with this instruction.
        if is_branching {
            blocks.push(curr_block);
            curr_block = BasicBlock::default();
        }
    }

    if !curr_block.instructions.is_empty() {
        blocks.push(curr_block);
    }

    assert!(blocks
        .iter()
        .filter(|b| b.instructions.len() > 1)
        .flat_map(|b| &b.instructions)
        .all(|(_, i)| is_allowed(i)));

    let blocks = merge_with_target_on_unconditional_jump_impl(
        &blocks,
        is_allowed,
        unconditional_jump_target,
    );

    assert!(blocks
        .iter()
        .filter(|b| b.instructions.len() > 1)
        .flat_map(|b| &b.instructions)
        .all(|(_, i)| is_allowed(i)));

    tracing::info!(
        "Got {} basic blocks from `collect_basic_blocks`",
        blocks.len()
    );
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
        CJumpToB,
        AJumpToB,
        BJumpToC,
        CDisallowed,
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
            &|(_, instr), _previous| match instr {
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

    #[test]
    fn detects_long_unconditional_jump_chains() {
        // Program order is A, B, C where B -> A and C -> B (unconditional).
        let program = TestProgram {
            instrs: vec![TestInstr::A, TestInstr::BJumpToA, TestInstr::CJumpToB],
        };
        // Force block starts so base blocks are A, B, C.
        let jumpdest_set = BTreeSet::from([4u64, 8u64]);

        let blocks = collect_basic_blocks_impl(
            &program,
            &jumpdest_set,
            &|instr| matches!(instr, TestInstr::BJumpToA | TestInstr::CJumpToB),
            &|_| true,
            &|(_, instr), _previous| match instr {
                TestInstr::BJumpToA => Some(0),
                TestInstr::CJumpToB => Some(4),
                _ => None,
            },
        );

        assert_eq!(blocks.len(), 3);
        let block_instrs = blocks
            .iter()
            .map(|b| b.instructions.iter().map(|(_, i)| i).collect::<Vec<_>>())
            .collect::<Vec<_>>();

        assert!(block_instrs.contains(&vec![&TestInstr::A]));
        assert!(block_instrs.contains(&vec![&TestInstr::BJumpToA, &TestInstr::A]));
        assert!(block_instrs.contains(&vec![
            &TestInstr::CJumpToB,
            &TestInstr::BJumpToA,
            &TestInstr::A
        ]));
    }

    #[test]
    fn stops_expansion_before_disallowed_target() {
        let program = TestProgram {
            instrs: vec![
                TestInstr::AJumpToB,
                TestInstr::BJumpToC,
                TestInstr::CDisallowed,
            ],
        };
        let jumpdest_set = BTreeSet::from([4u64, 8u64]);

        let blocks = collect_basic_blocks_impl(
            &program,
            &jumpdest_set,
            &|instr| matches!(instr, TestInstr::AJumpToB | TestInstr::BJumpToC),
            &|instr| !matches!(instr, TestInstr::CDisallowed),
            &|(_, instr), _previous| match instr {
                TestInstr::AJumpToB => Some(4),
                TestInstr::BJumpToC => Some(8),
                _ => None,
            },
        );

        assert_eq!(blocks.len(), 3);
        let block_instrs = blocks
            .iter()
            .map(|b| b.instructions.iter().map(|(_, i)| i).collect::<Vec<_>>())
            .collect::<Vec<_>>();

        assert!(block_instrs.contains(&vec![&TestInstr::AJumpToB, &TestInstr::BJumpToC]));
        assert!(block_instrs.contains(&vec![&TestInstr::BJumpToC]));
        assert!(block_instrs.contains(&vec![&TestInstr::CDisallowed]));
        assert!(blocks
            .iter()
            .filter(|b| b.instructions.len() > 1)
            .flat_map(|b| &b.instructions)
            .all(|(_, instr)| !matches!(instr, TestInstr::CDisallowed)));
    }
}
