use std::collections::BTreeSet;
use std::path::Path;

use lean_vm::{Bytecode, Hint, Instruction as LvInstruction};
use powdr_autoprecompiles::blocks::{BasicBlock, SuperBlock};
use powdr_leanvm::instruction::LeanVmInstruction;
use powdr_leanvm::test_utils;

#[allow(dead_code)]
pub fn assert_machine_output(
    program: SuperBlock<LeanVmInstruction>,
    module_name: &str,
    test_name: &str,
) {
    let snapshot_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("apc_snapshots");
    test_utils::assert_apc_machine_output(program, &snapshot_dir, module_name, test_name);
}

/// Extract basic blocks from compiled bytecode.
///
/// Block boundaries are determined by:
/// - `Hint::Label` at a PC starts a new block
/// - `Instruction::Jump` terminates the current block (inclusive)
///
/// Blocks containing `Instruction::Precompile` are filtered out.
#[allow(dead_code)]
pub fn extract_basic_blocks(bytecode: &Bytecode) -> Vec<BasicBlock<LeanVmInstruction>> {
    let label_pcs: BTreeSet<usize> = bytecode
        .hints
        .iter()
        .filter(|(_, hints)| hints.iter().any(|h| matches!(h, Hint::Label { .. })))
        .map(|(pc, _)| *pc)
        .collect();

    let mut blocks = Vec::new();
    let mut current_instructions: Vec<LeanVmInstruction> = Vec::new();
    let mut current_start_pc: usize = 0;

    for (pc, instruction) in bytecode.instructions.iter().enumerate() {
        // Start new block if this PC has a label and current block is non-empty
        if label_pcs.contains(&pc) && !current_instructions.is_empty() {
            blocks.push(BasicBlock {
                start_pc: current_start_pc as u64,
                instructions: std::mem::take(&mut current_instructions),
            });
            current_start_pc = pc;
        }

        current_instructions.push(LeanVmInstruction(instruction.clone()));

        // End block after a Jump instruction
        if matches!(instruction, LvInstruction::Jump { .. }) {
            blocks.push(BasicBlock {
                start_pc: current_start_pc as u64,
                instructions: std::mem::take(&mut current_instructions),
            });
            current_start_pc = pc + 1;
        }
    }

    // Finalize any remaining instructions
    if !current_instructions.is_empty() {
        blocks.push(BasicBlock {
            start_pc: current_start_pc as u64,
            instructions: current_instructions,
        });
    }

    // Filter out blocks containing Precompile instructions
    blocks.retain(|bb| {
        !bb.instructions
            .iter()
            .any(|i| matches!(i.0, LvInstruction::Precompile { .. }))
    });

    blocks
}
