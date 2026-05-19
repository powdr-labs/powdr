use std::collections::{BTreeMap, BTreeSet};

use crate::{
    adapter::Adapter,
    blocks::{BasicBlock, Program},
};

/// Collects basic blocks from a program
pub fn collect_basic_blocks<A: Adapter>(
    program: &A::Program,
    jumpdest_set: &BTreeSet<u64>,
) -> Vec<BasicBlock<A::Instruction>> {
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

    blocks
}

/// Computes the maximal static sequences in the program: chains of basic
/// blocks linked by statically determined jumps.
///
/// Input is expected to be already filtered to only valid APC basic blocks.
pub fn compute_static_sequences<A: Adapter>(
    basic_blocks: &[BasicBlock<A::Instruction>],
) -> Vec<Vec<u64>> {
    let by_start_pc: BTreeMap<u64, &BasicBlock<A::Instruction>> =
        basic_blocks.iter().map(|bb| (bb.start_pc, bb)).collect();

    let mut sequences = Vec::new();
    // visited set to avoid emitting subsequences (e.g., BC when ABC is a sequence)
    let mut emitted: BTreeSet<u64> = BTreeSet::new();

    for (start_pc, bb) in &by_start_pc {
        if emitted.contains(start_pc) {
            // subsequence of already emitted sequence
            continue;
        }

        // visited set for cycle detection within this chain.
        let mut visited: BTreeSet<u64> = BTreeSet::new();
        visited.insert(*start_pc);
        let mut current = vec![*start_pc];
        let mut tail = bb;

        loop {
            let (last, previous) = {
                let mut iter = tail.instructions();
                let last = iter.next_back().unwrap();
                let previous = iter.next_back();
                (last, previous)
            };

            let Some(target_pc) = A::try_static_target(last, previous) else {
                // not a static jump
                break;
            };
            if visited.contains(&target_pc) {
                // cycle
                break;
            }
            let Some(next) = by_start_pc.get(&target_pc) else {
                // static jump to an invalid APC target
                break;
            };

            visited.insert(target_pc);
            current.push(target_pc);
            tail = next;
        }

        if current.len() >= 2 {
            emitted.extend(current.iter().copied());
            sequences.push(current);
        }
    }

    sequences
}

#[cfg(test)]
mod tests {
    use std::{
        collections::BTreeSet,
        fmt::{Display, Formatter},
        hash::Hash,
        marker::PhantomData,
    };

    use powdr_constraint_solver::{
        constraint_system::{BusInteraction, BusInteractionHandler},
        grouped_expression::GroupedExpression,
        range_constraint::RangeConstraint,
    };
    use powdr_number::GoldilocksField;
    use serde::{Deserialize, Serialize};

    use super::{collect_basic_blocks, compute_static_sequences};
    use crate::{
        adapter::Adapter,
        blocks::{BasicBlock, Instruction, PcStep, Program},
        constraint_optimizer::IsBusStateful,
        evaluation::AirStats,
        execution::ExecutionState,
        memory_optimizer::{MemoryBusInteraction, MemoryBusInteractionConversionError, MemoryOp},
        range_constraint_optimizer::{
            MakeRangeConstraintsError, RangeConstraintHandler, RangeConstraints,
        },
        AdapterApc, DegreeBound, InstructionHandler, SymbolicMachine,
    };

    #[derive(Clone, Debug, Serialize, Deserialize)]
    enum TestInstruction {
        Fallthrough(String),
        Jump(String, u64),
        Stop(String),
        Disallowed(String),
    }

    impl TestInstruction {
        fn label(&self) -> &str {
            match self {
                Self::Fallthrough(label)
                | Self::Jump(label, _)
                | Self::Stop(label)
                | Self::Disallowed(label) => label,
            }
        }
    }

    impl Display for TestInstruction {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.label())
        }
    }

    impl PcStep for TestInstruction {
        fn pc_step() -> u32 {
            1
        }
    }

    impl Instruction<GoldilocksField> for TestInstruction {
        fn pc_lookup_row(&self, pc: u64) -> Vec<GoldilocksField> {
            vec![pc.into()]
        }
    }

    struct TestProgram(Vec<TestInstruction>);

    impl Program<TestInstruction> for TestProgram {
        fn base_pc(&self) -> u64 {
            0
        }

        fn instructions(&self) -> Box<dyn Iterator<Item = TestInstruction> + '_> {
            Box::new(self.0.iter().cloned())
        }

        fn length(&self) -> u32 {
            self.0.len() as u32
        }
    }

    struct TestInstructionHandler;

    impl InstructionHandler for TestInstructionHandler {
        type Field = GoldilocksField;
        type Instruction = TestInstruction;
        type AirId = ();

        fn degree_bound(&self) -> DegreeBound {
            DegreeBound {
                identities: 0,
                bus_interactions: 0,
            }
        }

        fn get_instruction_air_and_id(
            &self,
            _instruction: &Self::Instruction,
        ) -> (Self::AirId, &SymbolicMachine<Self::Field>) {
            unreachable!()
        }

        fn get_instruction_air_stats(&self, _instruction: &Self::Instruction) -> AirStats {
            AirStats::default()
        }
    }

    #[derive(Clone, Default)]
    struct TestBusHandler;

    impl BusInteractionHandler<GoldilocksField> for TestBusHandler {
        fn handle_bus_interaction(
            &self,
            bus_interaction: BusInteraction<RangeConstraint<GoldilocksField>>,
        ) -> BusInteraction<RangeConstraint<GoldilocksField>> {
            bus_interaction
        }
    }

    impl IsBusStateful<GoldilocksField> for TestBusHandler {
        fn is_stateful(&self, _bus_id: GoldilocksField) -> bool {
            false
        }
    }

    impl RangeConstraintHandler<GoldilocksField> for TestBusHandler {
        fn pure_range_constraints<V: Ord + Clone + Eq + Display + Hash>(
            &self,
            _bus_interaction: &BusInteraction<GroupedExpression<GoldilocksField, V>>,
        ) -> Option<RangeConstraints<GoldilocksField, V>> {
            None
        }

        fn batch_make_range_constraints<V: Ord + Clone + Eq + Display + Hash>(
            &self,
            _range_constraints: RangeConstraints<GoldilocksField, V>,
        ) -> Result<
            Vec<BusInteraction<GroupedExpression<GoldilocksField, V>>>,
            MakeRangeConstraintsError,
        > {
            Ok(vec![])
        }
    }

    struct TestMemoryInteraction<V>(PhantomData<V>);

    impl<V: Ord + Clone + Eq + Display + Hash> MemoryBusInteraction<GoldilocksField, V>
        for TestMemoryInteraction<V>
    {
        type Address = Vec<GroupedExpression<GoldilocksField, V>>;

        fn try_from_bus_interaction(
            _bus_interaction: &BusInteraction<GroupedExpression<GoldilocksField, V>>,
            _memory_bus_id: u64,
        ) -> Result<Option<Self>, MemoryBusInteractionConversionError> {
            Ok(None)
        }

        fn addr(&self) -> Self::Address {
            unreachable!()
        }

        fn data(&self) -> &[GroupedExpression<GoldilocksField, V>] {
            unreachable!()
        }

        fn timestamp_limbs(&self) -> &[GroupedExpression<GoldilocksField, V>] {
            unreachable!()
        }

        fn op(&self) -> MemoryOp {
            unreachable!()
        }
    }

    #[derive(Clone, Copy)]
    struct TestExecutionState;

    impl ExecutionState for TestExecutionState {
        type RegisterAddress = u8;
        type Value = u64;

        fn pc(&self) -> Self::Value {
            0
        }

        fn value_limb(value: Self::Value, _limb_index: usize) -> Self::Value {
            value
        }

        fn reg(&self, _address: &Self::RegisterAddress) -> Self::Value {
            0
        }

        fn global_clk(&self) -> usize {
            0
        }
    }

    #[derive(Clone, Debug, Default, Eq, PartialEq, Serialize, Deserialize)]
    struct TestBusTypes;

    impl Display for TestBusTypes {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "test-bus")
        }
    }

    struct TestAdapter;

    impl Adapter for TestAdapter {
        type Field = GoldilocksField;
        type PowdrField = GoldilocksField;
        type InstructionHandler = TestInstructionHandler;
        type BusInteractionHandler = TestBusHandler;
        type Program = TestProgram;
        type Instruction = TestInstruction;
        type MemoryBusInteraction<V: Ord + Clone + Eq + Display + Hash> = TestMemoryInteraction<V>;
        type CustomBusTypes = TestBusTypes;
        type ApcStats = ();
        type AirId = ();
        type ExecutionState = TestExecutionState;

        fn into_field(e: Self::PowdrField) -> Self::Field {
            e
        }

        fn from_field(e: Self::Field) -> Self::PowdrField {
            e
        }

        fn apc_stats(
            _apc: std::sync::Arc<AdapterApc<Self>>,
            _instruction_handler: &Self::InstructionHandler,
        ) -> Self::ApcStats {
        }

        fn is_branching(instr: &Self::Instruction) -> bool {
            matches!(instr, TestInstruction::Jump(_, _))
        }

        fn is_allowed(instr: &Self::Instruction) -> bool {
            !matches!(instr, TestInstruction::Disallowed(_))
        }

        fn try_static_target(
            last: (u64, &Self::Instruction),
            _previous: Option<(u64, &Self::Instruction)>,
        ) -> Option<u64> {
            match last.1 {
                TestInstruction::Fallthrough(_) => Some(last.0 + 1),
                TestInstruction::Jump(_, target) => Some(*target),
                TestInstruction::Stop(_) | TestInstruction::Disallowed(_) => None,
            }
        }
    }

    fn ft(label: &str) -> TestInstruction {
        TestInstruction::Fallthrough(label.to_string())
    }

    fn jmp(label: &str, target: u64) -> TestInstruction {
        TestInstruction::Jump(label.to_string(), target)
    }

    fn stop(label: &str) -> TestInstruction {
        TestInstruction::Stop(label.to_string())
    }

    fn dis(label: &str) -> TestInstruction {
        TestInstruction::Disallowed(label.to_string())
    }

    fn program(instructions: Vec<TestInstruction>) -> TestProgram {
        TestProgram(instructions)
    }

    fn jumpdests(pcs: &[u64]) -> BTreeSet<u64> {
        pcs.iter().copied().collect()
    }

    /// Helper for creating a BB label from its instruction labels
    fn bb_label(bb: &BasicBlock<TestInstruction>) -> String {
        bb.instructions.iter().map(|i| i.label()).collect()
    }

    /// Helper for creating the sequence of BB labels for a given superblock
    fn sb_labels(superblock: &[u64], blocks: &[BasicBlock<TestInstruction>]) -> Vec<String> {
        let by_pc: std::collections::HashMap<u64, &BasicBlock<TestInstruction>> =
            blocks.iter().map(|b| (b.start_pc, b)).collect();
        superblock.iter().map(|pc| bb_label(by_pc[pc])).collect()
    }

    /// Helper for creating expected values for static superblocks.
    fn sb(bbs: &[&str]) -> Vec<String> {
        bbs.iter().map(|s| (*s).to_string()).collect()
    }

    fn basic_blocks(instructions: Vec<TestInstruction>, jumpdest_pcs: &[u64]) -> Vec<String> {
        collect_basic_blocks::<TestAdapter>(&program(instructions), &jumpdests(jumpdest_pcs))
            .iter()
            .map(bb_label)
            .collect()
    }

    fn static_bb_sequences(
        instructions: Vec<TestInstruction>,
        jumpdest_pcs: &[u64],
    ) -> Vec<Vec<String>> {
        let blocks: Vec<_> =
            collect_basic_blocks::<TestAdapter>(&program(instructions), &jumpdests(jumpdest_pcs))
                .into_iter()
                .filter(|bb| bb.instructions.len() > 1) // filter for valid BBs
                .collect();
        compute_static_sequences::<TestAdapter>(&blocks)
            .into_iter()
            .map(|superblock| sb_labels(&superblock, &blocks))
            .collect()
    }

    #[test]
    fn expands_fallthrough_superblock_from_each_basic_block() {
        assert_eq!(
            static_bb_sequences(
                vec![ft("A1"), ft("A2"), ft("B1"), ft("B2"), ft("C1"), stop("C2"),],
                &[2, 4],
            ),
            vec![sb(&["A1A2", "B1B2", "C1C2"])]
        );
    }

    #[test]
    fn disallowed_instruction_breaks_fallthrough() {
        assert!(static_bb_sequences(
            vec![ft("A1"), ft("A2"), dis("X"), ft("B1"), stop("B2")],
            &[3],
        )
        .is_empty());
    }

    #[test]
    fn jump_target_to_allowed_block_extends() {
        assert_eq!(
            static_bb_sequences(
                vec![ft("A1"), jmp("A2", 3), dis("X"), ft("C1"), stop("C2"),],
                &[3],
            ),
            vec![sb(&["A1A2", "C1C2"])]
        );
    }

    #[test]
    fn jump_target_to_disallowed_block_does_not_extend() {
        assert!(static_bb_sequences(vec![ft("A1"), jmp("A2", 2), dis("X")], &[]).is_empty());
    }

    #[test]
    fn no_jumpdests_keeps_single_fallthrough_block() {
        assert_eq!(
            basic_blocks(vec![ft("A"), ft("B"), stop("C")], &[]),
            vec!["ABC"]
        );
        assert!(static_bb_sequences(vec![ft("A"), ft("B"), stop("C")], &[]).is_empty());
    }

    #[test]
    fn disallowed_instruction_is_its_own_block() {
        assert_eq!(basic_blocks(vec![dis("X")], &[]), vec!["X"]);
        assert!(static_bb_sequences(vec![dis("X")], &[]).is_empty());
    }

    #[test]
    fn self_cycle() {
        assert!(static_bb_sequences(vec![ft("A1"), jmp("A2", 0)], &[]).is_empty());
    }

    #[test]
    fn cycle() {
        assert_eq!(
            static_bb_sequences(vec![ft("B1"), ft("B2"), ft("C1"), jmp("C2", 0)], &[2],),
            vec![sb(&["B1B2", "C1C2"])]
        );
    }
}
