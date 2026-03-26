use std::collections::{BTreeSet, HashMap, HashSet};

use crate::{
    adapter::Adapter,
    blocks::{BasicBlock, Program, StaticBlocks, SuperBlock},
};

/// Collects static blocks from a program
pub fn collect_static_blocks<A: Adapter>(
    program: &A::Program,
    jumpdest_set: &BTreeSet<u64>,
    should_expand_basic_blocks: bool,
) -> StaticBlocks<A::Instruction> {
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
        "Got {} static blocks from `collect_static_blocks`",
        blocks.len()
    );

    if should_expand_basic_blocks {
        expand_blocks::<A>(blocks)
    } else {
        StaticBlocks::new(blocks)
    }
}

/// Expand basic blocks with their successor as long as the successor is statically know
fn expand_blocks<A: Adapter>(
    blocks: Vec<BasicBlock<A::Instruction>>,
) -> StaticBlocks<A::Instruction> {
    let mut expander = BasicBlockExpander::<A>::new(blocks.clone());
    StaticBlocks::new(
        blocks
            .into_iter()
            .map(|b| expander.expand(b.into(), &mut HashSet::default())),
    )
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
        visited: &mut HashSet<Vec<u64>>,
    ) -> SuperBlock<A::Instruction> {
        visited.insert(block.start_pcs());

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

        if let Some(target_pc) = A::try_static_target(last, previous) {
            if let Some(tail) = self
                .start_pc_to_allowed_basic_block
                .get(&target_pc)
                .cloned()
            {
                if !visited.contains(&tail.start_pcs()) {
                    block.extend(self.expand(tail, visited));
                }
            }
        }

        block
    }
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

    use super::collect_static_blocks;
    use crate::{
        adapter::Adapter,
        blocks::{Instruction, PcStep, Program},
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
        Fallthrough(char),
        Jump(char, u64),
        Stop(char),
        Disallowed(char),
    }

    impl TestInstruction {
        fn label(&self) -> char {
            match self {
                Self::Fallthrough(label)
                | Self::Jump(label, _)
                | Self::Stop(label)
                | Self::Disallowed(label) => *label,
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

    fn ft(label: char) -> TestInstruction {
        TestInstruction::Fallthrough(label)
    }

    fn jmp(label: char, target: u64) -> TestInstruction {
        TestInstruction::Jump(label, target)
    }

    fn stop(label: char) -> TestInstruction {
        TestInstruction::Stop(label)
    }

    fn dis(label: char) -> TestInstruction {
        TestInstruction::Disallowed(label)
    }

    fn program(instructions: Vec<TestInstruction>) -> TestProgram {
        TestProgram(instructions)
    }

    fn jumpdests(pcs: &[u64]) -> BTreeSet<u64> {
        pcs.iter().copied().collect()
    }

    fn blocks(instructions: Vec<TestInstruction>, jumpdest_pcs: &[u64]) -> Vec<String> {
        collect_static_blocks::<TestAdapter>(&program(instructions), &jumpdests(jumpdest_pcs), true)
            .into_iter()
            .map(|(_, block)| {
                block
                    .instructions()
                    .map(|(_, instruction)| instruction.label())
                    .collect()
            })
            .collect()
    }

    #[test]
    fn expands_fallthrough_chain_from_each_basic_block() {
        assert_eq!(
            blocks(vec![ft('A'), ft('B'), stop('C')], &[1, 2]),
            vec!["ABC", "BC", "C"]
        );
    }

    #[test]
    fn disallowed_instruction_breaks_fallthrough() {
        assert_eq!(
            blocks(vec![stop('A'), dis('X'), stop('B'), jmp('C', 2)], &[2, 3]),
            vec!["A", "X", "B", "CB"]
        );
    }

    #[test]
    fn jump_target_to_allowed_block_extends() {
        assert_eq!(
            blocks(vec![jmp('A', 2), dis('X'), stop('C')], &[2]),
            vec!["AC", "X", "C"]
        );
    }

    #[test]
    fn jump_target_to_disallowed_block_does_not_extend() {
        assert_eq!(blocks(vec![jmp('A', 1), dis('X')], &[]), vec!["A", "X"]);
    }

    #[test]
    fn no_jumpdests_keeps_single_fallthrough_block() {
        assert_eq!(blocks(vec![ft('A'), ft('B'), stop('C')], &[]), vec!["ABC"]);
    }

    #[test]
    fn disallowed_instruction_is_its_own_block() {
        assert_eq!(blocks(vec![dis('X')], &[]), vec!["X"]);
    }

    #[test]
    fn self_cycle() {
        assert_eq!(blocks(vec![jmp('A', 0)], &[]), vec!["A"]);
    }

    #[test]
    fn cycle() {
        assert_eq!(blocks(vec![ft('B'), jmp('C', 0)], &[1]), vec!["BC", "CB"]);
    }
}
