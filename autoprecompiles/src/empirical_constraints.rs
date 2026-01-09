use std::collections::btree_map::Entry;
use std::collections::BTreeMap;
use std::fmt::Debug;
use std::hash::Hash;

use serde::{Deserialize, Serialize};

pub use crate::equivalence_classes::{EquivalenceClass, Partition};

use crate::{
    adapter::Adapter,
    blocks::{BasicBlock, PcStep},
    expression::{AlgebraicExpression, AlgebraicReference},
    optimistic::algebraic_references::BlockCellAlgebraicReferenceMapper,
    SymbolicConstraint,
};

/// "Constraints" that were inferred from execution statistics. They hold empirically
/// (most of the time), but are not guaranteed to hold in all cases.
#[derive(Serialize, Deserialize, Default, Debug)]
pub struct EmpiricalConstraints {
    /// For each program counter, the range constraints for each column.
    /// The range might not hold in 100% of cases.
    pub column_ranges_by_pc: BTreeMap<u32, Vec<(u32, u32)>>,
    /// For each basic block (identified by its starting PC), the equivalence classes of columns.
    /// Each equivalence class is a list of (instruction index in block, column index).
    pub equivalence_classes_by_block: BTreeMap<u64, Partition<BlockCell>>,
    pub debug_info: DebugInfo,
    /// Count of how many times each program counter was executed in the sampled executions.
    /// This can be used to set a threshold for applying constraints only to frequently executed PCs.
    pub pc_counts: BTreeMap<u32, u64>,
}

/// Empirical constraints for a specific basic block.
pub struct BlockEmpiricalConstraints {
    /// For each program counter in the block, the range constraints for each column.
    /// The range might not hold in 100% of cases.
    pub column_ranges_by_pc: BTreeMap<u32, Vec<(u32, u32)>>,
    /// The equivalence classes of columns in the block.
    pub equivalence_classes: Partition<BlockCell>,
}

/// Debug information mapping AIR ids to program counters and column names.
#[derive(Serialize, Deserialize, Default, Debug, Clone)]
pub struct DebugInfo {
    /// Mapping from program counter to the ID of the AIR implementing this instruction.
    pub air_id_by_pc: BTreeMap<u32, usize>,
    /// Mapping from AIR ID to column names.
    pub column_names_by_air_id: BTreeMap<usize, Vec<String>>,
}

impl EmpiricalConstraints {
    /// Combines the empirical constraints with another set of empirical constraints.
    /// The resulting constraints are the most conservative combination of both.
    pub fn combine_with(&mut self, other: EmpiricalConstraints) {
        // Combine column ranges by PC
        for (pc, ranges) in other.column_ranges_by_pc {
            self.column_ranges_by_pc
                .entry(pc)
                .and_modify(|existing_ranges| {
                    for (i, (min, max)) in ranges.iter().enumerate() {
                        if let Some((existing_min, existing_max)) = existing_ranges.get_mut(i) {
                            *existing_min = (*existing_min).min(*min);
                            *existing_max = (*existing_max).max(*max);
                        }
                    }
                })
                .or_insert(ranges);
        }

        // Combine equivalence classes by block
        for (block_pc, classes) in other.equivalence_classes_by_block {
            // Compute the new equivalence classes for this block
            let new_equivalence_class = match self.equivalence_classes_by_block.entry(block_pc) {
                Entry::Vacant(_) => classes,
                Entry::Occupied(e) => e.remove().intersected_with(classes),
            };
            assert!(self
                .equivalence_classes_by_block
                .insert(block_pc, new_equivalence_class)
                .is_none());
        }

        self.debug_info.combine_with(other.debug_info);

        // Combine pc counts
        for (pc, count) in other.pc_counts {
            *self.pc_counts.entry(pc).or_insert(0) += count;
        }
    }

    /// Extracts the empirical constraints relevant for a specific basic block.
    pub fn for_block<I: PcStep>(&self, block: &BasicBlock<I>) -> BlockEmpiricalConstraints {
        let block_pc: u32 = block.start_pc.try_into().unwrap();
        let next_block_pc = block_pc + <I as PcStep>::pc_step() * (block.statements.len() as u32);

        BlockEmpiricalConstraints {
            column_ranges_by_pc: self
                .column_ranges_by_pc
                .range(block_pc..next_block_pc)
                .map(|(&pc, ranges)| (pc, ranges.clone()))
                .collect(),
            equivalence_classes: self
                .equivalence_classes_by_block
                .get(&(block_pc as u64))
                .cloned()
                .unwrap_or_default(),
        }
    }

    /// Returns a new `EmpiricalConstraints` instance containing only the constraints
    /// (both range and equivalence) that are based on a number of executions greater
    /// than or equal to a threshold passed in the `POWDR_OP_EXECUTION_COUNT_THRESHOLD`
    /// environment variable (or `DEFAULT_EXECUTION_COUNT_THRESHOLD`).
    /// This should mitigate overfitting to rare execution paths.
    pub fn apply_pc_threshold(&self) -> Self {
        let threshold = std::env::var("POWDR_OP_EXECUTION_COUNT_THRESHOLD")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(DEFAULT_EXECUTION_COUNT_THRESHOLD);
        EmpiricalConstraints {
            column_ranges_by_pc: self
                .column_ranges_by_pc
                .iter()
                .filter(|(pc, _)| self.pc_counts.get(pc).cloned().unwrap_or(0) >= threshold)
                .map(|(pc, ranges)| (*pc, ranges.clone()))
                .collect(),
            equivalence_classes_by_block: self
                .equivalence_classes_by_block
                .iter()
                .filter(|(&block_pc, _)| {
                    // For equivalence classes, it is enough to check the pc_counts of the first
                    // instruction in the block, as all other instruction will be executed at least
                    // as often.
                    self.pc_counts.get(&(block_pc as u32)).cloned().unwrap_or(0) >= threshold
                })
                .map(|(block_pc, classes)| (*block_pc, classes.clone()))
                .collect(),
            pc_counts: self.pc_counts.clone(),
            debug_info: self.debug_info.clone(),
        }
    }
}

impl DebugInfo {
    pub fn combine_with(&mut self, other: DebugInfo) {
        merge_maps(&mut self.air_id_by_pc, other.air_id_by_pc);
        merge_maps(
            &mut self.column_names_by_air_id,
            other.column_names_by_air_id,
        );
    }

    pub fn take(&mut self) -> Self {
        Self {
            air_id_by_pc: std::mem::take(&mut self.air_id_by_pc),
            column_names_by_air_id: std::mem::take(&mut self.column_names_by_air_id),
        }
    }
}

/// Merges two maps, asserting that existing keys map to equal values.
fn merge_maps<K: Ord, V: Eq + Debug>(map1: &mut BTreeMap<K, V>, map2: BTreeMap<K, V>) {
    for (key, value) in map2 {
        match map1.entry(key) {
            Entry::Vacant(v) => {
                v.insert(value);
            }
            Entry::Occupied(existing) => {
                assert_eq!(*existing.get(), value,);
            }
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Debug, Copy, Clone)]
pub struct BlockCell {
    /// Instruction index within the basic block
    instruction_idx: usize,
    /// The column index within the instruction air
    column_idx: usize,
}

impl BlockCell {
    pub fn new(instruction_idx: usize, column_idx: usize) -> Self {
        Self {
            instruction_idx,
            column_idx,
        }
    }
}

/// For any program line that was not executed at least this many times in the traces,
/// discard any empirical constraints associated with it.
const DEFAULT_EXECUTION_COUNT_THRESHOLD: u64 = 100;

/// Generates symbolic constraints based on empirical constraints for a given basic block.
pub struct ConstraintGenerator<'a, A: Adapter> {
    empirical_constraints: BlockEmpiricalConstraints,
    algebraic_references: BlockCellAlgebraicReferenceMapper,
    block: &'a BasicBlock<A::Instruction>,
}

impl<'a, A: Adapter> ConstraintGenerator<'a, A> {
    /// Creates a new `ConstraintGenerator`.
    ///
    /// Arguments:
    /// - `empirical_constraints`: The empirical constraints to use.
    /// - `algebraic_references`: The mapping from block cells to algebraic references.
    /// - `block`: The basic block for which to generate constraints.
    pub fn new(
        empirical_constraints: BlockEmpiricalConstraints,
        algebraic_references: BlockCellAlgebraicReferenceMapper,
        block: &'a BasicBlock<A::Instruction>,
    ) -> Self {
        let execution_count_threshold = std::env::var("POWDR_OP_EXECUTION_COUNT_THRESHOLD")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(DEFAULT_EXECUTION_COUNT_THRESHOLD);
        tracing::info!(
            "Using execution count threshold: {}",
            execution_count_threshold
        );

        Self {
            empirical_constraints,
            algebraic_references,
            block,
        }
    }

    fn get_algebraic_reference(&self, block_cell: &BlockCell) -> AlgebraicReference {
        self.algebraic_references
            .get_algebraic_reference(block_cell)
            .cloned()
            .unwrap_or_else(|| {
                panic!(
                    "Missing reference for in block {}: {block_cell:?}",
                    self.block.start_pc
                )
            })
    }

    /// Generates constraints of the form `var = <value>` for columns whose value is
    /// always the same empirically.
    // TODO: We could also enforce looser range constraints.
    // This is a bit more complicated though, because we'd have to add bus interactions
    // to actually enforce them.
    pub fn range_constraints(&self) -> Vec<SymbolicConstraint<<A as Adapter>::PowdrField>> {
        let mut constraints = Vec::new();

        for i in 0..self.block.statements.len() {
            let pc = (self.block.start_pc + (i * 4) as u64) as u32;
            let Some(range_constraints) = self.empirical_constraints.column_ranges_by_pc.get(&pc)
            else {
                continue;
            };
            for (col_index, (min, max)) in range_constraints.iter().enumerate() {
                let block_cell = BlockCell::new(i, col_index);
                if min == max {
                    let value = A::PowdrField::from(*min as u64);
                    let reference = self.get_algebraic_reference(&block_cell);
                    let constraint = AlgebraicExpression::Reference(reference)
                        - AlgebraicExpression::Number(value);

                    constraints.push(SymbolicConstraint { expr: constraint });
                }
            }
        }

        constraints
    }

    pub fn equivalence_constraints(&self) -> Vec<SymbolicConstraint<<A as Adapter>::PowdrField>> {
        let mut constraints = Vec::new();

        for equivalence_class in self.empirical_constraints.equivalence_classes.to_classes() {
            let first = equivalence_class.first().unwrap();
            let first_ref = self.get_algebraic_reference(first);
            for other in equivalence_class.iter().skip(1) {
                let other_ref = self.get_algebraic_reference(other);
                let constraint = AlgebraicExpression::Reference(first_ref.clone())
                    - AlgebraicExpression::Reference(other_ref.clone());
                constraints.push(SymbolicConstraint { expr: constraint });
            }
        }

        constraints
    }
}
