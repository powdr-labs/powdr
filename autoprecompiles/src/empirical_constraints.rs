use std::collections::btree_map::Entry;
use std::collections::BTreeMap;
use std::fmt::Debug;
use std::hash::Hash;

use itertools::Itertools;
use serde::{Deserialize, Serialize};

pub use crate::equivalence_classes::{EquivalenceClass, Partition};
use crate::optimistic::algebraic_references::BlockCellAlgebraicReferenceMapper;

use crate::optimistic::config::optimistic_precompile_config;
use crate::{
    adapter::Adapter,
    blocks::{BasicBlock, PcStep},
    expression::{AlgebraicExpression, AlgebraicReference},
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
    pub equivalence_classes_by_block: BTreeMap<u64, Partition<BlockCell>>,
    pub debug_info: DebugInfo,
    /// Count of how many times each program counter was executed in the sampled executions.
    /// This can be used to set a threshold for applying constraints only to frequently executed PCs.
    pub pc_counts: BTreeMap<u32, u64>,
}

/// Empirical constraints for a specific basic block.
pub struct BlockEmpiricalConstraints {
    /// The starting program counter of the basic block.
    block_pc: u64,
    /// For each program counter in the block, the range constraints for each column, if any.
    /// The range might not hold in 100% of cases.
    pub column_ranges_by_pc: BTreeMap<u32, BTreeMap<usize, (u32, u32)>>,
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
            block_pc: block.start_pc,
            column_ranges_by_pc: self
                .column_ranges_by_pc
                .range(block_pc..next_block_pc)
                .map(|(&pc, ranges)| (pc, ranges.iter().cloned().enumerate().collect()))
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
    pub fn apply_pc_threshold(self) -> Self {
        let threshold = optimistic_precompile_config().execution_count_threshold;
        EmpiricalConstraints {
            column_ranges_by_pc: self
                .column_ranges_by_pc
                .into_iter()
                .filter(|(pc, _)| self.pc_counts.get(pc).cloned().unwrap_or(0) >= threshold)
                .collect(),
            equivalence_classes_by_block: self
                .equivalence_classes_by_block
                .into_iter()
                .filter(|&(block_pc, _)| {
                    // For equivalence classes, it is enough to check the pc_counts of the first
                    // instruction in the block, as all other instruction will be executed at least
                    // as often.
                    self.pc_counts.get(&(block_pc as u32)).cloned().unwrap_or(0) >= threshold
                })
                .collect(),
            pc_counts: self.pc_counts.clone(),
            debug_info: self.debug_info.clone(),
        }
    }
}

impl BlockEmpiricalConstraints {
    /// Returns a new `BlockEmpiricalConstraints` instance containing only the
    /// constraints (both range and equivalence) for which the provided
    /// predicate on `BlockCell`s returns true.
    pub fn filtered(self, predicate: impl Fn(&BlockCell) -> bool, pc_step: u32) -> Self {
        let column_ranges_by_pc = self
            .column_ranges_by_pc
            .into_iter()
            .map(|(pc, ranges)| {
                let instruction_idx = ((pc - (self.block_pc as u32)) / pc_step) as usize;
                let ranges = ranges
                    .into_iter()
                    .enumerate()
                    .filter_map(|(col_idx, range)| {
                        let cell = BlockCell::new(instruction_idx, col_idx);
                        // Keep the range only if the predicate holds for the cell
                        predicate(&cell).then_some(range)
                    })
                    .collect();
                (pc, ranges)
            })
            .collect();
        let equivalence_classes = self
            .equivalence_classes
            .to_classes()
            .into_iter()
            .map(|class| {
                // Remove cells from the equivalence class for which the predicate does not hold
                class
                    .into_iter()
                    .filter(|cell| predicate(cell))
                    .collect_vec()
            })
            .collect();
        Self {
            block_pc: self.block_pc,
            column_ranges_by_pc,
            equivalence_classes,
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

    /// Generates all equality constraints
    pub fn generate_constraints(&self) -> Vec<EqualityConstraint<A::PowdrField>> {
        self.range_constraints()
            .into_iter()
            .chain(self.equivalence_constraints())
            .collect_vec()
    }

    /// Generates constraints of the form `var = <value>` for columns whose value is
    /// always the same empirically.
    // TODO: We could also enforce looser range constraints.
    // This is a bit more complicated though, because we'd have to add bus interactions
    // to actually enforce them.
    fn range_constraints(&self) -> Vec<EqualityConstraint<A::PowdrField>> {
        let mut constraints = Vec::new();

        for i in 0..self.block.statements.len() {
            let pc = (self.block.start_pc + (i * 4) as u64) as u32;
            let Some(range_constraints) = self.empirical_constraints.column_ranges_by_pc.get(&pc)
            else {
                continue;
            };
            for (col_index, (min, max)) in range_constraints {
                let block_cell = BlockCell::new(i, *col_index);
                if min == max {
                    let value = A::PowdrField::from(*min as u64);
                    let reference = self.get_algebraic_reference(&block_cell);

                    constraints.push(EqualityConstraint {
                        left: EqualityExpression::Reference(reference),
                        right: EqualityExpression::Number(value),
                    });
                }
            }
        }

        constraints
    }

    fn equivalence_constraints(&self) -> Vec<EqualityConstraint<A::PowdrField>> {
        let mut constraints = Vec::new();

        for equivalence_class in self.empirical_constraints.equivalence_classes.to_classes() {
            let first = equivalence_class.first().unwrap();
            let first_ref = self.get_algebraic_reference(first);
            for other in equivalence_class.iter().skip(1) {
                let other_ref = self.get_algebraic_reference(other);
                constraints.push(EqualityConstraint {
                    left: EqualityExpression::Reference(first_ref.clone()),
                    right: EqualityExpression::Reference(other_ref.clone()),
                });
            }
        }

        constraints
    }
}

/// An expression used in equality constraints.
/// This is a simplified version of `AlgebraicExpression` that only allows
/// references and numbers.
pub enum EqualityExpression<T> {
    Reference(AlgebraicReference),
    Number(T),
}

impl<T> From<EqualityExpression<T>> for AlgebraicExpression<T> {
    fn from(expr: EqualityExpression<T>) -> Self {
        match expr {
            EqualityExpression::Reference(r) => AlgebraicExpression::Reference(r),
            EqualityExpression::Number(n) => AlgebraicExpression::Number(n),
        }
    }
}

/// An equality constraint between two `EqualityExpression`s.
pub struct EqualityConstraint<T> {
    pub left: EqualityExpression<T>,
    pub right: EqualityExpression<T>,
}

impl<T> From<EqualityConstraint<T>> for SymbolicConstraint<T> {
    fn from(constraint: EqualityConstraint<T>) -> Self {
        SymbolicConstraint {
            expr: AlgebraicExpression::from(constraint.left)
                - AlgebraicExpression::from(constraint.right),
        }
    }
}
