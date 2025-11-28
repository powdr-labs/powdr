use std::collections::btree_map::Entry;
use std::collections::BTreeMap;
use std::fmt::Debug;
use std::hash::Hash;

use serde::{Deserialize, Serialize};

pub use crate::equivalence_classes::{EquivalenceClass, Partition};

use crate::{
    adapter::Adapter,
    blocks::BasicBlock,
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
    /// Each equivalence class is a list of (instruction index in block, column index).
    pub equivalence_classes_by_block: BTreeMap<u64, Partition<BlockCell>>,
    pub debug_info: DebugInfo,
    /// Count of how many times each program counter was executed in the sampled executions.
    /// This can be used to set a threshold for applying constraints only to frequently executed PCs.
    pub pc_counts: BTreeMap<u32, u64>,
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
                Entry::Occupied(e) => {
                    // Remove the value and compute the intersection
                    // This is because `intersect_partitions` takes inputs by value
                    let existing = e.remove();
                    Partition::intersect(&[existing, classes])
                }
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

    pub fn apply_pc_threshold(&self, threshold: u64) -> Self {
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
                    // For equivalence classes, we check the pc_counts of the first instruction in the block
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
const EXECUTION_COUNT_THRESHOLD: u64 = 100;

pub struct ConstraintGenerator<'a, A: Adapter> {
    empirical_constraints: EmpiricalConstraints,
    algebraic_references: BTreeMap<(usize, usize), AlgebraicReference>,
    block: &'a BasicBlock<A::Instruction>,
}

impl<'a, A: Adapter> ConstraintGenerator<'a, A> {
    pub fn new(
        empirical_constraints: &EmpiricalConstraints,
        subs: &[Vec<u64>],
        columns: impl Iterator<Item = AlgebraicReference>,
        block: &'a BasicBlock<A::Instruction>,
    ) -> Self {
        let reverse_subs = subs
            .iter()
            .enumerate()
            .flat_map(|(instr_index, subs)| {
                subs.iter()
                    .enumerate()
                    .map(move |(col_index, &poly_id)| (poly_id, (instr_index, col_index)))
            })
            .collect::<BTreeMap<_, _>>();
        let algebraic_references = columns
            .map(|r| (*reverse_subs.get(&r.id).unwrap(), r.clone()))
            .collect::<BTreeMap<_, _>>();

        Self {
            empirical_constraints: empirical_constraints
                .apply_pc_threshold(EXECUTION_COUNT_THRESHOLD),
            algebraic_references,
            block,
        }
    }

    fn get_algebraic_reference(&self, instr_index: usize, col_index: usize) -> AlgebraicReference {
        self.algebraic_references
            .get(&(instr_index, col_index))
            .cloned()
            .unwrap_or_else(|| {
                panic!(
                    "Missing reference for (i: {}, col_index: {}, block_id: {})",
                    instr_index, col_index, self.block.start_pc
                )
            })
    }

    pub fn range_constraints(&self) -> Vec<SymbolicConstraint<<A as Adapter>::PowdrField>> {
        let mut constraints = Vec::new();

        for i in 0..self.block.statements.len() {
            let pc = (self.block.start_pc + (i * 4) as u64) as u32;
            let Some(range_constraints) = self.empirical_constraints.column_ranges_by_pc.get(&pc)
            else {
                continue;
            };
            for (col_index, range) in range_constraints.iter().enumerate() {
                // TODO: We could also enforce looser range constraints.
                // This is a bit more complicated though, because we'd have to add bus interactions
                // to actually enforce them.
                if range.0 == range.1 {
                    let value = A::PowdrField::from(range.0 as u64);
                    let reference = self.get_algebraic_reference(i, col_index);
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

        if let Some(equivalence_classes) = self
            .empirical_constraints
            .equivalence_classes_by_block
            .get(&self.block.start_pc)
        {
            for equivalence_class in equivalence_classes.iter() {
                let first = equivalence_class.first().unwrap();
                let first_ref =
                    self.get_algebraic_reference(first.instruction_idx, first.column_idx);
                for other in equivalence_class.iter().skip(1) {
                    let other_ref =
                        self.get_algebraic_reference(other.instruction_idx, other.column_idx);
                    let constraint = AlgebraicExpression::Reference(first_ref.clone())
                        - AlgebraicExpression::Reference(other_ref.clone());
                    constraints.push(SymbolicConstraint { expr: constraint });
                }
            }
        }

        constraints
    }
}
