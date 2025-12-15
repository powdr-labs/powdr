use std::collections::btree_map::Entry;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt::Debug;
use std::hash::Hash;

use itertools::Itertools;
use serde::{Deserialize, Serialize};

use crate::{
    adapter::Adapter,
    blocks::BasicBlock,
    expression::{AlgebraicExpression, AlgebraicReference},
    SymbolicConstraint,
};

/// "Constraints" that were inferred from execution statistics. They hold empirically
/// (most of the time), but are not guaranteed to hold in all cases.
#[derive(Serialize, Deserialize, Clone, Default, Debug)]
pub struct EmpiricalConstraints {
    /// For each program counter, the range constraints for each column.
    /// The range might not hold in 100% of cases.
    pub column_ranges_by_pc: BTreeMap<u32, Vec<(u32, u32)>>,
    /// For each basic block (identified by its starting PC), the equivalence classes of columns.
    /// Each equivalence class is a list of (instruction index in block, column index).
    pub equivalence_classes_by_block: BTreeMap<u64, BTreeSet<BTreeSet<(usize, usize)>>>,
    /// Count of how many times each program counter was executed in the sampled executions.
    /// This can be used to set a threshold for applying constraints only to frequently executed PCs.
    pub pc_counts: BTreeMap<u32, u64>,
}

/// Debug information mapping AIR ids to program counters and column names.
#[derive(Serialize, Deserialize, Default)]
pub struct DebugInfo {
    /// Mapping from program counter to AIR id.
    pub air_id_by_pc: BTreeMap<u32, usize>,
    /// Mapping from AIR id to column names.
    pub column_names_by_air_id: BTreeMap<usize, Vec<String>>,
}

#[derive(Serialize, Deserialize)]
pub struct EmpiricalConstraintsJson {
    pub empirical_constraints: EmpiricalConstraints,
    pub debug_info: DebugInfo,
}

impl EmpiricalConstraints {
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
            self.equivalence_classes_by_block
                .entry(block_pc)
                .and_modify(|existing_classes| {
                    let combined =
                        intersect_partitions(&[existing_classes.clone(), classes.clone()]);
                    *existing_classes = combined;
                })
                .or_insert(classes);
        }

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
            for equivalence_class in equivalence_classes {
                let first = equivalence_class.first().unwrap();
                let first_ref = self.get_algebraic_reference(first.0, first.1);
                for other in equivalence_class.iter().skip(1) {
                    let other_ref = self.get_algebraic_reference(other.0, other.1);
                    let constraint = AlgebraicExpression::Reference(first_ref.clone())
                        - AlgebraicExpression::Reference(other_ref.clone());
                    constraints.push(SymbolicConstraint { expr: constraint });
                }
            }
        }

        constraints
    }

    // within each equivalance class, one range has to hold over all variables
    // therefore, we don't need one range per variable, rather one range per eq class
    // this can be checked/asserted
    // once we find that value, constraint creation can:
    // eg: ({a,b,c},x), a = x, a = b, b = c
    // here, {a,b} may have been removed by the solver already
    // for each eq class, remove all vars that do not appear in the first optimized machine
    // note: do not remove singletons at this point
    // now, we can generate constraints for an eq class ({t,w,z},y):
    // t=y, t=w, w=z
    pub fn strong_constraints(
        &self,
        variables: &BTreeSet<AlgebraicReference>,
    ) -> Vec<(SymbolicConstraint<<A as Adapter>::PowdrField>, bool)> {
        println!("Generating strong constraints");
        let mut constraints = Vec::new();

        let mut ranges = BTreeMap::new();
        for i in 0..self.block.statements.len() {
            let pc = (self.block.start_pc + (i * 4) as u64) as u32;
            let Some(range_constraints) = self.empirical_constraints.column_ranges_by_pc.get(&pc)
            else {
                continue;
            };
            for (col_index, range) in range_constraints.iter().enumerate() {
                if range.0 == range.1 {
                    ranges.insert((i, col_index), (range.0, range.1));
                }
            }
        }

        println!("Ranges map:\n");
        for (k, v) in &ranges {
            let col = self.get_algebraic_reference(k.0, k.1);
            println!("{col} = {v:?}");
        }

        if let Some(equivalence_classes) = self
            .empirical_constraints
            .equivalence_classes_by_block
            .get(&self.block.start_pc)
        {
            for (r, v) in &ranges {
                if !equivalence_classes
                    .iter()
                    .any(|eq_class| eq_class.contains(r))
                {
                    let col = self.get_algebraic_reference(r.0, r.1);
                    println!("Range variable {col} is not in any equivalence class");
                    if v.0 == v.1 {
                        let value = A::PowdrField::from(v.0 as u64);
                        let reference = self.get_algebraic_reference(r.0, r.1);
                        if variables.contains(&col) {
                            let constraint = AlgebraicExpression::Reference(reference)
                                - AlgebraicExpression::Number(value);
                            println!("Adding constraint {constraint}");
                            constraints.push((SymbolicConstraint { expr: constraint }, true));
                        } else {
                            println!("Optimized machine does not have it so not adding constraint");
                        }
                    }
                }
            }

            for equivalence_class in equivalence_classes {
                let eq_constraints =
                    self.strong_constraints_per_eq_class(equivalence_class, &ranges, variables);
                constraints.extend(eq_constraints);
            }
        }

        constraints
    }

    pub fn strong_constraints_per_eq_class(
        &self,
        eq_class: &BTreeSet<(usize, usize)>,
        ranges: &BTreeMap<(usize, usize), (u32, u32)>,
        variables: &BTreeSet<AlgebraicReference>,
    ) -> Vec<(SymbolicConstraint<<A as Adapter>::PowdrField>, bool)> {
        println!("Computing strong constraints for eq_class\n");
        for e in eq_class {
            let col = self.get_algebraic_reference(e.0, e.1);
            println!("{col}");
        }

        let range = eq_class
            .iter()
            .filter_map(|k| {
                let a = ranges.get(k);
                println!("Saw range {a:?}");
                a
            })
            .filter(|k| k.0 == k.1)
            .copied()
            .unique()
            .exactly_one()
            .ok();

        println!("Collecting range: {range:?}");

        // Remove variables from this equivalence class that are not in the optimized
        // machine.
        let eq_class = eq_class
            .iter()
            .filter(|e| {
                let col = self.get_algebraic_reference(e.0, e.1);
                variables.contains(&col)
            })
            .copied()
            .collect::<BTreeSet<_>>();

        println!(
            "Cols ({}) after removing cols that were removed by the solver already:",
            eq_class.len()
        );
        for e in &eq_class {
            let col = self.get_algebraic_reference(e.0, e.1);
            println!("{col}");
        }

        if eq_class.is_empty() {
            println!(
                "No variables left in this eq class after filtering, returning empty constraints"
            );
            return vec![];
        }

        match range {
            // There is a single range, continue as planned.
            Some(range) => {
                println!("there is a single range");

                match eq_class.len() {
                    0 => panic!("should have returned earlier"),
                    _ => {
                        // Create chain of equalities.
                        let mut chain = self.chain_of_equalities(&eq_class, true);
                        // Add range constraint to the first var.
                        let first = eq_class.first().unwrap();
                        let first_ref = self.get_algebraic_reference(first.0, first.1);
                        let range_eq =
                            AlgebraicExpression::Number(A::PowdrField::from(range.0 as u64));
                        let constraint =
                            AlgebraicExpression::Reference(first_ref.clone()) - range_eq.clone();
                        chain.push((SymbolicConstraint { expr: constraint }, true));
                        chain
                    }
                }
            }
            // There is no range over any variable in this eq class.
            // Only return the chain of equalities.
            None => {
                println!("there is NO single range, using only chain of equalities");
                self.chain_of_equalities(&eq_class, false)
            }
        }
    }

    pub fn chain_of_equalities(
        &self,
        eq_class: &BTreeSet<(usize, usize)>,
        have_range: bool,
    ) -> Vec<(SymbolicConstraint<<A as Adapter>::PowdrField>, bool)> {
        let mut iter = eq_class.iter().copied();
        let Some(first) = iter.next() else {
            return Vec::new();
        };
        let (_, constraints) = iter.fold((first, Vec::new()), |(prev, mut acc), curr| {
            let first_ref = self.get_algebraic_reference(prev.0, prev.1);
            let other_ref = self.get_algebraic_reference(curr.0, curr.1);
            let constraint = AlgebraicExpression::Reference(first_ref.clone())
                - AlgebraicExpression::Reference(other_ref.clone());
            acc.push((SymbolicConstraint { expr: constraint }, have_range));
            (curr, acc)
        });
        constraints
    }
}

/// Intersects multiple partitions of the same universe into a single partition.
/// In other words, two elements are in the same equivalence class in the resulting partition
/// if and only if they are in the same equivalence class in all input partitions.
/// Singleton equivalence classes are omitted from the result.
pub fn intersect_partitions<Id>(partitions: &[BTreeSet<BTreeSet<Id>>]) -> BTreeSet<BTreeSet<Id>>
where
    Id: Eq + Hash + Copy + Ord,
{
    // For each partition, build a map: Id -> class_index
    let class_ids: Vec<HashMap<Id, usize>> = partitions
        .iter()
        .map(|partition| {
            partition
                .iter()
                .enumerate()
                .flat_map(|(class_idx, class)| class.iter().map(move |&id| (id, class_idx)))
                .collect()
        })
        .collect();

    // Iterate over all elements in the universe
    partitions
        .iter()
        .flat_map(|partition| partition.iter())
        .flat_map(|class| class.iter().copied())
        .unique()
        .filter_map(|id| {
            // Build the signature of the element: the list of class indices it belongs to
            // (one index per partition)
            class_ids
                .iter()
                .map(|m| m.get(&id).cloned())
                // If an element did not appear in any one of the partitions, it is
                // a singleton and we skip it.
                .collect::<Option<Vec<usize>>>()
                .map(|signature| (signature, id))
        })
        // Group elements by their signatures
        .into_group_map()
        .into_values()
        // Remove singletons and convert to Set
        .filter_map(|ids| (ids.len() > 1).then_some(ids.into_iter().collect()))
        .collect()
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    fn partition(sets: Vec<Vec<u32>>) -> BTreeSet<BTreeSet<u32>> {
        sets.into_iter().map(|s| s.into_iter().collect()).collect()
    }

    #[test]
    fn test_intersect_partitions() {
        let partition1 = partition(vec![
            // Two classes: 1-4 and 5-9
            vec![1, 2, 3, 4],
            vec![5, 6, 7, 8, 9],
        ]);
        let partition2 = partition(vec![
            // Four classes: 1, 2-3, 4-5, 6-8, 9 (implicit)
            vec![1],
            vec![2, 3],
            vec![4, 5],
            vec![6, 7, 8],
        ]);

        let result = super::intersect_partitions(&[partition1, partition2]);

        let expected = partition(vec![vec![2, 3], vec![6, 7, 8]]);

        assert_eq!(result, expected);
    }
}
