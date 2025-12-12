use std::collections::btree_map::Entry;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt::Debug;
use std::hash::Hash;

use itertools::Itertools;
use serde::{Deserialize, Serialize};

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
