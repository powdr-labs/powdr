use std::collections::btree_map::Entry;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt::Debug;
use std::hash::Hash;

use itertools::Itertools;
use serde::{Deserialize, Serialize};

pub use crate::empirical_constraints::equivalence_class::EquivalenceClass;

/// "Constraints" that were inferred from execution statistics. They hold empirically
/// (most of the time), but are not guaranteed to hold in all cases.
#[derive(Serialize, Default, Debug)]
pub struct EmpiricalConstraints {
    /// For each program counter, the range constraints for each column.
    /// The range might not hold in 100% of cases.
    pub column_ranges_by_pc: BTreeMap<u32, Vec<(u32, u32)>>,
    /// For each basic block (identified by its starting PC), the equivalence classes of columns.
    /// Each equivalence class is a list of (instruction index in block, column index).
    pub equivalence_classes_by_block: BTreeMap<u64, EquivalenceClasses<BlockCell>>,
    pub debug_info: DebugInfo,
}

/// Debug information mapping AIR ids to program counters and column names.
#[derive(Serialize, Deserialize, Default, Debug)]
pub struct DebugInfo {
    /// Mapping from program counter to AIR id.
    pub air_id_by_pc: BTreeMap<u32, usize>,
    /// Mapping from AIR id to column names.
    pub column_names_by_air_id: BTreeMap<usize, Vec<String>>,
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
            let existing = self
                .equivalence_classes_by_block
                .entry(block_pc)
                .or_default();

            *existing = intersect_partitions(vec![std::mem::take(existing), classes]);
        }
        self.debug_info.combine_with(other.debug_info);
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
    /// The row index which is also the instruction index within the basic block
    row_idx: usize,
    /// The column index within the instruction air
    column_idx: usize,
}

impl BlockCell {
    pub fn new(row_idx: usize, column_idx: usize) -> Self {
        Self {
            row_idx,
            column_idx,
        }
    }
}

mod equivalence_class {
    use std::collections::BTreeSet;

    use serde::Serialize;

    /// An equivalence class with the following guarantees
    /// - It cannot be empty
    /// - It cannot hold a single element
    #[derive(Serialize, Debug, PartialOrd, Ord, PartialEq, Eq)]
    pub struct EquivalenceClass<T> {
        inner: BTreeSet<T>,
    }

    impl<T> Default for EquivalenceClass<T> {
        fn default() -> Self {
            Self {
                inner: BTreeSet::default(),
            }
        }
    }

    impl<T> EquivalenceClass<T> {
        pub fn iter(&self) -> impl Iterator<Item = &T> {
            self.inner.iter()
        }
    }

    impl<T: Ord> FromIterator<T> for EquivalenceClass<T> {
        fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
            let inner: BTreeSet<_> = iter.into_iter().collect();
            if inner.len() > 1 {
                Self { inner }
            } else {
                Self::default()
            }
        }
    }
}

/// A collection of equivalence classes
#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct EquivalenceClasses<T> {
    inner: BTreeSet<EquivalenceClass<T>>,
}

impl<T> Default for EquivalenceClasses<T> {
    fn default() -> Self {
        Self {
            inner: Default::default(),
        }
    }
}

impl<T: Ord> FromIterator<EquivalenceClass<T>> for EquivalenceClasses<T> {
    fn from_iter<I: IntoIterator<Item = EquivalenceClass<T>>>(iter: I) -> Self {
        Self {
            inner: iter.into_iter().collect(),
        }
    }
}

/// Intersects multiple partitions of the same universe into a single partition.
/// In other words, two elements are in the same equivalence class in the resulting partition
/// if and only if they are in the same equivalence class in all input partitions.
/// Singleton equivalence classes are omitted from the result.
pub fn intersect_partitions<T: Eq + Hash + Copy + Ord>(
    partitions: Vec<EquivalenceClasses<T>>,
) -> EquivalenceClasses<T> {
    // For each partition, build a map: Id -> class_index
    let class_ids: Vec<HashMap<T, usize>> = partitions
        .iter()
        .map(|partition| {
            partition
                .inner
                .iter()
                .enumerate()
                .flat_map(|(class_idx, class)| class.iter().map(move |&id| (id, class_idx)))
                .collect()
        })
        .collect();

    // Iterate over all elements in the universe
    let res = partitions
        .iter()
        .flat_map(|partition| &partition.inner)
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
        .collect();

    EquivalenceClasses { inner: res }
}

#[cfg(test)]
mod tests {
    use crate::empirical_constraints::EquivalenceClasses;

    fn partition(sets: Vec<Vec<u32>>) -> EquivalenceClasses<u32> {
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

        let result = super::intersect_partitions(vec![partition1, partition2]);

        let expected = partition(vec![vec![2, 3], vec![6, 7, 8]]);

        assert_eq!(result, expected);
    }
}
