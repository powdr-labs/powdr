use std::collections::{BTreeSet, HashMap};
use std::hash::Hash;

use itertools::Itertools;
use serde::{Deserialize, Serialize};

/// An equivalence class, i.e, a set of values of type `T` which are considered equivalent
pub type EquivalenceClass<T> = BTreeSet<T>;

/// A collection of equivalence classes where all classes are guaranteed to have at least two elements
/// This is enforced by construction of this type only happening through collection, where we ignore empty and singleton classes
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
#[serde(bound(deserialize = "T: Ord + Deserialize<'de>"))]
pub struct Partition<T> {
    inner: BTreeSet<EquivalenceClass<T>>,
}

impl<T: Ord> FromIterator<EquivalenceClass<T>> for Partition<T> {
    fn from_iter<I: IntoIterator<Item = EquivalenceClass<T>>>(iter: I) -> Self {
        // When collecting, we ignore classes with 0 or 1 elements as they are useless
        Self {
            inner: iter.into_iter().filter(|class| class.len() > 1).collect(),
        }
    }
}

impl<T> Partition<T> {
    pub fn iter(&self) -> impl Iterator<Item = &EquivalenceClass<T>> {
        self.inner.iter()
    }
}

impl<T: Ord + Hash + Copy> Partition<T> {
    /// Intersects multiple partitions of the same universe into a single partition.
    /// In other words, two elements are in the same equivalence class in the resulting partition
    /// if and only if they are in the same equivalence class in all input partitions.
    /// Singleton equivalence classes are omitted from the result.
    pub fn intersect(partitions: &[Self]) -> Self {
        // For each partition, build a map: Id -> class_index
        let class_ids: Vec<HashMap<T, usize>> = partitions
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
            .flat_map(|class| class.iter())
            .unique()
            .filter_map(|id| {
                // Build the signature of the element: the list of class indices it belongs to
                // (one index per partition)
                class_ids
                    .iter()
                    .map(|m| m.get(id).cloned())
                    // If an element did not appear in any one of the partitions, it is
                    // a singleton and we skip it.
                    .collect::<Option<Vec<usize>>>()
                    .map(|signature| (signature, id))
            })
            // Group elements by their signatures
            .into_group_map()
            .into_values()
            // Convert to set
            .map(|ids| ids.into_iter().copied().collect())
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use crate::equivalence_classes::Partition;

    fn partition(sets: Vec<Vec<u32>>) -> Partition<u32> {
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

        let result = Partition::intersect(&[partition1, partition2]);

        let expected = partition(vec![vec![2, 3], vec![6, 7, 8]]);

        assert_eq!(result, expected);
    }
}
