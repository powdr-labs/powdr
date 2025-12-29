use std::collections::{BTreeSet, HashMap};
use std::hash::Hash;

use itertools::Itertools;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

/// An equivalence class, i.e, a set of values of type `T` which are considered equivalent
pub type EquivalenceClass<T> = BTreeSet<T>;

/// A collection of equivalence classes where all classes are guaranteed to have at least two elements.
/// This is enforced by construction of this type only happening through collection, where we ignore empty and singleton classes.
///
/// Internally represented as a map from element to class ID for efficient intersection operations.
/// Serializes as Vec<Vec<T>> for JSON compatibility (JSON requires string keys in objects).
#[derive(Debug, Clone)]
pub struct Partition<T> {
    /// Maps each element to its class ID (0..num_classes)
    class_of: HashMap<T, usize>,
    /// Number of classes
    num_classes: usize,
}

impl<T: Eq + Hash + Serialize + Clone> Serialize for Partition<T> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        // Serialize as Vec<Vec<T>> for JSON compatibility
        let classes: Vec<Vec<T>> = self.iter().collect();
        classes.serialize(serializer)
    }
}

impl<'de, T: Eq + Hash + Ord + Deserialize<'de>> Deserialize<'de> for Partition<T> {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        // Deserialize from Vec<Vec<T>>, reusing FromIterator logic
        let classes: Vec<Vec<T>> = Vec::deserialize(deserializer)?;
        Ok(classes
            .into_iter()
            .map(|class| class.into_iter().collect())
            .collect())
    }
}

impl<T: Eq + Hash> FromIterator<EquivalenceClass<T>> for Partition<T> {
    fn from_iter<I: IntoIterator<Item = EquivalenceClass<T>>>(iter: I) -> Self {
        let mut class_of = HashMap::new();
        let mut num_classes = 0;

        for class in iter {
            // Ignore classes with 0 or 1 elements as they are useless
            if class.len() > 1 {
                for element in class {
                    class_of.insert(element, num_classes);
                }
                num_classes += 1;
            }
        }

        Self {
            class_of,
            num_classes,
        }
    }
}

impl<T: Eq + Hash + Clone> Partition<T> {
    /// Iterates over equivalence classes, returning each class as a Vec of its elements.
    pub fn iter(&self) -> impl Iterator<Item = Vec<T>> + '_ {
        (0..self.num_classes).map(|class_id| {
            self.class_of
                .iter()
                .filter(move |(_, &c)| c == class_id)
                .map(|(e, _)| e.clone())
                .collect()
        })
    }
}

impl<T: Eq + Hash + Copy> Partition<T> {
    /// Intersects multiple partitions of the same universe into a single partition.
    /// In other words, two elements are in the same equivalence class in the resulting partition
    /// if and only if they are in the same equivalence class in all input partitions.
    /// Singleton equivalence classes are omitted from the result.
    pub fn intersect(partitions: &[Self]) -> Self {
        // Collect references to each partition's class_of map
        let class_maps: Vec<&HashMap<T, usize>> =
            partitions.iter().map(|p| &p.class_of).collect();

        // Iterate over all elements in the universe (union of all partition elements)
        let all_elements: Vec<T> = partitions
            .iter()
            .flat_map(|p| p.class_of.keys())
            .copied()
            .unique()
            .collect();

        // Group elements by their signature (tuple of class IDs across all partitions)
        let grouped: HashMap<Vec<usize>, Vec<T>> = all_elements
            .into_iter()
            .filter_map(|element| {
                // Build the signature: the list of class indices for this element (one per partition)
                let signature: Option<Vec<usize>> = class_maps
                    .iter()
                    .map(|m| m.get(&element).copied())
                    // If an element did not appear in any one of the partitions, it is
                    // a singleton and we skip it.
                    .collect();
                signature.map(|sig| (sig, element))
            })
            .into_group_map();

        // Build result: assign new class IDs to groups with 2+ elements
        let mut class_of = HashMap::new();
        let mut num_classes = 0;

        for elements in grouped.into_values() {
            if elements.len() > 1 {
                for element in elements {
                    class_of.insert(element, num_classes);
                }
                num_classes += 1;
            }
        }

        Self {
            class_of,
            num_classes,
        }
    }
}

/// Equality implementation that converts to canonical form for comparison.
/// This is intentionally simple (not optimized) since it's primarily used in tests.
impl<T: Eq + Hash + Ord + Clone> PartialEq for Partition<T> {
    fn eq(&self, other: &Self) -> bool {
        self.to_canonical() == other.to_canonical()
    }
}

impl<T: Eq + Hash + Ord + Clone> Eq for Partition<T> {}

impl<T: Eq + Hash + Ord + Clone> Partition<T> {
    /// Converts to a canonical BTreeSet<BTreeSet<T>> form for equality comparison.
    fn to_canonical(&self) -> BTreeSet<BTreeSet<T>> {
        self.iter().map(|class| class.into_iter().collect()).collect()
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
