use std::collections::{BTreeSet, HashMap};
use std::hash::Hash;

use itertools::Itertools;
use rayon::prelude::*;
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
        self.to_classes().serialize(serializer)
    }
}

impl<'de, T: Eq + Hash + Deserialize<'de>> Deserialize<'de> for Partition<T> {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        // Deserialize from Vec<Vec<T>>, reusing FromIterator logic
        let classes: Vec<Vec<T>> = Vec::deserialize(deserializer)?;
        Ok(classes.into_iter().collect())
    }
}

impl<T: Eq + Hash, C: IntoIterator<Item = T>> FromIterator<C> for Partition<T>
where
    C::IntoIter: ExactSizeIterator,
{
    fn from_iter<I: IntoIterator<Item = C>>(iter: I) -> Self {
        let mut class_of = HashMap::new();
        let mut num_classes = 0;

        for class in iter {
            let class_iter = class.into_iter();
            // Ignore classes with 0 or 1 elements as they are useless
            if class_iter.len() > 1 {
                for element in class_iter {
                    assert!(class_of.insert(element, num_classes).is_none());
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
    /// Returns all equivalence classes as a Vec<Vec<T>>.
    /// This is O(n) where n is the number of elements.
    #[allow(clippy::iter_over_hash_type)] // Order within classes doesn't matter semantically
    pub fn to_classes(&self) -> Vec<Vec<T>> {
        let mut classes: Vec<Vec<T>> = vec![Vec::new(); self.num_classes];
        for (elem, &class_id) in &self.class_of {
            classes[class_id].push(elem.clone());
        }
        classes
    }

    /// Intersects multiple partitions of the same universe into a single partition.
    /// In other words, two elements are in the same equivalence class in the resulting partition
    /// if and only if they are in the same equivalence class in all input partitions.
    /// Singleton equivalence classes are omitted from the result.
    pub fn intersect_many(partitions: impl IntoIterator<Item = Self>) -> Self {
        // Pairwise intersection: fold over partitions, intersecting two at a time.
        // This is more efficient than building Vec<usize> signatures because:
        // 1. We only hash (usize, usize) tuples instead of Vec<usize>
        // 2. The result shrinks after each intersection, making later steps faster
        partitions
            .into_iter()
            .reduce(Partition::intersected_with)
            .expect("expected at least one element")
    }

    /// Intersects two partitions.
    pub fn intersected_with(self, other: Self) -> Self {
        // Group elements by (class_in_self, class_in_other)
        // Elements with the same pair end up in the same result class
        self.class_of
            .into_iter()
            // Note that if an element is not in self or other, it is a
            // singleton and will also not be in the intersection.
            .filter_map(|(elem, class_a)| {
                other
                    .class_of
                    .get(&elem)
                    .map(|&class_b| ((class_a, class_b), elem.clone()))
            })
            .into_group_map()
            .into_values()
            .collect()
    }
}

/// Number of partitions to combine in each chunk before parallelizing.
const CHUNK_SIZE: usize = 64;

impl<T: Eq + Hash + Copy + Send + Sync> Partition<T> {
    /// Intersects multiple partitions in parallel using a chunked tree reduction.
    ///
    /// Partitions are grouped into chunks, each chunk is intersected sequentially,
    /// then the chunk results are combined recursively in parallel.
    pub fn parallel_intersect(partitions: impl IndexedParallelIterator<Item = Self>) -> Self {
        if partitions.len() <= CHUNK_SIZE {
            // Base case: We only have one chunk, intersect sequentially
            let partitions = partitions.collect::<Vec<_>>();
            return Self::intersect_many(partitions);
        }

        // Chunk partitions and intersect each chunk in parallel
        let chunk_results = partitions
            .chunks(CHUNK_SIZE)
            .map(Self::intersect_many)
            // Not collecting here causes the type checker to hit the recursion limit...
            .collect::<Vec<_>>();

        // Recursively combine chunk results
        Self::parallel_intersect(chunk_results.into_par_iter())
    }
}

/// Equality implementation that converts to canonical form for comparison.
/// This is intentionally simple (not optimized) since it's only used in tests.
impl<T: Eq + Hash + Ord + Clone> PartialEq for Partition<T> {
    fn eq(&self, other: &Self) -> bool {
        self.to_canonical() == other.to_canonical()
    }
}

impl<T: Eq + Hash + Ord + Clone> Eq for Partition<T> {}

impl<T: Eq + Hash + Ord + Clone> Partition<T> {
    /// Converts to a canonical BTreeSet<BTreeSet<T>> form for equality comparison.
    fn to_canonical(&self) -> BTreeSet<BTreeSet<T>> {
        self.to_classes()
            .into_iter()
            .map(|class| class.into_iter().collect())
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use crate::equivalence_classes::Partition;

    fn partition(sets: Vec<Vec<u32>>) -> Partition<u32> {
        sets.into_iter().collect()
    }

    #[test]
    fn test_intersect_partitions() {
        let partition1 = partition(vec![
            // Two classes: {1,2,3,4} and {5,6,7,8,9}
            vec![1, 2, 3, 4],
            vec![5, 6, 7, 8, 9],
        ]);
        let partition2 = partition(vec![
            // Classes: {2,3}, {4,5}, {6,7,8} (1 and 9 are singletons)
            vec![2, 3],
            vec![4, 5],
            vec![6, 7, 8],
        ]);
        let partition3 = partition(vec![
            // Classes: {2,3}, {6,7}, {8,9} (splits {6,7,8} into {6,7} and {8})
            vec![2, 3],
            vec![6, 7],
            vec![8, 9],
        ]);

        let result = Partition::intersect_many([partition1, partition2, partition3]);

        // After intersecting all three:
        // - {2,3} survives (in same class in all three)
        // - {6,7} survives (6,7,8 in p2 intersected with 6,7 in p3)
        // - 8 becomes singleton (was with 6,7 in p2, but with 9 in p3, and 9 not in p1's class)
        let expected = partition(vec![vec![2, 3], vec![6, 7]]);

        assert_eq!(result, expected);
    }
}
