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

    /// Returns true if there are no equivalence classes.
    pub fn is_empty(&self) -> bool {
        self.num_classes == 0
    }
}

impl<T: Eq + Hash + Copy> Partition<T> {
    /// Creates a partition by grouping elements that have the same value.
    /// Elements with unique values are not included (singleton classes are omitted).
    pub fn from_values<V: Eq + Hash>(elements_with_values: impl Iterator<Item = (T, V)>) -> Self {
        let grouped: HashMap<V, Vec<T>> = elements_with_values
            .map(|(elem, value)| (value, elem))
            .into_group_map();

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

    /// Refines this partition by splitting classes where elements have different values.
    /// Elements in the same class that have the same value stay together.
    /// Elements for which `get_value` returns `None` are removed from the partition.
    /// Singleton classes are removed.
    pub fn refine_with<V: Eq + Hash>(&mut self, get_value: impl Fn(&T) -> Option<V>) {
        if self.class_of.is_empty() {
            return;
        }

        // Group surviving elements by (current_class, value)
        let grouped: HashMap<(usize, V), Vec<T>> = self
            .class_of
            .keys()
            .filter_map(|&elem| {
                let class = self.class_of.get(&elem).copied()?;
                let value = get_value(&elem)?;
                Some(((class, value), elem))
            })
            .into_group_map();

        // Rebuild with groups of 2+ elements
        self.class_of.clear();
        self.num_classes = 0;
        for elements in grouped.into_values() {
            if elements.len() > 1 {
                for element in elements {
                    self.class_of.insert(element, self.num_classes);
                }
                self.num_classes += 1;
            }
        }
    }

    /// Intersects multiple partitions of the same universe into a single partition.
    /// In other words, two elements are in the same equivalence class in the resulting partition
    /// if and only if they are in the same equivalence class in all input partitions.
    /// Singleton equivalence classes are omitted from the result.
    pub fn intersect(partitions: &[Self]) -> Self {
        let Some(first) = partitions.first() else {
            return Self {
                class_of: HashMap::new(),
                num_classes: 0,
            };
        };

        // Pairwise intersection: fold over partitions, intersecting two at a time.
        // This is more efficient than building Vec<usize> signatures because:
        // 1. We only hash (usize, usize) tuples instead of Vec<usize>
        // 2. The result shrinks after each intersection, making later steps faster
        partitions[1..]
            .iter()
            .fold(first.clone(), |acc, p| Self::intersect_two(&acc, p))
    }

    /// Intersects two partitions.
    fn intersect_two(a: &Self, b: &Self) -> Self {
        // Group elements by (class_in_a, class_in_b)
        // Elements with the same pair end up in the same result class
        let grouped: HashMap<(usize, usize), Vec<T>> = a
            .class_of
            .iter()
            .filter_map(|(&elem, &class_a)| {
                b.class_of
                    .get(&elem)
                    .map(|&class_b| ((class_a, class_b), elem))
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
        self.iter()
            .map(|class| class.into_iter().collect())
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

    #[test]
    fn test_from_values() {
        // Elements 1,2,3 have value 'a', elements 4,5 have value 'b', element 6 has value 'c'
        let p = Partition::from_values(
            [(1, 'a'), (2, 'a'), (3, 'a'), (4, 'b'), (5, 'b'), (6, 'c')].into_iter(),
        );
        let expected = partition(vec![vec![1, 2, 3], vec![4, 5]]);
        assert_eq!(p, expected);
    }

    #[test]
    fn test_refine_with() {
        // Start with elements 1-5 all in one class
        let mut p = Partition::from_values([(1, 0), (2, 0), (3, 0), (4, 0), (5, 0)].into_iter());
        assert_eq!(p, partition(vec![vec![1, 2, 3, 4, 5]]));

        // Refine: 1,2,3 have value 'a', 4,5 have value 'b'
        p.refine_with(|&x| Some(if x <= 3 { 'a' } else { 'b' }));
        assert_eq!(p, partition(vec![vec![1, 2, 3], vec![4, 5]]));

        // Refine again: 1 has value 1, 2,3 have value 2, 4,5 have value 3
        p.refine_with(|&x| {
            Some(if x == 1 {
                1
            } else if x <= 3 {
                2
            } else {
                3
            })
        });
        // Now 1 is singleton (removed), 2,3 stay together, 4,5 stay together
        assert_eq!(p, partition(vec![vec![2, 3], vec![4, 5]]));
    }

    #[test]
    fn test_refine_with_removal() {
        // Start with elements 1-4 all in one class
        let mut p = Partition::from_values([(1, 0), (2, 0), (3, 0), (4, 0)].into_iter());

        // Refine but element 3 returns None (removed)
        p.refine_with(|&x| if x == 3 { None } else { Some(x % 2) });
        // 1 has value 1 (odd), 2 has value 0 (even), 4 has value 0 (even)
        // So 2,4 stay together, 1 is singleton (removed)
        assert_eq!(p, partition(vec![vec![2, 4]]));
    }
}
