use std::collections::{BTreeMap, BTreeSet};

use serde::{Deserialize, Deserializer, Serialize};

/// An equivalence class, i.e, a set of values of type `T` which are considered equivalent
pub type EquivalenceClass<T> = BTreeSet<T>;

/// A collection of equivalence classes where all classes are guaranteed to have at least one element
/// and no element can appear in more than one class.
#[derive(Serialize, Debug, PartialEq, Eq, Clone)]
pub struct Partition<T> {
    inner: BTreeSet<EquivalenceClass<T>>,
}

impl<T: Ord> FromIterator<EquivalenceClass<T>> for Partition<T> {
    fn from_iter<I: IntoIterator<Item = EquivalenceClass<T>>>(iter: I) -> Self {
        Self::from_classes(iter)
    }
}

impl<T> Partition<T> {
    pub fn iter(&self) -> impl Iterator<Item = &EquivalenceClass<T>> {
        self.inner.iter()
    }
}

impl<T: Ord> Partition<T> {
    fn from_classes<I>(classes: I) -> Self
    where
        I: IntoIterator<Item = EquivalenceClass<T>>,
    {
        let mut merged: Vec<EquivalenceClass<T>> = Vec::new();

        for mut class in classes {
            if class.is_empty() {
                continue;
            }

            let mut i = 0;
            while i < merged.len() {
                if class.is_disjoint(&merged[i]) {
                    i += 1;
                    continue;
                }

                let mut existing = std::mem::take(&mut merged[i]);
                class.append(&mut existing);
                merged.swap_remove(i);
            }

            merged.push(class);
        }

        Self {
            inner: merged.into_iter().collect(),
        }
    }
}

impl<'de, T> Deserialize<'de> for Partition<T>
where
    T: Ord + Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let classes = BTreeSet::<EquivalenceClass<T>>::deserialize(deserializer)?;
        Ok(Self::from_classes(classes))
    }
}

impl<T: Ord + Clone> Partition<T> {
    /// Intersects multiple partitions of the same universe into a single partition.
    /// In other words, two elements are in the same equivalence class in the resulting partition
    /// if and only if they are in the same equivalence class in all input partitions.
    /// Empty equivalence classes are omitted from the result.
    pub fn intersect(partitions: &[Self]) -> Self {
        if partitions.is_empty() {
            return Self::from_classes(std::iter::empty());
        }

        if partitions.len() == 1 {
            return partitions[0].clone();
        }

        let class_ids: Vec<BTreeMap<&T, usize>> = partitions
            .iter()
            .map(|partition| {
                let mut map = BTreeMap::new();
                for (class_idx, class) in partition.iter().enumerate() {
                    for item in class {
                        let previous = map.insert(item, class_idx);
                        debug_assert!(previous.is_none(), "Element appears in multiple classes");
                    }
                }
                map
            })
            .collect();

        let base = class_ids
            .iter()
            .min_by_key(|map| map.len())
            .expect("partitions is not empty");

        let mut grouped: BTreeMap<Vec<usize>, EquivalenceClass<T>> = BTreeMap::new();

        for item in base.keys().copied() {
            let mut signature = Vec::with_capacity(class_ids.len());
            let mut in_all = true;
            for map in &class_ids {
                if let Some(class_id) = map.get(item) {
                    signature.push(*class_id);
                } else {
                    in_all = false;
                    break;
                }
            }
            if !in_all {
                continue;
            }

            grouped
                .entry(signature)
                .or_default()
                .insert(item.clone());
        }

        Self::from_classes(grouped.into_values())
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

        let expected = partition(vec![
            vec![1],
            vec![2, 3],
            vec![4],
            vec![5],
            vec![6, 7, 8],
        ]);

        assert_eq!(result, expected);
    }

    #[test]
    fn test_merge_overlapping_classes() {
        let merged = partition(vec![vec![1, 2], vec![2, 3], vec![4, 5], vec![6]]);
        let expected = partition(vec![vec![1, 2, 3], vec![4, 5], vec![6]]);
        assert_eq!(merged, expected);
    }
}
