use std::collections::{BTreeMap, BTreeSet};

use serde::{Deserialize, Deserializer, Serialize};

/// An equivalence class, i.e, a set of values of type `T` which are considered equivalent
pub type EquivalenceClass<T> = BTreeSet<T>;

/// A collection of equivalence classes where all classes are non-empty
/// and each element appears in exactly one class.
#[derive(Serialize, Debug, PartialEq, Eq, Clone)]
pub struct Partition<T> {
    inner: BTreeSet<EquivalenceClass<T>>,
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
        let mut uf = UnionFind::default();
        let mut index = BTreeMap::<T, usize>::new();

        for class in classes {
            let mut iter = class.into_iter();
            let Some(first) = iter.next() else {
                continue;
            };
            let first_id = intern(first, &mut uf, &mut index);
            for item in iter {
                let id = intern(item, &mut uf, &mut index);
                uf.union(first_id, id);
            }
        }

        let mut grouped: BTreeMap<usize, EquivalenceClass<T>> = BTreeMap::new();
        for (item, id) in index {
            let root = uf.find(id);
            grouped.entry(root).or_default().insert(item);
        }

        Self {
            inner: grouped.into_values().collect(),
        }
    }
}

impl<T: Ord> FromIterator<EquivalenceClass<T>> for Partition<T> {
    fn from_iter<I: IntoIterator<Item = EquivalenceClass<T>>>(iter: I) -> Self {
        Self::from_classes(iter)
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
        let classes = Vec::<EquivalenceClass<T>>::deserialize(deserializer)?;
        Ok(Self::from_classes(classes))
    }
}

impl<T: Ord + Clone> Partition<T> {
    fn class_index(&self) -> BTreeMap<T, usize> {
        let mut map = BTreeMap::new();
        for (class_idx, class) in self.inner.iter().enumerate() {
            for item in class {
                map.insert(item.clone(), class_idx);
            }
        }
        map
    }

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

        let class_ids: Vec<BTreeMap<T, usize>> =
            partitions.iter().map(|partition| partition.class_index()).collect();

        let base = class_ids
            .iter()
            .min_by_key(|map| map.len())
            .expect("partitions is not empty");

        let mut grouped: BTreeMap<Vec<usize>, EquivalenceClass<T>> = BTreeMap::new();

        for item in base.keys() {
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

            grouped.entry(signature).or_default().insert(item.clone());
        }

        Self::from_classes(grouped.into_values())
    }
}

fn intern<T: Ord>(value: T, uf: &mut UnionFind, index: &mut BTreeMap<T, usize>) -> usize {
    if let Some(&id) = index.get(&value) {
        return id;
    }

    let id = uf.make_set();
    index.insert(value, id);
    id
}

#[derive(Default)]
struct UnionFind {
    parent: Vec<usize>,
}

impl UnionFind {
    fn make_set(&mut self) -> usize {
        let id = self.parent.len();
        self.parent.push(id);
        id
    }

    fn find(&mut self, id: usize) -> usize {
        let parent = self.parent[id];
        if parent == id {
            id
        } else {
            let root = self.find(parent);
            self.parent[id] = root;
            root
        }
    }

    fn union(&mut self, a: usize, b: usize) {
        let root_a = self.find(a);
        let root_b = self.find(b);
        if root_a != root_b {
            self.parent[root_b] = root_a;
        }
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
