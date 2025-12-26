use std::collections::{BTreeMap, BTreeSet};

use serde::{Deserialize, Deserializer, Serialize, Serializer};

/// An equivalence class, i.e, a set of values of type `T` which are considered equivalent
pub type EquivalenceClass<T> = BTreeSet<T>;

/// A collection of equivalence classes where all classes are non-empty
/// and each element appears in exactly one class.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Partition<T> {
    class_of: BTreeMap<T, T>,
}

impl<T: Ord + Clone> Partition<T> {
    fn from_classes<I>(classes: I) -> Self
    where
        I: IntoIterator<Item = EquivalenceClass<T>>,
    {
        let mut uf = UnionFind::default();
        let mut index = BTreeMap::<T, usize>::new();

        classes.into_iter().for_each(|class| {
            class.into_iter().fold(None, |first_id, item| {
                let id = intern(item, &mut uf, &mut index);
                match first_id {
                    Some(first_id) => {
                        uf.union(first_id, id);
                        Some(first_id)
                    }
                    None => Some(id),
                }
            });
        });

        let mut min_by_root = BTreeMap::<usize, T>::new();
        index.iter().for_each(|(item, id)| {
            let root = uf.find(*id);
            min_by_root
                .entry(root)
                .and_modify(|min| {
                    if item < min {
                        *min = item.clone();
                    }
                })
                .or_insert_with(|| item.clone());
        });

        let class_of = index
            .into_iter()
            .map(|(item, id)| {
                let root = uf.find(id);
                let rep = min_by_root
                    .get(&root)
                    .expect("root must have a representative")
                    .clone();
                (item, rep)
            })
            .collect();

        Self { class_of }
    }
}

impl<T: Ord + Clone> FromIterator<EquivalenceClass<T>> for Partition<T> {
    fn from_iter<I: IntoIterator<Item = EquivalenceClass<T>>>(iter: I) -> Self {
        Self::from_classes(iter)
    }
}

impl<T: Ord + Clone> Partition<T> {
    pub fn iter(&self) -> impl Iterator<Item = EquivalenceClass<T>> + '_ {
        self.classes().into_values()
    }

    fn classes(&self) -> BTreeMap<T, EquivalenceClass<T>> {
        self.class_of.iter().fold(
            BTreeMap::<T, EquivalenceClass<T>>::new(),
            |mut acc, (item, rep)| {
                acc.entry(rep.clone()).or_default().insert(item.clone());
                acc
            },
        )
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

        let base = partitions
            .iter()
            .min_by_key(|partition| partition.class_of.len())
            .expect("partitions is not empty");

        let mut grouped: BTreeMap<Vec<T>, EquivalenceClass<T>> = BTreeMap::new();

        base.class_of
            .keys()
            .filter_map(|item| {
                let signature = partitions
                    .iter()
                    .map(|partition| partition.class_of.get(item).cloned())
                    .collect::<Option<Vec<_>>>()?;
                Some((signature, item.clone()))
            })
            .for_each(|(signature, item)| {
                grouped.entry(signature).or_default().insert(item);
            });

        Self::from_classes(grouped.into_values())
    }
}

impl<T: Ord + Clone + Serialize> Serialize for Partition<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.iter().collect::<Vec<_>>().serialize(serializer)
    }
}

impl<'de, T> Deserialize<'de> for Partition<T>
where
    T: Ord + Clone + Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let classes = Vec::<EquivalenceClass<T>>::deserialize(deserializer)?;
        Ok(Self::from_classes(classes))
    }
}

fn intern<T: Ord>(value: T, uf: &mut UnionFind, index: &mut BTreeMap<T, usize>) -> usize {
    index.get(&value).copied().unwrap_or_else(|| {
        let id = uf.make_set();
        index.insert(value, id);
        id
    })
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
