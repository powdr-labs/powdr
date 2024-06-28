use std::collections::{BTreeMap, BTreeSet};

pub(crate) type MergedMachines = MergedMachinesImpl<String>;

/// A simple union-find data structure to keep track of which machines to merge.
pub(crate) struct MergedMachinesImpl<T: Clone + Ord> {
    // Maps a machine ID to its "parent", i.e., the next hop towards
    // the representative of the equivalence class.
    // If a machine ID is not included in the map, it is a representative.
    parent: BTreeMap<T, T>,
}

impl<T: Clone + Ord> MergedMachinesImpl<T> {
    pub(crate) fn new() -> Self {
        MergedMachinesImpl {
            parent: BTreeMap::new(),
        }
    }

    fn find(&self, machine: &T) -> T {
        let mut current = machine;
        while let Some(parent) = self.parent.get(current) {
            current = parent;
        }
        current.clone()
    }

    pub(crate) fn merge(&mut self, machine1: T, machine2: T) {
        let root1 = self.find(&machine1);
        let root2 = self.find(&machine2);

        if root1 != root2 {
            self.parent.insert(root2, root1);
        }
    }

    pub(crate) fn merged_machines(&self) -> BTreeSet<BTreeSet<T>> {
        let mut groups: BTreeMap<T, BTreeSet<T>> = BTreeMap::new();
        for machine in self.parent.keys() {
            let root = self.find(machine);
            groups
                .entry(root.clone())
                .or_default()
                .insert(machine.clone());
            // The root itself does not appear in the map, so we need to insert it manually.
            groups.get_mut(&root).unwrap().insert(root);
        }
        groups.into_values().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_merged_machines() {
        let mut merged_machines = MergedMachinesImpl::new();
        // Equivalence classes: {{1, 2, 3, 4}, {5, 6}}
        merged_machines.merge(1, 2);
        merged_machines.merge(3, 4);
        merged_machines.merge(2, 3);
        merged_machines.merge(1, 4);
        merged_machines.merge(5, 6);

        let merged_machines = merged_machines.merged_machines();
        assert_eq!(merged_machines.len(), 2);
        assert!(merged_machines.contains(&[1, 2, 3, 4].iter().cloned().collect()));
        assert!(merged_machines.contains(&[5, 6].iter().cloned().collect()));
    }
}
