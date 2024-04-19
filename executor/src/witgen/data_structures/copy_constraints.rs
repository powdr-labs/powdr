use std::collections::{BTreeMap, BTreeSet};

/// Global copy constraints, represented as a graph on all cells. Each node has one outgoing edge
/// that points to the next cell that should be equal to it, forming a set of cycles.
pub struct CopyConstraints<CellId> {
    /// The outgoing edge for each cell. If a cell is not in the map, it is its own next cell.
    edges: BTreeMap<CellId, CellId>,
}

// Can't derive Default if CellId doesn't implement Default :o
impl<CellId: Ord + Copy> Default for CopyConstraints<CellId> {
    fn default() -> Self {
        Self {
            edges: Default::default(),
        }
    }
}

impl<CellId: Ord + Copy> CopyConstraints<CellId> {
    pub fn new(constraint_pairs: &[(CellId, CellId)]) -> Self {
        let mut copy_constraints = CopyConstraints::default();
        for &(a, b) in constraint_pairs {
            copy_constraints.add_copy_constraint(a, b);
        }
        copy_constraints
    }

    fn add_copy_constraint(&mut self, a: CellId, b: CellId) {
        if self.is_connected(a, b) {
            // The algorithm below does not work if the two cells are already connected.
            // Note that this also filters out self-cycles.
            return;
        }

        // In the general case, a and b will already be in a cycle,
        // where a has a next node n_a and b has a next node n_b.
        // We want to change the edges such that a -> n_b and b -> n_a.
        // If a node does not have an entry in the edge list, its previous
        // and next nodes are the node itself.
        let n_a = self.edges.get(&a).copied().unwrap_or(a);
        let n_b = self.edges.get(&b).copied().unwrap_or(b);
        self.edges.insert(a, n_b);
        self.edges.insert(b, n_a);
    }

    /// Returns the next cell in the cycle that the given cell is part of.
    pub fn next(&self, cell_ref: CellId) -> CellId {
        self.edges.get(&cell_ref).copied().unwrap_or(cell_ref)
    }

    pub fn is_empty(&self) -> bool {
        self.edges.is_empty()
    }

    fn is_connected(&self, a: CellId, b: CellId) -> bool {
        for cell in self.iter_equivalence_class(a) {
            if cell == b {
                return true;
            }
        }
        false
    }

    fn iter_equivalence_class(&self, start: CellId) -> impl Iterator<Item = CellId> + '_ {
        std::iter::successors(Some(start), move |&cell_ref| {
            let next = self.next(cell_ref);
            if next == start {
                None
            } else {
                Some(next)
            }
        })
    }

    #[allow(dead_code)]
    /// Returns all equivalence classes of size > 1.
    pub fn non_trivial_equivalence_classes(&self) -> BTreeSet<BTreeSet<CellId>> {
        let mut seen: BTreeSet<CellId> = BTreeSet::new();
        let mut result = BTreeSet::new();
        for &cell_ref in self.edges.keys() {
            if seen.contains(&cell_ref) {
                continue;
            }
            let equivalence_class: BTreeSet<CellId> =
                self.iter_equivalence_class(cell_ref).collect();
            for cell in &equivalence_class {
                assert!(seen.insert(*cell));
            }
            result.insert(equivalence_class);
        }

        assert!(seen.len() == self.edges.len());
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_copy_constraints() {
        let constraints = vec![
            // Cycle 1 -> 2 -> 3 -> 4 -> 1
            (1, 2),
            (2, 3),
            (3, 4),
            // Cycle 5 -> 6 -> 7 -> 5
            (5, 6),
            (6, 7),
            // Merge previous two cycles
            (4, 7),
            // Add redundant edges (no-op)
            (7, 1),
            (7, 3),
            // Add independent cycle
            (8, 9),
            // Add self-cycle (no-op)
            (10, 10),
        ];
        let equivalence_classes =
            CopyConstraints::new(&constraints).non_trivial_equivalence_classes();
        println!("{:?}", equivalence_classes);
        assert_eq!(equivalence_classes.len(), 2);
        assert!(equivalence_classes.contains(&[1, 2, 3, 4, 5, 6, 7].iter().copied().collect()));
        assert!(equivalence_classes.contains(&[8, 9].iter().copied().collect()));
    }
}
