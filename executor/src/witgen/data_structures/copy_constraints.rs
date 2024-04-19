use std::collections::BTreeMap;

use powdr_ast::analyzed::PolyID;

use crate::witgen::rows::RowIndex;

type CellRef = (PolyID, RowIndex);

#[derive(Default)]
pub struct CopyConstraints {
    copy_cycle: BTreeMap<CellRef, CellRef>,
}

impl CopyConstraints {
    pub fn new(constraint_pairs: Vec<((PolyID, RowIndex), (PolyID, RowIndex))>) -> Self {
        let mut copy_cycle = BTreeMap::new();
        let mut back_edges = BTreeMap::new();

        for (a, b) in constraint_pairs {
            // In the general case, a and b will already be in a cycle,
            // where a has a next node n_a and b has a previous node p_b.
            // We want to change the edges such that a -> b and p_b -> n_a.
            // If a node does not have an entry in the edge list, its previous
            // and next nodes are the node itself.
            let n_a = copy_cycle.get(&a).copied().unwrap_or(a);
            let p_b = back_edges.get(&b).copied().unwrap_or(b);
            copy_cycle.insert(a, b);
            back_edges.insert(b, a);
            copy_cycle.insert(p_b, n_a);
            back_edges.insert(n_a, p_b);
        }
        Self { copy_cycle }
    }

    pub fn next(&self, cell_ref: CellRef) -> CellRef {
        self.copy_cycle.get(&cell_ref).copied().unwrap_or(cell_ref)
    }

    pub fn is_empty(&self) -> bool {
        self.copy_cycle.is_empty()
    }
}
