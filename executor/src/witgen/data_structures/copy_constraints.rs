use std::collections::{BTreeMap, HashSet};

use powdr_ast::analyzed::PolyID;
use powdr_number::FieldElement;

use crate::witgen::{rows::RowIndex, FixedData};

type CellRef = (PolyID, RowIndex);

#[derive(Default)]
pub struct CopyConstraints {
    copy_cycle: BTreeMap<CellRef, CellRef>,
}

impl CopyConstraints {
    pub fn from_fixed_data<T: FieldElement>(
        fixed_data: &FixedData<T>,
        witness_cols: &HashSet<PolyID>,
    ) -> Self {
        let is_example = witness_cols
            .iter()
            .all(|c| fixed_data.column_name(c).starts_with("main_pythagoras."));

        if is_example {
            log::info!("Using hard-coded copy constraints for Pythagoras example");
            // Hard-code copy constraints every 4 rows:
            // - a[0] = a[1]
            // - a[0] = b[1]
            // - b[0] = a[2]
            // - b[0] = b[2]
            // - c[1] = a[3]
            // - c[2] = b[3]
            // - c[0] = c[3]
            let a = fixed_data.try_column_by_name("main_pythagoras.a").unwrap();
            let b = fixed_data.try_column_by_name("main_pythagoras.b").unwrap();
            let c = fixed_data.try_column_by_name("main_pythagoras.c").unwrap();
            let d = fixed_data.degree;
            let index =
                |block_index, local_index| RowIndex::from_degree(block_index * 4 + local_index, d);

            let mut constraints = Vec::new();

            for i in 0..d / 4 {
                constraints.push(((a, index(i, 0)), (a, index(i, 1))));
                constraints.push(((a, index(i, 0)), (b, index(i, 1))));
                constraints.push(((b, index(i, 0)), (a, index(i, 2))));
                constraints.push(((b, index(i, 0)), (b, index(i, 2))));
                constraints.push(((c, index(i, 1)), (a, index(i, 3))));
                constraints.push(((c, index(i, 2)), (b, index(i, 3))));
                constraints.push(((c, index(i, 0)), (c, index(i, 3))));
            }
            CopyConstraints::new(constraints)
        } else {
            CopyConstraints::default()
        }
    }

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
