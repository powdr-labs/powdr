use std::hash::{Hash, Hasher};

use powdr_ast::analyzed::AlgebraicReference;

/// The identifier of a cell in the trace table, relative to a "zero row".
#[derive(Debug, Clone, Eq)]
pub struct Cell {
    pub column_name: String,
    pub id: u64,
    pub row_offset: i32,
}

impl Hash for Cell {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.row_offset.hash(state);
    }
}

impl PartialEq for Cell {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.row_offset == other.row_offset
    }
}

impl Ord for Cell {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self.id, self.row_offset).cmp(&(other.id, other.row_offset))
    }
}

impl PartialOrd for Cell {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Cell {
    pub fn from_reference(r: &AlgebraicReference, offset: i32) -> Self {
        assert!(r.is_witness());
        let row_offset = r.next as i32 + offset;
        Self {
            column_name: r.name.clone(),
            id: r.poly_id.id,
            row_offset,
        }
    }
}
