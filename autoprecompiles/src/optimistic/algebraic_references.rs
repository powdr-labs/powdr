use std::collections::BTreeMap;

use crate::{empirical_constraints::BlockCell, expression::AlgebraicReference};

/// Maps BlockCells to their corresponding AlgebraicReferences.
pub struct BlockCellAlgebraicReferenceMapper {
    block_cell_to_algebraic_reference: BTreeMap<BlockCell, AlgebraicReference>,
}

impl BlockCellAlgebraicReferenceMapper {
    /// Creates a new BlockCellAlgebraicReferenceMapper.
    /// Arguments:
    /// - `subs`: A mapping from instruction index and column index to polynomial IDs.
    ///   This would typically come from a `ColumnAllocator`.
    /// - `columns`: An iterator over the algebraic references for the columns in the block.
    pub fn new(subs: &[Vec<u64>], columns: impl Iterator<Item = AlgebraicReference>) -> Self {
        let poly_id_to_block_cell = subs
            .iter()
            .enumerate()
            .flat_map(|(instr_index, subs)| {
                subs.iter().enumerate().map(move |(col_index, &poly_id)| {
                    (poly_id, BlockCell::new(instr_index, col_index))
                })
            })
            .collect::<BTreeMap<_, _>>();
        let block_cell_to_algebraic_reference = columns
            .map(|r| (*poly_id_to_block_cell.get(&r.id).unwrap(), r.clone()))
            .collect::<BTreeMap<_, _>>();
        Self {
            block_cell_to_algebraic_reference,
        }
    }

    pub fn get_algebraic_reference(&self, block_cell: &BlockCell) -> Option<&AlgebraicReference> {
        self.block_cell_to_algebraic_reference.get(block_cell)
    }

    pub fn has_block_cell(&self, block_cell: &BlockCell) -> bool {
        self.block_cell_to_algebraic_reference
            .contains_key(block_cell)
    }
}
