use std::{
    collections::HashSet,
    ops::{Index, IndexMut},
};

use bit_vec::BitVec;
use itertools::Itertools;
use powdr_ast::analyzed::{PolyID, PolynomialType};
use powdr_number::FieldElement;

use crate::witgen::rows::Row;

/// Sequence of rows of field elements, stored in a compact form.
#[derive(Clone)]
struct CompactData<T: FieldElement> {
    /// The ID of the first column used in the table.
    first_column_id: u64,
    /// The length of a row in the table.
    column_count: usize,
    /// The cell values, stored in row-major order.
    data: Vec<T>,
    /// Bit vector of known cells, stored in row-major order.
    known_cells: BitVec,
}

impl<T: FieldElement> CompactData<T> {
    /// Creates a new empty compact data storage. The column IDs have to be sorted.
    fn new(column_ids: &[PolyID]) -> Self {
        let first_column_id = column_ids.first().map_or(0, |id| id.id);
        let column_count = column_ids.len();
        Self {
            first_column_id,
            column_count,
            data: Vec::new(),
            known_cells: BitVec::new(),
        }
    }

    #[inline]
    fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Returns the number of stored rows.
    #[inline]
    fn len(&self) -> usize {
        self.data.len() / self.column_count
    }

    /// Truncates the data to `len` rows.
    fn truncate(&mut self, len: usize) {
        self.data.truncate(len * self.column_count);
        self.known_cells.truncate(len * self.column_count);
    }

    fn clear(&mut self) {
        self.data.clear();
        self.known_cells.clear();
    }

    /// Appends a non-finalized row to the data, turning it into a finalized row.
    #[inline]
    fn push(&mut self, row: Row<T>) {
        self.data.reserve(self.data.len() + self.column_count);
        self.known_cells
            .reserve(self.known_cells.len() + self.column_count);
        for col_id in self.first_column_id..(self.first_column_id + self.column_count as u64) {
            if let Some(v) = row.value(&PolyID {
                id: col_id,
                ptype: PolynomialType::Committed,
            }) {
                self.data.push(v);
                self.known_cells.push(true);
            } else {
                self.data.push(T::zero());
                self.known_cells.push(false);
            }
        }
    }

    #[inline]
    fn try_remove_last_row(&mut self) -> bool {
        if self.data.len() < self.column_count {
            false
        } else {
            self.data.truncate(self.data.len() - self.column_count);
            self.known_cells
                .truncate(self.known_cells.len() - self.column_count);
            true
        }
    }

    #[inline]
    fn get(&self, row: usize, col: u64) -> (T, bool) {
        let col = col - self.first_column_id;
        let idx = row * self.column_count + col as usize;
        (self.data[idx], self.known_cells[idx])
    }
}

/// A data structure that stores witness data.
/// It allows to finalize rows, which means that those rows are then stored in a more
/// compact form. Information about range constraints on those rows is lost, but the
/// information which cells are known is preserved.
/// There is always a single contigous area of finalized rows and this area can only "grow"
/// towards higher row indices, i.e. an area at the beginning can only be finalized
/// if nothing has been finalized yet.
/// Once a row has been finalized, any operation trying to access it again will fail at runtime.
/// [FinalizableData::take_transposed] can be used to access the final cells.
/// This data structure is more efficient if the used column IDs are contiguous.
#[derive(Clone)]
pub struct FinalizableData<T: FieldElement> {
    /// The non-finalized rows before the finalized data.
    pre_finalized_data: Vec<Row<T>>,
    /// Finalized data stored in a compact form.
    finalized_data: CompactData<T>,
    /// The non-finalized rows after the finalized data.
    post_finalized_data: Vec<Row<T>>,
    /// The list of column IDs (in sorted order), used to index finalized rows.
    column_ids: Vec<PolyID>,
}

impl<T: FieldElement> FinalizableData<T> {
    pub fn new(column_ids: &HashSet<PolyID>) -> Self {
        Self::with_initial_rows_in_progress(column_ids, [].into_iter())
    }

    pub fn with_initial_rows_in_progress(
        column_ids: &HashSet<PolyID>,
        rows: impl Iterator<Item = Row<T>>,
    ) -> Self {
        let column_ids = column_ids.iter().cloned().sorted().collect::<Vec<_>>();
        Self {
            pre_finalized_data: rows.collect_vec(),
            finalized_data: CompactData::new(&column_ids),
            post_finalized_data: Vec::new(),
            column_ids,
        }
    }

    /// Returns the total number of rows, including non-finalized rows.
    pub fn len(&self) -> usize {
        self.pre_finalized_data.len() + self.finalized_data.len() + self.post_finalized_data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.pre_finalized_data.is_empty()
            && self.finalized_data.is_empty()
            && self.post_finalized_data.is_empty()
    }

    pub fn push(&mut self, row: Row<T>) {
        if self.finalized_data.is_empty() && self.post_finalized_data.is_empty() {
            self.pre_finalized_data.push(row);
        } else {
            self.post_finalized_data.push(row);
        }
    }

    pub fn pop(&mut self) -> Option<Row<T>> {
        if !self.post_finalized_data.is_empty() {
            self.post_finalized_data.pop()
        } else if !self.finalized_data.is_empty() {
            panic!("Row already finalized");
        } else {
            self.pre_finalized_data.pop()
        }
    }

    /// Removes the last row, even if it has been finalized.
    pub fn try_remove_last_row(&mut self) {
        if !self.post_finalized_data.is_empty() {
            self.post_finalized_data.pop();
        } else if !self.finalized_data.is_empty() {
            self.finalized_data.try_remove_last_row();
        } else {
            self.pre_finalized_data.pop();
        }
    }

    pub fn extend(&mut self, other: Self) {
        if self.finalized_data.is_empty() {
            self.pre_finalized_data
                .extend(self.post_finalized_data.drain(..));
            self.pre_finalized_data.extend(other.pre_finalized_data);
            self.finalized_data = other.finalized_data;
            self.post_finalized_data = other.post_finalized_data;
        } else if other.finalized_data.is_empty() {
            self.post_finalized_data.extend(other.pre_finalized_data);
            self.post_finalized_data.extend(other.post_finalized_data);
        } else if self.post_finalized_data.is_empty() && other.pre_finalized_data.is_empty() {
            self.finalized_data.data.extend(other.finalized_data.data);
            self.post_finalized_data = other.post_finalized_data;
        } else if other.pre_finalized_data.is_empty() {
            self.finalize_range(
                (self.pre_finalized_data.len() + self.finalized_data.len())..self.len(),
            );
            self.finalized_data.data.extend(other.finalized_data.data);
            self.post_finalized_data = other.post_finalized_data;
        } else {
            panic!(
                "Cannot extend. Please try to ensure that `other` does not contain finalized rows."
            );
        }
    }

    pub fn remove(&mut self, i: usize) -> Row<T> {
        if i < self.pre_finalized_data.len()
            && self.finalized_data.is_empty()
            && self.post_finalized_data.is_empty()
        {
            self.pre_finalized_data.remove(i)
        } else if i < self.pre_finalized_data.len() + self.finalized_data.len() {
            panic!("Row {i} already finalized.");
        } else {
            self.post_finalized_data
                .remove(i - self.pre_finalized_data.len() - self.finalized_data.len())
        }
    }

    pub fn truncate(&mut self, len: usize) {
        if len <= self.pre_finalized_data.len() {
            self.pre_finalized_data.truncate(len);
            self.finalized_data.clear();
            self.post_finalized_data.clear();
        } else if len <= self.pre_finalized_data.len() + self.finalized_data.len() {
            self.finalized_data
                .truncate(len - self.pre_finalized_data.len());
            self.post_finalized_data.clear();
        } else {
            self.post_finalized_data
                .truncate(len - self.pre_finalized_data.len() - self.finalized_data.len());
        }
    }

    pub fn get_mut(&mut self, i: usize) -> Option<&mut Row<T>> {
        if i < self.pre_finalized_data.len() {
            Some(&mut self.pre_finalized_data[i])
        } else if i < self.pre_finalized_data.len() + self.finalized_data.len() {
            panic!("Row {i} already finalized.");
        } else {
            self.post_finalized_data
                .get_mut(i - self.pre_finalized_data.len() - self.finalized_data.len())
        }
    }

    pub fn last(&self) -> Option<&Row<T>> {
        if !self.post_finalized_data.is_empty() {
            self.post_finalized_data.last()
        } else if !self.finalized_data.is_empty() {
            panic!("Row already finalized");
        } else {
            self.pre_finalized_data.last()
        }
    }

    pub fn mutable_row_pair(&mut self, i: usize) -> (&mut Row<T>, &mut Row<T>) {
        let (before, after) = if i + 1 < self.pre_finalized_data.len() {
            self.pre_finalized_data.split_at_mut(i + 1)
        } else if i < self.pre_finalized_data.len() + self.finalized_data.len() {
            panic!("Row {i} or {} (or both) already finalized.", i + 1);
        } else {
            self.post_finalized_data
                .split_at_mut(i - self.pre_finalized_data.len() - self.finalized_data.len() + 1)
        };
        let current = before.last_mut().unwrap();
        let next = after.first_mut().unwrap();
        (current, next)
    }

    pub fn finalize_range(&mut self, range: std::ops::Range<usize>) -> usize {
        if range.is_empty() {
            return 0;
        }
        if self.finalized_data.is_empty() {
            if !self.post_finalized_data.is_empty() {
                self.pre_finalized_data
                    .extend(std::mem::take(&mut self.post_finalized_data));
            }
            let start = std::cmp::min(range.start, self.pre_finalized_data.len());
            let end = std::cmp::min(range.end, self.pre_finalized_data.len());
            self.pre_finalized_data
                .drain(start..)
                .enumerate()
                .for_each(|(i, row)| {
                    if start + i < end {
                        self.finalized_data.push(row)
                    } else {
                        self.post_finalized_data.push(row)
                    }
                });
            end - start
        } else {
            // If we are asked to finalize the pre-finalized data, we just don't do it.
            let mut new_post_data = vec![];
            let mut counter = 0;
            let row_shift = self.pre_finalized_data.len() + self.finalized_data.len();
            std::mem::take(&mut self.post_finalized_data)
                .into_iter()
                .enumerate()
                .for_each(|(i, row)| {
                    if range.contains(&(i + row_shift)) {
                        self.finalized_data.push(row);
                        counter += 1;
                    } else {
                        new_post_data.push(row);
                    }
                });
            self.post_finalized_data = new_post_data;
            counter
        }
    }

    /// Takes all data out of the [FinalizableData] and returns it as a list of columns.
    /// Columns are represented as a tuple of:
    /// - A list of values
    /// - A bit vector indicating which cells are known. Values of unknown cells should be ignored.
    pub fn take_transposed(&mut self) -> impl Iterator<Item = (PolyID, (Vec<T>, BitVec))> {
        log::debug!(
            "Transposing {} rows with {} columns...",
            self.len(),
            self.column_ids.len()
        );
        log::debug!("Finalizing remaining rows...");
        let counter = self.finalize_range(
            (self.pre_finalized_data.len() + self.finalized_data.len())..self.len(),
        );
        log::debug!("Needed to finalize {} / {} rows.", counter, self.len());
        assert!(self.post_finalized_data.is_empty());

        // Store transposed columns in vectors for performance reasons
        let mut columns = vec![Vec::with_capacity(self.len()); self.column_ids.len()];
        let mut known_cells_col = vec![BitVec::with_capacity(self.len()); self.column_ids.len()];

        // There could still be pre-finalized data.
        for row in &self.pre_finalized_data {
            for (i, col_id) in self.column_ids.iter().enumerate() {
                if let Some(v) = row.value(col_id) {
                    columns[i].push(v);
                    known_cells_col[i].push(true);
                } else {
                    columns[i].push(T::zero());
                    known_cells_col[i].push(false);
                }
            }
        }

        for row in 0..self.finalized_data.len() {
            for (i, col_id) in self.column_ids.iter().enumerate() {
                let (v, known) = self.finalized_data.get(row, col_id.id);
                columns[i].push(v);
                known_cells_col[i].push(known);
            }
        }

        log::debug!("Done transposing.");

        // Pair columns with their IDs
        let column_ids = std::mem::take(&mut self.column_ids);
        columns.into_iter().zip(known_cells_col).enumerate().map(
            move |(col_index, (column, known_cells))| {
                (column_ids[col_index], (column, known_cells))
            },
        )
    }
}

impl<T: FieldElement> Index<usize> for FinalizableData<T> {
    type Output = Row<T>;

    fn index(&self, index: usize) -> &Self::Output {
        if index < self.pre_finalized_data.len() {
            &self.pre_finalized_data[index]
        } else if index < self.pre_finalized_data.len() + self.finalized_data.len() {
            panic!("Row {index} already finalized.");
        } else {
            &self.post_finalized_data
                [index - self.pre_finalized_data.len() - self.finalized_data.len()]
        }
    }
}

impl<T: FieldElement> IndexMut<usize> for FinalizableData<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        if index < self.pre_finalized_data.len() {
            &mut self.pre_finalized_data[index]
        } else if index < self.pre_finalized_data.len() + self.finalized_data.len() {
            panic!("Row {index} already finalized.");
        } else {
            &mut self.post_finalized_data
                [index - self.pre_finalized_data.len() - self.finalized_data.len()]
        }
    }
}
