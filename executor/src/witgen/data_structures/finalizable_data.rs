use std::{
    collections::HashSet,
    ops::{Index, IndexMut},
};

use bit_vec::BitVec;
use powdr_ast::analyzed::RawPolyID as PolyID;
use powdr_number::FieldElement;

use crate::witgen::rows::Row;

/// A row entry in [FinalizableData].
#[derive(Clone)]
enum Entry<'a, T: FieldElement> {
    /// The row is still in progress, and range constraints are still available.
    InProgress(Row<'a, T>),
    /// A finalized row, represented as a vector of values (corresponding to the columns
    /// stored in [FinalizableData]) and a bit vector indicating which cells are known.
    /// The value of unknown cells should be ignored.
    Finalized(Vec<T>, BitVec),
}

/// A data structure that stores rows of a witness table, and behaves much like a `Vec<Row<T>>`.
/// However, it also allows to finalize rows, which means that memory for things like range
/// constraints is freed. The information which cells are known is preserved, though.
/// Once a row has been finalized, any operation trying to access it again will fail at runtime.
/// [FinalizableData::take_transposed] can be used to access the final cells.
#[derive(Clone)]
pub struct FinalizableData<'a, T: FieldElement> {
    /// The list of rows (either in progress or finalized)
    data: Vec<Entry<'a, T>>,
    /// The list of column IDs (in sorted order), used to index finalized rows.
    column_ids: Vec<PolyID>,
}

impl<'a, T: FieldElement> FinalizableData<'a, T> {
    pub fn new(column_ids: &HashSet<PolyID>) -> Self {
        Self::with_initial_rows_in_progress(column_ids, [].into_iter())
    }

    pub fn with_initial_rows_in_progress(
        column_ids: &HashSet<PolyID>,
        rows: impl Iterator<Item = Row<'a, T>>,
    ) -> Self {
        let mut column_ids = column_ids.iter().cloned().collect::<Vec<_>>();
        column_ids.sort();
        let data = rows.map(Entry::InProgress).collect::<Vec<_>>();
        Self { data, column_ids }
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn push(&mut self, row: Row<'a, T>) {
        self.data.push(Entry::InProgress(row));
    }

    pub fn pop(&mut self) -> Option<Row<'a, T>> {
        match self.data.pop() {
            Some(Entry::InProgress(row)) => Some(row),
            Some(Entry::Finalized(_, _)) => panic!("Row already finalized."),
            None => None,
        }
    }

    pub fn extend(&mut self, other: Self) {
        self.data.extend(other.data);
    }

    pub fn remove(&mut self, i: usize) -> Row<'a, T> {
        match self.data.remove(i) {
            Entry::InProgress(row) => row,
            Entry::Finalized(_, _) => panic!("Row {i} already finalized."),
        }
    }

    pub fn truncate(&mut self, len: usize) {
        self.data.truncate(len);
    }

    pub fn get_mut(&mut self, i: usize) -> Option<&mut Row<'a, T>> {
        match &mut self.data[i] {
            Entry::InProgress(row) => Some(row),
            Entry::Finalized(_, _) => panic!("Row {i} already finalized."),
        }
    }

    pub fn last(&self) -> Option<&Row<'a, T>> {
        match self.data.last() {
            Some(Entry::InProgress(row)) => Some(row),
            Some(Entry::Finalized(_, _)) => panic!("Last row already finalized."),
            None => None,
        }
    }

    pub fn mutable_row_pair(&mut self, i: usize) -> (&mut Row<'a, T>, &mut Row<'a, T>) {
        let (before, after) = self.data.split_at_mut(i + 1);
        let current = before.last_mut().unwrap();
        let next = after.first_mut().unwrap();
        match (current, next) {
            (Entry::InProgress(current), Entry::InProgress(next)) => (current, next),
            _ => panic!("Row {} or {} (or both) already finalized.", i, i + 1),
        }
    }

    pub fn finalize(&mut self, i: usize) -> bool {
        if let Entry::InProgress(row) = &self.data[i] {
            let (values, known_cells) = self
                .column_ids
                .iter()
                .map(|c| (row[c].value.unwrap_or_default(), row[c].value.is_known()))
                .unzip();
            self.data[i] = Entry::Finalized(values, known_cells);
            true
        } else {
            false
        }
    }

    pub fn finalize_range(&mut self, range: impl Iterator<Item = usize>) {
        for i in range {
            self.finalize(i);
        }
    }

    /// Takes all data out of the [FinalizableData] and returns it as a list of columns.
    /// Columns are represented as a tuple of:
    /// - A list of values
    /// - A bit vector indicating which cells are known. Values of unknown cells should be ignored.
    pub fn take_transposed(&mut self) -> impl Iterator<Item = (PolyID, (Vec<T>, BitVec))> {
        log::debug!(
            "Transposing {} rows with {} columns...",
            self.data.len(),
            self.column_ids.len()
        );
        log::debug!("Finalizing remaining rows...");
        let mut counter = 0;
        for i in 0..self.data.len() {
            if self.finalize(i) {
                counter += 1;
            }
        }
        log::debug!("Needed to finalize {} / {} rows.", counter, self.data.len());

        // Store transposed columns in vectors for performance reasons
        let mut columns = vec![Vec::with_capacity(self.data.len()); self.column_ids.len()];
        let mut known_cells_col =
            vec![BitVec::with_capacity(self.data.len()); self.column_ids.len()];
        for row in std::mem::take(&mut self.data) {
            match row {
                Entry::InProgress(_) => unreachable!(),
                Entry::Finalized(row, known_cells) => {
                    for (col_index, (value, is_known)) in
                        row.into_iter().zip(known_cells).enumerate()
                    {
                        known_cells_col[col_index].push(is_known);
                        columns[col_index].push(value);
                    }
                }
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

impl<'a, T: FieldElement> Index<usize> for FinalizableData<'a, T> {
    type Output = Row<'a, T>;

    fn index(&self, index: usize) -> &Self::Output {
        match &self.data[index] {
            Entry::InProgress(row) => row,
            Entry::Finalized(_, _) => panic!("Row {index} already finalized."),
        }
    }
}

impl<'a, T: FieldElement> IndexMut<usize> for FinalizableData<'a, T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        match &mut self.data[index] {
            Entry::InProgress(row) => row,
            Entry::Finalized(_, _) => panic!("Row {index} already finalized."),
        }
    }
}
