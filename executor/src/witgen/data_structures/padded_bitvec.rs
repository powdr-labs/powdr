/// A bit vector tuned to be used as flags for a trace matrix.
/// The benefit of this bit vector is that each row starts
/// at a new word, so the access to the bit vector is uniform
/// for each row and thus setting the same bits in each row
/// can be optimized to setting a full word.
#[derive(Clone)]
pub struct PaddedBitVec {
    data: Vec<u32>,
    bits_per_row: usize,
    words_per_row: usize,
    rows: usize,
    bits_in_last_row: usize,
}

impl PaddedBitVec {
    pub fn new(bits_per_row: usize) -> Self {
        let words_per_row = (bits_per_row + 31) / 32;
        Self {
            data: Vec::new(),
            bits_per_row,
            words_per_row,
            rows: 0,
            bits_in_last_row: 0,
        }
    }

    pub fn truncate_to_rows(&mut self, len: usize) {
        assert!(len <= self.rows);
        self.data.truncate(len * self.words_per_row);
        self.rows = len;
        self.bits_in_last_row = 0;
    }

    pub fn clear(&mut self) {
        self.data.clear();
        self.rows = 0;
        self.bits_in_last_row = 0;
    }

    pub fn reserve_rows(&mut self, count: usize) {
        self.data.reserve(count * self.words_per_row);
    }

    /// Append a single bit.
    pub fn push(&mut self, value: bool) {
        if self.bits_in_last_row == 0 {
            self.data.push(value as u32);
            self.rows += 1;
        } else {
            let last_word = self.data.last_mut().unwrap();
            if value {
                *last_word |= 1 << (self.bits_in_last_row - 1);
            }
        }
        self.bits_in_last_row = (self.bits_in_last_row + 1) % self.bits_per_row;
    }

    /// Append a number of ne empty rows.
    pub fn append_empty_rows(&mut self, count: usize) {
        assert!(self.bits_in_last_row == 0);
        self.data
            .resize(self.data.len() + count * self.words_per_row, 0);
        self.rows += count;
    }

    pub fn get(&self, row: usize, col: u64) -> bool {
        let word = &self.data[row * self.words_per_row + (col / 32) as usize];
        (word & (1 << (col % 32))) != 0
    }

    pub fn set(&mut self, row: usize, col: u64, value: bool) {
        let word = &mut self.data[row * self.words_per_row + (col / 32) as usize];
        if value {
            *word |= 1 << (col % 32);
        } else {
            *word &= !(1 << (col % 32));
        }
    }
}
