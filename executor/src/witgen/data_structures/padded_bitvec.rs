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
        let words_per_row = bits_per_row.div_ceil(32);
        Self {
            data: Vec::new(),
            bits_per_row,
            words_per_row,
            rows: 0,
            bits_in_last_row: bits_per_row,
        }
    }

    pub fn truncate_to_rows(&mut self, len: usize) {
        assert!(len <= self.rows);
        if len < self.rows {
            self.data.truncate(len * self.words_per_row);
            self.bits_in_last_row = self.bits_per_row;
            self.rows = len;
        }
    }

    pub fn clear(&mut self) {
        self.data.clear();
        self.rows = 0;
        self.bits_in_last_row = self.bits_per_row
    }

    pub fn reserve_rows(&mut self, count: usize) {
        self.data.reserve(count * self.words_per_row);
    }

    /// Append a single bit.
    pub fn push(&mut self, value: bool) {
        if self.bits_in_last_row == self.bits_per_row {
            self.data.push(value as u32);
            self.rows += 1;
            self.bits_in_last_row = 1;
        } else {
            if self.bits_in_last_row % 32 == 0 {
                self.data.push(value as u32);
            } else if value {
                let last_word = self.data.last_mut().unwrap();
                let bit_in_last_word = self.bits_in_last_row % 32;
                *last_word |= 1 << bit_in_last_word;
            }
            self.bits_in_last_row += 1;
        }
    }

    /// Append a number of new empty rows.
    pub fn append_empty_rows(&mut self, count: usize) {
        assert!(self.bits_in_last_row == self.bits_per_row);
        self.data
            .resize(self.data.len() + count * self.words_per_row, 0);
        self.rows += count;
    }

    pub fn get(&self, row: usize, col: u64) -> bool {
        let (word_index, bit_mask) = self.to_word_and_bit_mask(row, col);
        let word = &self.data[word_index];
        (word & bit_mask) != 0
    }

    pub fn set(&mut self, row: usize, col: u64, value: bool) {
        let (word_index, bit_mask) = self.to_word_and_bit_mask(row, col);
        let word = &mut self.data[word_index];
        if value {
            *word |= bit_mask;
        } else {
            *word &= !bit_mask;
        }
    }

    fn to_word_and_bit_mask(&self, row: usize, col: u64) -> (usize, u32) {
        if row >= self.rows || (row + 1 == self.rows && col >= self.bits_in_last_row as u64) {
            panic!("Out of bounds");
        }
        let word_index = row * self.words_per_row + (col / 32) as usize;
        let bit_mask = 1 << (col % 32);
        (word_index, bit_mask)
    }

    pub fn as_mut_slice(&mut self) -> &mut [u32] {
        &mut self.data
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_push() {
        let mut vec = PaddedBitVec::new(38);
        let v = 0x20500600a0u64;
        for i in 0..38 {
            vec.push((v & (1 << i)) != 0);
        }
        assert_eq!(vec.data, vec![0x500600a0, 0x20]);

        assert_eq!(v, (0..38).map(|i| (vec.get(0, i) as u64) << i).sum::<u64>());
    }

    #[test]
    #[should_panic = "Out of bounds"]
    fn test_out_of_bouts() {
        let mut vec = PaddedBitVec::new(38);
        let v = 0x20500600a0u64;
        for i in 0..38 {
            vec.push((v & (1 << i)) != 0);
        }
        assert!(vec.get(0, 38));
    }

    #[test]
    fn test_multirow() {
        let mut vec = PaddedBitVec::new(3);
        vec.push(true);
        vec.push(false);
        vec.push(true);
        vec.push(true);
        vec.push(false);
        vec.push(false);
        vec.push(true);
        vec.push(false);
        assert_eq!(vec.data, vec![5, 1, 1]);
    }
}
