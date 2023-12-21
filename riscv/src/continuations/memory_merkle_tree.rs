use std::collections::BTreeMap;

use crate::bootloader::{
    BYTES_PER_WORD, MERKLE_TREE_DEPTH as MERKLE_TREE_DEPTH_BOOTLOADER,
    WORDS_PER_PAGE as WORDS_PER_PAGE_BOOTLOADER,
};
use number::FieldElement;
use riscv_executor::poseidon_gl::poseidon_gl;

/// A Merkle tree of memory pages.
pub struct MerkleTree<
    T: FieldElement,
    const MERKLE_TREE_DEPTH: usize = MERKLE_TREE_DEPTH_BOOTLOADER,
    const WORDS_PER_PAGE: usize = WORDS_PER_PAGE_BOOTLOADER,
> {
    /// Hashes of all nodes of the Merkle tree. The vector has MERKLE_TREE_DEPTH + 1 entries
    /// where hashes[0] is [root_hash], hashes[1] is [level_1_hash_0, level_1_hash_1], etc.
    /// The last entry hashes[MERKLE_TREE_DEPTH] is the hash of the leaves.
    /// These hashes should be updated whenever the data is updated.
    hashes: Vec<Vec<[T; 4]>>,
    /// Memory pages, contiguous.
    data: Vec<[T; WORDS_PER_PAGE]>,
}

/// Computes the Poseidon hash of two 4-field-element inputs, using a capacity of 0.
fn hash_cap0<T: FieldElement>(data1: &[T; 4], data2: &[T; 4]) -> [T; 4] {
    let mut buffer = [T::zero(); 12];
    buffer[..4].copy_from_slice(data1);
    buffer[4..8].copy_from_slice(data2);
    poseidon_gl(&buffer)
}

impl<T: FieldElement, const MERKLE_TREE_DEPTH: usize, const WORDS_PER_PAGE: usize>
    MerkleTree<T, MERKLE_TREE_DEPTH, WORDS_PER_PAGE>
{
    /// Build a new Merkle tree starting from an all-zero memory.
    pub fn new() -> Self {
        let zero_page = [T::zero(); WORDS_PER_PAGE];
        let mut hash = Self::hash_page(&zero_page);
        let mut hashes = vec![vec![]; MERKLE_TREE_DEPTH + 1];

        assert!(usize::BITS >= MERKLE_TREE_DEPTH as u32);
        for level in (0..=MERKLE_TREE_DEPTH).rev() {
            hashes[level] = vec![hash; 1 << level];
            hash = hash_cap0(&hash, &hash);
        }

        assert_eq!(hashes[0].len(), 1);

        let data = vec![zero_page; 1 << MERKLE_TREE_DEPTH];

        Self { hashes, data }
    }

    /// Computes the linearly iterated hash of a single page
    fn hash_page(page: &[T; WORDS_PER_PAGE]) -> [T; 4] {
        let mut hash = [T::zero(); 4];
        for chunk in page.chunks_exact(4) {
            hash = hash_cap0(&hash, chunk.try_into().unwrap());
        }
        hash
    }

    /// Applies updates, given an iterator of (memory address, value) pairs.
    /// Memory addresses are assumed to be word-aligned.
    pub fn update(&mut self, updates: impl Iterator<Item = (u32, u32)>) {
        // Organize by page
        let mut updates_by_page: BTreeMap<usize, Vec<(usize, u32)>> = BTreeMap::new();
        for (addr, value) in updates {
            assert!(addr % BYTES_PER_WORD as u32 == 0);
            let word_index = addr as usize / BYTES_PER_WORD;
            let page_index = word_index / WORDS_PER_PAGE;
            let index_within_page = word_index - page_index * WORDS_PER_PAGE;
            updates_by_page
                .entry(page_index)
                .or_default()
                .push((index_within_page, value));
        }

        // Update each page
        for (page_index, updates) in updates_by_page {
            let page = &mut self.data[page_index];
            for (index, value) in updates {
                page[index] = T::from(value);
            }
            self.update_hashes(page_index)
        }
    }

    /// Updates the hashes of a page and all its ancestors.
    fn update_hashes(&mut self, page_index: usize) {
        self.hashes[MERKLE_TREE_DEPTH][page_index] = Self::hash_page(&self.data[page_index]);
        for (level, index) in self.iter_path(page_index).skip(1) {
            self.hashes[level][index] = hash_cap0(
                &self.hashes[level + 1][index * 2],
                &self.hashes[level + 1][index * 2 + 1],
            );
        }
    }

    #[allow(dead_code)]
    /// Returns the root hash of the Merkle tree.
    pub fn root_hash(&self) -> &[T; 4] {
        &self.hashes[0][0]
    }

    /// Returns the data and Merkle proof for a given page.
    pub fn get(&self, page_index: usize) -> (&[T; WORDS_PER_PAGE], Vec<&[T; 4]>) {
        let mut proof = vec![];
        for (level, index) in self.iter_path(page_index).take(MERKLE_TREE_DEPTH) {
            let sibling_index = index ^ 1;
            proof.push(&self.hashes[level][sibling_index]);
        }
        assert_eq!(proof.len(), MERKLE_TREE_DEPTH);

        (&self.data[page_index], proof)
    }

    /// Yields (level, index) pairs for the path from the given page to the root.
    fn iter_path(&self, page_index: usize) -> impl Iterator<Item = (usize, usize)> {
        (0..=MERKLE_TREE_DEPTH).rev().map(move |level| {
            let index = page_index >> (MERKLE_TREE_DEPTH - level);
            (level, index)
        })
    }
}

#[cfg(test)]
mod test {
    use number::GoldilocksField;

    use super::*;

    fn hash_page<T: FieldElement>(page: &[u64; 8]) -> [T; 4] {
        // Convert to field elements
        let page: [T; 8] = page
            .iter()
            .map(|&x| T::from(x))
            .collect::<Vec<_>>()
            .try_into()
            .unwrap();

        // Linearly hash page
        let hash = hash_cap0(&[T::zero(); 4], &page[..4].try_into().unwrap());
        hash_cap0(&hash, &page[4..].try_into().unwrap())
    }

    fn root_hash<T: FieldElement>(pages: &[[u64; 8]; 4]) -> [T; 4] {
        let page_hashes = pages.iter().map(|page| hash_page(page)).collect::<Vec<_>>();

        let hash_1_0 = hash_cap0(&page_hashes[0], &page_hashes[1]);
        let hash_1_1 = hash_cap0(&page_hashes[2], &page_hashes[3]);
        hash_cap0(&hash_1_0, &hash_1_1)
    }

    #[test]
    fn test_zero_root_hash() {
        let tree = MerkleTree::<GoldilocksField, 2, 8>::new();
        let data = [[0; 8]; 4];
        let expected_root_hash = root_hash::<GoldilocksField>(&data);
        assert_eq!(tree.root_hash(), &expected_root_hash);
    }

    #[test]
    fn test_update() {
        let mut tree = MerkleTree::<GoldilocksField, 2, 8>::new();
        let mut data = [[0; 8]; 4];

        // Update page 0
        data[0][4] = 1;
        tree.update([(4 * 4, 1)].into_iter());
        let expected_root_hash = root_hash::<GoldilocksField>(&data);
        assert_eq!(tree.root_hash(), &expected_root_hash);

        // Update page 1
        data[1][3] = 2;
        tree.update([((8 + 3) * 4, 2)].into_iter());
        let expected_root_hash = root_hash::<GoldilocksField>(&data);
        assert_eq!(tree.root_hash(), &expected_root_hash);

        // Update page 2
        data[2][7] = 3;
        tree.update([((2 * 8 + 7) * 4, 3)].into_iter());
        let expected_root_hash = root_hash::<GoldilocksField>(&data);
        assert_eq!(tree.root_hash(), &expected_root_hash);

        // Update page 3
        data[3][6] = 4;
        tree.update([((3 * 8 + 6) * 4, 4)].into_iter());
        let expected_root_hash = root_hash::<GoldilocksField>(&data);
        assert_eq!(tree.root_hash(), &expected_root_hash);

        // Update page 0, again
        data[0][3] = 5;
        tree.update([(4 * 3, 5)].into_iter());
        let expected_root_hash = root_hash::<GoldilocksField>(&data);
        assert_eq!(tree.root_hash(), &expected_root_hash);

        // Update all at once
        let mut tree = MerkleTree::<GoldilocksField, 2, 8>::new();
        tree.update(
            [
                (4 * 4, 1),
                ((8 + 3) * 4, 2),
                ((2 * 8 + 7) * 4, 3),
                ((3 * 8 + 6) * 4, 4),
                (4 * 3, 5),
            ]
            .into_iter(),
        );
        assert_eq!(tree.root_hash(), &expected_root_hash);
    }

    #[test]
    fn test_get() {
        let g = GoldilocksField::from;
        let mut tree = MerkleTree::<GoldilocksField, 2, 8>::new();
        tree.update(
            [
                (4 * 4, 1),
                ((8 + 3) * 4, 2),
                ((2 * 8 + 7) * 4, 3),
                ((3 * 8 + 6) * 4, 4),
                (4 * 3, 5),
            ]
            .into_iter(),
        );
        let root_hash = tree.root_hash();

        // Get page 0
        let (page, proof) = tree.get(0);
        let expected_page = [g(0), g(0), g(0), g(5), g(1), g(0), g(0), g(0)];
        assert_eq!(page, &expected_page);

        // Verify Merkle proof
        assert_eq!(proof.len(), 2);
        let computed_hash = hash_cap0(&hash_page(&[0, 0, 0, 5, 1, 0, 0, 0]), proof[0]);
        let computed_hash = hash_cap0(&computed_hash, proof[1]);
        assert_eq!(computed_hash, *root_hash);

        // Get page 1
        let (page, proof) = tree.get(1);
        let expected_page = [g(0), g(0), g(0), g(2), g(0), g(0), g(0), g(0)];
        assert_eq!(page, &expected_page);

        // Verify Merkle proof
        assert_eq!(proof.len(), 2);
        let computed_hash = hash_cap0(proof[0], &hash_page(&[0, 0, 0, 2, 0, 0, 0, 0]));
        let computed_hash = hash_cap0(&computed_hash, proof[1]);
        assert_eq!(computed_hash, *root_hash);

        // Get page 2
        let (page, proof) = tree.get(2);
        let expected_page = [g(0), g(0), g(0), g(0), g(0), g(0), g(0), g(3)];
        assert_eq!(page, &expected_page);

        // Verify Merkle proof
        assert_eq!(proof.len(), 2);
        let computed_hash = hash_cap0(&hash_page(&[0, 0, 0, 0, 0, 0, 0, 3]), proof[0]);
        let computed_hash = hash_cap0(proof[1], &computed_hash);
        assert_eq!(computed_hash, *root_hash);

        // Get page 3
        let (page, proof) = tree.get(3);
        let expected_page = [g(0), g(0), g(0), g(0), g(0), g(0), g(4), g(0)];
        assert_eq!(page, &expected_page);

        // Verify Merkle proof
        assert_eq!(proof.len(), 2);
        let computed_hash = hash_cap0(proof[0], &hash_page(&[0, 0, 0, 0, 0, 0, 4, 0]));
        let computed_hash = hash_cap0(proof[1], &computed_hash);
        assert_eq!(computed_hash, *root_hash);
    }
}
