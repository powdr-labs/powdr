use std::collections::{BTreeMap, HashMap};

use super::bootloader::{
    BYTES_PER_WORD, N_LEAVES_LOG, WORDS_PER_PAGE as WORDS_PER_PAGE_BOOTLOADER,
};

use powdr_number::{BabyBearField, FieldElement, GoldilocksField, LargeInt};
use powdr_riscv_executor::executor_16::poseidon_bb::poseidon_bb;
use powdr_riscv_executor::executor_32::poseidon_gl::poseidon_gl;

pub trait MerkleTypes {
    type Fe: FieldElement;
    const FE_PER_WORD: usize;
    type Page;
    type Hash;
    fn update_page(page: &mut Self::Page, idx: usize, word: u32);
    fn hash_page(page: &Self::Page) -> Self::Hash;
    fn hash_two(a: &Self::Hash, b: &Self::Hash) -> Self::Hash;
    fn zero_hash() -> Self::Hash;
    fn zero_page() -> Self::Page;
    // iterate over a hash value as machine words (should be
    // bootloader::WORDS_PER_HASH!), in their field element representation (FE_PER_WORD)
    fn iter_hash_as_fe(h: &Self::Hash) -> impl Iterator<Item = Self::Fe>;
    // iterate over the page words, in their field element representation
    fn iter_page_as_fe(p: &Self::Page) -> impl Iterator<Item = Self::Fe>;
    // iterate over a word value in its field element representation
    fn iter_word_as_fe(w: u32) -> impl Iterator<Item = Self::Fe>;
}

pub fn gl_split_fe(v: &GoldilocksField) -> [GoldilocksField; 2] {
    let v = v.to_integer().try_into_u64().unwrap();
    [((v & 0xffffffff) as u32).into(), ((v >> 32) as u32).into()]
}

impl MerkleTypes for GoldilocksField {
    type Fe = GoldilocksField;
    const FE_PER_WORD: usize = 1;
    type Page = [Self::Fe; WORDS_PER_PAGE];
    type Hash = [Self::Fe; 4];

    fn update_page(page: &mut Self::Page, idx: usize, word: u32) {
        page[idx] = GoldilocksField::from(word);
    }

    fn hash_page(page: &Self::Page) -> Self::Hash {
        let mut hash = [0.into(); 4];
        for chunk in page.chunks_exact(4) {
            hash = Self::hash_two(&hash, chunk.try_into().unwrap());
        }
        hash
    }

    fn hash_two(a: &Self::Hash, b: &Self::Hash) -> Self::Hash {
        let mut buffer = [0.into(); 12];
        buffer[..4].copy_from_slice(a);
        buffer[4..8].copy_from_slice(b);
        poseidon_gl(&buffer)
    }

    fn zero_hash() -> Self::Hash {
        [0.into(); 4]
    }

    fn zero_page() -> Self::Page {
        [0.into(); WORDS_PER_PAGE]
    }

    fn iter_hash_as_fe(h: &Self::Hash) -> impl Iterator<Item = Self::Fe> {
        h.iter().flat_map(|f| gl_split_fe(f).into_iter())
    }

    fn iter_page_as_fe(p: &Self::Page) -> impl Iterator<Item = Self::Fe> {
        p.iter().copied()
    }

    fn iter_word_as_fe(w: u32) -> impl Iterator<Item = Self::Fe> {
        std::iter::once(w.into())
    }
}

pub fn bb_split_word(v: u32) -> (BabyBearField, BabyBearField) {
    ((v & 0xffff).into(), (v >> 16).into())
}

pub fn bb_split_fe(v: &BabyBearField) -> [BabyBearField; 2] {
    let v = v.to_integer().try_into_u32().unwrap();
    [(v >> 16).into(), (v & 0xffff).into()]
}

impl MerkleTypes for BabyBearField {
    type Fe = BabyBearField;
    const FE_PER_WORD: usize = 2;
    type Page = [Self::Fe; 2 * WORDS_PER_PAGE];
    type Hash = [Self::Fe; 8];

    fn update_page(page: &mut Self::Page, idx: usize, word: u32) {
        let (hi, lo) = bb_split_word(word);
        // TODO: check proper endianess here!
        page[idx] = hi;
        page[idx + 1] = lo;
    }

    fn hash_page(page: &Self::Page) -> Self::Hash {
        let mut hash = [0.into(); 8];
        for chunk in page.chunks_exact(8) {
            hash = Self::hash_two(&hash, chunk.try_into().unwrap());
        }
        hash
    }

    fn hash_two(a: &Self::Hash, b: &Self::Hash) -> Self::Hash {
        let mut buffer = [0.into(); 16];
        buffer[..8].copy_from_slice(a);
        buffer[8..16].copy_from_slice(b);
        poseidon_bb(&buffer)
    }

    fn zero_hash() -> Self::Hash {
        [0.into(); 8]
    }

    fn zero_page() -> Self::Page {
        [0.into(); 2 * WORDS_PER_PAGE]
    }

    fn iter_hash_as_fe(h: &Self::Hash) -> impl Iterator<Item = Self::Fe> {
        h.iter().flat_map(|f| bb_split_fe(f).into_iter())
    }

    fn iter_page_as_fe(p: &Self::Page) -> impl Iterator<Item = Self::Fe> {
        p.iter().copied()
    }

    fn iter_word_as_fe(v: u32) -> impl Iterator<Item = Self::Fe> {
        let (hi, lo) = bb_split_word(v);
        // TODO: check proper endianess here!
        [hi, lo].into_iter()
    }
}

const N_LEVELS_DEFAULT: usize = N_LEAVES_LOG + 1;
const N_LEVELS: usize = N_LEVELS_DEFAULT;
const WORDS_PER_PAGE: usize = WORDS_PER_PAGE_BOOTLOADER;

/// A Merkle tree of memory pages.
pub struct MerkleTree<M: MerkleTypes> {
    /// Hashes of non-default nodes of the Merkle tree.
    ///
    /// The key is the tuple (level, index), where level is the tree level, and
    /// index is the index of the node in that level.
    ///
    /// Level 0 is the tree root, and only contains node index 0, the root_hash.
    ///
    /// Level N_LEVELS - 1 is the last level, and contains up to 2**(N_LEVELS-1)
    /// nodes with the hash of the non-zero leaves. These hashes should be
    /// updated whenever the data is updated.
    ///
    /// If a node is not present in the map, it is assumed to be the default.
    hashes: HashMap<(usize, usize), M::Hash>,
    /// Memory pages, numbered sequentially.
    data: HashMap<usize, M::Page>,

    /// The default hash for each level of the tree, when all leaves below are
    /// zeroed.
    default_hashes: [M::Hash; N_LEVELS],

    /// A zeroed page, used as a default value for missing pages.
    zero_page: M::Page,
}

/// Takes the ordered list of node indices in one level, and updates the list
/// in-place with the indices of the parent nodes in the level above.
fn level_up(ordered_indices: &mut Vec<usize>) {
    // I tried implementing this as a single dedup_by_key() call, but it doesn't
    // invoke the closure in case of a single element, so it is not updated.
    for i in ordered_indices.iter_mut() {
        *i >>= 1;
    }
    ordered_indices.dedup();
}

impl<M: MerkleTypes> MerkleTree<M> {
    /// Build a new Merkle tree starting from an all-zero memory.
    pub fn new() -> Self {
        Self {
            hashes: HashMap::new(),
            data: HashMap::new(),
            default_hashes: Self::default_hashes_per_level(),
            zero_page: M::zero_page(),
        }
    }

    /// Function update() relies on the result being sorted by page_index, so we
    /// use a BTreeMap here.
    pub fn organize_updates_by_page(
        &self,
        updates: impl Iterator<Item = (u32, u32)>,
    ) -> BTreeMap<usize, Vec<(usize, u32)>> {
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
        updates_by_page
    }

    /// Applies updates, given an iterator of (memory address, value) pairs.
    /// Memory addresses are assumed to be word-aligned.
    pub fn update(&mut self, updates: impl Iterator<Item = (u32, u32)>) {
        let mut updated_indices = Vec::new();

        // Update all leaves first:
        for (page_index, updates) in self.organize_updates_by_page(updates) {
            self.update_page_impl(page_index, &updates);
            self.update_leaf_hash(page_index);
            // Insert page index in ascending order.
            updated_indices.push(page_index);
        }

        // Then update all inner hashes, per level:
        for level in (0..(N_LEVELS - 1)).rev() {
            level_up(&mut updated_indices);
            for &index in &updated_indices {
                self.update_inner_hash(level, index);
            }
        }
    }

    /// Applies updates to a single page, given an iterator of (word index, value) pairs.
    /// Word indices addresses are assumed to be word-aligned.
    pub fn update_page(&mut self, page_index: usize, updates: &[(usize, u32)]) {
        self.update_page_impl(page_index, updates);
        self.update_hashes(page_index)
    }

    fn update_page_impl(&mut self, page_index: usize, updates: &[(usize, u32)]) {
        let page = &mut self
            .data
            .entry(page_index)
            .or_insert_with(|| M::zero_page());
        for (index, value) in updates {
            M::update_page(page, *index, *value);
        }
    }

    fn get_hash(&self, level: usize, index: usize) -> &M::Hash {
        assert!(level < N_LEVELS);
        assert!(index < (1 << level));
        self.hashes
            .get(&(level, index))
            .unwrap_or(&self.default_hashes[level])
    }

    /// Updates the hashes of a page and all its ancestors.
    fn update_hashes(&mut self, page_index: usize) {
        self.update_leaf_hash(page_index);
        for (level, index) in self.iter_path(page_index).skip(1) {
            self.update_inner_hash(level, index);
        }
    }

    fn update_leaf_hash(&mut self, page_index: usize) {
        self.hashes.insert(
            (N_LEVELS - 1, page_index),
            M::hash_page(&self.data[&page_index]),
        );
    }

    fn update_inner_hash(&mut self, level: usize, index: usize) {
        let new_hash = M::hash_two(
            self.get_hash(level + 1, index * 2),
            self.get_hash(level + 1, index * 2 + 1),
        );
        self.hashes.insert((level, index), new_hash);
    }

    /// Returns the root hash of the Merkle tree.
    pub fn root_hash(&self) -> &M::Hash {
        self.get_hash(0, 0)
    }

    /// Returns the data and Merkle proof for a given page.
    pub fn get(&self, page_index: usize) -> (&M::Page, &M::Hash, Vec<&M::Hash>) {
        let mut proof = vec![];
        for (level, index) in self.iter_path(page_index).take(N_LEVELS - 1) {
            let sibling_index = index ^ 1;
            proof.push(self.get_hash(level, sibling_index));
        }
        assert_eq!(proof.len(), N_LEVELS - 1);

        let page_hash = self.get_hash(N_LEVELS - 1, page_index);

        let page_data = self.data.get(&page_index).unwrap_or(&self.zero_page);

        (page_data, page_hash, proof)
    }

    /// Yields (level, index) pairs for the path from the given page to the root.
    fn iter_path(&self, page_index: usize) -> impl Iterator<Item = (usize, usize)> {
        (0..N_LEVELS).rev().map(move |level| {
            let index = page_index >> (N_LEVELS - level - 1);
            (level, index)
        })
    }

    fn default_hashes_per_level() -> [M::Hash; N_LEVELS] {
        assert!(usize::BITS >= N_LEVELS as u32);
        let zero_page = M::zero_page();

        let mut generator = std::iter::successors(Some(M::hash_page(&zero_page)), |hash| {
            Some(M::hash_two(hash, hash))
        });
        let mut result = std::array::from_fn(|_| generator.next().unwrap());
        result.reverse();

        result
    }
}

impl<M: MerkleTypes> Default for MerkleTree<M> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod test {
    // use powdr_number::GoldilocksField;

    // use super::*;

    // fn hash_page<T: FieldElement>(page: &[u64; 8]) -> M::Hash {
    //     // Convert to field elements
    //     let page: [T; 8] = page
    //         .iter()
    //         .map(|&x| T::from(x))
    //         .collect::<Vec<_>>()
    //         .try_into()
    //         .unwrap();

    //     // Linearly hash page
    //     let hash = hash_cap0(&[T::zero(); 4], &page[..4].try_into().unwrap());
    //     hash_cap0(&hash, &page[4..].try_into().unwrap())
    // }

    // fn root_hash<T: FieldElement>(pages: &[[u64; 8]; 4]) -> M::Hash {
    //     let page_hashes = pages.iter().map(|page| hash_page(page)).collect::<Vec<_>>();

    //     let hash_1_0 = hash_cap0(&page_hashes[0], &page_hashes[1]);
    //     let hash_1_1 = hash_cap0(&page_hashes[2], &page_hashes[3]);
    //     hash_cap0(&hash_1_0, &hash_1_1)
    // }

    // #[test]
    // fn update() {
    //     let mut tree = MerkleTree::<GoldilocksField, 3, 8>::new();
    //     let mut data = [[0; 8]; 4];

    //     // Update page 0
    //     data[0][4] = 1;
    //     tree.update([(4 * 4, 1)].into_iter());
    //     let expected_root_hash = root_hash::<GoldilocksField>(&data);
    //     assert_eq!(tree.root_hash(), &expected_root_hash);

    //     // Update page 1
    //     data[1][3] = 2;
    //     tree.update([((8 + 3) * 4, 2)].into_iter());
    //     let expected_root_hash = root_hash::<GoldilocksField>(&data);
    //     assert_eq!(tree.root_hash(), &expected_root_hash);

    //     // Update page 2
    //     data[2][7] = 3;
    //     tree.update([((2 * 8 + 7) * 4, 3)].into_iter());
    //     let expected_root_hash = root_hash::<GoldilocksField>(&data);
    //     assert_eq!(tree.root_hash(), &expected_root_hash);

    //     // Update page 3
    //     data[3][6] = 4;
    //     tree.update([((3 * 8 + 6) * 4, 4)].into_iter());
    //     let expected_root_hash = root_hash::<GoldilocksField>(&data);
    //     assert_eq!(tree.root_hash(), &expected_root_hash);

    //     // Update page 0, again
    //     data[0][3] = 5;
    //     tree.update([(4 * 3, 5)].into_iter());
    //     let expected_root_hash = root_hash::<GoldilocksField>(&data);
    //     assert_eq!(tree.root_hash(), &expected_root_hash);

    //     // Update all at once
    //     let mut tree = MerkleTree::<GoldilocksField, 3, 8>::new();
    //     tree.update(
    //         [
    //             (4 * 4, 1),
    //             ((8 + 3) * 4, 2),
    //             ((2 * 8 + 7) * 4, 3),
    //             ((3 * 8 + 6) * 4, 4),
    //             (4 * 3, 5),
    //         ]
    //         .into_iter(),
    //     );
    //     assert_eq!(tree.root_hash(), &expected_root_hash);
    // }

    // #[test]
    // fn get() {
    //     let g = GoldilocksField::from;
    //     let mut tree = MerkleTree::<GoldilocksField, 3, 8>::new();
    //     tree.update(
    //         [
    //             (4 * 4, 1),
    //             ((8 + 3) * 4, 2),
    //             ((2 * 8 + 7) * 4, 3),
    //             ((3 * 8 + 6) * 4, 4),
    //             (4 * 3, 5),
    //         ]
    //         .into_iter(),
    //     );
    //     let root_hash = tree.root_hash();

    //     // Get page 0
    //     let (page, page_hash, proof) = tree.get(0);
    //     let expected_page = [g(0), g(0), g(0), g(5), g(1), g(0), g(0), g(0)];
    //     assert_eq!(page, &expected_page);
    //     assert_eq!(page_hash, &hash_page(&[0, 0, 0, 5, 1, 0, 0, 0]));

    //     // Verify Merkle proof
    //     assert_eq!(proof.len(), 2);
    //     let computed_hash = hash_cap0(page_hash, proof[0]);
    //     let computed_hash = hash_cap0(&computed_hash, proof[1]);
    //     assert_eq!(computed_hash, *root_hash);

    //     // Get page 1
    //     let (page, page_hash, proof) = tree.get(1);
    //     let expected_page = [g(0), g(0), g(0), g(2), g(0), g(0), g(0), g(0)];
    //     assert_eq!(page, &expected_page);
    //     assert_eq!(page_hash, &hash_page(&[0, 0, 0, 2, 0, 0, 0, 0]));

    //     // Verify Merkle proof
    //     assert_eq!(proof.len(), 2);
    //     let computed_hash = hash_cap0(proof[0], page_hash);
    //     let computed_hash = hash_cap0(&computed_hash, proof[1]);
    //     assert_eq!(computed_hash, *root_hash);

    //     // Get page 2
    //     let (page, page_hash, proof) = tree.get(2);
    //     let expected_page = [g(0), g(0), g(0), g(0), g(0), g(0), g(0), g(3)];
    //     assert_eq!(page, &expected_page);
    //     assert_eq!(page_hash, &hash_page(&[0, 0, 0, 0, 0, 0, 0, 3]));

    //     // Verify Merkle proof
    //     assert_eq!(proof.len(), 2);
    //     let computed_hash = hash_cap0(page_hash, proof[0]);
    //     let computed_hash = hash_cap0(proof[1], &computed_hash);
    //     assert_eq!(computed_hash, *root_hash);

    //     // Get page 3
    //     let (page, page_hash, proof) = tree.get(3);
    //     let expected_page = [g(0), g(0), g(0), g(0), g(0), g(0), g(4), g(0)];
    //     assert_eq!(page, &expected_page);
    //     assert_eq!(page_hash, &hash_page(&[0, 0, 0, 0, 0, 0, 4, 0]));

    //     // Verify Merkle proof
    //     assert_eq!(proof.len(), 2);
    //     let computed_hash = hash_cap0(proof[0], page_hash);
    //     let computed_hash = hash_cap0(proof[1], &computed_hash);
    //     assert_eq!(computed_hash, *root_hash);
    // }
}
