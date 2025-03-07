use powdr_number::FieldElement;
use powdr_number::FieldSize;
use powdr_number::LargeInt;

use std::collections::BTreeSet;

use super::memory_merkle_tree::MerkleTree;

use powdr_number::KnownField;

use static_assertions::const_assert;

/// 32-Bit architecture -> 2^32 bytes of addressable memory
pub const MEMORY_SIZE_LOG: usize = 32;

/// Page size is 2KB
pub const PAGE_SIZE_BYTES_LOG: usize = 11;

/// 32-Bit architecture -> 4 bytes per word
pub const BYTES_PER_WORD: usize = 4;

use crate::code_gen::{REGISTER_MEMORY_NAMES, REGISTER_NAMES};
use crate::large_field;

// Derived constants
pub const WORDS_PER_PAGE: usize = (1 << (PAGE_SIZE_BYTES_LOG)) / BYTES_PER_WORD;
pub const N_LEAVES_LOG: usize = MEMORY_SIZE_LOG - PAGE_SIZE_BYTES_LOG;
pub const MERKLE_TREE_DEPTH: usize = N_LEAVES_LOG + 1;
pub const PAGE_SIZE_BYTES: usize = 1 << PAGE_SIZE_BYTES_LOG;
pub const PAGE_NUMBER_MASK: usize = (1 << N_LEAVES_LOG) - 1;
pub const WORDS_PER_HASH: usize = 4;
pub const BOOTLOADER_INPUTS_PER_PAGE: usize =
    WORDS_PER_PAGE + 1 + WORDS_PER_HASH + (MERKLE_TREE_DEPTH - 1) * WORDS_PER_HASH;
pub const MEMORY_HASH_START_INDEX: usize = 2 * (REGISTER_MEMORY_NAMES.len() + REGISTER_NAMES.len());
pub const NUM_PAGES_INDEX: usize = MEMORY_HASH_START_INDEX + WORDS_PER_HASH * 2;
pub const PAGE_INPUTS_OFFSET: usize = NUM_PAGES_INDEX + 1;

// Ensure we have enough addresses for the scratch space.
// TODO: review this number
const_assert!(PAGE_SIZE_BYTES > 1024);

/// Computes the size of the bootloader given the number of input pages.
pub fn bootloader_size(accessed_pages: &BTreeSet<u32>) -> usize {
    let constant_overhead = 1 + // jump bootloader_init
        2 + // load number of pages
        8 + // init memory hash
        1 + // page idx = 0
        1 + // branch_if_diff_equal if no pages
        8 + // assert final merkle root
        REGISTER_MEMORY_NAMES.len() + // load memory regs
        REGISTER_NAMES.len() * 2 +  // load asm regs
        1; // jump_to_bootloader_input

    let cost_per_page_fixed = 3 + // load page number and check != 0
        24 + // zero out scratch space
        1 + // set page offset
        WORDS_PER_PAGE * 4 + WORDS_PER_PAGE/4 + // load page data + poseidon hash every 4 words
        1 + // set x9=0 (validation phase)
    // VALIDATION PHASE
        1 + // branch_if_diff_nonzero
        16 + // assert root
        1 + // jump
        1 + // set phase to update
        16 + // load claimed updated page
        1 + // jump
    // UPDATE PHASE
        1 + // branch_if_diff_nonzero
        8 + // mloads
        1 + // affine
        1; // jump

    let cost_ith_bit_zero = 17 + // 8 load_bootloader_input/mstore + 1 jump
        3; // set x4 + if + poseidon call
    let cost_ith_bit_one = 32 + // 8 mload/mstore + 8 load_bootloader_input/mstore
        3; // set x4 + if + poseidon call

    let mut cost = constant_overhead + accessed_pages.len() * cost_per_page_fixed;
    for page in accessed_pages {
        for i in 0..N_LEAVES_LOG {
            if page & (1 << i) == 0 {
                cost += cost_ith_bit_zero * 2; // times 2 because there are 2 phases
            } else {
                cost += cost_ith_bit_one * 2;
            }
        }
    }

    cost
}

/// Computes an upper bound of how long the shutdown routine will run, for a given number of pages.
pub fn shutdown_routine_upper_bound(num_pages: usize) -> usize {
    // Regardless of the number of pages, we have to:
    // - Jump to the start of the routine
    // - Assert all register values are correct (except the PC)
    // - Start the page loop
    // - Jump to shutdown sink
    let constant_overhead = 6 + (REGISTER_MEMORY_NAMES.len() + REGISTER_NAMES.len()) - 1;

    // For each page, we have to:
    // TODO is 14 still the true number?
    // - Start the page loop (14 instructions)
    // - Load all words of the page
    // - Invoke the hash function once every 4 words
    // - Assert the page hash is as claimed (8 instructions)
    // TODO is 2 still the true number?
    // - Increment the page index and jump back to the loop start (2 instructions)
    let cost_per_page = 14 + WORDS_PER_PAGE + WORDS_PER_PAGE / 4 + WORDS_PER_HASH + 2;

    constant_overhead + num_pages * cost_per_page
}

pub fn bootloader_specific_instruction_names(field: KnownField) -> [&'static str; 2] {
    match field.field_size() {
        FieldSize::Small => {
            todo!()
        }
        FieldSize::Large => large_field::bootloader::BOOTLOADER_SPECIFIC_INSTRUCTION_NAMES,
    }
}

pub fn bootloader_preamble(field: KnownField) -> String {
    match field.field_size() {
        FieldSize::Small => {
            todo!()
        }
        FieldSize::Large => large_field::bootloader::bootloader_preamble(),
    }
}

pub fn bootloader_and_shutdown_routine(field: KnownField) -> String {
    match field.field_size() {
        FieldSize::Small => {
            todo!()
        }
        FieldSize::Large => large_field::bootloader::bootloader_and_shutdown_routine(),
    }
}

/// Index of the PC in the bootloader input.
pub const PC_INDEX: usize = REGISTER_MEMORY_NAMES.len() + REGISTER_NAMES.len() - 1;

/// The default PC that can be used in first chunk, will just continue with whatever comes after the bootloader.
///
/// The value is 3, because we added a jump instruction at the beginning of the code.
/// Specifically, the first instructions are:
/// 0: reset
/// 1: jump_to_operation
/// 2: jump submachine_init
/// 3: jump computation_start
pub const DEFAULT_PC: u64 = 3;

/// Analogous to the `DEFAULT_PC`, this well-known PC jumps to the shutdown routine.
pub const SHUTDOWN_START: u64 = 4;

/// Helper struct to construct the bootloader inputs, placing each element in
/// its correct position.
struct InputCreator<'a, F, Pages>
where
    F: FieldElement,
    Pages: ExactSizeIterator<Item = InputPage<'a, F>>,
{
    register_values: Vec<F>,
    merkle_tree_root_hash: &'a [F; 4],
    pages: Pages,
}

/// Pages of memory, each with its hash and proof.
struct InputPage<'a, F: FieldElement> {
    page_idx: u32,
    data: &'a [F; WORDS_PER_PAGE],
    hash: &'a [F; 4],
    proof: Vec<&'a [F; 4]>,
}

impl<'a, F, I> InputCreator<'a, F, I>
where
    F: powdr_number::FieldElement,
    I: ExactSizeIterator<Item = InputPage<'a, F>>,
{
    fn into_input(self) -> Vec<F> {
        let mut inputs = self.register_values;
        inputs.extend_from_within(..);
        inputs.extend(self.merkle_tree_root_hash.iter().flat_map(|v| split_fe(*v)));
        inputs.extend(self.merkle_tree_root_hash.iter().flat_map(|v| split_fe(*v)));

        inputs.push((self.pages.len() as i64).into());
        for page in self.pages {
            inputs.push(page.page_idx.into());
            inputs.extend(page.data);
            inputs.extend(page.hash.iter().flat_map(|v| split_fe(*v)));
            for sibling in page.proof {
                inputs.extend(sibling.iter().flat_map(|v| split_fe(*v)));
            }
        }
        inputs
    }
}

pub fn create_input<F: FieldElement, Pages: ExactSizeIterator<Item = u32>>(
    register_values: Vec<F>,
    merkle_tree: &MerkleTree<F>,
    accessed_pages: Pages,
) -> Vec<F> {
    InputCreator {
        register_values,
        merkle_tree_root_hash: merkle_tree.root_hash(),
        pages: accessed_pages.map(|page_index| {
            let (page, page_hash, proof) = merkle_tree.get(page_index as usize);
            InputPage {
                page_idx: page_index,
                data: page,
                hash: page_hash,
                proof,
            }
        }),
    }
    .into_input()
}

pub fn default_register_values<F: FieldElement>() -> Vec<F> {
    let mut register_values = vec![0.into(); REGISTER_MEMORY_NAMES.len() + REGISTER_NAMES.len()];
    register_values[PC_INDEX] = DEFAULT_PC.into();
    register_values
}

/// The bootloader input that is equivalent to not using a bootloader, i.e.:
/// - No pages are initialized
/// - All registers are set to 0 (including the PC, which causes the bootloader to do nothing)
/// - The state at the end of the execution is the same as the beginning
pub fn default_input<F: FieldElement>(accessed_pages: &[u64]) -> Vec<F> {
    // Set all registers and the number of pages to zero
    let register_values = default_register_values();
    let merkle_tree = MerkleTree::<F>::new();

    // TODO: We don't have a way to know the memory state *after* the execution.
    // For now, we'll just claim that the memory doesn't change.
    // This is fine for now, because the bootloader does not yet enforce that the memory
    // state is actually as claimed. In the future, the `accessed_pages` argument won't be
    // supported anymore (it's anyway only used by the benchmark).
    create_input(
        register_values,
        &merkle_tree,
        accessed_pages.iter().map(|&x| x as u32),
    )
}

pub fn split_fe<F: FieldElement>(v: F) -> [F; 2] {
    let v = v.to_integer().try_into_u64().unwrap();
    [((v & 0xffffffff) as u32).into(), ((v >> 32) as u32).into()]
}
