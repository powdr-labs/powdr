use powdr_number::FieldElement;
use powdr_number::LargeInt;

use super::memory_merkle_tree::MerkleTree;

use powdr_number::KnownField;

use static_assertions::const_assert;

/// 32-Bit architecture -> 2^32 bytes of addressable memory
pub const MEMORY_SIZE_LOG: usize = 32;

/// Page size is 2KB
pub const PAGE_SIZE_BYTES_LOG: usize = 11;

/// 32-Bit architecture -> 4 bytes per word
pub const BYTES_PER_WORD: usize = 4;

use crate::continuations::bootloader_16;
use crate::continuations::bootloader_32;

// Derived constants
pub const WORDS_PER_PAGE: usize = (1 << (PAGE_SIZE_BYTES_LOG)) / BYTES_PER_WORD;
pub const N_LEAVES_LOG: usize = MEMORY_SIZE_LOG - PAGE_SIZE_BYTES_LOG;
pub const MERKLE_TREE_DEPTH: usize = N_LEAVES_LOG + 1;
pub const PAGE_SIZE_BYTES: usize = 1 << PAGE_SIZE_BYTES_LOG;
pub const PAGE_NUMBER_MASK: usize = (1 << N_LEAVES_LOG) - 1;
pub const WORDS_PER_HASH: usize = 8;
pub const BOOTLOADER_INPUTS_PER_PAGE: usize =
    WORDS_PER_PAGE + 1 + WORDS_PER_HASH + (MERKLE_TREE_DEPTH - 1) * WORDS_PER_HASH;
pub const MEMORY_HASH_START_INDEX: usize = 2 * REGISTER_NAMES.len();
pub const NUM_PAGES_INDEX: usize = MEMORY_HASH_START_INDEX + WORDS_PER_HASH * 2;
pub const PAGE_INPUTS_OFFSET: usize = NUM_PAGES_INDEX + 1;

/// This trait provides all the field specific types and implementations that
/// the bootloader needs.
/// For now, this trait is implemented directly by each `FieldElement` type that supports it.
pub trait BootloaderImpl {
    type Fe: FieldElement;
    const FE_PER_WORD: usize;
    type Page;
    type Hash;
    fn update_page(page: &mut Self::Page, idx: usize, word: u32);
    fn hash_page(page: &Self::Page) -> Self::Hash;
    fn hash_two(a: &Self::Hash, b: &Self::Hash) -> Self::Hash;
    fn zero_hash() -> Self::Hash;
    fn zero_page() -> Self::Page;
    // iterate over a hash value as machine words as field elements (i.e., WORDS_PER_HASH * Self::FE_PER_WORD field elements),
    fn iter_hash_as_fe(h: &Self::Hash) -> impl Iterator<Item = Self::Fe>;
    // iterate over the page words, in their field element representation
    fn iter_page_as_fe(p: &Self::Page) -> impl Iterator<Item = Self::Fe>;
    // iterate over a word value in its field element representation
    fn iter_word_as_fe(w: u32) -> impl Iterator<Item = Self::Fe>;
}

/// Creates the bootloader input, placing elements in the layout expected by the
/// machine bootloader.
pub fn create_input<B: BootloaderImpl, I: ExactSizeIterator<Item = u32>>(
    register_values: Vec<B::Fe>,
    merkle_tree: &MerkleTree<B>,
    accessed_pages: I,
) -> Vec<B::Fe> {
    // initial register values
    let mut inputs = register_values;
    // final register values
    inputs.extend_from_within(..);
    let root_hash = merkle_tree.root_hash();
    // initial hash
    inputs.extend(B::iter_hash_as_fe(root_hash));
    // final hash
    inputs.extend(B::iter_hash_as_fe(root_hash));
    // number of pages
    inputs.extend(B::iter_word_as_fe(accessed_pages.len() as u32));
    // page data
    for page_index in accessed_pages {
        let (page_data, page_hash, proof) = merkle_tree.get(page_index as usize);
        inputs.extend(B::iter_word_as_fe(page_index));
        inputs.extend(B::iter_page_as_fe(page_data));
        inputs.extend(B::iter_hash_as_fe(page_hash));
        for sibling in proof {
            inputs.extend(B::iter_hash_as_fe(sibling));
        }
    }
    inputs
}

// Ensure we have enough addresses for the scratch space.
const_assert!(PAGE_SIZE_BYTES > 384);

/// Computes an upper bound of how long the shutdown routine will run, for a given number of pages.
pub fn shutdown_routine_upper_bound(num_pages: usize) -> usize {
    // Regardless of the number of pages, we have to:
    // - Jump to the start of the routine
    // - Assert all register values are correct (except the PC)
    // - Start the page loop
    // - Jump to shutdown sink
    let constant_overhead = 6 + REGISTER_NAMES.len() - 1;

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
    match field {
        KnownField::BabyBearField | KnownField::Mersenne31Field => {
            bootloader_16::BOOTLOADER_SPECIFIC_INSTRUCTION_NAMES
        }
        KnownField::GoldilocksField | KnownField::Bn254Field => {
            bootloader_32::BOOTLOADER_SPECIFIC_INSTRUCTION_NAMES
        }
    }
}

pub fn bootloader_preamble(field: KnownField) -> String {
    match field {
        KnownField::BabyBearField | KnownField::Mersenne31Field => {
            bootloader_16::bootloader_preamble()
        }
        KnownField::GoldilocksField | KnownField::Bn254Field => {
            bootloader_32::bootloader_preamble()
        }
    }
}

pub fn bootloader_and_shutdown_routine(
    field: KnownField,
    submachine_initialization: &[String],
) -> String {
    match field {
        KnownField::BabyBearField | KnownField::Mersenne31Field => {
            bootloader_16::bootloader_and_shutdown_routine(submachine_initialization)
        }
        KnownField::GoldilocksField | KnownField::Bn254Field => {
            bootloader_32::bootloader_and_shutdown_routine(submachine_initialization)
        }
    }
}

/// The names of the registers in the order in which they are expected by the bootloader.
pub const REGISTER_NAMES: [&str; 37] = [
    "main.x1",
    "main.x2",
    "main.x3",
    "main.x4",
    "main.x5",
    "main.x6",
    "main.x7",
    "main.x8",
    "main.x9",
    "main.x10",
    "main.x11",
    "main.x12",
    "main.x13",
    "main.x14",
    "main.x15",
    "main.x16",
    "main.x17",
    "main.x18",
    "main.x19",
    "main.x20",
    "main.x21",
    "main.x22",
    "main.x23",
    "main.x24",
    "main.x25",
    "main.x26",
    "main.x27",
    "main.x28",
    "main.x29",
    "main.x30",
    "main.x31",
    "main.tmp1",
    "main.tmp2",
    "main.tmp3",
    "main.tmp4",
    "main.lr_sc_reservation",
    "main.pc",
];

/// Index of the PC in the bootloader input.
pub const PC_INDEX: usize = REGISTER_NAMES.len() - 1;

/// The default PC that can be used in first chunk, will just continue with whatever comes after the bootloader.
/// The value is 3, because we added a jump instruction at the beginning of the code.
/// Specifically, the first instructions are:
/// 0: reset
/// 1: jump_to_operation
/// 2: jump submachine_init
/// 3: jump computation_start
pub const DEFAULT_PC: u64 = 3;

/// Analogous to the `DEFAULT_PC`, this well-known PC jumps to the shutdown routine.
pub const SHUTDOWN_START: u64 = 4;

pub fn default_register_values<F: FieldElement>() -> Vec<F> {
    let mut register_values = vec![0.into(); REGISTER_NAMES.len()];
    register_values[PC_INDEX] = DEFAULT_PC.into();
    register_values
}

/// The bootloader input that is equivalent to not using a bootloader, i.e.:
/// - No pages are initialized
/// - All registers are set to 0 (including the PC, which causes the bootloader to do nothing)
/// - The state at the end of the execution is the same as the beginning
pub fn default_input<B: BootloaderImpl>(accessed_pages: &[u64]) -> Vec<B::Fe> {
    // Set all registers and the number of pages to zero
    let register_values = default_register_values();
    let merkle_tree = MerkleTree::<B>::new();

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
