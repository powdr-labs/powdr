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

use crate::large_field;
use crate::small_field;

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

pub struct BootloaderInputs<B: BootloaderImpl>(pub Vec<B::Fe>);

impl<B: BootloaderImpl> BootloaderInputs<B> {
    pub fn new<I: ExactSizeIterator<Item = u32>>(
        register_values: Vec<B::Fe>,
        merkle_tree: &MerkleTree<B>,
        accessed_pages: I,
    ) -> Self {
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
        BootloaderInputs(inputs)
    }

    /// The bootloader input that is equivalent to not using a bootloader, i.e.:
    /// - No pages are initialized
    /// - All registers are set to 0 (including the PC, which causes the bootloader to do nothing)
    /// - The state at the end of the execution is the same as the beginning
    pub fn default_for(accessed_pages: &[u64]) -> BootloaderInputs<B> {
        // Set all registers and the number of pages to zero
        let register_values = default_register_values::<B>();
        let merkle_tree = MerkleTree::<B>::new();

        // TODO: We don't have a way to know the memory state *after* the execution.
        // For now, we'll just claim that the memory doesn't change.
        // This is fine for now, because the bootloader does not yet enforce that the memory
        // state is actually as claimed. In the future, the `accessed_pages` argument won't be
        // supported anymore (it's anyway only used by the benchmark).
        BootloaderInputs::new(
            register_values,
            &merkle_tree,
            accessed_pages.iter().map(|&x| x as u32),
        )
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn initial_memory_hash(&self) -> &[B::Fe] {
        // &bootloader_inputs[MEMORY_HASH_START_INDEX..MEMORY_HASH_START_INDEX + 8]
        let hash_start = MEMORY_HASH_START_INDEX * B::FE_PER_WORD;
        &self.0[hash_start..hash_start + WORDS_PER_HASH * B::FE_PER_WORD]
    }

    pub fn final_memory_hash(&self) -> &[B::Fe] {
        let hash_start = (MEMORY_HASH_START_INDEX + WORDS_PER_HASH) * B::FE_PER_WORD;
        &self.0[hash_start..hash_start + WORDS_PER_HASH * B::FE_PER_WORD]
    }

    fn proof_start(input_page_idx: usize) -> usize {
        B::FE_PER_WORD
            * (PAGE_INPUTS_OFFSET
                + BOOTLOADER_INPUTS_PER_PAGE * input_page_idx
                + 1
                + WORDS_PER_PAGE
                + WORDS_PER_HASH)
    }

    pub fn update_proof(&mut self, input_page_idx: usize, sibling_idx: usize, hash: &[B::Fe]) {
        assert_eq!(hash.len(), WORDS_PER_HASH * B::FE_PER_WORD);
        let sibling_start =
            Self::proof_start(input_page_idx) + sibling_idx * WORDS_PER_HASH * B::FE_PER_WORD;
        let sibling_end =
            Self::proof_start(input_page_idx) + (sibling_idx + 1) * WORDS_PER_HASH * B::FE_PER_WORD;
        self.0[sibling_start..sibling_end].copy_from_slice(hash);
    }

    pub fn proof(&self, input_page_idx: usize, sibling_idx: usize) -> &[B::Fe] {
        let sibling_start =
            Self::proof_start(input_page_idx) + sibling_idx * WORDS_PER_HASH * B::FE_PER_WORD;
        let sibling_end =
            Self::proof_start(input_page_idx) + (sibling_idx + 1) * WORDS_PER_HASH * B::FE_PER_WORD;
        &self.0[sibling_start..sibling_end]
    }

    pub fn update_page_hash(&mut self, input_page_idx: usize, hash: &[B::Fe]) {
        assert_eq!(hash.len(), WORDS_PER_HASH * B::FE_PER_WORD);
        let hash_start =
            (PAGE_INPUTS_OFFSET + BOOTLOADER_INPUTS_PER_PAGE * input_page_idx + 1 + WORDS_PER_PAGE)
                * B::FE_PER_WORD;
        self.0[hash_start..hash_start + WORDS_PER_HASH * B::FE_PER_WORD].copy_from_slice(hash);
    }

    pub fn update_final_registers(&mut self, register_values: &[B::Fe]) {
        assert_eq!(register_values.len(), REGISTER_NAMES.len() * B::FE_PER_WORD);
        let final_registers_start = REGISTER_NAMES.len() * B::FE_PER_WORD;
        self.0
            [final_registers_start..final_registers_start + REGISTER_NAMES.len() * B::FE_PER_WORD]
            .copy_from_slice(register_values);
    }

    pub fn update_final_root(&mut self, hash: &[B::Fe]) {
        assert_eq!(hash.len(), WORDS_PER_HASH * B::FE_PER_WORD);
        let root_hash_start = (MEMORY_HASH_START_INDEX + WORDS_PER_HASH) * B::FE_PER_WORD;
        self.0[root_hash_start..root_hash_start + WORDS_PER_HASH * B::FE_PER_WORD]
            .copy_from_slice(hash);
    }

    pub fn pc(&mut self) -> B::Fe {
        // the PC is a powdr asm register, which is a single fe, but for the
        // bootloader inputs, we store it as a "word", the same representation
        // as the other registers in memory, which may be more than one fe
        // (e.g., 2 for small fields such as BabyBear).
        // Here we return the composed fe value.
        match B::FE_PER_WORD {
            1 => self.0[PC_INDEX],
            2 => {
                let hi = self.0[PC_INDEX * B::FE_PER_WORD]
                    .to_integer()
                    .try_into_u32()
                    .unwrap();
                let lo = self.0[PC_INDEX * B::FE_PER_WORD + 1]
                    .to_integer()
                    .try_into_u32()
                    .unwrap();
                assert!(lo <= 0xffff);
                let pc = hi << 16 | lo;
                assert!(pc < B::Fe::modulus().try_into_u32().unwrap());
                pc.into()
            }
            _ => unreachable!(),
        }
    }

    /// Convert the bootloader inputs into the format expected by the external witness.
    /// Column names here must match the column names in the `bootloader_inputs` submachines.
    pub fn into_external_witness(self) -> Vec<(String, Vec<B::Fe>)> {
        match B::FE_PER_WORD {
            1 => vec![("main_bootloader_inputs::value".to_string(), self.0)],
            2 => {
                // split the words into two colums with hi and lo
                let (hi, lo) =
                    self.0
                        .chunks_exact(2)
                        .fold((vec![], vec![]), |(mut hi, mut lo), chunk| {
                            hi.push(chunk[0]);
                            lo.push(chunk[1]);
                            (hi, lo)
                        });
                vec![
                    ("main_bootloader_inputs::value1".to_string(), hi),
                    ("main_bootloader_inputs::value2".to_string(), lo),
                ]
            }
            _ => unreachable!(),
        }
    }
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
            small_field::bootloader::BOOTLOADER_SPECIFIC_INSTRUCTION_NAMES
        }
        KnownField::GoldilocksField | KnownField::Bn254Field => {
            large_field::bootloader::BOOTLOADER_SPECIFIC_INSTRUCTION_NAMES
        }
    }
}

pub fn bootloader_preamble(field: KnownField) -> String {
    match field {
        KnownField::BabyBearField | KnownField::Mersenne31Field => {
            small_field::bootloader::bootloader_preamble()
        }
        KnownField::GoldilocksField | KnownField::Bn254Field => {
            large_field::bootloader::bootloader_preamble()
        }
    }
}

pub fn bootloader_and_shutdown_routine(
    field: KnownField,
    submachine_initialization: &[String],
) -> String {
    match field {
        KnownField::BabyBearField | KnownField::Mersenne31Field => {
            small_field::bootloader::bootloader_and_shutdown_routine(submachine_initialization)
        }
        KnownField::GoldilocksField | KnownField::Bn254Field => {
            large_field::bootloader::bootloader_and_shutdown_routine(submachine_initialization)
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

pub fn default_register_values<B: BootloaderImpl>() -> Vec<B::Fe> {
    let mut register_values = vec![0.into(); REGISTER_NAMES.len() * B::FE_PER_WORD];
    // default pc value fits in least significant field element of the register
    assert!(B::FE_PER_WORD <= 2 && DEFAULT_PC <= u16::MAX as u64);
    register_values[(PC_INDEX + 1) * B::FE_PER_WORD - 1] = DEFAULT_PC.into();
    register_values
}

pub fn split_fe<F: FieldElement>(v: F) -> [F; 2] {
    let v = v.to_integer().try_into_u64().unwrap();
    [((v & 0xffffffff) as u32).into(), ((v >> 32) as u32).into()]
}
