use powdr_number::FieldElement;
use powdr_number::LargeInt;
use powdr_riscv_executor::Elem;

use super::memory_merkle_tree::MerkleTree;
use crate::code_gen::Register;

use static_assertions::const_assert;

/// 32-Bit architecture -> 2^32 bytes of addressable memory
pub const MEMORY_SIZE_LOG: usize = 32;

/// Page size is 2KB
pub const PAGE_SIZE_BYTES_LOG: usize = 11;

/// 32-Bit architecture -> 4 bytes per word
pub const BYTES_PER_WORD: usize = 4;

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

pub const BOOTLOADER_SPECIFIC_INSTRUCTION_NAMES: [&str; 2] =
    ["load_bootloader_input", "jump_to_bootloader_input"];

pub fn bootloader_preamble() -> String {
    let mut preamble = r#"
    // ============== bootloader-specific instructions =======================
    // Write-once memory
    std::machines::write_once_memory::WriteOnceMemory bootloader_inputs;

    instr load_bootloader_input X, Y, Z, W
        link ~> tmp1_col = regs.mload(X, STEP)
        link => bootloader_inputs.access(tmp1_col * Z + W, tmp3_col)
        link ~> regs.mstore(Y, STEP + 2, tmp3_col);

    instr assert_bootloader_input X, Y, Z, W
        link ~> tmp1_col = regs.mload(X, STEP)
        link ~> tmp2_col = regs.mload(Y, STEP + 1)
        link => bootloader_inputs.access(tmp1_col * Z + W, tmp2_col);

    // Sets the PC to the bootloader input at the provided index
    instr jump_to_bootloader_input X link => bootloader_inputs.access(X, pc');

    // ============== Shutdown routine constraints =======================
    // Insert a `jump_to_shutdown_routine` witness column, which will let the prover indicate that
    // the normal PC update rule should be bypassed and instead set to the start of the shutdown routine.
    // Nothing of this is enforced yet, and the flag will be ignored.
    let jump_to_shutdown_routine;
    jump_to_shutdown_routine * (1 - jump_to_shutdown_routine) = 0;

    // Expose initial register values as public outputs
"#.to_string();

    for (i, reg) in REGISTER_NAMES.iter().enumerate() {
        let reg = reg.strip_prefix("main::").unwrap();
        preamble.push_str(&format!(
            "    //public initial_{reg} = main_bootloader_inputs::value({i});\n"
        ));
    }
    for (i, reg) in REGISTER_NAMES.iter().enumerate() {
        let reg = reg.strip_prefix("main::").unwrap();
        preamble.push_str(&format!(
            "    //public final_{reg} = main_bootloader_inputs::value({});\n",
            i + REGISTER_NAMES.len()
        ));
    }
    preamble.push_str(&format!(
        r#"
    //public initial_memory_hash_1 = main_bootloader_inputs::value({});
    //public initial_memory_hash_2 = main_bootloader_inputs::value({});
    //public initial_memory_hash_3 = main_bootloader_inputs::value({});
    //public initial_memory_hash_4 = main_bootloader_inputs::value({});
    //public initial_memory_hash_5 = main_bootloader_inputs::value({});
    //public initial_memory_hash_6 = main_bootloader_inputs::value({});
    //public initial_memory_hash_7 = main_bootloader_inputs::value({});
    //public initial_memory_hash_8 = main_bootloader_inputs::value({});
"#,
        MEMORY_HASH_START_INDEX,
        MEMORY_HASH_START_INDEX + 1,
        MEMORY_HASH_START_INDEX + 2,
        MEMORY_HASH_START_INDEX + 3,
        MEMORY_HASH_START_INDEX + 4,
        MEMORY_HASH_START_INDEX + 5,
        MEMORY_HASH_START_INDEX + 6,
        MEMORY_HASH_START_INDEX + 7,
    ));
    preamble.push_str(&format!(
        r#"
    //public final_memory_hash_1 = main_bootloader_inputs::value({});
    //public final_memory_hash_2 = main_bootloader_inputs::value({});
    //public final_memory_hash_3 = main_bootloader_inputs::value({});
    //public final_memory_hash_4 = main_bootloader_inputs::value({});
    //public final_memory_hash_5 = main_bootloader_inputs::value({});
    //public final_memory_hash_6 = main_bootloader_inputs::value({});
    //public final_memory_hash_7 = main_bootloader_inputs::value({});
    //public final_memory_hash_8 = main_bootloader_inputs::value({});
"#,
        MEMORY_HASH_START_INDEX + 8,
        MEMORY_HASH_START_INDEX + 9,
        MEMORY_HASH_START_INDEX + 10,
        MEMORY_HASH_START_INDEX + 11,
        MEMORY_HASH_START_INDEX + 12,
        MEMORY_HASH_START_INDEX + 13,
        MEMORY_HASH_START_INDEX + 14,
        MEMORY_HASH_START_INDEX + 15,
    ));

    preamble
}

/// The bootloader: An assembly program that can be executed at the beginning of RISC-V execution.
/// It lets the prover provide arbitrary memory pages and writes them to memory, as well as values for
/// the registers (including the PC, which is set last).
/// This can be used to implement continuations. Note that this is completely non-sound. Progress to
/// make it sound is tracked in https://github.com/powdr-labs/powdr/issues/814.
/// Bootloader inputs are in the format:
/// - First 37 values: Values of x1-x31, tmp1-tmp4, lr_sc_reservation, and the PC
/// - Second 37 values: The same values, but after this chunk's execution
/// - The root hash of the memory Merkle tree (8 words)
/// - The root hash of the memory Merkle tree *after this chunk's execution* (8 words)
/// - Number of pages
/// - For each page:
///   - The page number
///   - The words of the page
///   - The hash (8 words) of the page *after* this chunk's execution
///   - For each level of the Merkle tree, except the root (1..=22):
///     - The hash (8 words) of the sibling page
pub fn bootloader_and_shutdown_routine(submachine_initialization: &[String]) -> String {
    let mut bootloader = String::new();

    bootloader.push_str(&format!(
        r#"
// Skip the next instruction
jump submachine_init, 32;

// For convenience, this instruction has a known fixed PC ({DEFAULT_PC}) and just jumps
// to whatever comes after the bootloader + shutdown routine. This avoids having to count
// the instructions of the bootloader and the submachine initialization.
jump computation_start, 32;

// Similarly, this instruction has a known fixed PC ({SHUTDOWN_START}) and just jumps
// to the shutdown routine.
jump shutdown_start, 32;

shutdown_sink:
jump shutdown_sink, 32;

// Submachine initialization: Calls each submachine once, because that helps witness
// generation figure out default values that can be used if the machine is never used.
submachine_init:
"#
    ));
    bootloader.push_str(&submachine_initialization.join("\n"));

    bootloader.push_str(&format!(
        r#"
// START OF BOOTLOADER

// During the execution of the bootloader, registers are used as follows:
// - x1: Number of pages (constant throughout the execution)
// - x2: Current page index
// - x3: Current page number
// - x4: The ith bit of the page number (during Merkle proof validation)
// - x18-x25: The current memory hash
// - x9: 0: Merkle tree validation phase; 1: Merkle tree update phase
// - Scratch space for hash operations:
//   - [0], [4], [8], [12], [16], [20], [24], [28] will usually contain the "current" hash (either in the context
//     of page hashing or Merkle proof validation)
//   - [32], [36], [40], [44], [48], [52], [56], [60] will contain some other inputs to the hash function
//   - [64], [68], [72], [76], [80], [84], [88], [92] will contain the capacity elements (0 throughout the execution)
// N.B. we assume the scratch space is never used by the program's memory.

// Number of pages
load_bootloader_input 0, 1, 1, {NUM_PAGES_INDEX};
add_wrap 1, 0, 0, 1;

// Initialize memory hash
load_bootloader_input 0, 18, 1, {MEMORY_HASH_START_INDEX};
load_bootloader_input 0, 19, 1, {MEMORY_HASH_START_INDEX} + 1;
load_bootloader_input 0, 20, 1, {MEMORY_HASH_START_INDEX} + 2;
load_bootloader_input 0, 21, 1, {MEMORY_HASH_START_INDEX} + 3;
load_bootloader_input 0, 22, 1, {MEMORY_HASH_START_INDEX} + 4;
load_bootloader_input 0, 23, 1, {MEMORY_HASH_START_INDEX} + 5;
load_bootloader_input 0, 24, 1, {MEMORY_HASH_START_INDEX} + 6;
load_bootloader_input 0, 25, 1, {MEMORY_HASH_START_INDEX} + 7;

// Current page index
set_reg 2, 0;

branch_if_diff_equal 1, 0, 0, bootloader_end_page_loop;

bootloader_start_page_loop:

// Page number
load_bootloader_input 2, 3, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET};
and 3, 0, {PAGE_NUMBER_MASK}, 3;

// Assert that the page number is not zero, as paging in page 0 would overwrite the scratch space.
branch_if_diff_nonzero 3, 0, page_number_ok;

page_number_zero:
fail;

page_number_ok:

// Store & hash {WORDS_PER_PAGE} page words. This is an unrolled loop that for each each word:
// - Loads the word into the P{{(i % 4) + 4}} register
// - Stores the word at the address x3 * {PAGE_SIZE_BYTES} + i * {BYTES_PER_WORD}
// - If i % 4 == 3: Hashes mem[0, 96), storing the result in mem[0, 32)
//
// At the end of the loop, we'll have a linear hash of the page in [0, 32), using a Merkle-Damgård
// construction. The initial [0, 32) values are 0, and the capacity [64, 96) is 0 throughout the
// bootloader execution.

// TODO currently the init call to poseidon_gl initializes the
// scratch space with mstore_bootloader.
// When the init call is removed, the calls below need to be replace by mstore_bootloader.
mstore 0, 0, 0, 0;
mstore 0, 0, 4, 0;
mstore 0, 0, 8, 0;
mstore 0, 0, 12, 0;
mstore 0, 0, 16, 0;
mstore 0, 0, 20, 0;
mstore 0, 0, 24, 0;
mstore 0, 0, 28, 0;
mstore 0, 0, 32, 0;
mstore 0, 0, 36, 0;
mstore 0, 0, 40, 0;
mstore 0, 0, 44, 0;
mstore 0, 0, 48, 0;
mstore 0, 0, 52, 0;
mstore 0, 0, 56, 0;
mstore 0, 0, 60, 0;
mstore 0, 0, 64, 0;
mstore 0, 0, 68, 0;
mstore 0, 0, 72, 0;
mstore 0, 0, 76, 0;
mstore 0, 0, 80, 0;
mstore 0, 0, 84, 0;
mstore 0, 0, 88, 0;
mstore 0, 0, 92, 0;
"#,
   ));

    bootloader.push_str(&format!("affine 3, 90, {PAGE_SIZE_BYTES}, 0;\n"));
    for i in 0..WORDS_PER_PAGE {
        // Multiply the index by 8 to skip 2 words, 4 words,
        // one used for the actual 32-bit word and a zero.
        let idx = ((i % 4) + 4) * WORDS_PER_HASH;
        bootloader.push_str(&format!(
            r#"
load_bootloader_input 2, 91, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + 1 + {i};
mstore 0, 0, {idx}, 91;

affine 3, 90, {PAGE_SIZE_BYTES}, {i} * {BYTES_PER_WORD};
mstore_bootloader 90, 0, 0, 91;"#
        ));

        // Hash if buffer is full
        if i % 4 == 3 {
            bootloader.push_str("poseidon_gl 0, 0;");
        }
    }

    bootloader.push_str(&format!(
        r#"
// == Merkle proof validation ==
// We commit to the memory content by hashing it in pages of {WORDS_PER_PAGE} words each.
// These hashes are stored in a binary Merkle tree of depth {MERKLE_TREE_DEPTH}.
// At this point, the current page hash is in mem[0, 32).
// 
// Now, we re-computed the Merkle root twice, in two phases:
// - First using the current page hash, as computed by the bootloader. The prover provides
//   the sibling values. At the end of this phase, the re-computed Merkle root is asserted
//   to be equal to the "current" Merkle root, stored in x18-x25.
// - Second, we repeat the same process (using the *same* siblings!), but using the claimed
//   updated page hash. At the end of this phase, the re-computed Merkle root stored as the
//   "current" Merkle root in x18-x25.
//
// So, any Merkle proof is expected to be based on the Merkle tree with all previous pages
// already updated. In the shutdown routine, we will validate that the final page hashes
// are as claimed. Also, at the end of the bootloader, we will assert that the final Merkle
// root is as claimed.

// Set phase to validation
set_reg 9, 0;

bootloader_merkle_proof_validation_loop:

// This is an unrolled loop that for each level:
// - If the ith bit of the page number is 0:
//   - Load sibling into mem[32, 64)
// - Else:
//   - Write mem[0, 32) to mem[32, 64)
//   - Load sibling into mem[0, 32)
// - Hash mem[0, 92), storing the result in mem[0, 32)
//
// At the end of the loop, we'll have the Merkle root in mem[0, 32).
"#,
    ));

    for i in 0..N_LEAVES_LOG {
        let mask = 1 << i;
        bootloader.push_str(&format!(
            r#"
and 3, 0, {mask}, 4;

branch_if_diff_nonzero 4, 0, bootloader_level_{i}_is_right;

load_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + {WORDS_PER_HASH} + {i} * {WORDS_PER_HASH} + 0;
mstore 0, 0, 32, 90;

load_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + {WORDS_PER_HASH} + {i} * {WORDS_PER_HASH} + 1;
mstore 0, 0, 36, 90;

load_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + {WORDS_PER_HASH} + {i} * {WORDS_PER_HASH} + 2;
mstore 0, 0, 40, 90;

load_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + {WORDS_PER_HASH} + {i} * {WORDS_PER_HASH} + 3;
mstore 0, 0, 44, 90;

load_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + {WORDS_PER_HASH} + {i} * {WORDS_PER_HASH} + 4;
mstore 0, 0, 48, 90;

load_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + {WORDS_PER_HASH} + {i} * {WORDS_PER_HASH} + 5;
mstore 0, 0, 52, 90;

load_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + {WORDS_PER_HASH} + {i} * {WORDS_PER_HASH} + 6;
mstore 0, 0, 56, 90;

load_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + {WORDS_PER_HASH} + {i} * {WORDS_PER_HASH} + 7;
mstore 0, 0, 60, 90;

jump bootloader_level_{i}_end, 90;
bootloader_level_{i}_is_right:
// reg[90], reg[91] = mload(0 + 0)
mload 0, 0, 90, 91;
mstore 0, 0, 32, 90;

mload 0, 4, 90, 91;
mstore 0, 0, 36, 90;

mload 0, 8, 90, 91;
mstore 0, 0, 40, 90;

mload 0, 12, 90, 91;
mstore 0, 0, 44, 90;

mload 0, 16, 90, 91;
mstore 0, 0, 48, 90;

mload 0, 20, 90, 91;
mstore 0, 0, 52, 90;

mload 0, 24, 90, 91;
mstore 0, 0, 56, 90;

mload 0, 28, 90, 91;
mstore 0, 0, 60, 90;

load_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + {WORDS_PER_HASH} + {i} * {WORDS_PER_HASH} + 0;
mstore 0, 0, 0, 90;

load_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + {WORDS_PER_HASH} + {i} * {WORDS_PER_HASH} + 1;
mstore 0, 0, 4, 90;

load_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + {WORDS_PER_HASH} + {i} * {WORDS_PER_HASH} + 2;
mstore 0, 0, 8, 90;

load_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + {WORDS_PER_HASH} + {i} * {WORDS_PER_HASH} + 3;
mstore 0, 0, 12, 90;

load_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + {WORDS_PER_HASH} + {i} * {WORDS_PER_HASH} + 4;
mstore 0, 0, 16, 90;

load_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + {WORDS_PER_HASH} + {i} * {WORDS_PER_HASH} + 5;
mstore 0, 0, 20, 90;

load_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + {WORDS_PER_HASH} + {i} * {WORDS_PER_HASH} + 6;
mstore 0, 0, 24, 90;

load_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + {WORDS_PER_HASH} + {i} * {WORDS_PER_HASH} + 7;
mstore 0, 0, 28, 90;

bootloader_level_{i}_end:
    poseidon_gl 0, 0;
"#
        ));
    }

    bootloader.push_str(&format!(
        r#"
branch_if_diff_nonzero 9, 0, bootloader_update_memory_hash;

// Assert Correct Merkle Root
mload 0, 0, 90, 91;
branch_if_diff_nonzero 18, 90, bootloader_memory_hash_mismatch;

mload 0, 4, 90, 91;
branch_if_diff_nonzero 19, 90, bootloader_memory_hash_mismatch;

mload 0, 8, 90, 91;
branch_if_diff_nonzero 20, 90, bootloader_memory_hash_mismatch;

mload 0, 12, 90, 91;
branch_if_diff_nonzero 21, 90, bootloader_memory_hash_mismatch;

mload 0, 16, 90, 91;
branch_if_diff_nonzero 22, 90, bootloader_memory_hash_mismatch;

mload 0, 20, 90, 91;
branch_if_diff_nonzero 23, 90, bootloader_memory_hash_mismatch;

mload 0, 24, 90, 91;
branch_if_diff_nonzero 24, 90, bootloader_memory_hash_mismatch;

mload 0, 28, 90, 91;
branch_if_diff_nonzero 25, 90, bootloader_memory_hash_mismatch;

jump bootloader_memory_hash_ok, 90;
bootloader_memory_hash_mismatch:
fail;
bootloader_memory_hash_ok:

// Set phase to update
set_reg 9, 1;

// Load claimed updated page hash into mem[0, 32)
load_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + 0;
mstore 0, 0, 0, 90;

load_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + 1;
mstore 0, 0, 4, 90;

load_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + 2;
mstore 0, 0, 8, 90;

load_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + 3;
mstore 0, 0, 12, 90;

load_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + 4;
mstore 0, 0, 16, 90;

load_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + 5;
mstore 0, 0, 20, 90;

load_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + 6;
mstore 0, 0, 24, 90;

load_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + 7;
mstore 0, 0, 28, 90;

// Repeat Merkle proof validation loop to compute updated Merkle root
jump bootloader_merkle_proof_validation_loop, 90;

bootloader_update_memory_hash:

mload 0, 0, 18, 90;
mload 0, 4, 19, 90;
mload 0, 8, 20, 90;
mload 0, 12, 21, 90;
mload 0, 16, 22, 90;
mload 0, 20, 23, 90;
mload 0, 24, 24, 90;
mload 0, 28, 25, 90;

// Increment page index
affine 2, 2, 1, 1;

branch_if_diff_nonzero 2, 1, bootloader_start_page_loop;

bootloader_end_page_loop:

// Assert final Merkle root is as claimed
assert_bootloader_input 0, 18, 1, {MEMORY_HASH_START_INDEX} + 8;
assert_bootloader_input 0, 19, 1, {MEMORY_HASH_START_INDEX} + 9;
assert_bootloader_input 0, 20, 1, {MEMORY_HASH_START_INDEX} + 10;
assert_bootloader_input 0, 21, 1, {MEMORY_HASH_START_INDEX} + 11;
assert_bootloader_input 0, 22, 1, {MEMORY_HASH_START_INDEX} + 12;
assert_bootloader_input 0, 23, 1, {MEMORY_HASH_START_INDEX} + 13;
assert_bootloader_input 0, 24, 1, {MEMORY_HASH_START_INDEX} + 14;
assert_bootloader_input 0, 25, 1, {MEMORY_HASH_START_INDEX} + 15;

// Initialize registers, starting with index 0
"#
    ));

    // Go over all registers except the PC
    let register_iter = REGISTER_NAMES.iter().take(REGISTER_NAMES.len() - 1);

    for (i, reg) in register_iter.enumerate() {
        let reg = reg.strip_prefix("main::").unwrap();
        bootloader.push_str(&format!(
            r#"load_bootloader_input 0, {}, 1, {i};"#,
            Register::from(reg).addr()
        ));
        bootloader.push('\n');
    }

    bootloader.push_str(&format!(
        r#"
// Default PC is 0, but we already started from 0, so in that case we do nothing.
// Otherwise, we jump to the PC.
jump_to_bootloader_input {PC_INDEX};

// END OF BOOTLOADER

"#
    ));

    bootloader.push_str(
        r#"
// START OF SHUTDOWN ROUTINE
//
// This code is currently never executed in practice!
//
// The shutdown routine is responsible for:
// - Validating that the final register values are equal to those in the bootloader inputs
//   (which are exposed as public outputs)
// - Validating that the final page hashes are equal to the claimed values provided in the
//   bootloader inputs (which have previously been used to update the Merkle tree)
//
// During the execution of the shutdown routine, registers are used as follows:
// - x1: Number of pages (constant throughout the execution)
// - x2: Current page index
// - x3: Current page number
// - mem[0, 92): Hash values, used to compute the page hash

shutdown_start:

// Assert final register values are as claimed
// Note that we cannot assert that the final PC is correct, because it will already
// have changed at this point. This will need to be done by whatever mechanism is used
// to jump to the shutdown routine.
"#,
    );

    // Go over all registers except the PC
    let register_iter = REGISTER_NAMES.iter().take(REGISTER_NAMES.len() - 1);

    for (i, reg) in register_iter.enumerate() {
        let reg = reg.strip_prefix("main::").unwrap();
        bootloader.push_str(&format!(
            "assert_bootloader_input {i}, {}, 1, {};\n",
            Register::from(reg).addr(),
            REGISTER_NAMES.len()
        ));
    }

    bootloader.push_str(&format!(
        r#"
// Number of pages
load_bootloader_input 0, 1, 1, {NUM_PAGES_INDEX};
add_wrap 1, 0, 0, 1;

// Current page index
set_reg 2, 0;

branch_if_diff_equal 1, 0, 0, shutdown_end_page_loop;

shutdown_start_page_loop:

// Page number
load_bootloader_input 2, 3, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET};
and 3, 0, {PAGE_NUMBER_MASK}, 3;

// Store & hash {WORDS_PER_PAGE} page words. This is an unrolled loop that for each each word:
// - Loads the word at the address x3 * {PAGE_SIZE_BYTES} + i * {BYTES_PER_WORD}
//   into the P{{(i % 4) + 4}} register
// - If i % 4 == 3: Hashes mem[0, 92), storing the result in mem[0, 32)
//
// At the end of the loop, we'll have a linear hash of the page in mem[0, 32), using a Merkle-Damgård
// construction. The initial mem[0, 32) values are 0, and the capacity (mem[64, 92)) is 0 throughout the
// execution of the shutdown routine.

mstore 0, 0, 0, 0;
mstore 0, 0, 4, 0;
mstore 0, 0, 8, 0;
mstore 0, 0, 12, 0;
mstore 0, 0, 16, 0;
mstore 0, 0, 20, 0;
mstore 0, 0, 24, 0;
mstore 0, 0, 28, 0;
mstore 0, 0, 32, 0;
mstore 0, 0, 36, 0;
mstore 0, 0, 40, 0;
mstore 0, 0, 44, 0;
mstore 0, 0, 48, 0;
mstore 0, 0, 52, 0;
mstore 0, 0, 56, 0;
mstore 0, 0, 60, 0;
mstore 0, 0, 64, 0;
mstore 0, 0, 68, 0;
mstore 0, 0, 72, 0;
mstore 0, 0, 76, 0;
mstore 0, 0, 80, 0;
mstore 0, 0, 84, 0;
mstore 0, 0, 88, 0;
mstore 0, 0, 92, 0;
"#,
    ));

    bootloader.push_str(&format!("affine 90, 3, {PAGE_SIZE_BYTES}, 0;\n"));
    for i in 0..WORDS_PER_PAGE {
        let idx = ((i % 4) + 4) * WORDS_PER_HASH;
        bootloader.push_str(&format!("mload 90, {i} * {BYTES_PER_WORD}, 90, 91;\n"));
        bootloader.push_str(&format!("mstore 0, 0, {idx}, 90;\n"));

        // Hash if buffer is full
        if i % 4 == 3 {
            bootloader.push_str("poseidon_gl 0, 0;\n");
        }
    }

    bootloader.push_str(&format!(
        r#"

// Assert page hash is as claimed
// At this point, mem[0, 32) contain the actual page hash at the end of the execution.

mload 0, 0, 90, 91;
assert_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + {WORDS_PER_PAGE} + 1 + 0;

mload 0, 4, 90, 91;
assert_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + {WORDS_PER_PAGE} + 1 + 1;

mload 0, 8, 90, 91;
assert_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + {WORDS_PER_PAGE} + 1 + 2;

mload 0, 12, 90, 91;
assert_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + {WORDS_PER_PAGE} + 1 + 3;

mload 0, 16, 90, 91;
assert_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + {WORDS_PER_PAGE} + 1 + 4;

mload 0, 20, 90, 91;
assert_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + {WORDS_PER_PAGE} + 1 + 5;

mload 0, 24, 90, 91;
assert_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + {WORDS_PER_PAGE} + 1 + 6;

mload 0, 28, 90, 91;
assert_bootloader_input 2, 90, {BOOTLOADER_INPUTS_PER_PAGE}, {PAGE_INPUTS_OFFSET} + {WORDS_PER_PAGE} + 1 + 7;

// Increment page index
affine 2, 2, 1, 1;

branch_if_diff_nonzero 2, 1, shutdown_start_page_loop;

shutdown_end_page_loop:

jump shutdown_sink, 90;

// END OF SHUTDOWN ROUTINE

computation_start:
"#,
    ));

    bootloader
}

/// The names of the registers in the order in which they are expected by the bootloader.
pub const REGISTER_NAMES: [&str; 37] = [
    "main::x1",
    "main::x2",
    "main::x3",
    "main::x4",
    "main::x5",
    "main::x6",
    "main::x7",
    "main::x8",
    "main::x9",
    "main::x10",
    "main::x11",
    "main::x12",
    "main::x13",
    "main::x14",
    "main::x15",
    "main::x16",
    "main::x17",
    "main::x18",
    "main::x19",
    "main::x20",
    "main::x21",
    "main::x22",
    "main::x23",
    "main::x24",
    "main::x25",
    "main::x26",
    "main::x27",
    "main::x28",
    "main::x29",
    "main::x30",
    "main::x31",
    "main::tmp1",
    "main::tmp2",
    "main::tmp3",
    "main::tmp4",
    "main::lr_sc_reservation",
    "main::pc",
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

/// Helper struct to construct the bootloader inputs, placing each element in
/// its correct position.
struct InputCreator<'a, F, Pages>
where
    F: FieldElement,
    Pages: ExactSizeIterator<Item = InputPage<'a, F>>,
{
    register_values: Vec<Elem<F>>,
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
    fn into_input(self) -> Vec<Elem<F>> {
        let mut inputs = self.register_values;
        inputs.extend_from_within(..);
        inputs.extend(
            self.merkle_tree_root_hash
                .iter()
                .flat_map(|v| split_fe_as_elem(*v)),
        );
        inputs.extend(
            self.merkle_tree_root_hash
                .iter()
                .flat_map(|v| split_fe_as_elem(*v)),
        );

        inputs.push(Elem::Binary(self.pages.len() as i64));
        for page in self.pages {
            inputs.push(page.page_idx.into());
            inputs.extend(page.data.map(|v| Elem::new_from_fe_as_bin(&v)));
            inputs.extend(page.hash.iter().flat_map(|v| split_fe_as_elem(*v)));
            for sibling in page.proof {
                inputs.extend(sibling.iter().flat_map(|v| split_fe_as_elem(*v)));
            }
        }
        inputs
    }
}

pub fn create_input<F: FieldElement, Pages: ExactSizeIterator<Item = u32>>(
    register_values: Vec<Elem<F>>,
    merkle_tree: &MerkleTree<F>,
    accessed_pages: Pages,
) -> Vec<Elem<F>> {
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

pub fn default_register_values<T: FieldElement>() -> Vec<Elem<T>> {
    let mut register_values = vec![Elem::Binary(0); REGISTER_NAMES.len()];
    register_values[PC_INDEX] = Elem::Binary(DEFAULT_PC as i64);
    register_values
}

/// The bootloader input that is equivalent to not using a bootloader, i.e.:
/// - No pages are initialized
/// - All registers are set to 0 (including the PC, which causes the bootloader to do nothing)
/// - The state at the end of the execution is the same as the beginning
pub fn default_input<T: FieldElement>(accessed_pages: &[u64]) -> Vec<Elem<T>> {
    // Set all registers and the number of pages to zero
    let register_values = default_register_values();
    let merkle_tree = MerkleTree::<T>::new();

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

pub fn split_fe_as_elem<T: FieldElement>(v: T) -> [Elem<T>; 2] {
    let v = v.to_integer().try_into_u64().unwrap();
    [
        Elem::from((v & 0xffffffff) as u32),
        Elem::from((v >> 32) as u32),
    ]
}
