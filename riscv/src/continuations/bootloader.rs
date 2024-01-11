use number::FieldElement;

use super::memory_merkle_tree::MerkleTree;

/// 32-Bit architecture -> 2^32 bytes of addressable memory
pub const MEMORY_SIZE_LOG: usize = 32;

/// Page size is 1KB
pub const PAGE_SIZE_BYTES_LOG: usize = 10;

/// 32-Bit architecture -> 4 bytes per word
pub const BYTES_PER_WORD: usize = 4;

// Derived constants
pub const WORDS_PER_PAGE: usize = (1 << (PAGE_SIZE_BYTES_LOG)) / BYTES_PER_WORD;
pub const N_LEAVES_LOG: usize = MEMORY_SIZE_LOG - PAGE_SIZE_BYTES_LOG;
pub const MERKLE_TREE_DEPTH: usize = N_LEAVES_LOG + 1;
pub const PAGE_SIZE_BYTES: usize = 1 << PAGE_SIZE_BYTES_LOG;
pub const PAGE_NUMBER_MASK: usize = (1 << N_LEAVES_LOG) - 1;
pub const BOOTLOADER_INPUTS_PER_PAGE: usize = WORDS_PER_PAGE + 1 + 4 + (MERKLE_TREE_DEPTH - 1) * 4;

pub const BOOTLOADER_SPECIFIC_INSTRUCTION_NAMES: [&str; 2] =
    ["load_bootloader_input", "jump_to_bootloader_input"];

pub fn bootloader_preamble() -> String {
    let mut preamble = r#"
    // ============== bootloader-specific instructions =======================
    // Write-once memory
    let BOOTLOADER_INPUT_ADDRESS = |i| i;
    let bootloader_input_value;
    // Loads a value. If the cell is empty, the prover can choose a value.
    instr load_bootloader_input X -> Y { {X, Y} in {BOOTLOADER_INPUT_ADDRESS, bootloader_input_value} }

    let tmp_bootloader_value;

    // Sets the PC to the bootloader input at the provided index if it is nonzero
    instr jump_to_bootloader_input X {
        // TODO: Putting {X, pc'} on the left-hand side should work, but this leads to a wrong PC update rule.
        {X, tmp_bootloader_value} in {BOOTLOADER_INPUT_ADDRESS, bootloader_input_value},
        pc' = tmp_bootloader_value
    }

    // Expose initial register values as public outputs
"#.to_string();

    for (i, reg) in REGISTER_NAMES.iter().enumerate() {
        let reg = reg.strip_prefix("main.").unwrap();
        preamble.push_str(&format!(
            "    public initial_{reg} = bootloader_input_value({i});\n"
        ));
    }
    preamble.push_str(&format!(
        r#"
    public initial_memory_hash_1 = bootloader_input_value({});
    public initial_memory_hash_2 = bootloader_input_value({});
    public initial_memory_hash_3 = bootloader_input_value({});
    public initial_memory_hash_4 = bootloader_input_value({});
"#,
        REGISTER_NAMES.len(),
        REGISTER_NAMES.len() + 1,
        REGISTER_NAMES.len() + 2,
        REGISTER_NAMES.len() + 3,
    ));
    preamble.push_str(&format!(
        r#"
    public final_memory_hash_1 = bootloader_input_value({});
    public final_memory_hash_2 = bootloader_input_value({});
    public final_memory_hash_3 = bootloader_input_value({});
    public final_memory_hash_4 = bootloader_input_value({});
"#,
        REGISTER_NAMES.len() + 4,
        REGISTER_NAMES.len() + 5,
        REGISTER_NAMES.len() + 6,
        REGISTER_NAMES.len() + 7,
    ));

    preamble
}

/// The bootloader: An assembly program that can be executed at the beginning a RISC-V execution.
/// It lets the prover provide arbitrary memory pages and writes them to memory, as well as values for
/// the registers (including the PC, which is set last).
/// This can be used to implement continuations. Note that this is completely non-sound. Progress to
/// make it sound is tracked in https://github.com/powdr-labs/powdr/issues/814.
/// Bootloader inputs are in the format:
/// - First 49 values: Values of x1-x31, tmp1-tmp4, lr_sc_reservation, P0-P11, and the PC
/// - The root hash of the memory Merkle tree (4 elements)
/// - The root hash of the memory Merkle tree *after this chunk's execution* (4 elements)
/// - Number of pages
/// - For each page:
///   - The page number
///   - The 256 words of the page
///   - The hash of the page *after* this chunk's execution
///   - For each level of the Merkle tree, except the root (1..=22):
///     - The hash (4 elements) of the sibling page
pub fn bootloader(submachine_initialization: &[String]) -> String {
    let mut bootloader = String::new();

    let memory_hash_start_index = REGISTER_NAMES.len();
    let num_pages_index = memory_hash_start_index + 8;
    let page_inputs_offset = num_pages_index + 1;

    bootloader.push_str(&format!(
        r#"
// Skip the next instruction
jump submachine_init;

// For convenience, this instruction has a known fixed PC ({DEFAULT_PC}) and just jumps
// to whatever comes after the bootloader. This avoids having to count the instructions
// of the bootloader and the submachine initialization.
jump end_of_bootloader;

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
// - x3: Currenr page number
// - x4: The ith bit of the page number (during Merkle proof validation)
// - x5-x8: The current memory hash
// - x9: 0: Merkle tree validation phase; 1: Merkle tree update phase
// - P0-P11: Hash registers:
//   - P0-P3 will usually contain the "current" hash (either in the context
//     of page hashing or Merkle proof validation)
//   - P4-P7 will contain some other inputs to the hash function
//   - P8-P11 will contain the capacity elements (0 throughout the execution)

// Number of pages
x1 <== load_bootloader_input({num_pages_index});
x1 <== wrap(x1);

// Initialize memory hash
x5 <== load_bootloader_input({memory_hash_start_index});
x6 <== load_bootloader_input({memory_hash_start_index} + 1);
x7 <== load_bootloader_input({memory_hash_start_index} + 2);
x8 <== load_bootloader_input({memory_hash_start_index} + 3);

// Current page index
x2 <=X= 0;

branch_if_zero x1, end_page_loop;

start_page_loop:

// Page number
x3 <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {page_inputs_offset});
x3 <== and(x3, {PAGE_NUMBER_MASK});

// Store & hash {WORDS_PER_PAGE} page words. This is an unrolled loop that for each each word:
// - Loads the word into the P{{(i % 4) + 4}} register
// - Stores the word at the address x3 * {PAGE_SIZE_BYTES} + i * {BYTES_PER_WORD}
// - If i % 4 == 3: Hashes registers P0-P11, storing the result in P0-P3
//
// At the end of the loop, we'll have a linear hash of the page in P0-P3, using a Merkle-Damgard
// construction. The initial P0-P3 values are 0, and the capacity (P8-P11) is 0 throughout the
// booloader execution.

P0 <=X= 0;
P1 <=X= 0;
P2 <=X= 0;
P3 <=X= 0;
P4 <=X= 0;
P5 <=X= 0;
P6 <=X= 0;
P7 <=X= 0;
"#,
    ));

    for i in 0..WORDS_PER_PAGE {
        let reg_index = (i % 4) + 4;
        bootloader.push_str(&format!(
            r#"
P{reg_index} <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {page_inputs_offset} + 1 + {i});
mstore_bootloader x3 * {PAGE_SIZE_BYTES} + {i} * {BYTES_PER_WORD}, P{reg_index};"#
        ));

        // Hash if buffer is full
        if i % 4 == 3 {
            bootloader.push_str(
                r#"
P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
"#,
            );
        }
    }

    bootloader.push_str(&format!(
        r#"
// == Merkle proof validation ==
// We commit to the memory content by hashing it in pages of {WORDS_PER_PAGE} words each.
// These hashes are stored in a binary Merkle tree of depth {MERKLE_TREE_DEPTH}.
// At this point, the current page hash is in P0-P3.
// 
// Now, we re-computed the Merkle root twice, in two phases:
// - First using the current page hash, as computed by the bootloader. The prover provides
//   the sibling values. At the end of this phase, the re-computed Merkle root is asserted
//   to be equal to the "current" Merkle root, stored in x5-x8.
// - Second, we repeat the same process (using the *same* siblings!), but using the claimed
//   updated page hash. At the end of this phase, the re-computed Merkle root stored as the
//   "current" Merkle root in x5-x8.
//
// So, any Merkle proof is expected to be based on the Merkle tree with all previous pages
// already updated. In the shutdown routine, we will validate that the final page hashes
// are as claimed. Also, at the end of the bootloader, we will assert that the final Merkle
// root is as claimed.

// Set phase to validation
x9 <=X= 0;

merkle_proof_validation_loop:

// This is an unrolled loop that for each level:
// - If the ith bit of the page number is 0:
//   - Load sibling into registers P4-P7
// - Else:
//   - Write registers P0-P3 to registers P4-P7
//   - Load sibling into registers P0-P3
// - Hash registers P0-P11, storing the result in P0-P3
//
// At the end of the loop, we'll have the Merkle root in P0-P3.
"#,
    ));

    for i in 0..N_LEAVES_LOG {
        let mask = 1 << i;
        bootloader.push_str(&format!(
            r#"
x4 <== and(x3, {mask});
branch_if_nonzero x4, level_{i}_is_right;
P4 <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {page_inputs_offset} + 1 + {WORDS_PER_PAGE} + 4 + {i} * 4 + 0);
P5 <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {page_inputs_offset} + 1 + {WORDS_PER_PAGE} + 4 + {i} * 4 + 1);
P6 <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {page_inputs_offset} + 1 + {WORDS_PER_PAGE} + 4 + {i} * 4 + 2);
P7 <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {page_inputs_offset} + 1 + {WORDS_PER_PAGE} + 4 + {i} * 4 + 3);
jump level_{i}_end;
level_{i}_is_right:
P4 <=X= P0;
P5 <=X= P1;
P6 <=X= P2;
P7 <=X= P3;
P0 <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {page_inputs_offset} + 1 + {WORDS_PER_PAGE} + 4 + {i} * 4 + 0);
P1 <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {page_inputs_offset} + 1 + {WORDS_PER_PAGE} + 4 + {i} * 4 + 1);
P2 <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {page_inputs_offset} + 1 + {WORDS_PER_PAGE} + 4 + {i} * 4 + 2);
P3 <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {page_inputs_offset} + 1 + {WORDS_PER_PAGE} + 4 + {i} * 4 + 3);
level_{i}_end:
P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
"#
        ));
    }

    bootloader.push_str(&format!(
        r#"
branch_if_nonzero x9, update_memory_hash;

// Assert Correct Merkle Root
branch_if_nonzero P0 - x5, memory_hash_mismatch;
branch_if_nonzero P1 - x6, memory_hash_mismatch;
branch_if_nonzero P2 - x7, memory_hash_mismatch;
branch_if_nonzero P3 - x8, memory_hash_mismatch;
jump memory_hash_ok;
memory_hash_mismatch:
fail;
memory_hash_ok:

// Set phase to update
x9 <=X= 1;

// Load claimed updated page hash into P0-P3
P0 <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {page_inputs_offset} + 1 + {WORDS_PER_PAGE} + 0);
P1 <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {page_inputs_offset} + 1 + {WORDS_PER_PAGE} + 1);
P2 <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {page_inputs_offset} + 1 + {WORDS_PER_PAGE} + 2);
P3 <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {page_inputs_offset} + 1 + {WORDS_PER_PAGE} + 3);

// Repeat Merkle proof validation loop to compute updated Merkle root
jump merkle_proof_validation_loop;

update_memory_hash:

x5 <=X= P0;
x6 <=X= P1;
x7 <=X= P2;
x8 <=X= P3;

// Increment page index
x2 <=X= x2 + 1;

branch_if_nonzero x2 - x1, start_page_loop;

end_page_loop:

// Assert final Merkle root is as claimed
P0 <== load_bootloader_input({memory_hash_start_index} + 4);
P1 <== load_bootloader_input({memory_hash_start_index} + 5);
P2 <== load_bootloader_input({memory_hash_start_index} + 6);
P3 <== load_bootloader_input({memory_hash_start_index} + 7);

branch_if_nonzero P0 - x5, final_memory_hash_mismatch;
branch_if_nonzero P1 - x6, final_memory_hash_mismatch;
branch_if_nonzero P2 - x7, final_memory_hash_mismatch;
branch_if_nonzero P3 - x8, final_memory_hash_mismatch;
jump final_memory_hash_ok;
final_memory_hash_mismatch:
fail;
final_memory_hash_ok:


// Initialize registers, starting with index 0
"#
    ));

    // Go over all registers except the PC
    let register_iter = REGISTER_NAMES.iter().take(REGISTER_NAMES.len() - 1);

    for (i, reg) in register_iter.enumerate() {
        let reg = reg.strip_prefix("main.").unwrap();
        bootloader.push_str(&format!(r#"{reg} <== load_bootloader_input({i});"#));
        bootloader.push('\n');
    }
    bootloader.push_str(&format!(
        r#"
// Default PC is 0, but we already started from 0, so in that case we do nothing.
// Otherwise, we jump to the PC.
jump_to_bootloader_input {PC_INDEX};

end_of_bootloader:
"#
    ));

    bootloader.push_str("\n// END OF BOOTLOADER\n");

    bootloader
}

/// The names of the registers in the order in which they are expected by the bootloader.
pub const REGISTER_NAMES: [&str; 49] = [
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
    "main.P0",
    "main.P1",
    "main.P2",
    "main.P3",
    "main.P4",
    "main.P5",
    "main.P6",
    "main.P7",
    "main.P8",
    "main.P9",
    "main.P10",
    "main.P11",
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
/// 3: jump end_of_bootloader
pub const DEFAULT_PC: u64 = 3;

pub fn default_register_values<T: FieldElement>() -> Vec<T> {
    let mut register_values = vec![T::zero(); REGISTER_NAMES.len()];
    register_values[PC_INDEX] = T::from(DEFAULT_PC);
    register_values
}

/// The bootloader input that is equivalent to not using a bootloader, i.e.:
/// - No pages are initialized
/// - All registers are set to 0 (including the PC, which causes the bootloader to do nothing)
pub fn default_input<T: FieldElement>(accessed_pages: &[u64]) -> Vec<T> {
    // Set all registers and the number of pages to zero
    let mut bootloader_inputs = default_register_values();

    if accessed_pages.is_empty() {
        bootloader_inputs.extend(MerkleTree::<T>::empty_hash());
        bootloader_inputs.extend(MerkleTree::<T>::empty_hash());
        bootloader_inputs.push(T::zero());
    } else {
        // TODO: We don't have a way to know the memory state *after* the execution.
        // For now, we'll just claim that the memory doesn't change.
        // This is fine for now, because the bootloader does not yet enforce that the memory
        // state is actually as claimed. In the future, the `accessed_pages` argument won't be
        // supported anymore (it's anyway only used by the benchmark).
        let merkle_tree = MerkleTree::<T>::new();
        bootloader_inputs.extend(merkle_tree.root_hash());
        bootloader_inputs.extend(merkle_tree.root_hash());
        bootloader_inputs.push((accessed_pages.len() as u64).into());
        for &page_index in accessed_pages.iter() {
            bootloader_inputs.push(page_index.into());
            let (page, page_hash, proof) = merkle_tree.get(page_index as usize);
            bootloader_inputs.extend(page);
            bootloader_inputs.extend(page_hash);
            for sibling in proof {
                bootloader_inputs.extend(sibling);
            }
        }
    }

    bootloader_inputs
}
