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
pub const BOOTLOADER_INPUTS_PER_PAGE: usize = WORDS_PER_PAGE + 1 + (MERKLE_TREE_DEPTH - 1) * 4;

pub const BOOTLOADER_SPECIFIC_INSTRUCTION_NAMES: [&str; 2] =
    ["load_bootloader_input", "jump_to_bootloader_input"];

pub fn bootloader_preamble() -> String {
    let mut preamble = r#"
    // ============== extra rules on memory when compiled with continuations =======================

    col witness m_is_bootloader_write;
    m_is_bootloader_write * (1 - m_is_bootloader_write) = 0;
    m_is_read * m_is_bootloader_write = 0;
    m_is_bootloader_write * m_is_write = 0;

    // The first operation has to be a write by the bootloader
    // TODO: Comment, and is this sound? Could we insert rows of nothing before the first read?
    m_change * (m_is_write' + m_is_read') = 0;

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


    /// Stores Z at address Y % 2**32. Y can be between 0 and 2**33.
    /// Y should be a multiple of 4, but this instruction does not enforce it.
    instr mstore_bootloader Y, Z {
        { X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000, STEP, Z } is m_is_bootloader_write { m_addr, m_step, m_value },
        // Wrap the addr value
        Y = (X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000) + wrap_bit * 2**32
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
/// - Number of pages
/// - For each page:
///   - The page number
///   - The 256 words of the page
///   - For each level of the Merkle tree, except the root (1..=22):
///     - The hash (4 elements) of the sibling page
pub fn bootloader(submachine_initialization: &[String]) -> String {
    let mut bootloader = String::new();

    let memory_hash_start_index = REGISTER_NAMES.len();
    let num_pages_index = memory_hash_start_index + 4;
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

// Number of pages
x1 <== load_bootloader_input({num_pages_index});
x1 <== wrap(x1);

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
// At this point, the current page hash is in P0-P3. In order to validate the Merkle proof,
// we need to re-compute the Merkle root from the prover-provided sibling page hashes.
//
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
P4 <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {page_inputs_offset} + 1 + {WORDS_PER_PAGE} + {i} * 4 + 0);
P5 <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {page_inputs_offset} + 1 + {WORDS_PER_PAGE} + {i} * 4 + 1);
P6 <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {page_inputs_offset} + 1 + {WORDS_PER_PAGE} + {i} * 4 + 2);
P7 <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {page_inputs_offset} + 1 + {WORDS_PER_PAGE} + {i} * 4 + 3);
jump level_{i}_end;
level_{i}_is_right:
P4 <=X= P0;
P5 <=X= P1;
P6 <=X= P2;
P7 <=X= P3;
P0 <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {page_inputs_offset} + 1 + {WORDS_PER_PAGE} + {i} * 4 + 0);
P1 <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {page_inputs_offset} + 1 + {WORDS_PER_PAGE} + {i} * 4 + 1);
P2 <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {page_inputs_offset} + 1 + {WORDS_PER_PAGE} + {i} * 4 + 2);
P3 <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {page_inputs_offset} + 1 + {WORDS_PER_PAGE} + {i} * 4 + 3);
level_{i}_end:
P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
"#
        ));
    }

    bootloader.push_str(&format!(
        r#"
// Assert Correct Merkle Root
// At this point, the re-computed Merkle root is in P0-P3 and P4-P7 are not needed anymore.
P4 <== load_bootloader_input({memory_hash_start_index});
P5 <== load_bootloader_input({memory_hash_start_index} + 1);
P6 <== load_bootloader_input({memory_hash_start_index} + 2);
P7 <== load_bootloader_input({memory_hash_start_index} + 3);
branch_if_nonzero P0 - P4, memory_hash_mismatch;
branch_if_nonzero P1 - P5, memory_hash_mismatch;
branch_if_nonzero P2 - P6, memory_hash_mismatch;
branch_if_nonzero P3 - P7, memory_hash_mismatch;
jump memory_hash_ok;
memory_hash_mismatch:
fail;
memory_hash_ok:

// Increment page index
x2 <=X= x2 + 1;

branch_if_nonzero x2 - x1, start_page_loop;

end_page_loop:

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
        bootloader_inputs.push(T::zero());
    } else {
        let merkle_tree = MerkleTree::<T>::new();
        bootloader_inputs.extend(merkle_tree.root_hash());
        bootloader_inputs.push((accessed_pages.len() as u64).into());
        for &page_index in accessed_pages.iter() {
            bootloader_inputs.push(page_index.into());
            let (page, proof) = merkle_tree.get(page_index as usize);
            bootloader_inputs.extend(page);
            for sibling in proof {
                bootloader_inputs.extend(sibling);
            }
        }
    }

    bootloader_inputs
}
