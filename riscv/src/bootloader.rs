use number::FieldElement;

// 32-Bit architecture -> 2^32 bytes of addressable memory
pub const MEMORY_SIZE_LOG: usize = 32;

// Page size is 1KB
pub const PAGE_SIZE_BYTES_LOG: usize = 10;

// 32-Bit architecture -> 4 bytes per word
pub const BYTES_PER_WORD: usize = 4;

/// The bootloader: An assembly program that can be executed at the beginning a RISC-V execution.
/// It lets the prover provide arbitrary memory pages and writes them to memory, as well as values for
/// the registers (including the PC, which is set last).
/// This can be used to implement continuations. Note that this is completely non-sound as the prover
/// can provide arbitrary values. In the future, these should be exposed as public inputs (with Merkle
/// proofs for the memory pages).
/// Bootloader inputs are in the format:
/// - First 37 values: Values of x1-x31, tmp1-tmp4, lr_sc_reservation, and the PC
/// - Number of pages
/// - For each page:
///   - The page number
///   - The 256 words of the page
pub fn bootloader() -> String {
    let mut bootloader = String::new();

    let num_registers = REGISTER_NAMES.len();
    let page_size_bytes = 1 << PAGE_SIZE_BYTES_LOG;
    let words_per_page = (1 << (PAGE_SIZE_BYTES_LOG)) / BYTES_PER_WORD;
    let merkle_tree_depth = MEMORY_SIZE_LOG - PAGE_SIZE_BYTES_LOG;
    let page_number_mask = (1 << merkle_tree_depth) - 1;

    bootloader.push_str(&format!(
        r#"
// START OF BOOTLOADER

// Number of pages
x1 <=X= ${{ ("bootloader_input", {num_registers}) }};
x1 <== wrap(x1);

// Current page index
x2 <=X= 0;

branch_if_zero x1, end_page_loop;

start_page_loop::

// Page number
x3 <=X= ${{ ("bootloader_input", x2 * ({words_per_page} + 1) + {num_registers} + 1) }};
x3 <== and(x3, {page_number_mask});

// Store & hash {words_per_page} page words. This is an unrolled loop that for each each word:
// - Loads the word into the P{{(i % 4) + 4}} register
// - Stores the word at the address x3 * {page_size_bytes} + i * {BYTES_PER_WORD}
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
"#
    ));

    for i in 0..words_per_page {
        let reg_index = (i % 4) + 4;
        bootloader.push_str(&format!(
            r#"
P{reg_index} <=X= ${{ ("bootloader_input", x2 * ({words_per_page} + 1) + {num_registers} + 2 + {i})}};
mstore x3 * {page_size_bytes} + {i} * {BYTES_PER_WORD}, P{reg_index};"#
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

    bootloader.push_str(
        r#"
// Simulate a Merkle proof, using 0 as the sibling hashes for now...
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
    );

    for i in 0..merkle_tree_depth {
        let mask = 1 << i;
        bootloader.push_str(&format!(
            r#"
x4 <== and(x3, {mask});
branch_if_nonzero x4, level_{i}_is_right;
P4 <=X= 0;
P5 <=X= 0;
P6 <=X= 0;
P7 <=X= 0;
jump level_{i}_end;
level_{i}_is_right::
P4 <=X= P0;
P5 <=X= P1;
P6 <=X= P2;
P7 <=X= P3;
P0 <=X= 0;
P1 <=X= 0;
P2 <=X= 0;
P3 <=X= 0;
level_{i}_end::
P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
"#
        ));
    }

    bootloader.push_str(
        r#"
// Increment page index
x2 <=X= x2 + 1;

branch_if_nonzero x2 - x1, start_page_loop;

end_page_loop::

// Initialize registers, starting with index 0
"#,
    );

    // Go over all registers except the PC
    let register_iter = REGISTER_NAMES.iter().take(REGISTER_NAMES.len() - 1);

    for (i, reg) in register_iter.enumerate() {
        let reg = reg.strip_prefix("main.").unwrap();
        bootloader.push_str(&format!(r#"{reg} <=X= ${{ ("bootloader_input", {i}) }};"#));
        bootloader.push('\n');
    }
    bootloader.push_str(&format!(
        r#"
// Default PC is 0, but we already started from 0, so in that case we do nothing.
// Otherwise, we jump to the PC.
jump_dyn_if_nonzero ${{ ("bootloader_input", {}) }};
"#,
        PC_INDEX
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

/// The bootloader input that is equivalent to not using a bootloader, i.e.:
/// - No pages are initialized
/// - All registers are set to 0 (including the PC, which causes the bootloader to do nothing)
pub fn default_input<T: FieldElement>() -> Vec<T> {
    // Set all registers and the number of pages to zero
    vec![T::zero(); REGISTER_NAMES.len() + 1]
}
