use number::FieldElement;

/// The bootloader: An assembly program that can be executed at the beginning a RISC-V execution.
/// It lets the prover provide arbitrary memory pages and writes them to memory, as well as values for
/// the registers (including the PC, which is set last).
/// This can be used to implement continuations. Note that this is completely non-sound as the prover
/// can provide arbitrary values. In the future, these should be exposed as public inputs (with Merkle
/// proofs for the memory pages).
/// Bootloader inputs are in the format:
/// - First 36 values: Values of x1-x31, tmp1-tmp3, lr_sc_reservation, and the PC
/// - Number of pages
/// - For each page:
///   - The start address
///   - The 256 words of the page
pub const BOOTLOADER: &str = r#"
// Number of pages
x1 <=X= ${ ("bootloader_input", 37) };
x1 <== wrap(x1);

// Current page index
x2 <=X= 0;

branch_if_zero x1, end_page_loop;

start_page_loop::

// Start address
x3 <=X= ${ ("bootloader_input", x2 * (256 + 1) + 37 + 1) };
x3 <== wrap(x3);

// Current word index
x4 <=X= 0;

start_word_loop::

// Store word
mstore x3 + x4 * 4, ${ ("bootloader_input", x2 * (256 + 1) + 37 + 2 + x4) };

// Increment word index
x4 <=X= x4 + 1;

branch_if_nonzero x4 - 256, start_word_loop;

end_word_loop::

// Increment page index
x2 <=X= x2 + 1;

branch_if_nonzero x2 - x1, start_page_loop;

end_page_loop::

// Initialize registers, starting with index 0
x1 <=X= ${ ("bootloader_input", 0) };
x2 <=X= ${ ("bootloader_input", 1) };
x3 <=X= ${ ("bootloader_input", 2) };
x4 <=X= ${ ("bootloader_input", 3) };
x5 <=X= ${ ("bootloader_input", 4) };
x6 <=X= ${ ("bootloader_input", 5) };
x7 <=X= ${ ("bootloader_input", 6) };
x8 <=X= ${ ("bootloader_input", 7) };
x9 <=X= ${ ("bootloader_input", 8) };
x10 <=X= ${ ("bootloader_input", 9) };
x11 <=X= ${ ("bootloader_input", 10) };
x12 <=X= ${ ("bootloader_input", 11) };
x13 <=X= ${ ("bootloader_input", 12) };
x14 <=X= ${ ("bootloader_input", 13) };
x15 <=X= ${ ("bootloader_input", 14) };
x16 <=X= ${ ("bootloader_input", 15) };
x17 <=X= ${ ("bootloader_input", 16) };
x18 <=X= ${ ("bootloader_input", 17) };
x19 <=X= ${ ("bootloader_input", 18) };
x20 <=X= ${ ("bootloader_input", 19) };
x21 <=X= ${ ("bootloader_input", 20) };
x22 <=X= ${ ("bootloader_input", 21) };
x23 <=X= ${ ("bootloader_input", 22) };
x24 <=X= ${ ("bootloader_input", 23) };
x25 <=X= ${ ("bootloader_input", 24) };
x26 <=X= ${ ("bootloader_input", 25) };
x27 <=X= ${ ("bootloader_input", 26) };
x28 <=X= ${ ("bootloader_input", 27) };
x29 <=X= ${ ("bootloader_input", 28) };
x30 <=X= ${ ("bootloader_input", 29) };
x31 <=X= ${ ("bootloader_input", 30) };
tmp1 <=X= ${ ("bootloader_input", 31) };
tmp2 <=X= ${ ("bootloader_input", 32) };
tmp3 <=X= ${ ("bootloader_input", 33) };
tmp4 <=X= ${ ("bootloader_input", 34) };
lr_sc_reservation <=X= ${ ("bootloader_input", 35) };

// Set the PC
jump_dyn ${ ("bootloader_input", 36) };
"#;

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

/// The bootloader input that is equivalent to not using a bootloader, i.e.:
/// - No pages are initialized
/// - All registers are set to 0
/// - The PC is set to 51 (the first instruction after the bootloader)
pub fn default_input<T: FieldElement>() -> Vec<T> {
    // 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,51,0
    let mut bootloader_inputs = vec![T::zero(); 38];
    // Dispatcher + Bootloader takes 51 instructions, so 51 is the first instruction after the bootloader.
    bootloader_inputs[36] = T::from(51u64);
    bootloader_inputs
}
