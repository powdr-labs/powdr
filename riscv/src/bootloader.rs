use number::FieldElement;

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
///   - The start address
///   - The 256 words of the page
pub fn bootloader() -> (String, usize) {
    let mut bootloader = String::new();
    let mut instructions = 0;
    let num_registers = REGISTER_NAMES.len();

    bootloader.push_str(&format!(
        r#"
// Number of pages
x1 <=X= ${{ ("bootloader_input", {num_registers}) }};
x1 <== wrap(x1);

// Current page index
x2 <=X= 0;

branch_if_zero x1, end_page_loop;

start_page_loop::

// Start address
x3 <=X= ${{ ("bootloader_input", x2 * (256 + 1) + {num_registers} + 1) }};
x3 <== wrap(x3);

// Store & hash 256 page words. This is an unrolled loop that for each each word:
// - Loads the word into the P{{(i % 4) + 4}} register
// - Stores the word at the address x3 + i * 4
// - If i % 4 == 3: Hashes registers P0-P11, storing the result in P0-P3
//
// At the end of the loop, we'll have a linear hash of the page in P0-P3, using a Merkle-Damgard
// construction. The initial P0-P3 values are 0, and the capacity (P8-P11) is 0 throughout the
// booloader execution.

P0 <=X= 0;
P1 <=X= 0;
P2 <=X= 0;
P3 <=X= 0;
"#
    ));
    instructions += 10;

    for i in 0..256 {
        // Store the word in registers
        let reg_index = (i % 4) + 4;
        bootloader.push_str(&format!(
            r#"
P{reg_index} <=X= ${{ ("bootloader_input", x2 * (256 + 1) + {num_registers} + 2 + {i})}};"#
        ));
        instructions += 1;

        // Write to memory
        bootloader.push_str(&format!(
            r#"
mstore x3 + {i} * 4, P{reg_index};"#
        ));
        instructions += 1;

        // Hash if buffer is full
        if i % 4 == 3 {
            bootloader.push_str(
                r#"
P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
"#,
            );
            instructions += 1;
        }
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
    instructions += 2;

    for (i, reg) in REGISTER_NAMES.iter().enumerate() {
        let reg = reg.strip_prefix("main.").unwrap();
        if i != PC_INDEX {
            bootloader.push_str(&format!(r#"{reg} <=X= ${{ ("bootloader_input", {i}) }};"#));
        } else {
            bootloader.push_str(&format!(r#"jump_dyn ${{ ("bootloader_input", {}) }};"#, i));
        }
        bootloader.push('\n');
        instructions += 1;
    }

    (bootloader, instructions)
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

/// The bootloader input that is equivalent to not using a bootloader, i.e.:
/// - No pages are initialized
/// - All registers are set to 0
/// - The PC is set to 51 (the first instruction after the bootloader)
pub fn default_input<T: FieldElement>() -> Vec<T> {
    // Set all registers and the number of pages to zero
    let mut bootloader_inputs = vec![T::zero(); REGISTER_NAMES.len() + 1];

    // PC should be set to the next instruction after the dispatcher (2 instructions) and bootloader
    let (_, num_instructions) = bootloader();
    bootloader_inputs[PC_INDEX] = T::from(num_instructions as u64 + 2);

    bootloader_inputs
}
