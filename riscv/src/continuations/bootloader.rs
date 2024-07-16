use powdr_number::FieldElement;
use powdr_riscv_executor::Elem;

use powdr_riscv_syscalls::SYSCALL_REGISTERS;

use super::memory_merkle_tree::MerkleTree;

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
pub const BOOTLOADER_INPUTS_PER_PAGE: usize = WORDS_PER_PAGE + 1 + 4 + (MERKLE_TREE_DEPTH - 1) * 4;
pub const MEMORY_HASH_START_INDEX: usize = 2 * REGISTER_NAMES.len();
pub const NUM_PAGES_INDEX: usize = MEMORY_HASH_START_INDEX + 8;
pub const PAGE_INPUTS_OFFSET: usize = NUM_PAGES_INDEX + 1;

/// Computes an upper bound of how long the shutdown routine will run, for a given number of pages.
pub fn shutdown_routine_upper_bound(num_pages: usize) -> usize {
    // Regardless of the number of pages, we have to:
    // - Jump to the start of the routine
    // - Assert all register values are correct (except the PC)
    // - Start the page loop
    // - Jump to shutdown sink
    let constant_overhead = 6 + REGISTER_NAMES.len() - 1;

    // For each page, we have to:
    // - Start the page loop (14 instructions)
    // - Load all words of the page
    // - Invoke the hash function once every 4 words
    // - Assert the page hash is as claimed (4 instructions)
    // - Increment the page index and jump back to the loop start (2 instructions)
    let cost_per_page = 14 + WORDS_PER_PAGE + WORDS_PER_PAGE / 4 + 4 + 2;

    constant_overhead + num_pages * cost_per_page
}

pub const BOOTLOADER_SPECIFIC_INSTRUCTION_NAMES: [&str; 2] =
    ["load_bootloader_input", "jump_to_bootloader_input"];

pub fn bootloader_preamble() -> String {
    let mut preamble = r#"
    // ============== bootloader-specific instructions =======================
    // Write-once memory
    std::machines::write_once_memory::WriteOnceMemory bootloader_inputs;

    instr load_bootloader_input X -> Y link => bootloader_inputs.access(X, Y);
    instr assert_bootloader_input X, Y -> link => bootloader_inputs.access(X, Y);

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
        let reg = reg.strip_prefix("main.").unwrap();
        preamble.push_str(&format!(
            "    //public initial_{reg} = main_bootloader_inputs.value({i});\n"
        ));
    }
    for (i, reg) in REGISTER_NAMES.iter().enumerate() {
        let reg = reg.strip_prefix("main.").unwrap();
        preamble.push_str(&format!(
            "    //public final_{reg} = main_bootloader_inputs.value({});\n",
            i + REGISTER_NAMES.len()
        ));
    }
    preamble.push_str(&format!(
        r#"
    //public initial_memory_hash_1 = main_bootloader_inputs.value({});
    //public initial_memory_hash_2 = main_bootloader_inputs.value({});
    //public initial_memory_hash_3 = main_bootloader_inputs.value({});
    //public initial_memory_hash_4 = main_bootloader_inputs.value({});
"#,
        MEMORY_HASH_START_INDEX,
        MEMORY_HASH_START_INDEX + 1,
        MEMORY_HASH_START_INDEX + 2,
        MEMORY_HASH_START_INDEX + 3,
    ));
    preamble.push_str(&format!(
        r#"
    //public final_memory_hash_1 = main_bootloader_inputs.value({});
    //public final_memory_hash_2 = main_bootloader_inputs.value({});
    //public final_memory_hash_3 = main_bootloader_inputs.value({});
    //public final_memory_hash_4 = main_bootloader_inputs.value({});
"#,
        MEMORY_HASH_START_INDEX + 4,
        MEMORY_HASH_START_INDEX + 5,
        MEMORY_HASH_START_INDEX + 6,
        MEMORY_HASH_START_INDEX + 7,
    ));

    preamble
}

// registers used by poseidon, for easier reference and string interpolation
static P0: &str = SYSCALL_REGISTERS[0];
static P1: &str = SYSCALL_REGISTERS[1];
static P2: &str = SYSCALL_REGISTERS[2];
static P3: &str = SYSCALL_REGISTERS[3];
static P4: &str = SYSCALL_REGISTERS[4];
static P5: &str = SYSCALL_REGISTERS[5];
static P6: &str = SYSCALL_REGISTERS[6];
static P7: &str = SYSCALL_REGISTERS[7];
static P8: &str = SYSCALL_REGISTERS[8];
static P9: &str = SYSCALL_REGISTERS[9];
static P10: &str = SYSCALL_REGISTERS[10];
static P11: &str = SYSCALL_REGISTERS[11];

/// The bootloader: An assembly program that can be executed at the beginning a RISC-V execution.
/// It lets the prover provide arbitrary memory pages and writes them to memory, as well as values for
/// the registers (including the PC, which is set last).
/// This can be used to implement continuations. Note that this is completely non-sound. Progress to
/// make it sound is tracked in https://github.com/powdr-labs/powdr/issues/814.
/// Bootloader inputs are in the format:
/// - First 49 values: Values of x1-x31, tmp1-tmp4, lr_sc_reservation, P0-P11, and the PC
/// - Second 49 values: The same values, but after this chunk's execution
/// - The root hash of the memory Merkle tree (4 elements)
/// - The root hash of the memory Merkle tree *after this chunk's execution* (4 elements)
/// - Number of pages
/// - For each page:
///   - The page number
///   - The 256 words of the page
///   - The hash of the page *after* this chunk's execution
///   - For each level of the Merkle tree, except the root (1..=22):
///     - The hash (4 elements) of the sibling page
pub fn bootloader_and_shutdown_routine(submachine_initialization: &[String]) -> String {
    let mut bootloader = String::new();

    bootloader.push_str(&format!(
        r#"
// Skip the next instruction
tmp1 <== jump(submachine_init);

// For convenience, this instruction has a known fixed PC ({DEFAULT_PC}) and just jumps
// to whatever comes after the bootloader + shutdown routine. This avoids having to count
// the instructions of the bootloader and the submachine initialization.
tmp1 <== jump(computation_start);

// Similarly, this instruction has a known fixed PC ({SHUTDOWN_START}) and just jumps
// to the shutdown routine.
tmp1 <== jump(shutdown_start);

shutdown_sink:
tmp1 <== jump(shutdown_sink);

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
// (NB. `SYSCALL_REGISTERS` are mapped into the `P*` variables)
// - x1: Number of pages (constant throughout the execution)
// - x2: Current page index
// - x3: Current page number
// - x4: The ith bit of the page number (during Merkle proof validation)
// - x18-x21: The current memory hash
// - x9: 0: Merkle tree validation phase; 1: Merkle tree update phase
// - P0-P11: Hash registers:
//   - P0-P3 will usually contain the "current" hash (either in the context
//     of page hashing or Merkle proof validation)
//   - P4-P7 will contain some other inputs to the hash function
//   - P8-P11 will contain the capacity elements (0 throughout the execution)

// Number of pages
x1 <== load_bootloader_input({NUM_PAGES_INDEX});
x1 <== wrap(x1);

// Initialize memory hash
x18 <== load_bootloader_input({MEMORY_HASH_START_INDEX});
x19 <== load_bootloader_input({MEMORY_HASH_START_INDEX} + 1);
x20 <== load_bootloader_input({MEMORY_HASH_START_INDEX} + 2);
x21 <== load_bootloader_input({MEMORY_HASH_START_INDEX} + 3);

// Current page index
x2 <=X= 0;

branch_if_zero x1, bootloader_end_page_loop;

bootloader_start_page_loop:

// Page number
x3 <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {PAGE_INPUTS_OFFSET});
x3 <== and(x3, {PAGE_NUMBER_MASK});

// Store & hash {WORDS_PER_PAGE} page words. This is an unrolled loop that for each each word:
// - Loads the word into the P{{(i % 4) + 4}} register
// - Stores the word at the address x3 * {PAGE_SIZE_BYTES} + i * {BYTES_PER_WORD}
// - If i % 4 == 3: Hashes registers P0-P11, storing the result in P0-P3
//
// At the end of the loop, we'll have a linear hash of the page in P0-P3, using a Merkle-Damgård
// construction. The initial P0-P3 values are 0, and the capacity (P8-P11) is 0 throughout the
// bootloader execution.

{P0} <=X= 0;
{P1} <=X= 0;
{P2} <=X= 0;
{P3} <=X= 0;
{P4} <=X= 0;
{P5} <=X= 0;
{P6} <=X= 0;
{P7} <=X= 0;
"#,
    ));

    for i in 0..WORDS_PER_PAGE {
        let reg = SYSCALL_REGISTERS[(i % 4) + 4];
        bootloader.push_str(&format!(
            r#"
{reg} <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {PAGE_INPUTS_OFFSET} + 1 + {i});
mstore_bootloader x3 * {PAGE_SIZE_BYTES} + {i} * {BYTES_PER_WORD}, {reg};"#
        ));

        // Hash if buffer is full
        if i % 4 == 3 {
            bootloader.push_str("poseidon_gl;");
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
//   to be equal to the "current" Merkle root, stored in x18-x21.
// - Second, we repeat the same process (using the *same* siblings!), but using the claimed
//   updated page hash. At the end of this phase, the re-computed Merkle root stored as the
//   "current" Merkle root in x18-x21.
//
// So, any Merkle proof is expected to be based on the Merkle tree with all previous pages
// already updated. In the shutdown routine, we will validate that the final page hashes
// are as claimed. Also, at the end of the bootloader, we will assert that the final Merkle
// root is as claimed.

// Set phase to validation
x9 <=X= 0;

bootloader_merkle_proof_validation_loop:

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
branch_if_nonzero x4, bootloader_level_{i}_is_right;
{P4} <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + 4 + {i} * 4 + 0);
{P5} <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + 4 + {i} * 4 + 1);
{P6} <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + 4 + {i} * 4 + 2);
{P7} <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + 4 + {i} * 4 + 3);
tmp1 <== jump(bootloader_level_{i}_end);
bootloader_level_{i}_is_right:
{P4} <=X= {P0};
{P5} <=X= {P1};
{P6} <=X= {P2};
{P7} <=X= {P3};
{P0} <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + 4 + {i} * 4 + 0);
{P1} <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + 4 + {i} * 4 + 1);
{P2} <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + 4 + {i} * 4 + 2);
{P3} <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + 4 + {i} * 4 + 3);
bootloader_level_{i}_end:
    poseidon_gl;
"#
        ));
    }

    bootloader.push_str(&format!(
        r#"
branch_if_nonzero x9, bootloader_update_memory_hash;

// Assert Correct Merkle Root
branch_if_nonzero {P0} - x18, bootloader_memory_hash_mismatch;
branch_if_nonzero {P1} - x19, bootloader_memory_hash_mismatch;
branch_if_nonzero {P2} - x20, bootloader_memory_hash_mismatch;
branch_if_nonzero {P3} - x21, bootloader_memory_hash_mismatch;
tmp1 <== jump(bootloader_memory_hash_ok);
bootloader_memory_hash_mismatch:
fail;
bootloader_memory_hash_ok:

// Set phase to update
x9 <=X= 1;

// Load claimed updated page hash into {P0}-{P3}
{P0} <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + 0);
{P1} <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + 1);
{P2} <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + 2);
{P3} <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {PAGE_INPUTS_OFFSET} + 1 + {WORDS_PER_PAGE} + 3);

// Repeat Merkle proof validation loop to compute updated Merkle root
tmp1 <== jump(bootloader_merkle_proof_validation_loop);

bootloader_update_memory_hash:

x18 <=X= {P0};
x19 <=X= {P1};
x20 <=X= {P2};
x21 <=X= {P3};

// Increment page index
x2 <=X= x2 + 1;

branch_if_nonzero x2 - x1, bootloader_start_page_loop;

bootloader_end_page_loop:

// Assert final Merkle root is as claimed
assert_bootloader_input {MEMORY_HASH_START_INDEX} + 4, x18;
assert_bootloader_input {MEMORY_HASH_START_INDEX} + 5, x19;
assert_bootloader_input {MEMORY_HASH_START_INDEX} + 6, x20;
assert_bootloader_input {MEMORY_HASH_START_INDEX} + 7, x21;

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
// - P0-P11: Hash registers, used to compute the page hash

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
        let reg = reg.strip_prefix("main.").unwrap();
        bootloader.push_str(&format!(
            "assert_bootloader_input {}, {reg};\n",
            i + REGISTER_NAMES.len()
        ));
    }

    bootloader.push_str(&format!(
        r#"
// Number of pages
x1 <== load_bootloader_input({NUM_PAGES_INDEX});
x1 <== wrap(x1);

// Current page index
x2 <=X= 0;

branch_if_zero x1, shutdown_end_page_loop;

shutdown_start_page_loop:

// Page number
x3 <== load_bootloader_input(x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {PAGE_INPUTS_OFFSET});
x3 <== and(x3, {PAGE_NUMBER_MASK});

// Store & hash {WORDS_PER_PAGE} page words. This is an unrolled loop that for each each word:
// - Loads the word at the address x3 * {PAGE_SIZE_BYTES} + i * {BYTES_PER_WORD}
//   into the P{{(i % 4) + 4}} register
// - If i % 4 == 3: Hashes registers P0-P11, storing the result in P0-P3
//
// At the end of the loop, we'll have a linear hash of the page in P0-P3, using a Merkle-Damgård
// construction. The initial P0-P3 values are 0, and the capacity (P8-P11) is 0 throughout the
// execution of the shutdown routine.

{P0} <=X= 0;
{P1} <=X= 0;
{P2} <=X= 0;
{P3} <=X= 0;
{P4} <=X= 0;
{P5} <=X= 0;
{P6} <=X= 0;
{P7} <=X= 0;
{P8} <=X= 0;
{P9} <=X= 0;
{P10} <=X= 0;
{P11} <=X= 0;
"#,
    ));

    for i in 0..WORDS_PER_PAGE {
        let reg = SYSCALL_REGISTERS[(i % 4) + 4];
        bootloader.push_str(&format!(
            "{reg}, x0 <== mload(x3 * {PAGE_SIZE_BYTES} + {i} * {BYTES_PER_WORD});\n"
        ));

        // Hash if buffer is full
        if i % 4 == 3 {
            bootloader.push_str("poseidon_gl;\n");
        }
    }

    bootloader.push_str(&format!(
        r#"

// Assert page hash is as claimed
// At this point, P0-P3 contain the actual page hash at the end of the execution.
assert_bootloader_input x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {PAGE_INPUTS_OFFSET} + {WORDS_PER_PAGE} + 1 + 0, {P0};
assert_bootloader_input x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {PAGE_INPUTS_OFFSET} + {WORDS_PER_PAGE} + 1 + 1, {P1};
assert_bootloader_input x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {PAGE_INPUTS_OFFSET} + {WORDS_PER_PAGE} + 1 + 2, {P2};
assert_bootloader_input x2 * {BOOTLOADER_INPUTS_PER_PAGE} + {PAGE_INPUTS_OFFSET} + {WORDS_PER_PAGE} + 1 + 3, {P3};

// Increment page index
x2 <=X= x2 + 1;

branch_if_nonzero x2 - x1, shutdown_start_page_loop;

shutdown_end_page_loop:


tmp1 <== jump(shutdown_sink);

// END OF SHUTDOWN ROUTINE

computation_start:
"#,
    ));

    bootloader
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
        inputs.extend(self.merkle_tree_root_hash.map(Elem::Field));
        inputs.extend(self.merkle_tree_root_hash.map(Elem::Field));
        inputs.push(Elem::Binary(self.pages.len() as i64));
        for page in self.pages {
            inputs.push(page.page_idx.into());
            inputs.extend(page.data.map(|v| Elem::new_from_fe_as_bin(&v)));
            inputs.extend(page.hash.map(Elem::Field));
            for sibling in page.proof {
                inputs.extend(sibling.map(Elem::Field));
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
