use openvm_transpiler::elf::Elf;
use powdr_riscv_elf::debug_info::SymbolTable;

// Re-export the core optimizer from the standalone crate.
pub use powdr_elf_optimizer::{optimize_instructions, parse_pc_base, parse_pc_base_data};

/// Optimize an ELF by replacing memcpy/memmove calls with constant-length arguments
/// with specialized unrolled routines. Modifies `elf.instructions` in place.
/// `pc_base` is the byte address of the first instruction (lowest executable segment vaddr).
pub fn optimize_elf(elf: &mut Elf, symbols: &mut SymbolTable, pc_base: u32) {
    optimize_instructions(&mut elf.instructions, symbols, pc_base);
}
