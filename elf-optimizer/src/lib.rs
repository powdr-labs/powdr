//! ELF optimizer library: memcpy/memmove/memcmp specialization and inline
//! bytecopy → word-copy transformations for RISC-V ELF binaries.

mod bytecopy;
mod call_site;
mod memcmp;
mod memcpy;
pub(crate) mod rv_insn;

use std::collections::BTreeMap;
use std::path::Path;

use powdr_riscv_elf::debug_info::SymbolTable;

use bytecopy::optimize_bytecopy;
use call_site::*;
use memcmp::{
    rv_generate_aligned_memcmp, rv_generate_optimized_memcmp_loop, rv_generate_specialized_memcmp,
};
use memcpy::{
    rv_generate_aligned_memcpy, rv_generate_aligned_memmove, rv_generate_specialized_memcpy,
    rv_generate_specialized_memmove,
};
use rv_insn::*;

/// Parse the base address (lowest executable segment vaddr) from a raw ELF binary.
pub fn parse_pc_base(elf_path: &Path) -> u32 {
    parse_pc_base_data(&std::fs::read(elf_path).expect("Failed to read ELF binary"))
}

/// Parse the base address (lowest executable segment vaddr) from raw ELF data.
pub fn parse_pc_base_data(data: &[u8]) -> u32 {
    assert!(data.len() > 52, "ELF too small");
    assert_eq!(&data[0..4], b"\x7fELF", "Not an ELF file");
    assert_eq!(data[4], 1, "Expected 32-bit ELF"); // EI_CLASS = ELFCLASS32

    let e_phoff = u32::from_le_bytes(data[28..32].try_into().unwrap()) as usize;
    let e_phentsize = u16::from_le_bytes(data[42..44].try_into().unwrap()) as usize;
    let e_phnum = u16::from_le_bytes(data[44..46].try_into().unwrap()) as usize;

    let mut min_vaddr = u32::MAX;
    for i in 0..e_phnum {
        let off = e_phoff + i * e_phentsize;
        let p_type = u32::from_le_bytes(data[off..off + 4].try_into().unwrap());
        let p_vaddr = u32::from_le_bytes(data[off + 8..off + 12].try_into().unwrap());
        let p_flags = u32::from_le_bytes(data[off + 24..off + 28].try_into().unwrap());
        // PT_LOAD = 1, PF_X = 1
        if p_type == 1 && (p_flags & 1) != 0 && p_vaddr < min_vaddr {
            min_vaddr = p_vaddr;
        }
    }
    assert_ne!(min_vaddr, u32::MAX, "No executable segment found");
    min_vaddr
}

/// Optimize a RISC-V ELF binary in memory.
///
/// Takes raw ELF bytes, applies all optimizations (memcpy/memmove/memcmp
/// specialization and inline byte-copy → word-copy), and returns the patched
/// ELF binary with optimizer-generated symbols embedded in `.symtab`/`.strtab`.
/// The returned binary can be written directly to a file or passed to
/// `powdr_riscv_elf::load_elf_from_buffer` to obtain the full symbol table.
pub fn optimize_elf(data: &[u8]) -> Vec<u8> {
    let (pc_base, mut instructions, text_offsets) = extract_instructions(data);
    let original_count = instructions.len();
    let mut symbols = build_symbol_table(data);

    optimize_instructions(&mut instructions, &mut symbols, pc_base);

    let appended = instructions.len() - original_count;
    let patched = patch_elf(data, &instructions, original_count, appended, &text_offsets);
    let new_sym_min_addr = pc_base + (original_count as u32) * 4;
    embed_extra_symbols(patched, &symbols, new_sym_min_addr)
}

/// Result of optimizing ELF instructions, providing access to both the
/// instruction streams and the symbol table for further processing (e.g.
/// generating HTML diffs or disassembly output).
pub struct OptimizedElf {
    /// Base address (lowest executable segment vaddr).
    pub pc_base: u32,
    /// Original instruction stream before optimization.
    pub original_instructions: Vec<u32>,
    /// Optimized instruction stream (may be longer due to appended routines).
    pub instructions: Vec<u32>,
    /// Symbol table (includes synthetic symbols for generated routines).
    pub symbols: SymbolTable,
    /// Mapping from instruction index to file offset in the original ELF.
    text_offsets: Vec<(usize, usize)>,
}

impl OptimizedElf {
    /// Number of instructions that were appended (generated routines).
    pub fn appended_count(&self) -> usize {
        self.instructions.len() - self.original_instructions.len()
    }

    /// Produce a patched ELF binary from the optimization result.
    ///
    /// Optimizer-generated symbols are embedded in the ELF's `.symtab`/`.strtab`
    /// so that any subsequent `load_elf_from_buffer` call sees the full symbol table.
    pub fn to_patched_elf(&self, original_data: &[u8]) -> Vec<u8> {
        let patched = patch_elf(
            original_data,
            &self.instructions,
            self.original_instructions.len(),
            self.appended_count(),
            &self.text_offsets,
        );
        let new_sym_min_addr = self.pc_base + (self.original_instructions.len() as u32) * 4;
        embed_extra_symbols(patched, &self.symbols, new_sym_min_addr)
    }
}

/// Optimize a RISC-V ELF binary, returning the full optimization result
/// including both instruction streams and the symbol table.
///
/// Use this when you need access to the intermediate data (e.g. for
/// generating HTML diffs or disassembly). For simple patching, use
/// [`optimize_elf`] instead.
pub fn optimize_elf_detailed(data: &[u8]) -> OptimizedElf {
    let (pc_base, mut instructions, text_offsets) = extract_instructions(data);
    let original_instructions = instructions.clone();
    let mut symbols = build_symbol_table(data);

    optimize_instructions(&mut instructions, &mut symbols, pc_base);

    OptimizedElf {
        pc_base,
        original_instructions,
        instructions,
        symbols,
        text_offsets,
    }
}

/// Extract 32-bit instructions from all executable PT_LOAD segments of a raw ELF.
/// Returns (pc_base, instructions, text_file_offsets) where text_file_offsets
/// maps instruction index to file offset (for binary patching).
fn extract_instructions(data: &[u8]) -> (u32, Vec<u32>, Vec<(usize, usize)>) {
    assert!(data.len() > 52, "ELF too small");
    assert_eq!(&data[0..4], b"\x7fELF", "Not an ELF file");

    let pc_base = parse_pc_base_data(data);

    let e_phoff = u32::from_le_bytes(data[28..32].try_into().unwrap()) as usize;
    let e_phentsize = u16::from_le_bytes(data[42..44].try_into().unwrap()) as usize;
    let e_phnum = u16::from_le_bytes(data[44..46].try_into().unwrap()) as usize;

    let mut segments: Vec<(u32, usize, usize)> = Vec::new();
    for i in 0..e_phnum {
        let off = e_phoff + i * e_phentsize;
        let p_type = u32::from_le_bytes(data[off..off + 4].try_into().unwrap());
        let p_offset = u32::from_le_bytes(data[off + 4..off + 8].try_into().unwrap()) as usize;
        let p_vaddr = u32::from_le_bytes(data[off + 8..off + 12].try_into().unwrap());
        let p_filesz = u32::from_le_bytes(data[off + 16..off + 20].try_into().unwrap()) as usize;
        let p_flags = u32::from_le_bytes(data[off + 24..off + 28].try_into().unwrap());
        if p_type == 1 && (p_flags & 1) != 0 {
            segments.push((p_vaddr, p_offset, p_filesz));
        }
    }

    segments.sort_by_key(|(vaddr, _, _)| *vaddr);

    let mut instructions = Vec::new();
    let mut text_offsets = Vec::new();

    for (seg_vaddr, seg_file_offset, seg_size) in &segments {
        let insn_start_in_stream = ((*seg_vaddr - pc_base) / 4) as usize;

        while instructions.len() < insn_start_in_stream {
            text_offsets.push((instructions.len(), 0));
            instructions.push(0x00000013); // NOP
        }

        let num_insns = seg_size / 4;
        for j in 0..num_insns {
            let file_off = seg_file_offset + j * 4;
            let word = u32::from_le_bytes(data[file_off..file_off + 4].try_into().unwrap());
            text_offsets.push((instructions.len(), file_off));
            instructions.push(word);
        }
    }

    (pc_base, instructions, text_offsets)
}

/// Build a symbol table from the ELF's symbol table section.
fn build_symbol_table(data: &[u8]) -> SymbolTable {
    let elf = goblin::elf::Elf::parse(data).expect("Failed to parse ELF");
    let mut table: BTreeMap<u32, Vec<String>> = BTreeMap::new();
    for sym in &elf.syms {
        if sym.st_name == 0 {
            continue;
        }
        if let Some(name) = elf.strtab.get_at(sym.st_name) {
            let addr = sym.st_value as u32;
            table.entry(addr).or_default().push(name.to_string());
        }
    }
    SymbolTable::from_table(table)
}

/// Produce a patched ELF binary by writing optimized instructions back and
/// extending the last executable segment for any appended routines.
///
/// Appended instructions are spliced into the file right after the text
/// segment, and all subsequent segment/section offsets are shifted forward
/// so that the resulting ELF is structurally valid.
fn patch_elf(
    data: &[u8],
    instructions: &[u32],
    original_count: usize,
    appended: usize,
    text_offsets: &[(usize, usize)],
) -> Vec<u8> {
    let mut patched = data.to_vec();

    // Step 1: Patch existing instructions in-place.
    for &(insn_idx, file_offset) in text_offsets {
        if insn_idx < instructions.len() && file_offset > 0 {
            let word = instructions[insn_idx];
            patched[file_offset..file_offset + 4].copy_from_slice(&word.to_le_bytes());
        }
    }

    if appended == 0 {
        return patched;
    }

    let extra_bytes = (appended as u32) * 4;
    let extra_bytes_usize = extra_bytes as usize;

    let e_phoff = u32::from_le_bytes(data[28..32].try_into().unwrap()) as usize;
    let e_phentsize = u16::from_le_bytes(data[42..44].try_into().unwrap()) as usize;
    let e_phnum = u16::from_le_bytes(data[44..46].try_into().unwrap()) as usize;

    // Find the last executable segment (highest vaddr end).
    let mut last_exec_ph: Option<usize> = None;
    let mut last_exec_file_end: usize = 0;
    let mut last_exec_vaddr_end: u32 = 0;

    for i in 0..e_phnum {
        let off = e_phoff + i * e_phentsize;
        let p_type = u32::from_le_bytes(patched[off..off + 4].try_into().unwrap());
        let p_offset = u32::from_le_bytes(patched[off + 4..off + 8].try_into().unwrap()) as usize;
        let p_vaddr = u32::from_le_bytes(patched[off + 8..off + 12].try_into().unwrap());
        let p_filesz = u32::from_le_bytes(patched[off + 16..off + 20].try_into().unwrap()) as usize;
        let p_memsz = u32::from_le_bytes(patched[off + 20..off + 24].try_into().unwrap());
        let p_flags = u32::from_le_bytes(patched[off + 24..off + 28].try_into().unwrap());
        if p_type == 1 && (p_flags & 1) != 0 {
            let end = p_vaddr + p_memsz;
            if end >= last_exec_vaddr_end {
                last_exec_vaddr_end = end;
                last_exec_file_end = p_offset + p_filesz;
                last_exec_ph = Some(i);
            }
        }
    }

    let Some(ph_idx) = last_exec_ph else {
        return patched;
    };

    // Step 2: Serialize appended instructions.
    let mut extra_data = Vec::with_capacity(extra_bytes_usize);
    for insn in instructions.iter().skip(original_count) {
        extra_data.extend_from_slice(&insn.to_le_bytes());
    }

    // Step 3: Splice the extra bytes into the file at the insertion point
    // (right after the text segment), shifting everything after it.
    let insert_pos = last_exec_file_end;
    patched.splice(insert_pos..insert_pos, extra_data);

    // Step 4: Update the text segment's p_filesz and p_memsz.
    let ph_off = e_phoff + ph_idx * e_phentsize;
    let old_filesz = u32::from_le_bytes(patched[ph_off + 16..ph_off + 20].try_into().unwrap());
    let old_memsz = u32::from_le_bytes(patched[ph_off + 20..ph_off + 24].try_into().unwrap());
    patched[ph_off + 16..ph_off + 20].copy_from_slice(&(old_filesz + extra_bytes).to_le_bytes());
    patched[ph_off + 20..ph_off + 24].copy_from_slice(&(old_memsz + extra_bytes).to_le_bytes());

    // Step 5: Shift p_offset of all program headers that come after the
    // insertion point in the file.
    for i in 0..e_phnum {
        if i == ph_idx {
            continue;
        }
        let off = e_phoff + i * e_phentsize;
        let p_offset = u32::from_le_bytes(patched[off + 4..off + 8].try_into().unwrap());
        if p_offset as usize >= insert_pos {
            patched[off + 4..off + 8]
                .copy_from_slice(&(p_offset + extra_bytes).to_le_bytes());
        }
    }

    // Step 6: Update section header table offset (e_shoff) in ELF header.
    let e_shoff = u32::from_le_bytes(patched[32..36].try_into().unwrap()) as usize;
    let e_shentsize = u16::from_le_bytes(patched[46..48].try_into().unwrap()) as usize;
    let e_shnum = u16::from_le_bytes(patched[48..50].try_into().unwrap()) as usize;

    if e_shoff > 0 && e_shoff >= insert_pos {
        let new_shoff = (e_shoff + extra_bytes_usize) as u32;
        patched[32..36].copy_from_slice(&new_shoff.to_le_bytes());
    }

    // Step 7: Shift sh_offset of all section headers that come after the
    // insertion point, and extend the .text section's sh_size.
    let actual_shoff = u32::from_le_bytes(patched[32..36].try_into().unwrap()) as usize;
    if actual_shoff > 0 && e_shentsize >= 40 {
        for i in 0..e_shnum {
            let sh_off = actual_shoff + i * e_shentsize;
            if sh_off + 40 > patched.len() {
                break;
            }

            // Shift sh_offset for sections after the insertion point.
            let sh_file_offset =
                u32::from_le_bytes(patched[sh_off + 16..sh_off + 20].try_into().unwrap());
            if sh_file_offset as usize >= insert_pos {
                patched[sh_off + 16..sh_off + 20]
                    .copy_from_slice(&(sh_file_offset + extra_bytes).to_le_bytes());
            }

            // Extend .text section (SHT_PROGBITS with SHF_EXECINSTR whose
            // end matches the original text segment end).
            let sh_type =
                u32::from_le_bytes(patched[sh_off + 4..sh_off + 8].try_into().unwrap());
            let sh_flags =
                u32::from_le_bytes(patched[sh_off + 8..sh_off + 12].try_into().unwrap());
            let sh_addr =
                u32::from_le_bytes(patched[sh_off + 12..sh_off + 16].try_into().unwrap());
            let sh_size =
                u32::from_le_bytes(patched[sh_off + 20..sh_off + 24].try_into().unwrap());
            if sh_type == 1
                && (sh_flags & 4) != 0
                && sh_addr + sh_size == last_exec_vaddr_end
            {
                patched[sh_off + 20..sh_off + 24]
                    .copy_from_slice(&(sh_size + extra_bytes).to_le_bytes());
            }
        }
    }

    patched
}

/// Shift all file offsets stored inside the ELF that are >= `threshold` by `delta`.
///
/// Updates: `e_shoff` in the ELF header, `p_offset` in every program header,
/// and `sh_offset` in every section header.  Does NOT update `sh_size` or
/// segment size fields — those must be handled separately by the caller.
fn shift_elf_offsets(patched: &mut Vec<u8>, threshold: usize, delta: usize) {
    let e_phoff = u32::from_le_bytes(patched[28..32].try_into().unwrap()) as usize;
    let e_phentsize = u16::from_le_bytes(patched[42..44].try_into().unwrap()) as usize;
    let e_phnum = u16::from_le_bytes(patched[44..46].try_into().unwrap()) as usize;

    // ELF header: e_shoff
    let e_shoff = u32::from_le_bytes(patched[32..36].try_into().unwrap()) as usize;
    if e_shoff >= threshold {
        patched[32..36].copy_from_slice(&((e_shoff + delta) as u32).to_le_bytes());
    }

    // Program headers: p_offset
    for i in 0..e_phnum {
        let off = e_phoff + i * e_phentsize;
        let p_offset = u32::from_le_bytes(patched[off + 4..off + 8].try_into().unwrap()) as usize;
        if p_offset >= threshold {
            patched[off + 4..off + 8].copy_from_slice(&((p_offset + delta) as u32).to_le_bytes());
        }
    }

    // Section headers: sh_offset  (re-read e_shoff — it may have been updated above)
    let e_shoff2 = u32::from_le_bytes(patched[32..36].try_into().unwrap()) as usize;
    let e_shentsize = u16::from_le_bytes(patched[46..48].try_into().unwrap()) as usize;
    let e_shnum = u16::from_le_bytes(patched[48..50].try_into().unwrap()) as usize;
    if e_shoff2 > 0 && e_shentsize >= 40 {
        for i in 0..e_shnum {
            let sh_off = e_shoff2 + i * e_shentsize;
            if sh_off + 20 > patched.len() {
                break;
            }
            let sh_file_offset =
                u32::from_le_bytes(patched[sh_off + 16..sh_off + 20].try_into().unwrap())
                    as usize;
            if sh_file_offset >= threshold {
                patched[sh_off + 16..sh_off + 20]
                    .copy_from_slice(&((sh_file_offset + delta) as u32).to_le_bytes());
            }
        }
    }
}

/// Append optimizer-generated symbols (those with address >= `new_sym_min_addr`)
/// directly into the ELF's `.symtab` and `.strtab` sections.
///
/// After this call, `powdr_riscv_elf::load_elf_from_buffer` will find the
/// generated routines in the symbol table without any separate side-channel.
fn embed_extra_symbols(mut patched: Vec<u8>, symbols: &SymbolTable, new_sym_min_addr: u32) -> Vec<u8> {
    // Collect new symbols in address order (BTreeMap guarantees this).
    let new_symbols: Vec<(u32, String)> = symbols
        .table()
        .range(new_sym_min_addr..)
        .flat_map(|(addr, names)| names.iter().map(move |n| (*addr, n.clone())))
        .collect();

    if new_symbols.is_empty() {
        return patched;
    }

    // Read section header table metadata.
    let e_shoff = u32::from_le_bytes(patched[32..36].try_into().unwrap()) as usize;
    let e_shentsize = u16::from_le_bytes(patched[46..48].try_into().unwrap()) as usize;
    let e_shnum = u16::from_le_bytes(patched[48..50].try_into().unwrap()) as usize;
    if e_shoff == 0 || e_shentsize < 40 || e_shnum == 0 {
        return patched;
    }

    // Find .symtab (SHT_SYMTAB = 2) and the .text section (SHT_PROGBITS=1 + SHF_EXECINSTR=4).
    let mut symtab_idx: Option<usize> = None;
    let mut text_section_idx: u16 = 1;
    for i in 0..e_shnum {
        let sh_off = e_shoff + i * e_shentsize;
        if sh_off + 40 > patched.len() {
            break;
        }
        let sh_type = u32::from_le_bytes(patched[sh_off + 4..sh_off + 8].try_into().unwrap());
        let sh_flags = u32::from_le_bytes(patched[sh_off + 8..sh_off + 12].try_into().unwrap());
        if sh_type == 2 {
            symtab_idx = Some(i);
        }
        // SHT_PROGBITS with SHF_EXECINSTR → .text
        if sh_type == 1 && (sh_flags & 4) != 0 {
            text_section_idx = i as u16;
        }
    }
    let symtab_idx = match symtab_idx {
        Some(i) => i,
        None => return patched,
    };

    // Read symtab section fields.
    let symtab_sh_off = e_shoff + symtab_idx * e_shentsize;
    let strtab_idx =
        u32::from_le_bytes(patched[symtab_sh_off + 24..symtab_sh_off + 28].try_into().unwrap())
            as usize;
    let symtab_file_offset =
        u32::from_le_bytes(patched[symtab_sh_off + 16..symtab_sh_off + 20].try_into().unwrap())
            as usize;
    let symtab_size =
        u32::from_le_bytes(patched[symtab_sh_off + 20..symtab_sh_off + 24].try_into().unwrap())
            as usize;

    // Read strtab section fields.
    let strtab_sh_off = e_shoff + strtab_idx * e_shentsize;
    let strtab_file_offset =
        u32::from_le_bytes(patched[strtab_sh_off + 16..strtab_sh_off + 20].try_into().unwrap())
            as usize;
    let strtab_size =
        u32::from_le_bytes(patched[strtab_sh_off + 20..strtab_sh_off + 24].try_into().unwrap())
            as usize;

    // ── Step 1: extend .strtab ────────────────────────────────────────────────
    // Build new string-table bytes: each name followed by a null terminator.
    let mut new_strtab: Vec<u8> = Vec::new();
    let mut name_offsets: Vec<u32> = Vec::new();
    for (_, name) in &new_symbols {
        name_offsets.push((strtab_size + new_strtab.len()) as u32);
        new_strtab.extend_from_slice(name.as_bytes());
        new_strtab.push(0);
    }
    let strtab_delta = new_strtab.len();

    // Insert new strtab bytes immediately after the existing strtab data.
    let strtab_insert_pos = strtab_file_offset + strtab_size;
    patched.splice(strtab_insert_pos..strtab_insert_pos, new_strtab);

    // Shift all ELF offsets that land at or after the insertion point.
    shift_elf_offsets(&mut patched, strtab_insert_pos, strtab_delta);

    // Update sh_size of the strtab section (offset may have been shifted; re-read e_shoff).
    {
        let e_shoff2 = u32::from_le_bytes(patched[32..36].try_into().unwrap()) as usize;
        let sh_off = e_shoff2 + strtab_idx * e_shentsize;
        let old_size =
            u32::from_le_bytes(patched[sh_off + 20..sh_off + 24].try_into().unwrap());
        patched[sh_off + 20..sh_off + 24]
            .copy_from_slice(&(old_size + strtab_delta as u32).to_le_bytes());
    }

    // ── Step 2: extend .symtab ────────────────────────────────────────────────
    // Build new Elf32_Sym entries (16 bytes each).
    // st_info = 0x12 → STB_GLOBAL (1) << 4 | STT_FUNC (2)
    let mut new_symtab: Vec<u8> = Vec::with_capacity(new_symbols.len() * 16);
    for (i, (addr, _)) in new_symbols.iter().enumerate() {
        let next_addr = new_symbols.get(i + 1).map(|(a, _)| *a).unwrap_or(*addr);
        let size: u32 = if next_addr > *addr { next_addr - addr } else { 0 };
        new_symtab.extend_from_slice(&name_offsets[i].to_le_bytes()); // st_name
        new_symtab.extend_from_slice(&addr.to_le_bytes()); // st_value
        new_symtab.extend_from_slice(&size.to_le_bytes()); // st_size
        new_symtab.push(0x12); // st_info: STB_GLOBAL | STT_FUNC
        new_symtab.push(0x00); // st_other: default visibility
        new_symtab.extend_from_slice(&text_section_idx.to_le_bytes()); // st_shndx
    }
    let symtab_delta = new_symtab.len();

    // Locate the (possibly shifted) end of the symtab section.
    let e_shoff3 = u32::from_le_bytes(patched[32..36].try_into().unwrap()) as usize;
    let symtab_sh_off2 = e_shoff3 + symtab_idx * e_shentsize;
    let symtab_file_offset2 =
        u32::from_le_bytes(patched[symtab_sh_off2 + 16..symtab_sh_off2 + 20].try_into().unwrap())
            as usize;
    let symtab_size2 =
        u32::from_le_bytes(patched[symtab_sh_off2 + 20..symtab_sh_off2 + 24].try_into().unwrap())
            as usize;

    let symtab_insert_pos = symtab_file_offset2 + symtab_size2;
    patched.splice(symtab_insert_pos..symtab_insert_pos, new_symtab);

    shift_elf_offsets(&mut patched, symtab_insert_pos, symtab_delta);

    // Update sh_size of the symtab section.
    {
        let e_shoff4 = u32::from_le_bytes(patched[32..36].try_into().unwrap()) as usize;
        let sh_off = e_shoff4 + symtab_idx * e_shentsize;
        let old_size =
            u32::from_le_bytes(patched[sh_off + 20..sh_off + 24].try_into().unwrap());
        patched[sh_off + 20..sh_off + 24]
            .copy_from_slice(&(old_size + symtab_delta as u32).to_le_bytes());
    }

    // Suppress unused-variable warnings for fields read before the splices.
    let _ = (symtab_file_offset, symtab_size, strtab_file_offset);

    patched
}
/// This is the core optimizer that replaces memcpy/memmove/memcmp calls and inline
/// byte-copy sequences with specialized routines.
pub fn optimize_instructions(instructions: &mut Vec<u32>, symbols: &mut SymbolTable, pc_base: u32) {
    let original_insn_count = instructions.len();
    let elf_end = pc_base + (original_insn_count as u32) * 4;
    println!(
        "memcpy_optimizer: pc_base=0x{pc_base:x}, {} instructions (range 0x{pc_base:x}..0x{elf_end:x})",
        instructions.len()
    );

    // Collect all addresses for each function family (there may be wrappers/aliases)
    let memcpy_addrs = find_symbols(symbols, "memcpy");
    let memmove_addrs = find_symbols(symbols, "memmove");
    // Filter memcmp symbols to avoid matching memcpy/memmove
    let memcmp_addrs: Vec<_> = find_symbols(symbols, "memcmp")
        .into_iter()
        .filter(|(addr, _)| {
            // Exclude addresses that are already claimed by memcpy or memmove
            !memcpy_addrs.iter().any(|(a, _)| *a == *addr)
                && !memmove_addrs.iter().any(|(a, _)| *a == *addr)
        })
        .collect();

    // Process memcpy
    if memcpy_addrs.is_empty() {
        println!("memcpy_optimizer: no memcpy symbol found");
    } else {
        for (addr, name) in &memcpy_addrs {
            println!("memcpy_optimizer: memcpy at addr 0x{addr:x} ({name})");
        }
        let target_addrs: Vec<u32> = memcpy_addrs.iter().map(|(a, _)| *a).collect();
        let (aligned, unaligned, _dyn_sites) =
            scan_call_sites_with_alignment(instructions, pc_base, &target_addrs);
        if aligned.is_empty() && unaligned.is_empty() {
            println!("memcpy_optimizer: no eligible memcpy calls found");
        } else {
            if !aligned.is_empty() {
                println!(
                    "memcpy_optimizer: memcpy ALIGNED call sites: {:?}",
                    aligned
                        .iter()
                        .map(|(len, sites)| (*len, sites.len()))
                        .collect::<Vec<_>>()
                );
                generate_and_patch(
                    instructions,
                    symbols,
                    pc_base,
                    &aligned,
                    rv_generate_aligned_memcpy,
                    "memcpy_aligned",
                );
            }
            if !unaligned.is_empty() {
                println!(
                    "memcpy_optimizer: memcpy unaligned call sites: {:?}",
                    unaligned
                        .iter()
                        .map(|(len, sites)| (*len, sites.len()))
                        .collect::<Vec<_>>()
                );
                generate_and_patch(
                    instructions,
                    symbols,
                    pc_base,
                    &unaligned,
                    rv_generate_specialized_memcpy,
                    "memcpy_opt",
                );
            }
        }
    }

    // Process memmove
    if memmove_addrs.is_empty() {
        println!("memcpy_optimizer: no memmove symbol found");
    } else {
        for (addr, name) in &memmove_addrs {
            println!("memcpy_optimizer: memmove at addr 0x{addr:x} ({name})");
        }
        let target_addrs: Vec<u32> = memmove_addrs.iter().map(|(a, _)| *a).collect();
        let (aligned, unaligned, _dyn_sites) =
            scan_call_sites_with_alignment(instructions, pc_base, &target_addrs);
        if aligned.is_empty() && unaligned.is_empty() {
            println!("memcpy_optimizer: no eligible memmove calls found");
        } else {
            if !aligned.is_empty() {
                println!(
                    "memcpy_optimizer: memmove ALIGNED call sites: {:?}",
                    aligned
                        .iter()
                        .map(|(len, sites)| (*len, sites.len()))
                        .collect::<Vec<_>>()
                );
                generate_and_patch(
                    instructions,
                    symbols,
                    pc_base,
                    &aligned,
                    rv_generate_aligned_memmove,
                    "memmove_aligned",
                );
            }
            if !unaligned.is_empty() {
                println!(
                    "memcpy_optimizer: memmove unaligned call sites: {:?}",
                    unaligned
                        .iter()
                        .map(|(len, sites)| (*len, sites.len()))
                        .collect::<Vec<_>>()
                );
                generate_and_patch(
                    instructions,
                    symbols,
                    pc_base,
                    &unaligned,
                    rv_generate_specialized_memmove,
                    "memmove_opt",
                );
            }
        }
    }

    // Analyze and patch memcmp call sites
    if memcmp_addrs.is_empty() {
        println!("memcpy_optimizer: no memcmp symbol found");
    } else {
        for (addr, name) in &memcmp_addrs {
            println!("memcpy_optimizer: memcmp at addr 0x{addr:x} ({name})");
        }
        let target_addrs: Vec<u32> = memcmp_addrs.iter().map(|(a, _)| *a).collect();

        for &addr in &target_addrs {
            debug_scan_calls(instructions, pc_base, addr, "memcmp");
        }
        analyze_a2_constants(instructions, pc_base, &target_addrs, "memcmp");

        let (aligned_const, unaligned_const, dyn_calls) =
            scan_call_sites_with_alignment(instructions, pc_base, &target_addrs);

        // Patch aligned constant-length call sites
        if !aligned_const.is_empty() {
            println!(
                "memcpy_optimizer: memcmp ALIGNED constant-length call sites: {:?}",
                aligned_const
                    .iter()
                    .map(|(len, sites)| (*len, sites.len()))
                    .collect::<Vec<_>>()
            );
            generate_and_patch(
                instructions,
                symbols,
                pc_base,
                &aligned_const,
                rv_generate_aligned_memcmp,
                "memcmp_aligned",
            );
        }

        // Patch unaligned constant-length call sites with alignment-dispatched memcmp
        if !unaligned_const.is_empty() {
            println!(
                "memcpy_optimizer: memcmp unaligned constant-length call sites: {:?}",
                unaligned_const
                    .iter()
                    .map(|(len, sites)| (*len, sites.len()))
                    .collect::<Vec<_>>()
            );
            generate_and_patch(
                instructions,
                symbols,
                pc_base,
                &unaligned_const,
                rv_generate_specialized_memcmp,
                "memcmp_opt",
            );
        }

        // Patch dynamic-length call sites with generic loop-based word memcmp
        if !dyn_calls.is_empty() {
            let routine = rv_generate_optimized_memcmp_loop();
            let routine_addr = pc_base + (instructions.len() as u32) * 4;
            println!(
                "memcmp_opt: generic loop at 0x{routine_addr:x} ({} insns), \
                 patching {} dynamic call sites",
                routine.len(),
                dyn_calls.len(),
            );
            symbols.insert(routine_addr, "memcmp_opt_loop".to_string());
            instructions.extend(routine);

            for &auipc_idx in &dyn_calls {
                let auipc_addr = pc_base + (auipc_idx as u32) * 4;
                let rd = rv_rd(instructions[auipc_idx]);
                let (new_auipc, new_jalr) = rv_encode_call(auipc_addr, routine_addr, rd);

                let verify = rv_auipc_jalr_target(new_auipc, new_jalr, auipc_addr);
                assert_eq!(
                    verify, routine_addr,
                    "AUIPC+JALR encoding mismatch at 0x{auipc_addr:x}"
                );

                instructions[auipc_idx] = new_auipc;
                instructions[auipc_idx + 1] = new_jalr;
            }
        }

        if aligned_const.is_empty() && unaligned_const.is_empty() && dyn_calls.is_empty() {
            println!("memcpy_optimizer: no memcmp call sites found");
        }
    }

    // Scan for inline byte-copy sequences and optimise with word copies
    optimize_bytecopy(instructions, symbols, pc_base);

    // Write extra symbols file for the trace analyzer.
    if instructions.len() > original_insn_count {
        let extra_path = "extra_symbols.txt";
        let new_end = pc_base + (instructions.len() as u32) * 4;
        let mut lines = Vec::new();
        for (addr, names) in symbols.table().range(elf_end..new_end) {
            for name in names {
                let next_addr = symbols
                    .table()
                    .range((addr + 1)..new_end)
                    .next()
                    .map(|(a, _)| *a)
                    .unwrap_or(new_end);
                let size = next_addr - addr;
                lines.push(format!("{addr:08x} {size} {name}"));
            }
        }
        if !lines.is_empty() {
            if let Err(e) = std::fs::write(extra_path, lines.join("\n") + "\n") {
                println!("memcpy_optimizer: failed to write {extra_path}: {e}");
            } else {
                println!(
                    "memcpy_optimizer: wrote {} extra symbols to {extra_path}",
                    lines.len()
                );
            }
        }
    }
}

#[cfg(test)]
mod tests;
