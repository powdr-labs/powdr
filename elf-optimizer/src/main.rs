#![allow(clippy::print_stdout)]

//! Standalone ELF optimizer: applies memcpy/memmove/memcmp specialization
//! and inline byte-copy → word-copy transformations to a RISC-V ELF binary.
//!
//! Usage:
//!   elf-optimizer <input.elf> [--asm | --bin <output.elf> | --html <output.html>]
//!
//! Modes:
//!   --asm              Print disassembly of the optimized instruction stream
//!   --bin <output>     Write a patched ELF binary (copies input, patches text section)
//!   --html <output>    Generate an interactive HTML diff view of the optimizations
//!   (default)          Print optimization summary only

mod disasm;
mod html;

use std::collections::BTreeMap;
use std::env;
use std::fs;
use std::path::Path;
use std::process;

use powdr_elf_optimizer::{optimize_instructions, parse_pc_base_data};
use powdr_riscv_elf::debug_info::SymbolTable;

use disasm::disasm;
use html::generate_html_diff;

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

    let mut segments: Vec<(u32, usize, usize)> = Vec::new(); // (vaddr, file_offset, size)
    for i in 0..e_phnum {
        let off = e_phoff + i * e_phentsize;
        let p_type = u32::from_le_bytes(data[off..off + 4].try_into().unwrap());
        let p_offset = u32::from_le_bytes(data[off + 4..off + 8].try_into().unwrap()) as usize;
        let p_vaddr = u32::from_le_bytes(data[off + 8..off + 12].try_into().unwrap());
        let p_filesz = u32::from_le_bytes(data[off + 16..off + 20].try_into().unwrap()) as usize;
        let p_flags = u32::from_le_bytes(data[off + 24..off + 28].try_into().unwrap());
        // PT_LOAD = 1, PF_X = 1
        if p_type == 1 && (p_flags & 1) != 0 {
            segments.push((p_vaddr, p_offset, p_filesz));
        }
    }

    segments.sort_by_key(|(vaddr, _, _)| *vaddr);

    let mut instructions = Vec::new();
    let mut text_offsets = Vec::new();

    for (seg_vaddr, seg_file_offset, seg_size) in &segments {
        let insn_start_in_stream = ((*seg_vaddr - pc_base) / 4) as usize;

        // Pad with NOPs if there's a gap
        while instructions.len() < insn_start_in_stream {
            text_offsets.push((instructions.len(), 0)); // dummy offset for padding
            instructions.push(0x00000013); // NOP (addi x0, x0, 0)
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

fn print_usage(prog: &str) {
    eprintln!("Usage: {prog} <input.elf> [--asm | --bin <output.elf> | --html <output.html>]");
    eprintln!();
    eprintln!("Modes:");
    eprintln!("  --asm              Print disassembly of the optimized instruction stream");
    eprintln!("  --bin <output>     Write patched ELF binary");
    eprintln!("  --html <output>    Generate interactive HTML diff of optimizations");
    eprintln!("  (default)          Print optimization summary only");
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_usage(&args[0]);
        process::exit(1);
    }

    let input_path = Path::new(&args[1]);
    if !input_path.exists() {
        eprintln!("Error: file '{}' does not exist", input_path.display());
        process::exit(1);
    }

    enum Mode {
        Summary,
        Asm,
        Bin(String),
        Html(String),
    }
    let mode = if args.len() >= 3 {
        match args[2].as_str() {
            "--asm" => Mode::Asm,
            "--bin" => {
                if args.len() < 4 {
                    eprintln!("Error: --bin requires an output path");
                    process::exit(1);
                }
                Mode::Bin(args[3].clone())
            }
            "--html" => {
                if args.len() < 4 {
                    eprintln!("Error: --html requires an output path");
                    process::exit(1);
                }
                Mode::Html(args[3].clone())
            }
            _ => {
                print_usage(&args[0]);
                process::exit(1);
            }
        }
    } else {
        Mode::Summary
    };

    let data = fs::read(input_path).unwrap_or_else(|e| {
        eprintln!("Error reading {}: {e}", input_path.display());
        process::exit(1);
    });

    let pc_base = parse_pc_base_data(&data);
    let (_, mut instructions, text_offsets) = extract_instructions(&data);
    let mut symbols = build_symbol_table(&data);

    let original_count = instructions.len();
    let original_instructions = instructions.clone();
    println!(
        "Input: {} instructions, pc_base=0x{pc_base:08x}",
        instructions.len()
    );

    optimize_instructions(&mut instructions, &mut symbols, pc_base);

    let appended = instructions.len() - original_count;
    println!(
        "Output: {} instructions ({original_count} original + {appended} appended)",
        instructions.len()
    );

    match mode {
        Mode::Summary => {}
        Mode::Html(ref output_path) => {
            generate_html_diff(
                &original_instructions,
                &instructions,
                &symbols,
                pc_base,
                output_path,
            );
            println!("Wrote HTML diff to {output_path}");
        }
        Mode::Asm => {
            for (i, &insn) in instructions.iter().enumerate() {
                let addr = pc_base + (i as u32) * 4;
                if let Some(names) = symbols.table().get(&addr) {
                    for name in names {
                        println!();
                        println!("{name}:");
                    }
                }
                println!("  {:08x}:  {:08x}  {}", addr, insn, disasm(insn, addr));
            }
        }
        Mode::Bin(output_path) => {
            write_patched_elf(
                &data,
                &instructions,
                original_count,
                appended,
                &text_offsets,
                &output_path,
            );
        }
    }

    // Print synthetic symbol summary
    let new_symbols: Vec<_> = symbols
        .table()
        .range((pc_base + (original_count as u32) * 4)..)
        .flat_map(|(addr, names)| names.iter().map(move |n| (*addr, n.clone())))
        .collect();
    if !new_symbols.is_empty() {
        println!();
        println!("Synthetic symbols ({}):", new_symbols.len());
        for (addr, name) in &new_symbols {
            println!("  0x{addr:08x}  {name}");
        }
    }
}

/// Write a patched ELF binary, extending the last executable segment if needed.
fn write_patched_elf(
    data: &[u8],
    instructions: &[u32],
    original_count: usize,
    appended: usize,
    text_offsets: &[(usize, usize)],
    output_path: &str,
) {
    let mut patched = data.to_vec();

    // Patch existing instructions in-place
    for &(insn_idx, file_offset) in text_offsets {
        if insn_idx < instructions.len() {
            let word = instructions[insn_idx];
            patched[file_offset..file_offset + 4].copy_from_slice(&word.to_le_bytes());
        }
    }

    // Extend the last executable segment for appended instructions
    if appended > 0 {
        let e_phoff = u32::from_le_bytes(data[28..32].try_into().unwrap()) as usize;
        let e_phentsize = u16::from_le_bytes(data[42..44].try_into().unwrap()) as usize;
        let e_phnum = u16::from_le_bytes(data[44..46].try_into().unwrap()) as usize;

        let mut last_exec_ph: Option<usize> = None;
        let mut last_exec_vaddr_end: u32 = 0;

        for i in 0..e_phnum {
            let off = e_phoff + i * e_phentsize;
            let p_type = u32::from_le_bytes(patched[off..off + 4].try_into().unwrap());
            let p_vaddr = u32::from_le_bytes(patched[off + 8..off + 12].try_into().unwrap());
            let p_memsz = u32::from_le_bytes(patched[off + 20..off + 24].try_into().unwrap());
            let p_flags = u32::from_le_bytes(patched[off + 24..off + 28].try_into().unwrap());
            if p_type == 1 && (p_flags & 1) != 0 {
                let end = p_vaddr + p_memsz;
                if end >= last_exec_vaddr_end {
                    last_exec_vaddr_end = end;
                    last_exec_ph = Some(i);
                }
            }
        }

        if let Some(ph_idx) = last_exec_ph {
            let ph_off = e_phoff + ph_idx * e_phentsize;
            let extra_bytes = (appended as u32) * 4;

            for insn in instructions.iter().skip(original_count) {
                patched.extend_from_slice(&insn.to_le_bytes());
            }

            // Update p_filesz and p_memsz
            let old_filesz =
                u32::from_le_bytes(patched[ph_off + 16..ph_off + 20].try_into().unwrap());
            let old_memsz =
                u32::from_le_bytes(patched[ph_off + 20..ph_off + 24].try_into().unwrap());
            patched[ph_off + 16..ph_off + 20]
                .copy_from_slice(&(old_filesz + extra_bytes).to_le_bytes());
            patched[ph_off + 20..ph_off + 24]
                .copy_from_slice(&(old_memsz + extra_bytes).to_le_bytes());

            // Update corresponding section header
            let e_shoff = u32::from_le_bytes(data[32..36].try_into().unwrap()) as usize;
            let e_shentsize = u16::from_le_bytes(data[46..48].try_into().unwrap()) as usize;
            let e_shnum = u16::from_le_bytes(data[48..50].try_into().unwrap()) as usize;

            if e_shoff > 0 && e_shentsize >= 40 {
                for i in 0..e_shnum {
                    let sh_off = e_shoff + i * e_shentsize;
                    if sh_off + 40 > patched.len() {
                        break;
                    }
                    let sh_type =
                        u32::from_le_bytes(patched[sh_off + 4..sh_off + 8].try_into().unwrap());
                    let sh_flags =
                        u32::from_le_bytes(patched[sh_off + 8..sh_off + 12].try_into().unwrap());
                    let sh_addr =
                        u32::from_le_bytes(patched[sh_off + 12..sh_off + 16].try_into().unwrap());
                    let sh_size =
                        u32::from_le_bytes(patched[sh_off + 20..sh_off + 24].try_into().unwrap());
                    // SHT_PROGBITS=1, SHF_EXECINSTR=4
                    if sh_type == 1
                        && (sh_flags & 4) != 0
                        && sh_addr + sh_size == last_exec_vaddr_end
                    {
                        patched[sh_off + 20..sh_off + 24]
                            .copy_from_slice(&(sh_size + extra_bytes).to_le_bytes());
                        break;
                    }
                }
            }

            println!("Appended {appended} instructions ({extra_bytes} bytes) to segment");
        } else {
            eprintln!(
                "Warning: could not find executable segment to extend; \
                 appended instructions not written"
            );
        }
    }

    fs::write(output_path, &patched).unwrap_or_else(|e| {
        eprintln!("Error writing {output_path}: {e}");
        process::exit(1);
    });
    println!("Wrote patched ELF to {output_path}");
}
