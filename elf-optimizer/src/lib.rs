//! ELF optimizer library: memcpy/memmove/memcmp specialization and inline
//! bytecopy → word-copy transformations for RISC-V ELF binaries.

mod bytecopy;
mod call_site;
mod memcmp;
mod memcpy;
pub(crate) mod rv_insn;

use std::path::Path;

use powdr_riscv_elf::debug_info::SymbolTable;

use bytecopy::optimize_bytecopy;
use call_site::*;
use memcmp::{rv_generate_optimized_memcmp_loop, rv_generate_specialized_memcmp};
use memcpy::{rv_generate_specialized_memcpy, rv_generate_specialized_memmove};
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

/// Optimize a raw instruction stream (independent of the transpiler `Elf` type).
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
        let calls = scan_call_sites_multi(instructions, pc_base, &target_addrs);
        if calls.is_empty() {
            println!("memcpy_optimizer: no eligible memcpy calls found");
        } else {
            println!(
                "memcpy_optimizer: memcpy call sites by length: {:?}",
                calls
                    .iter()
                    .map(|(len, sites)| (*len, sites.len()))
                    .collect::<Vec<_>>()
            );
            generate_and_patch(
                instructions,
                symbols,
                pc_base,
                &calls,
                rv_generate_specialized_memcpy,
                "memcpy_opt",
            );
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
        let calls = scan_call_sites_multi(instructions, pc_base, &target_addrs);
        if calls.is_empty() {
            println!("memcpy_optimizer: no eligible memmove calls found");
        } else {
            println!(
                "memcpy_optimizer: memmove call sites by length: {:?}",
                calls
                    .iter()
                    .map(|(len, sites)| (*len, sites.len()))
                    .collect::<Vec<_>>()
            );
            generate_and_patch(
                instructions,
                symbols,
                pc_base,
                &calls,
                rv_generate_specialized_memmove,
                "memmove_opt",
            );
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

        let (const_calls, dyn_calls) =
            scan_all_call_sites_multi(instructions, pc_base, &target_addrs);

        // Patch constant-length call sites with specialized unrolled memcmp
        if !const_calls.is_empty() {
            println!(
                "memcpy_optimizer: memcmp constant-length call sites: {:?}",
                const_calls
                    .iter()
                    .map(|(len, sites)| (*len, sites.len()))
                    .collect::<Vec<_>>()
            );
            generate_and_patch(
                instructions,
                symbols,
                pc_base,
                &const_calls,
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

        if const_calls.is_empty() && dyn_calls.is_empty() {
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
