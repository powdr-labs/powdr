#![allow(clippy::print_stdout)]

use goblin::elf::{
    header::{EI_CLASS, ELFCLASS32, ELFCLASS64},
    Elf,
};
use powdr_riscv_elf::{load_elf, rv64};
use std::env;
use std::fs;
use std::panic;
use std::path::Path;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <elf-file>", args[0]);
        process::exit(1);
    }

    let elf_path = Path::new(&args[1]);

    if !elf_path.exists() {
        eprintln!("Error: File '{}' does not exist", elf_path.display());
        process::exit(1);
    }

    // Read the file to check if it's 32-bit or 64-bit
    let file_buffer = match fs::read(elf_path) {
        Ok(buffer) => buffer,
        Err(e) => {
            eprintln!("Error reading file: {e}");
            process::exit(1);
        }
    };

    let elf = match Elf::parse(&file_buffer) {
        Ok(elf) => elf,
        Err(e) => {
            eprintln!("Error parsing ELF header: {e}");
            process::exit(1);
        }
    };

    match elf.header.e_ident[EI_CLASS] {
        ELFCLASS32 => {
            // The load_elf function panics on errors, so we catch it
            let result = panic::catch_unwind(|| load_elf(elf_path));

            match result {
                Ok(program) => {
                    println!(
                        "RV32 ELF file analyzed successfully: {}",
                        elf_path.display()
                    );
                    println!();
                    print_elf_info_32(&program);
                }
                Err(_) => {
                    eprintln!("Error loading RV32 ELF file: The file may be corrupted or not a valid RISC-V ELF");
                    process::exit(1);
                }
            }
        }
        ELFCLASS64 => {
            // The load_elf_rv64 function panics on errors, so we catch it
            let result = panic::catch_unwind(|| rv64::compute_jumpdests(elf_path));

            match result {
                Ok(labels) => {
                    println!(
                        "RV64 ELF file analyzed successfully: {}",
                        elf_path.display()
                    );
                    println!();
                    print_elf_info_64(&labels);
                }
                Err(_) => {
                    eprintln!("Error loading RV64 ELF file: The file may be corrupted or not a valid RISC-V ELF");
                    process::exit(1);
                }
            }
        }
        _ => {
            eprintln!("Unsupported ELF class");
            process::exit(1);
        }
    }
}

fn print_elf_info_32(program: &powdr_riscv_elf::ElfProgram) {
    // Get text labels from the program
    let text_labels = program.text_labels();

    if text_labels.is_empty() {
        println!("No text labels found in the ELF file.");
    } else {
        println!("Text labels found: {}", text_labels.len());
        println!();
        println!("{:<16}", "Address");
        println!("{}", "-".repeat(16));

        // Text labels are already sorted in BTreeSet
        for address in text_labels {
            println!("0x{address:08x}");
        }
    }

    // Report on debug symbols
    let debug_info = program.debug_info();
    println!();
    println!("Debug information:");

    // Since we can't iterate over SymbolTable directly, we'll use text_labels
    // and look up each address
    let mut symbol_count = 0;
    let mut function_symbols = Vec::new();

    for &addr in text_labels {
        if let Some(name) = debug_info.symbols.try_get_one(addr) {
            symbol_count += 1;
            // Simple heuristic for functions: doesn't start with $ or contain .
            if !name.starts_with("$") && !name.contains(".") {
                function_symbols.push((addr, name));
            }
        }
    }

    println!("  Symbols at text label addresses: {symbol_count}");
    println!("  Function symbols: {}", function_symbols.len());

    if !function_symbols.is_empty() {
        println!();
        println!("Function symbols:");
        println!("{:<16} {:<40}", "Address", "Symbol");
        println!("{}", "-".repeat(60));

        for (address, name) in function_symbols {
            println!("0x{address:08x}      {name}");
        }
    }

    // Also show notes if available
    if !debug_info.notes.is_empty() {
        println!();
        println!("Debug notes:");
        let mut notes: Vec<_> = debug_info.notes.iter().collect();
        notes.sort_by_key(|(addr, _)| *addr);

        for (addr, note) in notes {
            println!("0x{addr:08x}: {note}");
        }
    }
}

fn print_elf_info_64(labels: &rv64::Rv64Labels) {
    println!("Entry point: 0x{:016x}", labels.entry_point);
    println!("PC base: 0x{:016x}", labels.pc_base);
    println!();

    if labels.jumpdests.is_empty() {
        println!("No text labels or jump destinations found.");
    } else {
        println!(
            "Text labels and jump destinations found: {}",
            labels.jumpdests.len()
        );
        println!();

        // Show all labels with symbols if available
        println!("{:<20} {:<40}", "Address", "Symbol (if available)");
        println!("{}", "-".repeat(60));

        for &addr in &labels.jumpdests {
            // Find symbol name if available
            let symbol = labels
                .symbols
                .iter()
                .find(|(sym_addr, _)| *sym_addr == addr)
                .map(|(_, name)| name.as_str())
                .unwrap_or("");

            println!("0x{addr:016x}  {symbol}");
        }

        // Summary of symbols
        println!();
        println!("Summary:");
        println!("  Total labels/jumpdests: {}", labels.jumpdests.len());
        println!("  Named symbols: {}", labels.symbols.len());
        println!(
            "  Jumpdests without symbols: {}",
            labels.jumpdests_with_debug_info.len()
        );

        // Show function-like symbols separately
        let function_symbols: Vec<_> = labels
            .symbols
            .iter()
            .filter(|(_, name)| !name.starts_with("$") && !name.contains("."))
            .collect();

        if !function_symbols.is_empty() {
            println!("  Function symbols: {}", function_symbols.len());
        }

        // Show label to address map
        println!();
        println!("=== Label to Address Map ===");
        println!("{:<40} {:<20}", "Label", "Address");
        println!("{}", "-".repeat(60));

        let mut sorted_symbols = labels.symbols.clone();
        sorted_symbols.sort_by(|a, b| a.1.cmp(&b.1));

        for (addr, name) in sorted_symbols {
            println!("{name:<40} 0x{addr:016x}");
        }

        // Show jumpdests that are not labels
        println!();
        println!("=== Jump Destinations Without Symbols ===");
        println!(
            "{:<20} {:<20} {:<40}",
            "Target Address", "From Address", "Instruction"
        );
        println!("{}", "-".repeat(80));

        let mut sorted_jumpdests: Vec<_> = labels.jumpdests_with_debug_info.iter().collect();
        sorted_jumpdests.sort_by_key(|(addr, _)| *addr);

        for (target_addr, sources) in sorted_jumpdests {
            for source in sources {
                println!(
                    "0x{:016x}  0x{:016x}  {}",
                    target_addr, source.from_addr, source.instruction
                );
            }
        }

        println!();
        println!("PC Base: 0x{:016x}", labels.pc_base);
    }
}
