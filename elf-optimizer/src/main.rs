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

use std::env;
use std::fs;
use std::path::Path;
use std::process;

use powdr_elf_optimizer::optimize_elf_detailed;

use disasm::disasm;
use html::generate_html_diff;

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

    let result = optimize_elf_detailed(&data);

    let original_count = result.original_instructions.len();
    let appended = result.appended_count();
    println!(
        "Input: {} instructions, pc_base=0x{:08x}",
        original_count, result.pc_base
    );
    println!(
        "Output: {} instructions ({original_count} original + {appended} appended)",
        result.instructions.len()
    );

    match mode {
        Mode::Summary => {}
        Mode::Html(ref output_path) => {
            generate_html_diff(
                &result.original_instructions,
                &result.instructions,
                &result.symbols,
                result.pc_base,
                output_path,
            );
            println!("Wrote HTML diff to {output_path}");
        }
        Mode::Asm => {
            for (i, &insn) in result.instructions.iter().enumerate() {
                let addr = result.pc_base + (i as u32) * 4;
                if let Some(names) = result.symbols.table().get(&addr) {
                    for name in names {
                        println!();
                        println!("{name}:");
                    }
                }
                println!("  {:08x}:  {:08x}  {}", addr, insn, disasm(insn, addr));
            }
        }
        Mode::Bin(output_path) => {
            let patched = result.to_patched_elf(&data);
            fs::write(&output_path, &patched).unwrap_or_else(|e| {
                eprintln!("Error writing {output_path}: {e}");
                process::exit(1);
            });
            if appended > 0 {
                println!(
                    "Appended {appended} instructions ({} bytes) to segment",
                    appended * 4
                );
            }
            println!("Wrote patched ELF to {output_path}");
        }
    }

    // Print synthetic symbol summary
    let new_symbols: Vec<_> = result
        .symbols
        .table()
        .range((result.pc_base + (original_count as u32) * 4)..)
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
