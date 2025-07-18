use std::collections::BTreeSet;
use std::fs;
use std::path::Path;

use goblin::elf::{
    header::{EI_CLASS, EI_DATA, ELFCLASS64, ELFDATA2LSB, EM_RISCV},
    Elf,
};
use raki::{decode::Decode, instruction::OpcodeKind as Op, Isa};

/// Information about a jump destination
#[derive(Debug, Clone)]
pub struct JumpDest {
    /// The instruction address that generates this jump
    pub from_addr: u64,
    /// The instruction that generates this jump
    pub instruction: String,
}

/// Minimal RV64 ELF program representation for label/jumpdest collection
pub struct Rv64Labels {
    /// All text labels and jump destinations
    pub text_labels: BTreeSet<u64>,
    /// Entry point address
    pub entry_point: u64,
    /// Symbol table for debugging
    pub symbols: Vec<(u64, String)>,
    /// Jump destinations that are not symbols (address -> source instructions)
    pub jumpdests: BTreeMap<u64, Vec<JumpDest>>,
    /// PC base (lowest executable address)
    pub pc_base: u64,
}

pub fn load_elf_rv64(file_name: &Path) -> Rv64Labels {
    log::info!("Loading RV64 ELF file: {}", file_name.display());
    let file_buffer = fs::read(file_name).unwrap();
    load_elf_from_buffer_rv64(&file_buffer)
}

pub fn load_elf_from_buffer_rv64(file_buffer: &[u8]) -> Rv64Labels {
    let elf = Elf::parse(file_buffer).unwrap();

    // Verify it's a 64-bit RISC-V ELF
    assert_eq!(
        elf.header.e_ident[EI_CLASS], ELFCLASS64,
        "Only 64-bit ELF files are supported by rv64 module!"
    );
    assert_eq!(
        elf.header.e_ident[EI_DATA], ELFDATA2LSB,
        "Only little-endian ELF files are supported!"
    );
    assert_eq!(
        elf.header.e_machine, EM_RISCV,
        "Only RISC-V ELF files are supported!"
    );

    let mut text_labels = BTreeSet::new();
    let mut jumpdests = BTreeMap::new();

    // Add entry point
    text_labels.insert(elf.entry);

    // Find PC base (lowest executable address)
    let pc_base = elf
        .program_headers
        .iter()
        .filter(|ph| ph.is_executable())
        .map(|ph| ph.p_vaddr)
        .min()
        .unwrap_or(0);

    // Collect symbols that are in text sections
    let mut symbols = Vec::new();
    let mut symbol_addrs = BTreeSet::new();
    for sym in elf.syms.iter() {
        if sym.st_value != 0 {
            // Check if this symbol is in an executable section
            let in_text = elf.program_headers.iter().any(|ph| {
                ph.is_executable()
                    && sym.st_value >= ph.p_vaddr
                    && sym.st_value < ph.p_vaddr + ph.p_memsz
            });

            if in_text {
                text_labels.insert(sym.st_value);
                symbol_addrs.insert(sym.st_value);
                if let Some(name) = elf.strtab.get_at(sym.st_name) {
                    symbols.push((sym.st_value, name.to_string()));
                }
            }
        }
    }

    // Scan text sections for jump destinations
    for ph in elf.program_headers.iter() {
        if ph.is_executable() {
            let seg = &file_buffer[ph.p_offset as usize..(ph.p_offset + ph.p_filesz) as usize];
            scan_for_jump_targets(
                ph.p_vaddr,
                seg,
                &mut text_labels,
                &mut jumpdests,
                &symbol_addrs,
            );
        }
    }

    Rv64Labels {
        text_labels,
        entry_point: elf.entry,
        symbols,
        jumpdests,
        pc_base,
    }
}

use std::collections::BTreeMap;

fn scan_for_jump_targets(
    base_addr: u64,
    data: &[u8],
    text_labels: &mut BTreeSet<u64>,
    jumpdests: &mut BTreeMap<u64, Vec<JumpDest>>,
    symbol_addrs: &BTreeSet<u64>,
) {
    let mut addr = base_addr;
    let mut remaining = data;

    while remaining.len() >= 2 {
        // Check if it's a 32-bit or 16-bit instruction
        if remaining[0] & 0b11 == 0b11 && remaining.len() >= 4 {
            // 32-bit instruction
            let insn_bytes = u32::from_le_bytes(remaining[0..4].try_into().unwrap());

            if let Ok(insn) = insn_bytes.decode(Isa::Rv64) {
                // Check for jump/branch instructions
                match insn.opc {
                    Op::JAL => {
                        // JAL has a PC-relative immediate
                        if let Some(imm) = insn.imm {
                            let target = (addr as i64 + imm as i64) as u64;
                            text_labels.insert(target);

                            // Track non-symbol jumpdests
                            if !symbol_addrs.contains(&target) {
                                let jump_info = JumpDest {
                                    from_addr: addr,
                                    instruction: format!(
                                        "jal {}, 0x{:x}",
                                        insn.rd
                                            .map(|r| format!("x{r}"))
                                            .unwrap_or_else(|| "?".to_string()),
                                        target
                                    ),
                                };
                                jumpdests.entry(target).or_default().push(jump_info);
                            }
                        }
                    }
                    Op::BEQ | Op::BNE | Op::BLT | Op::BGE | Op::BLTU | Op::BGEU => {
                        // Conditional branches have PC-relative immediates
                        if let Some(imm) = insn.imm {
                            let target = (addr as i64 + imm as i64) as u64;
                            text_labels.insert(target);

                            // Track non-symbol jumpdests
                            if !symbol_addrs.contains(&target) {
                                let jump_info = JumpDest {
                                    from_addr: addr,
                                    instruction: format!(
                                        "{} {}, {}, 0x{:x}",
                                        format!("{:?}", insn.opc).to_lowercase(),
                                        insn.rs1
                                            .map(|r| format!("x{r}"))
                                            .unwrap_or_else(|| "?".to_string()),
                                        insn.rs2
                                            .map(|r| format!("x{r}"))
                                            .unwrap_or_else(|| "?".to_string()),
                                        target
                                    ),
                                };
                                jumpdests.entry(target).or_default().push(jump_info);
                            }
                        }
                    }
                    Op::AUIPC => {
                        // AUIPC is often followed by JALR for function calls and long jumps
                        // In statically linked binaries, these usually target known symbols
                        if remaining.len() >= 8 {
                            let next_insn_bytes =
                                u32::from_le_bytes(remaining[4..8].try_into().unwrap());
                            if let Ok(next_insn) = next_insn_bytes.decode(Isa::Rv64) {
                                if matches!(next_insn.opc, Op::JALR) && insn.rd == next_insn.rs1 {
                                    // This is an AUIPC+JALR pair
                                    if let (Some(auipc_imm), Some(jalr_imm)) =
                                        (insn.imm, next_insn.imm)
                                    {
                                        let target =
                                            (addr as i64 + auipc_imm as i64 + jalr_imm as i64)
                                                as u64;
                                        text_labels.insert(target);

                                        // Track non-symbol jumpdests
                                        if !symbol_addrs.contains(&target) {
                                            let jump_info = JumpDest {
                                                from_addr: addr,
                                                instruction: format!("auipc+jalr -> 0x{target:x}"),
                                            };
                                            jumpdests.entry(target).or_default().push(jump_info);
                                        }
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }

            addr += 4;
            remaining = &remaining[4..];
        } else {
            // 16-bit compressed instruction
            let insn_bytes = u16::from_le_bytes(remaining[0..2].try_into().unwrap());

            if let Ok(insn) = insn_bytes.decode(Isa::Rv64) {
                // Check for compressed jump/branch instructions
                match insn.opc {
                    Op::C_J | Op::C_JAL => {
                        // Compressed jumps
                        if let Some(imm) = insn.imm {
                            let target = (addr as i64 + imm as i64) as u64;
                            text_labels.insert(target);

                            // Track non-symbol jumpdests
                            if !symbol_addrs.contains(&target) {
                                let jump_info = JumpDest {
                                    from_addr: addr,
                                    instruction: format!(
                                        "{} 0x{:x}",
                                        format!("{:?}", insn.opc).to_lowercase(),
                                        target
                                    ),
                                };
                                jumpdests.entry(target).or_default().push(jump_info);
                            }
                        }
                    }
                    Op::C_BEQZ | Op::C_BNEZ => {
                        // Compressed branches
                        if let Some(imm) = insn.imm {
                            let target = (addr as i64 + imm as i64) as u64;
                            text_labels.insert(target);

                            // Track non-symbol jumpdests
                            if !symbol_addrs.contains(&target) {
                                let jump_info = JumpDest {
                                    from_addr: addr,
                                    instruction: format!(
                                        "{} {}, 0x{:x}",
                                        format!("{:?}", insn.opc).to_lowercase(),
                                        insn.rs1
                                            .map(|r| format!("x{r}"))
                                            .unwrap_or_else(|| "?".to_string()),
                                        target
                                    ),
                                };
                                jumpdests.entry(target).or_default().push(jump_info);
                            }
                        }
                    }
                    _ => {}
                }
            }

            addr += 2;
            remaining = &remaining[2..];
        }
    }
}
