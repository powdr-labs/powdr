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
    pub jumpdests: BTreeSet<u64>,
    /// Entry point address
    pub entry_point: u64,
    /// Symbol table for debugging
    pub symbols: Vec<(u64, String)>,
    /// Jump destinations that are not symbols (address -> source instructions)
    pub jumpdests_with_debug_info: BTreeMap<u64, Vec<JumpDest>>,
    /// PC base (lowest executable address)
    pub pc_base: u64,
}

pub fn compute_jumpdests(file_name: &Path) -> Rv64Labels {
    log::info!("Loading RV64 ELF file: {}", file_name.display());
    let file_buffer = fs::read(file_name).unwrap();
    compute_jumpdests_from_buffer(&file_buffer)
}

pub fn compute_jumpdests_from_buffer(file_buffer: &[u8]) -> Rv64Labels {
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

    let mut jumpdests = BTreeSet::new();
    let mut jumpdests_with_debug_info = BTreeMap::new();

    // Add entry point
    jumpdests.insert(elf.entry);

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
                jumpdests.insert(sym.st_value);
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
                &mut jumpdests,
                &mut jumpdests_with_debug_info,
                &symbol_addrs,
            );
        }
    }

    Rv64Labels {
        jumpdests,
        entry_point: elf.entry,
        symbols,
        jumpdests_with_debug_info,
        pc_base,
    }
}

use std::collections::BTreeMap;

fn scan_for_jump_targets(
    base_addr: u64,
    data: &[u8],
    jumpdests: &mut BTreeSet<u64>,
    jumpdests_with_debug_info: &mut BTreeMap<u64, Vec<JumpDest>>,
    label_addrs: &BTreeSet<u64>,
) {
    data.chunks(4)
        // Cast to [u8; 4]
        .map(|data| data.try_into().unwrap())
        .inspect(|data: &[u8; 4]| {
            assert!(data[0] & 0b11 == 0b11, "Expected 32-bit instruction");
        })
        .map(u32::from_le_bytes)
        // Decode the instruction bytes
        .map(|insn_bytes| {
            insn_bytes
                .decode(Isa::Rv64)
                .expect("Failed to decode instruction")
        })
        // Remember the `rs1` and `imm` of the previous instruction if it was AUIPC, used to propagate it to the next JALR
        .scan(None, |previous_if_auipc, insn| {
            let previous_auipc_rs1 = std::mem::replace(
                previous_if_auipc,
                matches!(insn.opc, Op::AUIPC).then_some((insn.rs1, insn.imm)),
            );
            Some((insn, previous_auipc_rs1))
        })
        .enumerate()
        .for_each(|(instruction_index, (insn, previous_if_auipc))| {
            let addr = base_addr + (instruction_index * 4) as u64;

            // Check for jump/branch instructions
            match insn.opc {
                Op::JAL => {
                    // JAL has a PC-relative immediate
                    if let Some(imm) = insn.imm {
                        let target = (addr as i64 + imm as i64) as u64;
                        jumpdests.insert(target);

                        // Track non-symbol jumpdests
                        if !label_addrs.contains(&target) {
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
                            jumpdests_with_debug_info
                                .entry(target)
                                .or_default()
                                .push(jump_info);
                        }
                    }
                }
                Op::BEQ | Op::BNE | Op::BLT | Op::BGE | Op::BLTU | Op::BGEU => {
                    // Conditional branches have PC-relative immediates
                    if let Some(imm) = insn.imm {
                        let target = (addr as i64 + imm as i64) as u64;
                        jumpdests.insert(target);

                        // Track non-symbol jumpdests
                        if !label_addrs.contains(&target) {
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
                            jumpdests_with_debug_info
                                .entry(target)
                                .or_default()
                                .push(jump_info);
                        }
                    }
                }
                Op::JALR => {
                    if let Some((rs1, imm)) = previous_if_auipc {
                        // JALR with a preceding AUIPC
                        if insn.rd == rs1 {
                            // This is an AUIPC+JALR pair, we can resolve it statically
                            if let (Some(auipc_imm), Some(jalr_imm)) = (imm, insn.imm) {
                                let target =
                                    (addr as i64 + auipc_imm as i64 + jalr_imm as i64) as u64;
                                jumpdests.insert(target);

                                // Track non-symbol jumpdests
                                if !label_addrs.contains(&target) {
                                    let jump_info = JumpDest {
                                        from_addr: addr,
                                        instruction: format!("auipc+jalr -> 0x{target:x}"),
                                    };
                                    jumpdests_with_debug_info
                                        .entry(target)
                                        .or_default()
                                        .push(jump_info);
                                }
                            }
                        }
                    } else {
                        // Standalone JALR without preceding AUIPC
                        // These are dynamic jumps we can't resolve statically:
                        // - Return instructions (jalr x0, x1, 0)
                        // - Indirect calls through function pointers
                        // - Computed jumps (switch statements, vtables)
                        // We just note their existence for completeness

                        let rs1_str = insn
                            .rs1
                            .map(|r| format!("x{r}"))
                            .unwrap_or_else(|| "?".to_string());
                        let rd_str = insn
                            .rd
                            .map(|r| format!("x{r}"))
                            .unwrap_or_else(|| "?".to_string());
                        let imm = insn.imm.unwrap_or(0);

                        // Only log if it's not a standard return (jalr x0, x1, 0)
                        if !(insn.rd == Some(0) && insn.rs1 == Some(1) && imm == 0) {
                            tracing::debug!(
                                "Note: Dynamic jump at 0x{addr:x}: jalr {rd_str}, {rs1_str}, {imm}",
                            );
                        }
                    }
                }
                _ => {}
            };
        });
}
