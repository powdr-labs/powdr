use std::{
    borrow::Cow,
    cmp::Ordering,
    collections::{btree_map::Entry, BTreeMap, BTreeSet, HashMap},
    fs,
    path::Path,
};

use goblin::{
    elf::sym::STT_OBJECT,
    elf::{
        header::{EI_CLASS, EI_DATA, ELFCLASS32, ELFDATA2LSB, EM_RISCV, ET_DYN},
        program_header,
        reloc::{R_RISCV_32, R_RISCV_HI20, R_RISCV_RELATIVE},
        sym::STT_FUNC,
        Elf,
    },
};
use itertools::{Either, Itertools};
use powdr_asm_utils::data_storage::SingleDataValue;
use powdr_number::FieldElement;
use raki::{
    decode::Decode,
    instruction::{Instruction as Ins, OpcodeKind as Op},
    Isa,
};

use crate::{
    code_gen::{self, InstructionArgs, MemEntry, Register, RiscVProgram, Statement},
    Runtime,
};

struct ElfProgram {
    symbol_table: SymbolTable,
    data_map: BTreeMap<u32, Data>,
    text_labels: BTreeSet<u32>,
    instructions: Vec<HighLevelInsn>,
    entry_point: u32,
}

pub fn elf_translate<F: FieldElement>(
    file_name: &Path,
    runtime: &Runtime,
    with_bootloader: bool,
) -> String {
    let elf_program = load_elf(file_name);
    code_gen::translate_program::<F>(elf_program, runtime, with_bootloader)
}

fn load_elf(file_name: &Path) -> ElfProgram {
    println!("Loading ELF file: {:?}", file_name);
    let file_buffer = fs::read(file_name).unwrap();

    let elf = Elf::parse(&file_buffer).unwrap();

    // Assert this in an 32-bit ELF file.
    assert_eq!(
        elf.header.e_ident[EI_CLASS], ELFCLASS32 as u8,
        "Only 32-bit ELF files are supported!"
    );

    // Assert this in a little-endian ELF file.
    assert_eq!(
        elf.header.e_ident[EI_DATA], ELFDATA2LSB as u8,
        "Only little-endian ELF files are supported!"
    );

    // Assert this is a RISC-V ELF file.
    assert_eq!(
        elf.header.e_machine, EM_RISCV,
        "Only RISC-V ELF files are supported!"
    );

    // Assert this is either a PIE file, or that we have the relocation symbols
    // available. This is needed because we need to lift all the references to
    // code addresses into labels.
    assert!(
        elf.header.e_type == ET_DYN || !elf.shdr_relocs.is_empty(),
        "We can only translate PIE ELFs (-pie) or ELFs with relocation symbols (--emit-relocs)."
    );

    // Map of addresses into memory sections, so we can know what address belong
    // in what section.
    let address_map = AddressMap(
        elf.program_headers
            .iter()
            .filter_map(|p| (p.p_type == program_header::PT_LOAD).then(|| (p.p_vaddr as u32, p)))
            .collect(),
    );

    // Set of R_RISCV_HI20 relocations, needed in non-PIE code to identify
    // loading of absolute addresses to text.
    let text_rellocs_set: BTreeSet<u32> = elf
        .shdr_relocs
        .iter()
        .flat_map(|(_, r)| r.iter())
        .filter_map(|r| (r.r_type == R_RISCV_HI20).then(|| r.r_offset as u32))
        .collect();

    // We simply extract all the text and data sections. There is no need to
    // perform reachability search, because we trust the linker to have done
    // that already.
    //
    // TODO: maybe exclude the section mapping the ELF header itself? But with
    // VADCOP, unused memory is practically free.
    let mut lifted_text_sections = Vec::new();
    let mut data_map = BTreeMap::new();

    // Keep a list of referenced text addresses, so we can generate the labels.
    let mut referenced_text_addrs = BTreeSet::from([elf.entry as u32]);

    // Actually load the sections.
    for (&addr, &p) in address_map.0.iter() {
        let section_data = &file_buffer[p.p_offset as usize..(p.p_offset + p.p_filesz) as usize];

        // Test if executable
        if p.p_flags & 1 == 1 {
            let insns = lift_instructions(
                addr,
                section_data,
                &address_map,
                &text_rellocs_set,
                &mut referenced_text_addrs,
            );
            if !insns.is_empty() {
                lifted_text_sections.push(insns);
            }
        } else {
            load_data_section(addr, section_data, &mut data_map);
        }
    }

    // Sort text sections by address and flatten them.
    lifted_text_sections.sort_by_key(|insns| insns[0].original_address);
    let lifted_text_sections = lifted_text_sections
        .into_iter()
        .flatten()
        .collect::<Vec<_>>();

    let symbol_table = SymbolTable::new(&elf);

    // All the references to code address have been lifted into labels in the
    // instructions, but not the data sections. Need to do it for data.
    (if elf.header.e_type == ET_DYN {
        pie_relocate_data_section
    } else {
        static_relocate_data_section
    })(
        &elf,
        &address_map,
        &mut data_map,
        &mut referenced_text_addrs,
    );

    assert_eq!(elf.dynrels.len(), 0, "Unsupported relocation type!");

    ElfProgram {
        symbol_table,
        data_map,
        text_labels: referenced_text_addrs,
        instructions: lifted_text_sections,
        entry_point: elf.entry as u32,
    }
}

fn pie_relocate_data_section(
    elf: &Elf,
    address_map: &AddressMap,
    data_map: &mut BTreeMap<u32, Data>,
    referenced_text_addrs: &mut BTreeSet<u32>,
) {
    // In PIE files, we can read the dynamic relocation table.
    for r in elf.dynrelas.iter() {
        let addr = r.r_offset as u32;
        if !address_map.is_in_data_section(addr) {
            unimplemented!("We assumed all dynamic relocations were data relocations!");
        }

        // We only support the R_RISCV_RELATIVE relocation type:
        assert_eq!(r.r_type, R_RISCV_RELATIVE, "Unsupported relocation type!");

        let original_addr = r.r_addend.unwrap() as u32;

        if address_map.is_in_text_section(original_addr) {
            data_map.insert(addr, Data::TextLabel(original_addr));

            // We also need to add the referenced address to the list of text
            // addresses, so we can generate the label.
            referenced_text_addrs.insert(original_addr);
        } else {
            data_map.insert(addr, Data::Value(original_addr));
        }
    }
}

fn static_relocate_data_section(
    elf: &Elf,
    address_map: &AddressMap,
    data_map: &mut BTreeMap<u32, Data>,
    referenced_text_addrs: &mut BTreeSet<u32>,
) {
    // In non-PIE files, we need to use the linking relocation table.
    for r in elf.shdr_relocs.iter().flat_map(|(_, relocs)| relocs.iter()) {
        let addr = r.r_offset as u32;
        if !address_map.is_in_data_section(addr) {
            // Relocation of the text section has already been handled in instruction lifting.
            continue;
        }

        // We only support the R_RISCV_32 relocation type for the data section:
        assert_eq!(r.r_type, R_RISCV_32, "Unsupported relocation type!");

        let Entry::Occupied(mut entry) = data_map.entry(r.r_offset as u32) else {
            panic!("Unexpected 0 in relocated data entry!");
        };

        let Data::Value(original_addr) = *entry.get() else {
            panic!("Related entry already replaced with a label!");
        };

        if address_map.is_in_text_section(original_addr) {
            entry.insert(Data::TextLabel(original_addr));

            // We also need to add the referenced address to the list of text
            // addresses, so we can generate the label.
            referenced_text_addrs.insert(original_addr);
        }
    }
}

struct SymbolTable(HashMap<u32, String>);

impl SymbolTable {
    fn new(elf: &Elf) -> SymbolTable {
        // TODO: read the symbols from the debug information to be more comprehensive.
        let mut deduplicator = HashMap::new();
        for sym in elf.syms.iter() {
            if sym.st_name == 0 || (sym.st_type() != STT_OBJECT && sym.st_type() != STT_FUNC) {
                continue;
            }
            deduplicator.insert(elf.strtab[sym.st_name].to_string(), sym.st_value as u32);
        }

        Self(
            deduplicator
                .into_iter()
                .map(|(name, addr)| (addr, name))
                .collect(),
        )
    }

    fn try_get(&self, addr: u32) -> Option<&str> {
        self.0.get(&addr).map(|name| name.as_str())
    }

    fn get(&self, addr: u32) -> Cow<str> {
        self.0
            .get(&addr)
            .map(|name| Cow::Borrowed(name.as_str()))
            .unwrap_or_else(|| Cow::Owned(format!("L{:08x}", addr)))
    }

    fn get_as_string(&self, addr: u32) -> String {
        self.get(addr).into_owned()
    }
}

impl RiscVProgram for ElfProgram {
    fn take_source_files_info(&mut self) -> impl Iterator<Item = crate::code_gen::SourceFileInfo> {
        // TODO: read the source files from the debug information.
        std::iter::empty()
    }

    fn take_initial_mem(&mut self) -> impl Iterator<Item = crate::code_gen::MemEntry> {
        self.data_map.iter().map(|(addr, data)| {
            let value = match data {
                Data::TextLabel(label) => {
                    SingleDataValue::LabelReference(self.symbol_table.get_as_string(*label))
                }
                Data::Value(value) => SingleDataValue::Value(*value),
            };

            MemEntry {
                label: self.symbol_table.try_get(*addr).map(|s| s.to_string()),
                addr: *addr,
                value,
            }
        })
    }

    fn take_executable_statements(
        &mut self,
    ) -> impl Iterator<Item = crate::code_gen::Statement<impl AsRef<str>, WrappedArgs>> {
        let labels = self.text_labels.iter();
        let instructions = self.instructions.iter();

        labels
            .merge_join_by(instructions, |&&next_label, next_insn| {
                match next_label.cmp(&next_insn.original_address) {
                    Ordering::Less => panic!("Label {next_label:08x} doesn't match exact address!"),
                    Ordering::Equal => true,
                    Ordering::Greater => false,
                }
            })
            .map(|result| match result {
                Either::Left(label) => Statement::Label(self.symbol_table.get(*label)),
                Either::Right(insn) => Statement::Instruction {
                    op: insn.op,
                    args: WrappedArgs {
                        args: &insn.args,
                        symbol_table: &self.symbol_table,
                    },
                },
            })
    }

    fn start_function(&self) -> Cow<str> {
        self.symbol_table.get(self.entry_point)
    }
}

struct WrappedArgs<'a> {
    args: &'a HighLevelArgs,
    symbol_table: &'a SymbolTable,
}

impl<'a> InstructionArgs for WrappedArgs<'a> {
    type Error = String;

    fn l(&self) -> Result<String, Self::Error> {
        match self.args {
            HighLevelArgs {
                imm: HighLevelImmediate::CodeLabel(addr),
                rd: None,
                rs1: None,
                rs2: None,
            } => Ok(self.symbol_table.get_as_string(*addr)),
            _ => Err(format!("Expected: label, got {:?}", self.args)),
        }
    }

    fn r(&self) -> Result<Register, Self::Error> {
        match self.args {
            HighLevelArgs {
                imm: HighLevelImmediate::None,
                rd: None,
                rs1: Some(rs1),
                rs2: None,
            } => Ok(Register::new(*rs1 as u8)),
            _ => Err(format!("Expected: rs1, got {:?}", self.args)),
        }
    }

    fn rri(&self) -> Result<(Register, Register, u32), Self::Error> {
        match self.args {
            HighLevelArgs {
                imm: HighLevelImmediate::Value(imm),
                rd: Some(rd),
                rs1: Some(rs1),
                rs2: None,
            } => Ok((
                Register::new(*rd as u8),
                Register::new(*rs1 as u8),
                *imm as u32,
            )),
            _ => Err(format!("Expected: rd, rs1, imm, got {:?}", self.args)),
        }
    }

    fn rrr(&self) -> Result<(Register, Register, Register), Self::Error> {
        match self.args {
            HighLevelArgs {
                imm: HighLevelImmediate::None,
                rd: Some(rd),
                rs1: Some(rs1),
                rs2: Some(rs2),
            } => Ok((
                Register::new(*rd as u8),
                Register::new(*rs1 as u8),
                Register::new(*rs2 as u8),
            )),
            _ => Err(format!("Expected: rd, rs1, rs2, got {:?}", self.args)),
        }
    }

    fn ri(&self) -> Result<(Register, u32), Self::Error> {
        match self.args {
            HighLevelArgs {
                imm: HighLevelImmediate::Value(imm),
                rd: Some(rd),
                rs1: None,
                rs2: None,
            } => Ok((Register::new(*rd as u8), *imm as u32)),
            _ => Err(format!("Expected: rd, imm, got {:?}", self.args)),
        }
    }

    fn rr(&self) -> Result<(Register, Register), Self::Error> {
        match self.args {
            HighLevelArgs {
                imm: HighLevelImmediate::None,
                rd: Some(rd),
                rs1: Some(rs1),
                rs2: None,
            } => Ok((Register::new(*rd as u8), Register::new(*rs1 as u8))),
            _ => Err(format!("Expected: rd, rs1, got {:?}", self.args)),
        }
    }

    fn rrl(&self) -> Result<(Register, Register, String), Self::Error> {
        match self.args {
            HighLevelArgs {
                imm: HighLevelImmediate::CodeLabel(addr),
                rd: None,
                rs1: Some(rs1),
                rs2: Some(rs2),
            } => Ok((
                Register::new(*rs1 as u8),
                Register::new(*rs2 as u8),
                self.symbol_table.get_as_string(*addr),
            )),
            _ => Err(format!("Expected: rs1, rs2, label, got {:?}", self.args)),
        }
    }

    fn rl(&self) -> Result<(Register, String), Self::Error> {
        match self.args {
            HighLevelArgs {
                imm: HighLevelImmediate::CodeLabel(addr),
                rd: None,
                rs1: Some(rs1),
                rs2: None,
            } => Ok((
                Register::new(*rs1 as u8),
                self.symbol_table.get_as_string(*addr),
            )),
            HighLevelArgs {
                imm: HighLevelImmediate::CodeLabel(addr),
                rd: Some(rd),
                rs1: None,
                rs2: None,
            } => Ok((
                Register::new(*rd as u8),
                self.symbol_table.get_as_string(*addr),
            )),
            _ => Err(format!("Expected: {{rs1|rd}}, label, got {:?}", self.args)),
        }
    }

    fn rro(&self) -> Result<(Register, Register, u32), Self::Error> {
        match self.args {
            HighLevelArgs {
                imm: HighLevelImmediate::Value(imm),
                rd: Some(rd),
                rs1: Some(rs1),
                rs2: None,
            } => Ok((
                Register::new(*rd as u8),
                Register::new(*rs1 as u8),
                *imm as u32,
            )),
            HighLevelArgs {
                imm: HighLevelImmediate::Value(imm),
                rd: None,
                rs1: Some(rs1),
                rs2: Some(rs2),
            } => Ok((
                Register::new(*rs2 as u8),
                Register::new(*rs1 as u8),
                *imm as u32,
            )),
            _ => Err(format!(
                "Expected: {{rd, rs2 | rs1, rs1}}, imm, got {:?}",
                self.args
            )),
        }
    }

    fn empty(&self) -> Result<(), Self::Error> {
        match self.args {
            HighLevelArgs {
                imm: HighLevelImmediate::None,
                rd: None,
                rs1: None,
                rs2: None,
            } => Ok(()),
            _ => Err(format!("Expected: no args, got {:?}", self.args)),
        }
    }
}

struct AddressMap<'a>(BTreeMap<u32, &'a program_header::ProgramHeader>);

impl AddressMap<'_> {
    fn is_in_data_section(&self, addr: u32) -> bool {
        if let Some(section) = self.get_section_of_addr(addr) {
            section.p_flags & 1 != 1
        } else {
            false
        }
    }

    fn is_in_text_section(&self, addr: u32) -> bool {
        if let Some(section) = self.get_section_of_addr(addr) {
            section.p_flags & 1 == 1
        } else {
            false
        }
    }

    fn get_section_of_addr(&self, addr: u32) -> Option<&program_header::ProgramHeader> {
        // Get the latest section that starts before the address.
        let section = self
            .0
            .range(..=addr)
            .next_back()
            .map(|(_, &section)| section)?;

        if addr > section.p_vaddr as u32 + section.p_memsz as u32 {
            // The address is after the end of the section.
            None
        } else {
            Some(section)
        }
    }
}

#[derive(Debug)]
enum Data {
    TextLabel(u32),
    Value(u32),
}

fn load_data_section(mut addr: u32, data: &[u8], data_map: &mut BTreeMap<u32, Data>) {
    for chunk in data.chunks(4) {
        let mut padded = [0; 4];
        padded[..chunk.len()].copy_from_slice(chunk);

        let value = u32::from_le_bytes(padded);
        if value != 0 {
            data_map.insert(addr, Data::Value(value));
        } else {
            // We don't need to store zero values, as they are implicit.
        }

        addr += 4;
    }
}

struct MaybeInstruction {
    address: u32,
    insn: Option<Ins>,
}

#[derive(Debug)]
enum HighLevelImmediate {
    None,
    CodeLabel(u32),
    Value(i32),
}

#[derive(Debug)]
struct HighLevelArgs {
    rd: Option<u32>,
    rs1: Option<u32>,
    rs2: Option<u32>,
    imm: HighLevelImmediate,
}

/// The default args are all empty.
impl Default for HighLevelArgs {
    fn default() -> Self {
        HighLevelArgs {
            rd: None,
            rs1: None,
            rs2: None,
            imm: HighLevelImmediate::None,
        }
    }
}

struct HighLevelInsn {
    original_address: u32,
    op: &'static str,
    args: HighLevelArgs,
}

struct InstructionLifter<'a> {
    rellocs_set: &'a BTreeSet<u32>,
    address_map: &'a AddressMap<'a>,
    referenced_text_addrs: &'a mut BTreeSet<u32>,
}

impl InstructionLifter<'_> {
    fn composed_immediate(
        &self,
        hi: i32,
        lo: i32,
        rd_ui: usize,
        rd_addi: usize,
        is_address: bool,
    ) -> Option<(&'static str, HighLevelArgs)> {
        let immediate = hi.wrapping_add(lo);

        let is_ref_to_text = is_address && self.address_map.is_in_text_section(immediate as u32) &&
            // This is very sad: sometimes the global pointer lands in the
            // middle of the text section, so we have to make an exception when
            // setting the gp (x3).
            rd_addi != 3;

        let (op, imm) = if is_ref_to_text {
            // If rd_ui != rd_addi, we don't set rd_ui, thus and our behavior is
            // not conformant, but it is probably fine for compiler generated
            // code, and it has worked so far.
            ("la", HighLevelImmediate::CodeLabel(immediate as u32))
        } else if rd_ui == rd_addi {
            ("li", HighLevelImmediate::Value(immediate))
        } else {
            // This pair of instructions leaks rd_ui. Since this is not a
            // reference to text, we can afford to be more conformant and handle
            // each instruction separately.
            return None;
        };

        Some((
            op,
            HighLevelArgs {
                rd: Some(rd_ui as u32),
                imm,
                ..Default::default()
            },
        ))
    }
}

impl TwoOrOneMapper<MaybeInstruction, HighLevelInsn> for InstructionLifter<'_> {
    fn try_map_two(
        &mut self,
        insn1: &MaybeInstruction,
        insn2: &MaybeInstruction,
    ) -> Option<HighLevelInsn> {
        let original_address = insn1.address;
        let (Some(insn1), Some(insn2)) = (&insn1.insn, &insn2.insn) else {
            return None;
        };

        let result = match (insn1, insn2) {
            (
                // li rd, immediate
                Ins {
                    opc: Op::LUI,
                    rd: Some(rd_lui),
                    imm: Some(hi),
                    ..
                },
                Ins {
                    opc: Op::ADDI,
                    rd: Some(rd_addi),
                    rs1: Some(rs1_addi),
                    imm: Some(lo),
                    ..
                },
            ) if rd_lui == rs1_addi => {
                // Sometimes, in non-PIE code, this pair of instructions is used
                // to load an address into a register. We must check if this is
                // the case, and if the address is in the text section, so we
                // must load it from a label.
                let is_address = self.rellocs_set.contains(&original_address);
                let (op, args) =
                    self.composed_immediate(*hi, *lo, *rd_lui, *rd_addi, is_address)?;

                HighLevelInsn {
                    op,
                    args,
                    original_address,
                }
            }
            (
                // All other double instructions we can lift starts with auipc.
                // Furthermore, we have to join every auipc, as we don't support
                // it independently.
                Ins {
                    opc: Op::AUIPC,
                    rd: Some(rd_auipc),
                    imm: Some(hi),
                    ..
                },
                insn2,
            ) => {
                let hi = hi.wrapping_add(original_address as i32);
                match insn2 {
                    // la rd, symbol
                    Ins {
                        opc: Op::ADDI,
                        rd: Some(rd_addi),
                        rs1: Some(rs1_addi),
                        imm: Some(lo),
                        ..
                    } if rd_auipc == rs1_addi => {
                        // AUIPC obviously always refer to an address.
                        let (op, args) =
                            self.composed_immediate(hi, *lo, *rd_auipc, *rd_addi, true)?;

                        HighLevelInsn {
                            op,
                            args,
                            original_address,
                        }
                    }
                    // TODO: uncomment when powdr supports the pseudoinstruction
                    // version of l{b|h|w} and s{b|h|w}. For now, it is better
                    // to just fail here if we encounter this usage of auipc.
                    /*
                    // l{b|h|w} rd, symbol
                    Ins {
                        opc: l_op,
                        rd: Some(rd_l),
                        rs1: Some(rs1_l),
                        rs2: None,
                        imm: Some(lo),
                        ..
                    } if matches!(l_op, Op::LB | Op::LH | Op::LW)
                        && rd_auipc == rd_l
                        && rd_l == rs1_l =>
                    {
                        HighLevelInsn {
                            op: l_op.to_string(),
                            args: HighLevelArgs {
                                rd: Some(*rd_l as u32),
                                imm: HighLevelImmediate::Value(hi.wrapping_add(*lo)),
                                ..Default::default()
                            },
                            original_address,
                        }
                    }
                    // s{b|h|w} rd, symbol, rt
                    Ins {
                        opc: l_op,
                        rd: None,
                        rs1: Some(rt_l),
                        rs2: Some(rd),
                        imm: Some(lo),
                        ..
                    } if matches!(l_op, Op::LB | Op::LH | Op::LW) && rd_auipc == rt_l => {
                        HighLevelInsn {
                            op: l_op.to_string(),
                            args: HighLevelArgs {
                                // TODO: If this pseudoinstruction is ever
                                // implemented in powdr, rs1 should end up
                                // containing the output of auipc, a value which
                                // doen't make sense in powdr.
                                rs1: Some(*rd_auipc as u32),
                                rs2: Some(*rd as u32),
                                imm: HighLevelImmediate::Value(hi.wrapping_add(*lo)),
                                ..Default::default()
                            },
                            original_address,
                        }
                    }
                    */
                    // call offset
                    Ins {
                        opc: Op::JALR,
                        rd: Some(1),
                        rs1: Some(1),
                        rs2: None,
                        imm: Some(lo),
                        ..
                    } if *rd_auipc == 1 => HighLevelInsn {
                        op: "call",
                        args: HighLevelArgs {
                            imm: HighLevelImmediate::CodeLabel(hi.wrapping_add(*lo) as u32),
                            ..Default::default()
                        },
                        original_address,
                    },
                    // tail offset
                    Ins {
                        opc: Op::JALR,
                        rd: Some(0),
                        rs1: Some(6),
                        rs2: None,
                        imm: Some(lo),
                        ..
                    } if *rd_auipc == 6 => HighLevelInsn {
                        op: "tail",
                        args: HighLevelArgs {
                            imm: HighLevelImmediate::CodeLabel(hi.wrapping_add(*lo) as u32),
                            ..Default::default()
                        },
                        original_address,
                    },
                    _ => {
                        panic!(
                            "Unexpected instruction after AUIPC: {:?} at {:08x}",
                            insn2, original_address
                        );
                    }
                }
            }
            _ => return None,
        };

        // TODO: implement here other kinds of RISC-V fusions as optimization.

        if let HighLevelImmediate::CodeLabel(addr) = &result.args.imm {
            self.referenced_text_addrs.insert(*addr);
        }

        Some(result)
    }

    fn map_one(&mut self, insn: MaybeInstruction) -> HighLevelInsn {
        let original_address = insn.address;
        let Some(insn) = insn.insn else {
            return HighLevelInsn {
                op: "unimp",
                args: Default::default(),
                original_address,
            };
        };

        let imm = match insn.opc {
            // All jump instructions that have the immediate as an address
            Op::JAL | Op::BEQ | Op::BNE | Op::BLT | Op::BGE | Op::BLTU | Op::BGEU => {
                let addr = (insn.imm.unwrap() + original_address as i32) as u32;
                self.referenced_text_addrs.insert(addr);

                HighLevelImmediate::CodeLabel(addr)
            }
            // We currently only support standalone jalr if offset is zero
            Op::JALR => {
                assert!(
                    insn.imm.unwrap() == 0,
                    "jalr with non-zero offset is not supported"
                );

                HighLevelImmediate::Value(0)
            }
            // LUI is special because the decoder already shifts the immediate,
            // but the code gen expects it unshifted, so we have to undo.
            Op::LUI => HighLevelImmediate::Value(insn.imm.unwrap() >> 12),
            // We don't support arbitrary AUIPCs, but it is trivial to transform
            // one to an LI. If it passed the two-by-two transformation and got
            // here, this is a reference to data, so it is safe to transform it.
            Op::AUIPC => {
                return HighLevelInsn {
                    op: "li",
                    args: HighLevelArgs {
                        rd: insn.rd.map(|x| x as u32),
                        imm: HighLevelImmediate::Value(
                            insn.imm.unwrap().wrapping_add(original_address as i32),
                        ),
                        ..Default::default()
                    },
                    original_address,
                };
            }
            // All other instructions, which have the immediate as a value
            _ => match insn.imm {
                Some(imm) => HighLevelImmediate::Value(imm as i32),
                None => HighLevelImmediate::None,
            },
        };

        // TODO: lift other instructions to their pseudoinstructions,
        // because they can have simplified implementations (like the
        // branch-zero variants and add to x0).

        let result = HighLevelInsn {
            op: insn.opc.to_string(),
            args: HighLevelArgs {
                rd: insn.rd.map(|x| x as u32),
                rs1: insn.rs1.map(|x| x as u32),
                rs2: insn.rs2.map(|x| x as u32),
                imm,
            },
            original_address,
        };

        result
    }
}

/// Lift the instructions back to higher-level instructions.
///
/// Turn addresses into labels and and merge instructions into
/// pseudoinstructions.
fn lift_instructions(
    base_addr: u32,
    data: &[u8],
    address_map: &AddressMap,
    rellocs_set: &BTreeSet<u32>,
    referenced_text_addrs: &mut BTreeSet<u32>,
) -> Vec<HighLevelInsn> {
    let instructions = RiscVInstructionIterator::new(base_addr, data);

    let pseudo_converter = InstructionLifter {
        rellocs_set,
        address_map,
        referenced_text_addrs,
    };
    try_map_two_by_two(instructions, pseudo_converter)
}

struct RiscVInstructionIterator<'a> {
    curr_address: u32,
    remaining_data: &'a [u8],
}

impl RiscVInstructionIterator<'_> {
    fn new(base_addr: u32, data: &[u8]) -> RiscVInstructionIterator {
        RiscVInstructionIterator {
            curr_address: base_addr,
            remaining_data: data,
        }
    }
}

impl Iterator for RiscVInstructionIterator<'_> {
    type Item = MaybeInstruction;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining_data.is_empty() {
            return None;
        }

        // Decide if the next instruction is 32 bits or 16 bits ("C" extension):
        let advance;
        let maybe_insn;
        if self.remaining_data[0] & 0b11 == 0b11 {
            // 32 bits
            advance = 4;
            let insn = u32::from_le_bytes(
                self.remaining_data[0..4]
                    .try_into()
                    .expect("Not enough bytes to complete a 32-bit instruction!"),
            )
            .decode(Isa::Rv32)
            .expect("Failed to decode instruction.");

            maybe_insn = MaybeInstruction {
                address: self.curr_address as u32,
                insn: Some(insn),
            };
        } else {
            // 16 bits
            advance = 2;
            let bin_instruction = u16::from_le_bytes(
                self.remaining_data[0..2]
                    .try_into()
                    .expect("Not enough bytes to complete a 16-bit instruction!"),
            );
            maybe_insn = MaybeInstruction {
                address: self.curr_address as u32,
                insn: match bin_instruction.decode(Isa::Rv32) {
                    Ok(c_insn) => Some(to_32bit_equivalent(c_insn)),
                    Err(raki::decode::DecodingError::IllegalInstruction) => {
                        // Although not a real RISC-V instruction, sometimes 0x0000
                        // is used on purpose as an illegal instruction (it even has
                        // its own mnemonic "unimp"), so we support it here.
                        // Otherwise, there is something more fishy going on, and we
                        // panic.
                        assert_eq!(bin_instruction, 0, "Illegal instruction found!");
                        None
                    }
                    Err(err) => panic!("Unexpected decoding error: {err:?}"),
                },
            };
        }

        // Advance the address and the data
        self.curr_address += advance;
        self.remaining_data = &self.remaining_data[advance as usize..];

        Some(maybe_insn)
    }
}

/// Translates an extension "C" instruction to the equivalent 32-bit instruction.
fn to_32bit_equivalent(mut insn: Ins) -> Ins {
    let new_opc = match insn.opc {
        Op::C_LW => Op::LW,
        Op::C_SW => Op::SW,
        Op::C_NOP => {
            return Ins {
                opc: Op::ADDI,
                rd: Some(0),
                rs1: Some(0),
                ..insn
            }
        }
        Op::C_ADDI | Op::C_ADDI16SP => Op::ADDI,
        Op::C_ADDI4SPN => {
            return Ins {
                opc: Op::ADDI,
                rs1: Some(2), // add to x2 (stack pointer)
                ..insn
            };
        }
        Op::C_LI => {
            return Ins {
                opc: Op::ADDI,
                rs1: Some(0),
                ..insn
            }
        }
        Op::C_JAL => {
            return Ins {
                opc: Op::JAL,
                rd: Some(1), // output to x1 (return address)
                ..insn
            };
        }
        Op::C_LUI => Op::LUI,
        Op::C_SRLI => Op::SRLI,
        Op::C_SRAI => Op::SRAI,
        Op::C_ANDI => Op::ANDI,
        Op::C_SUB => Op::SUB,
        Op::C_XOR => Op::XOR,
        Op::C_OR => Op::OR,
        Op::C_AND => Op::AND,
        Op::C_J => {
            return Ins {
                opc: Op::JAL,
                rd: Some(0), // discard output
                ..insn
            };
        }
        Op::C_BEQZ => {
            return Ins {
                opc: Op::BEQ,
                rs2: Some(0), // compare with zero
                ..insn
            };
        }
        Op::C_BNEZ => {
            return Ins {
                opc: Op::BNE,
                rs2: Some(0), // compare with zero
                ..insn
            };
        }
        Op::C_SLLI => Op::SLLI,
        Op::C_LWSP => {
            return Ins {
                opc: Op::LW,
                rs1: Some(2), // load relative to x2 (stack pointer)
                ..insn
            };
        }
        Op::C_JR => {
            return Ins {
                opc: Op::JALR,
                // discard the return address:
                rd: Some(0),
                // There is a binary value for rs2 in C.JR (set to 0), which is
                // returned by the decoder, but there isn't an equivalent to the
                // expanded JALR instruction, so we must set None here:
                rs2: None,
                imm: Some(0),
                ..insn
            };
        }
        Op::C_MV => {
            return Ins {
                opc: Op::ADD,
                rs1: Some(0), // add to zero
                ..insn
            };
        }
        Op::C_EBREAK => Op::EBREAK,
        Op::C_JALR => {
            return Ins {
                opc: Op::JALR,
                // output to x1 (return address):
                rd: Some(1),
                // There is a binary value for rs2 in C.JALR (set to 0), which
                // is returned by the decoder, but there isn't an equivalent to
                // the expanded JALR instruction, so we must set None here:
                rs2: None,
                imm: Some(0), // jump to the exact address
                ..insn
            };
        }
        Op::C_ADD => Op::ADD,
        Op::C_SWSP => {
            return Ins {
                opc: Op::SW,
                rs1: Some(2), // store relative to x2 (stack pointer)
                ..insn
            };
        }
        Op::C_LD | Op::C_SD | Op::C_ADDIW | Op::C_SUBW | Op::C_ADDW | Op::C_LDSP | Op::C_SDSP => {
            unreachable!("not a riscv32 instruction")
        }
        _ => unreachable!("not a RISC-V \"C\" extension instruction"),
    };

    insn.opc = new_opc;
    insn
}

trait TwoOrOneMapper<E, R> {
    fn try_map_two(&mut self, first: &E, second: &E) -> Option<R>;
    fn map_one(&mut self, element: E) -> R;
}

/// Takes an iterator, and maps the elements two by two. If fails, maps
/// individually.
///
/// TODO: this would be more elegant as a generator, but they are unstable.
fn try_map_two_by_two<E, R>(
    input: impl Iterator<Item = E>,
    mut mapper: impl TwoOrOneMapper<E, R>,
) -> Vec<R> {
    let mut result = Vec::new();
    let mut iter = input.peekable();

    while let Some(first) = iter.next() {
        if let Some(second) = iter.peek() {
            if let Some(mapped) = mapper.try_map_two(&first, second) {
                result.push(mapped);
                iter.next();
            } else {
                result.push(mapper.map_one(first));
            }
        } else {
            result.push(mapper.map_one(first));
        }
    }

    result
}
