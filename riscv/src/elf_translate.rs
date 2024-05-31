use std::{
    collections::{BTreeMap, HashSet},
    fs,
};

use goblin::elf::{program_header, Elf};
use raki::{
    decode::Decode,
    instruction::{Extensions, Instruction as Ins, OpcodeKind as Op},
    Isa,
};

pub fn elf_translate(file_name: &str) {
    let file_buffer = fs::read(file_name).unwrap();

    let elf = Elf::parse(&file_buffer).unwrap();

    // Map of addresses into memory sections, so we can know what address belong
    // in what section.
    let mut address_map = AddressMap(BTreeMap::new());

    // We simply extract all the text and data sections. There is no need to
    // perform reachability search, because we trust the linker to have done
    // that already.
    //
    // TODO: maybe exclude the section mapping the ELF header itself? There is
    // also this dynamic section that I think we can ignore. But with
    // continuations on, unused memory is practically free.
    let mut lifted_text_sections = Vec::new();
    let mut data_map = BTreeMap::new();

    // Keep a list of referenced text addresses, so we can generate the labels.
    let mut referenced_text_addrs = HashSet::from([elf.entry.try_into().unwrap()]);
    println!("entry: {:08x}:", elf.entry);

    for p in elf.program_headers.iter() {
        if p.p_type == program_header::PT_LOAD {
            let addr = p.p_vaddr as u32;
            let section_data =
                &file_buffer[p.p_offset as usize..(p.p_offset + p.p_filesz) as usize];

            address_map.0.insert(addr, p);

            // Test if executable
            if p.p_flags & 1 == 1 {
                let insns =
                    lift_instructions(addr, section_data, &address_map, &mut referenced_text_addrs);
                lifted_text_sections.push(insns);
            } else {
                load_data_section(addr, section_data, &mut data_map);
            }
        }
    }

    // All the references to code address have been lifted into labels in the
    // instructions, but not the data sections. Luckily, that is just a matter of
    // reading the dynamic relocation table.
    for r in elf.dynrelas.iter() {
        let addr = r.r_offset as u32;
        if address_map.is_in_data_section(addr) {
            // We only support the R_RISCV_RELATIVE relocation type:
            assert_eq!(r.r_type, 3, "Unsupported relocation type!");

            let original_addr = r.r_addend.unwrap() as u32;

            if address_map.is_in_text_section(original_addr) {
                data_map.insert(addr, Data::TextLabel(original_addr));

                // We also need to add the referenced address to the list of text
                // addresses, so we can generate the label.
                referenced_text_addrs.insert(original_addr);
                println!("reloc: {:08x}:", original_addr);
            } else {
                data_map.insert(addr, Data::Value(original_addr));
            }
        } else {
            panic!("Unsupported relocation in non-data section!\nTODO: maybe this is fine. Maybe the lifting of instructions have already taken care of this.");
        }
    }

    assert_eq!(elf.dynrels.len(), 0, "Unsupported relocation type!");

    println!("Text labels:");
    for label in referenced_text_addrs {
        println!("  label_{:08x}:", label);
    }

    println!("Non-zero data:");
    for (addr, data) in data_map {
        println!("  {addr:08x}: {data:?}:");
    }

    todo!();
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

enum MaybeInstruction {
    Unimplemented,
    Valid(Ins),
}

impl From<Ins> for MaybeInstruction {
    fn from(insn: Ins) -> Self {
        MaybeInstruction::Valid(insn)
    }
}

enum HighLevelImmediate {
    None,
    CodeLabel(i32), // The value is the original address
    Value(i32),
}

struct HighLevelInsn {
    original_address: u32,
    op: &'static str,
    rd: Option<u32>,
    rs1: Option<u32>,
    rs2: Option<u32>,
    imm: HighLevelImmediate,
}

struct InstructionLifter<'a> {
    base_addr: u32,
    address_map: &'a AddressMap<'a>,
    referenced_text_addrs: &'a mut HashSet<u32>,
}

impl TwoOrOneMapper<MaybeInstruction, HighLevelInsn> for InstructionLifter<'_> {
    fn try_map_two(
        &mut self,
        insn1: &MaybeInstruction,
        insn2: &MaybeInstruction,
    ) -> Option<HighLevelInsn> {
        let (insn1, insn2) = match (insn1, insn2) {
            (MaybeInstruction::Valid(insn1), MaybeInstruction::Valid(insn2)) => (insn1, insn2),
            _ => return None,
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
            ) if rd_lui == rd_addi && rd_lui == rs1_addi => HighLevelInsn {
                op: "li",
                rd: Some(*rd_lui as u32),
                rs1: None,
                rs2: None,
                imm: HighLevelImmediate::Value(*hi | *lo),
                original_address: self.base_addr,
            },
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
                let hi = self.base_addr as i32 + *hi;
                match insn2 {
                    // la rd, symbol
                    Ins {
                        opc: Op::ADDI,
                        rd: Some(rd_addi),
                        rs1: Some(rs1_addi),
                        imm: Some(lo),
                        ..
                    } if rd_auipc == rd_addi && rd_auipc == rs1_addi => {
                        let imm_addr = hi + lo;
                        let imm = if self.address_map.is_in_text_section(imm_addr as u32) {
                            HighLevelImmediate::CodeLabel(imm_addr)
                        } else {
                            HighLevelImmediate::Value(imm_addr)
                        };
                        HighLevelInsn {
                            op: "la",
                            rd: Some(*rd_auipc as u32),
                            rs1: None,
                            rs2: None,
                            imm,
                            original_address: self.base_addr,
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
                            rd: Some(*rd_l as u32),
                            rs1: None,
                            rs2: None,
                            imm: HighLevelImmediate::Value(hi + lo),
                            original_address: self.base_addr,
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
                            rd: None,
                            // TODO: If this pseudoinstruction is ever
                            // implemented in powdr, rs1 should end up
                            // containing the output of auipc, a value which
                            // doen't make sense in powdr.
                            rs1: Some(*rd_auipc as u32),
                            rs2: Some(*rd as u32),
                            imm: HighLevelImmediate::Value(hi + lo),
                            original_address: self.base_addr,
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
                        rd: None,
                        rs1: None,
                        rs2: None,
                        imm: HighLevelImmediate::CodeLabel(hi + lo),
                        original_address: self.base_addr,
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
                        rd: None,
                        rs1: None,
                        rs2: None,
                        imm: HighLevelImmediate::CodeLabel(hi + lo),
                        original_address: self.base_addr,
                    },
                    _ => panic!("auipc could not be joined!"),
                }
            }
            _ => return None,
        };

        self.base_addr += [insn1, insn2].map(ins_size).into_iter().sum::<u32>();

        if let HighLevelImmediate::CodeLabel(addr) = &result.imm {
            self.referenced_text_addrs.insert(*addr as u32);
            println!("insn {}: {:08x}", result.op, addr);
        }

        Some(result)
    }

    fn map_one(&mut self, insn: MaybeInstruction) -> HighLevelInsn {
        let MaybeInstruction::Valid(insn) = insn else {
            return HighLevelInsn {
                op: "unimp",
                rd: None,
                rs1: None,
                rs2: None,
                imm: HighLevelImmediate::None,
                original_address: self.base_addr,
            };
        };

        let imm = match insn.opc {
            // All jump instructions that have the immediate as an address
            Op::JAL | Op::BEQ | Op::BNE | Op::BLT | Op::BGE | Op::BLTU | Op::BGEU => {
                let addr = insn.imm.unwrap() + self.base_addr as i32;
                self.referenced_text_addrs.insert(addr as u32);

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
            // We currently don't support auipc by itself
            Op::AUIPC => panic!("auipc could not be joined!"),
            // All other instructions, which have the immediate as a value
            _ => match insn.imm {
                Some(imm) => HighLevelImmediate::Value(imm as i32),
                None => HighLevelImmediate::None,
            },
        };

        // We don't need to lift the branch instructions to their Z versions,
        // because powdr's optimizer should be able to figure out the comparison is
        // against a constant. But if needed, we could do it here...

        let result = HighLevelInsn {
            op: insn.opc.to_string(),
            rd: insn.rd.map(|x| x as u32),
            rs1: insn.rs1.map(|x| x as u32),
            rs2: insn.rs2.map(|x| x as u32),
            imm,
            original_address: self.base_addr,
        };

        self.base_addr += ins_size(&insn);

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
    referenced_text_addrs: &mut HashSet<u32>,
) -> Vec<HighLevelInsn> {
    let instructions = RiscVInstructionIterator::new(data);

    let pseudo_converter = InstructionLifter {
        base_addr,
        address_map,
        referenced_text_addrs,
    };
    try_map_two_by_two(instructions, pseudo_converter)
}

struct RiscVInstructionIterator<'a> {
    remaining_data: &'a [u8],
}

impl RiscVInstructionIterator<'_> {
    fn new(data: &[u8]) -> RiscVInstructionIterator {
        RiscVInstructionIterator {
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
        let insn;
        if self.remaining_data[0] & 0b11 == 0b11 {
            // 32 bits
            advance = 4;
            insn = u32::from_le_bytes(
                self.remaining_data[0..4]
                    .try_into()
                    .expect("Not enough bytes to complete a 32-bit instruction!"),
            )
            .decode(Isa::Rv32)
            .expect("Failed to decode instruction.")
            .into()
        } else {
            // 16 bits
            advance = 2;
            let bin_instruction = u16::from_le_bytes(
                self.remaining_data[0..2]
                    .try_into()
                    .expect("Not enough bytes to complete a 16-bit instruction!"),
            );
            insn = match bin_instruction.decode(Isa::Rv32) {
                Ok(c_insn) => to_32bit_equivalent(c_insn).into(),
                Err(raki::decode::DecodingError::IllegalInstruction) => {
                    // Although not a real RISC-V instruction, sometimes 0x0000
                    // is used on purpose as an illegal instruction (it even has
                    // its own mnemonic "unimp"), so we support it here.
                    // Otherwise, there is something more fishy going on, and we
                    // panic.
                    assert_eq!(bin_instruction, 0, "Illegal instruction found!");
                    MaybeInstruction::Unimplemented
                }
                Err(err) => panic!("Unexpected decoding error: {err:?}"),
            };
        }

        // Advance the iterator
        self.remaining_data = &self.remaining_data[advance..];

        Some(insn)
    }
}

/// Get the size, in bytes, of an instruction.
fn ins_size(ins: &Ins) -> u32 {
    match ins.extension {
        Extensions::C => 2,
        _ => 4,
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
        Op::C_SLLI => Op::C_SLLI,
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
                rd: Some(0),  // discard the return address
                imm: Some(0), // jump to the exact address
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
                rd: Some(1),  // output to x1 (return address)
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
