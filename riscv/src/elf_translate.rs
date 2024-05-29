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
    println!("{:#?}", elf);

    // Index the sections by their virtual address
    let mut text_sections = BTreeMap::new();
    let mut data_sections = BTreeMap::new();

    for p in elf.program_headers.iter() {
        if p.p_type == program_header::PT_LOAD {
            // Test if executable
            if p.p_flags & 1 == 1 {
                text_sections.insert(
                    p.p_vaddr as u32,
                    // Slice containing the section data. Since this is a
                    // text section, we assume any zeroed part beyond
                    // p_filesz (if any) is not relevant.
                    &file_buffer[p.p_offset as usize..(p.p_offset + p.p_filesz) as usize],
                );
            } else {
                data_sections.insert(p.p_vaddr, p);
            }
        }
    }

    extract_reachable_code(elf.entry.try_into().unwrap(), &text_sections);
    todo!();
}

fn extract_reachable_code(entry_point: u32, text_sections: &BTreeMap<u32, &[u8]>) {
    // Helper function to find the section containing the address
    let find_section_of_address = |addr| {
        let (&section_addr, &data) = text_sections
            .range(..=addr)
            .next_back()
            .expect("Jump address not found in any .text section");
        (section_addr, data)
    };

    let mut visited = HashSet::new();
    let mut to_visit = vec![find_section_of_address(entry_point)];

    while let Some((addr, section_data)) = to_visit.pop() {
        // Sanity check of the alignment
        assert_eq!(addr % 2, 0);

        visited.insert(addr);

        // She entire section should be code, so decode and translate it from start.
        let code = lift_instructions(addr, section_data);
    }
}

enum HighLevelImmediate {
    None,
    CodeLabel(i32), // The value is the original address
    Value(i32),
}

struct HighLevelInsn<'a> {
    original_address: u32,
    op: &'a str,
    rd: Option<u32>,
    rs1: Option<u32>,
    rs2: Option<u32>,
    imm: HighLevelImmediate,
}

struct PseudoInstructionConverter {
    base_addr: u32,
}

impl TwoOrOneMapper<Ins, HighLevelInsn<'static>> for PseudoInstructionConverter {
    fn try_map_two(&mut self, insn1: &Ins, insn2: &Ins) -> Option<HighLevelInsn<'static>> {
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
                imm: HighLevelImmediate::Value((*hi << 12) | *lo),
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
                let hi = self.base_addr as i32 + (*hi << 12);
                match insn2 {
                    // la rd, symbol
                    Ins {
                        opc: Op::ADDI,
                        rd: Some(rd_addi),
                        rs1: Some(rs1_addi),
                        imm: Some(lo),
                        ..
                    } if rd_auipc == rd_addi && rd_auipc == rs1_addi => HighLevelInsn {
                        op: "la",
                        rd: Some(*rd_auipc as u32),
                        rs1: None,
                        rs2: None,
                        imm: HighLevelImmediate::CodeLabel(hi + lo),
                        original_address: self.base_addr,
                    },
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

        Some(result)
    }

    fn map_one(&mut self, insn: Ins) -> HighLevelInsn<'static> {
        let imm = match insn.opc {
            // All jump instructions that have the immediate as an address
            Op::JAL | Op::BEQ | Op::BNE | Op::BLT | Op::BGE | Op::BLTU | Op::BGEU => {
                HighLevelImmediate::CodeLabel(insn.imm.unwrap() + self.base_addr as i32)
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
fn lift_instructions(base_addr: u32, data: &[u8]) -> Vec<HighLevelInsn> {
    let instructions = RiscVInstructionIterator::new(data);

    let pseudo_converter = PseudoInstructionConverter { base_addr };
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
    type Item = Ins;

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
        } else {
            // 16 bits
            advance = 2;
            let c_insn = u16::from_le_bytes(
                self.remaining_data[0..2]
                    .try_into()
                    .expect("Not enough bytes to complete a 16-bit instruction!"),
            )
            .decode(Isa::Rv32)
            .expect("Failed to decode instruction.");

            insn = to_32bit_equivalent(c_insn);
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
                opc: Op::C_ADDI,
                rd: Some(0),
                rs1: Some(0),
                ..insn
            }
        }
        Op::C_ADDI | Op::C_ADDI16SP => Op::ADDI,
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
