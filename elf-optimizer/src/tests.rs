use super::*;
use crate::bytecopy::*;

/// Minimal RISC-V interpreter for raw u32 instructions.
struct RvInterpreter {
    regs: [u32; 32],
    mem: Vec<u8>,
    pc: u32, // byte address
}

impl RvInterpreter {
    fn new(mem_size: usize) -> Self {
        Self {
            regs: [0; 32],
            mem: vec![0; mem_size],
            pc: 0,
        }
    }

    fn reg(&self, r: u32) -> u32 {
        if r == 0 {
            0
        } else {
            self.regs[r as usize]
        }
    }

    fn set_reg(&mut self, r: u32, val: u32) {
        if r != 0 {
            self.regs[r as usize] = val;
        }
    }

    fn run(&mut self, instrs: &[u32], max_steps: usize) {
        let trace = std::env::var("MEMCPY_TRACE").is_ok();
        for step in 0..max_steps {
            let idx = (self.pc / 4) as usize;
            if idx >= instrs.len() {
                return; // PC left the instruction array — execution complete
            }
            let insn = instrs[idx];
            let opcode = rv_opcode(insn);
            let rd = rv_rd(insn);
            let rs1 = rv_rs1(insn);
            let funct3 = rv_funct3(insn);

            if trace {
                eprintln!(
                    "  step {:4}: pc=0x{:04x} insn=0x{:08x}",
                    step, self.pc, insn
                );
            }

            match opcode {
                0x13 => {
                    // I-type ALU: ADDI, ANDI, ORI, SRLI, etc.
                    let imm = rv_imm_i(insn);
                    let rs1_val = self.reg(rs1);
                    let result = match funct3 {
                        0b000 => rs1_val.wrapping_add(imm as u32), // ADDI
                        0b001 => rs1_val << ((imm as u32) & 0x1F), // SLLI
                        0b010 => {
                            // SLTI
                            if (rs1_val as i32) < imm {
                                1
                            } else {
                                0
                            }
                        }
                        0b011 => {
                            // SLTIU
                            if rs1_val < (imm as u32) {
                                1
                            } else {
                                0
                            }
                        }
                        0b100 => rs1_val ^ (imm as u32), // XORI
                        0b101 => {
                            let shamt = (imm as u32) & 0x1F;
                            let funct7 = (insn >> 25) & 0x7F;
                            if funct7 == 0b0100000 {
                                ((rs1_val as i32) >> shamt) as u32 // SRAI
                            } else {
                                rs1_val >> shamt // SRLI
                            }
                        }
                        0b110 => rs1_val | (imm as u32),  // ORI
                        0b111 => rs1_val & (imm as u32),  // ANDI
                        _ => panic!("Unknown I-type ALU funct3: {funct3}"),
                    };
                    self.set_reg(rd, result);
                    self.pc += 4;
                }
                0x33 => {
                    // R-type ALU
                    let rs2 = (insn >> 20) & 0x1F;
                    let funct7 = insn >> 25;
                    let rs1_val = self.reg(rs1);
                    let rs2_val = self.reg(rs2);
                    let result = match (funct3, funct7) {
                        (0b000, 0b0000000) => rs1_val.wrapping_add(rs2_val), // ADD
                        (0b000, 0b0100000) => rs1_val.wrapping_sub(rs2_val), // SUB
                        (0b001, 0b0000000) => rs1_val << (rs2_val & 0x1F),   // SLL
                        (0b010, 0b0000000) => {
                            // SLT
                            if (rs1_val as i32) < (rs2_val as i32) {
                                1
                            } else {
                                0
                            }
                        }
                        (0b011, 0b0000000) => {
                            // SLTU
                            if rs1_val < rs2_val {
                                1
                            } else {
                                0
                            }
                        }
                        (0b100, 0b0000000) => rs1_val ^ rs2_val, // XOR
                        (0b101, 0b0000000) => rs1_val >> (rs2_val & 0x1F), // SRL
                        (0b101, 0b0100000) => {
                            ((rs1_val as i32) >> (rs2_val & 0x1F)) as u32 // SRA
                        }
                        (0b110, _) => rs1_val | rs2_val,  // OR
                        (0b111, _) => rs1_val & rs2_val,  // AND
                        _ => panic!("Unknown R-type funct3={funct3} funct7={funct7}"),
                    };
                    self.set_reg(rd, result);
                    self.pc += 4;
                }
                0x03 => {
                    // Loads
                    let imm = rv_imm_i(insn);
                    let addr = self.reg(rs1).wrapping_add(imm as u32);
                    let val = match funct3 {
                        0b000 => {
                            // LB (sign-extended byte)
                            self.mem[addr as usize] as i8 as i32 as u32
                        }
                        0b001 => {
                            // LH (sign-extended halfword)
                            let a = addr as usize;
                            let hw = u16::from_le_bytes([self.mem[a], self.mem[a + 1]]);
                            hw as i16 as i32 as u32
                        }
                        0b010 => {
                            // LW
                            assert!(addr.is_multiple_of(4), "Unaligned LW at 0x{:x}", addr);
                            let a = addr as usize;
                            u32::from_le_bytes([
                                self.mem[a],
                                self.mem[a + 1],
                                self.mem[a + 2],
                                self.mem[a + 3],
                            ])
                        }
                        0b100 => {
                            // LBU
                            self.mem[addr as usize] as u32
                        }
                        0b101 => {
                            // LHU
                            let a = addr as usize;
                            u16::from_le_bytes([self.mem[a], self.mem[a + 1]]) as u32
                        }
                        _ => panic!("Unknown load funct3: {}", funct3),
                    };
                    self.set_reg(rd, val);
                    self.pc += 4;
                }
                0x23 => {
                    // Stores (S-type)
                    let rs2 = (insn >> 20) & 0x1F;
                    let imm_4_0 = (insn >> 7) & 0x1F;
                    let imm_11_5 = (insn >> 25) & 0x7F;
                    let imm = ((imm_11_5 << 5) | imm_4_0) as i32;
                    // Sign-extend from 12 bits
                    let imm = if imm & 0x800 != 0 { imm | !0xFFF } else { imm };
                    let addr = self.reg(rs1).wrapping_add(imm as u32);
                    let val = self.reg(rs2);
                    match funct3 {
                        0b000 => {
                            // SB
                            self.mem[addr as usize] = (val & 0xFF) as u8;
                        }
                        0b001 => {
                            // SH
                            let a = addr as usize;
                            self.mem[a..a + 2]
                                .copy_from_slice(&(val as u16).to_le_bytes());
                        }
                        0b010 => {
                            // SW
                            assert!(addr.is_multiple_of(4), "Unaligned SW at 0x{:x}", addr);
                            let a = addr as usize;
                            self.mem[a..a + 4].copy_from_slice(&val.to_le_bytes());
                        }
                        _ => panic!("Unknown store funct3: {}", funct3),
                    }
                    self.pc += 4;
                }
                0x63 => {
                    // B-type branches
                    let rs2 = (insn >> 20) & 0x1F;
                    let rs1_val = self.reg(rs1);
                    let rs2_val = self.reg(rs2);
                    let take = match funct3 {
                        0b000 => rs1_val == rs2_val,                         // BEQ
                        0b001 => rs1_val != rs2_val,                         // BNE
                        0b100 => (rs1_val as i32) < (rs2_val as i32),        // BLT
                        0b101 => (rs1_val as i32) >= (rs2_val as i32),       // BGE
                        0b110 => rs1_val < rs2_val,                          // BLTU
                        0b111 => rs1_val >= rs2_val,                         // BGEU
                        _ => panic!("Unknown branch funct3: {}", funct3),
                    };
                    if take {
                        // Decode B-type immediate
                        let bit_11 = (insn >> 7) & 1;
                        let bits_4_1 = (insn >> 8) & 0xF;
                        let bits_10_5 = (insn >> 25) & 0x3F;
                        let bit_12 = (insn >> 31) & 1;
                        let offset =
                            (bit_12 << 12) | (bit_11 << 11) | (bits_10_5 << 5) | (bits_4_1 << 1);
                        // Sign-extend from 13 bits
                        let offset = if bit_12 != 0 {
                            offset | !0x1FFF
                        } else {
                            offset
                        };
                        self.pc = self.pc.wrapping_add(offset);
                    } else {
                        self.pc += 4;
                    }
                }
                0x67 => {
                    // JALR
                    let imm = rv_imm_i(insn);
                    let target = (self.reg(rs1).wrapping_add(imm as u32)) & !1;
                    self.set_reg(rd, self.pc + 4);
                    self.pc = target;
                }
                0x17 => {
                    // AUIPC
                    let imm = insn & 0xFFFFF000;
                    self.set_reg(rd, self.pc.wrapping_add(imm));
                    self.pc += 4;
                }
                0x37 => {
                    // LUI
                    let imm = insn & 0xFFFFF000;
                    self.set_reg(rd, imm);
                    self.pc += 4;
                }
                0x6F => {
                    // JAL rd, offset (J-type)
                    let bits_19_12 = (insn >> 12) & 0xFF;
                    let bit_11 = (insn >> 20) & 1;
                    let bits_10_1 = (insn >> 21) & 0x3FF;
                    let bit_20 = (insn >> 31) & 1;
                    let offset =
                        (bit_20 << 20) | (bits_19_12 << 12) | (bit_11 << 11) | (bits_10_1 << 1);
                    // Sign-extend from 21 bits
                    let offset = if bit_20 != 0 {
                        offset | !0x1FFFFF
                    } else {
                        offset
                    };
                    self.set_reg(rd, self.pc + 4);
                    self.pc = self.pc.wrapping_add(offset);
                }
                _ => panic!("Unknown opcode: 0x{:02x} at pc=0x{:x}", opcode, self.pc),
            }
        }
        panic!("Max steps exceeded ({})", max_steps);
    }
}

#[test]
fn test_specialized_memcpy_all_alignments() {
    let lengths: Vec<u32> = vec![0, 1, 2, 3, 4, 5, 7, 8, 12, 15, 16, 31, 32, 48, 63, 64, 128];

    for &length in &lengths {
        eprintln!("=== Generating routine for length={} ===", length);
        let routine = rv_generate_specialized_memcpy(length);
        eprintln!("  routine has {} instructions", routine.len());

        for src_align in 0..4u32 {
            for dst_align in 0..4u32 {
                eprintln!(
                    "  Testing length={}, src_align={}, dst_align={}",
                    length, src_align, dst_align
                );
                let src_base = 256 + src_align;
                let dst_base = 768 + dst_align;

                let mut interp = RvInterpreter::new(1024);
                interp.mem.fill(0xAA);

                for i in 0..length {
                    interp.mem[(src_base + i) as usize] =
                        ((i.wrapping_mul(7).wrapping_add(13 + length)) & 0x7F) as u8;
                }

                interp.set_reg(X10, dst_base); // a0 = dst
                interp.set_reg(X11, src_base); // a1 = src
                interp.set_reg(X1, (routine.len() as u32 + 10) * 4); // RA → beyond array

                interp.run(&routine, 10_000);

                // Verify copy correctness
                for i in 0..length {
                    let expected = interp.mem[(src_base + i) as usize];
                    let actual = interp.mem[(dst_base + i) as usize];
                    assert_eq!(
                        actual, expected,
                        "Mismatch at byte {} for length={}, src_align={}, dst_align={}",
                        i, length, src_align, dst_align
                    );
                }

                // Verify no stray writes
                let guard = 16;
                let check_start = dst_base.saturating_sub(guard);
                let check_end = (dst_base + length + guard).min(1024);
                for addr in check_start..check_end {
                    if addr >= dst_base && addr < dst_base + length {
                        continue;
                    }
                    assert_eq!(
                        interp.mem[addr as usize], 0xAA,
                        "Stray write at addr {} for length={}, src_align={}, dst_align={}",
                        addr, length, src_align, dst_align
                    );
                }
            }
        }
    }
}

#[test]
fn test_rv_encoding_roundtrip() {
    // Verify AUIPC+JALR encoding produces correct target
    let from = 0x10000u32;
    let targets = [0x20000, 0x10100, 0x10800, 0x0FF00, 0x30ABC, 0x10004];
    for &to in &targets {
        let (auipc, jalr) = rv_encode_call(from, to, 5);
        let decoded = rv_auipc_jalr_target(auipc, jalr, from);
        assert_eq!(
            decoded, to,
            "Encoding roundtrip failed: from=0x{:x}, to=0x{:x}, got=0x{:x}",
            from, to, decoded
        );
    }
}

#[test]
fn test_specialized_memmove_non_overlapping() {
    // Test memmove with non-overlapping regions (should behave like memcpy)
    let lengths: Vec<u32> = vec![0, 1, 2, 3, 4, 5, 7, 8, 12, 15, 16, 31, 32, 48, 63, 64, 128];

    for &length in &lengths {
        let routine = rv_generate_specialized_memmove(length);

        for src_align in 0..4u32 {
            for dst_align in 0..4u32 {
                // dst < src (forward path)
                {
                    let src_base = 768 + src_align;
                    let dst_base = 256 + dst_align;

                    let mut interp = RvInterpreter::new(1024);
                    interp.mem.fill(0xAA);

                    for i in 0..length {
                        interp.mem[(src_base + i) as usize] =
                            ((i.wrapping_mul(7).wrapping_add(13 + length)) & 0x7F) as u8;
                    }

                    interp.set_reg(X10, dst_base);
                    interp.set_reg(X11, src_base);
                    interp.set_reg(X1, (routine.len() as u32 + 10) * 4);
                    interp.run(&routine, 10_000);

                    for i in 0..length {
                        let expected = interp.mem[(src_base + i) as usize];
                        let actual = interp.mem[(dst_base + i) as usize];
                        assert_eq!(
                            actual, expected,
                            "Forward mismatch at byte {} for length={}, src_align={}, dst_align={}",
                            i, length, src_align, dst_align
                        );
                    }
                }

                // dst > src (backward path)
                {
                    let src_base = 256 + src_align;
                    let dst_base = 768 + dst_align;

                    let mut interp = RvInterpreter::new(1024);
                    interp.mem.fill(0xAA);

                    for i in 0..length {
                        interp.mem[(src_base + i) as usize] =
                            ((i.wrapping_mul(7).wrapping_add(13 + length)) & 0x7F) as u8;
                    }

                    interp.set_reg(X10, dst_base);
                    interp.set_reg(X11, src_base);
                    interp.set_reg(X1, (routine.len() as u32 + 10) * 4);
                    interp.run(&routine, 10_000);

                    for i in 0..length {
                        let expected = interp.mem[(src_base + i) as usize];
                        let actual = interp.mem[(dst_base + i) as usize];
                        assert_eq!(
                            actual, expected,
                            "Backward mismatch at byte {} for length={}, src_align={}, dst_align={}",
                            i, length, src_align, dst_align
                        );
                    }
                }
            }
        }
    }
}

#[test]
fn test_specialized_memmove_overlapping() {
    // Test memmove with overlapping regions where dst > src
    let lengths: Vec<u32> = vec![4, 5, 7, 8, 12, 15, 16, 31, 32, 48, 63, 64, 128];

    for &length in &lengths {
        let routine = rv_generate_specialized_memmove(length);

        // Test various overlap amounts
        for overlap in [1, 2, 3, 4, length / 2, length - 1] {
            if overlap == 0 || overlap >= length {
                continue;
            }
            let src_base = 256u32;
            let dst_base = src_base + (length - overlap);

            // Prepare source data
            let src_data: Vec<u8> = (0..length)
                .map(|i| ((i.wrapping_mul(7).wrapping_add(13 + length)) & 0xFF) as u8)
                .collect();

            let mut interp = RvInterpreter::new(1024);
            interp.mem.fill(0xAA);
            for i in 0..length {
                interp.mem[(src_base + i) as usize] = src_data[i as usize];
            }

            interp.set_reg(X10, dst_base);
            interp.set_reg(X11, src_base);
            interp.set_reg(X1, (routine.len() as u32 + 10) * 4);
            interp.run(&routine, 10_000);

            // Verify: dst should contain the original src data
            for i in 0..length {
                let expected = src_data[i as usize];
                let actual = interp.mem[(dst_base + i) as usize];
                assert_eq!(
                    actual, expected,
                    "Overlap mismatch at byte {} for length={}, overlap={}",
                    i, length, overlap
                );
            }
        }
    }
}

/// Reference memcmp implementation for testing.
fn ref_memcmp(a: &[u8], b: &[u8]) -> i32 {
    for (x, y) in a.iter().zip(b.iter()) {
        if x != y {
            return (*x as i32) - (*y as i32);
        }
    }
    0
}

#[test]
fn test_specialized_memcmp_all_alignments() {
    let lengths: Vec<u32> = vec![0, 1, 2, 3, 4, 5, 7, 8, 12, 15, 16, 31, 32, 48, 63, 64, 128];

    for &length in &lengths {
        eprintln!("=== Generating memcmp routine for length={length} ===");
        let routine = rv_generate_specialized_memcmp(length);
        eprintln!("  routine has {} instructions", routine.len());

        for src_align in 0..4u32 {
            for dst_align in 0..4u32 {
                // Test 1: equal buffers
                {
                    let ptr1 = 256 + src_align;
                    let ptr2 = 768 + dst_align;
                    let mut interp = RvInterpreter::new(1024);
                    for i in 0..length {
                        let v = ((i.wrapping_mul(7).wrapping_add(13)) & 0xFF) as u8;
                        interp.mem[(ptr1 + i) as usize] = v;
                        interp.mem[(ptr2 + i) as usize] = v;
                    }
                    interp.set_reg(X10, ptr1);
                    interp.set_reg(X11, ptr2);
                    interp.set_reg(X12, length);
                    interp.set_reg(X1, (routine.len() as u32 + 10) * 4);
                    interp.run(&routine, 50_000);
                    let result = interp.reg(X10) as i32;
                    assert_eq!(
                        result, 0,
                        "Expected equal (0) for length={length}, align=({src_align},{dst_align})"
                    );
                }

                // Test 2: first buffer less than second (difference at various positions)
                if length > 0 {
                    for diff_pos in [0, length / 2, length - 1] {
                        let ptr1 = 256 + src_align;
                        let ptr2 = 768 + dst_align;
                        let mut interp = RvInterpreter::new(1024);
                        for i in 0..length {
                            let v = 0x42u8;
                            interp.mem[(ptr1 + i) as usize] = v;
                            interp.mem[(ptr2 + i) as usize] = v;
                        }
                        interp.mem[(ptr1 + diff_pos) as usize] = 0x10;
                        interp.mem[(ptr2 + diff_pos) as usize] = 0x50;
                        let a_buf: Vec<u8> = (0..length)
                            .map(|i| interp.mem[(ptr1 + i) as usize])
                            .collect();
                        let b_buf: Vec<u8> = (0..length)
                            .map(|i| interp.mem[(ptr2 + i) as usize])
                            .collect();
                        let expected = ref_memcmp(&a_buf, &b_buf);

                        interp.set_reg(X10, ptr1);
                        interp.set_reg(X11, ptr2);
                        interp.set_reg(X12, length);
                        interp.set_reg(X1, (routine.len() as u32 + 10) * 4);
                        interp.run(&routine, 50_000);
                        let result = interp.reg(X10) as i32;
                        assert!(
                            (result < 0) == (expected < 0) && (result > 0) == (expected > 0),
                            "Sign mismatch for length={length}, align=({src_align},{dst_align}), \
                             diff_pos={diff_pos}: got {result}, expected {expected}"
                        );
                    }
                }
            }
        }
    }
}

#[test]
fn test_optimized_memcmp_loop() {
    let routine = rv_generate_optimized_memcmp_loop();
    eprintln!("Generic memcmp loop: {} instructions", routine.len());

    let lengths: Vec<u32> = vec![0, 1, 2, 3, 4, 5, 7, 8, 12, 15, 16, 31, 32, 48, 63, 64, 128];

    for &length in &lengths {
        for src_align in 0..4u32 {
            for dst_align in 0..4u32 {
                // Equal
                {
                    let ptr1 = 256 + src_align;
                    let ptr2 = 768 + dst_align;
                    let mut interp = RvInterpreter::new(1024);
                    for i in 0..length {
                        let v = ((i.wrapping_mul(11).wrapping_add(7)) & 0xFF) as u8;
                        interp.mem[(ptr1 + i) as usize] = v;
                        interp.mem[(ptr2 + i) as usize] = v;
                    }
                    interp.set_reg(X10, ptr1);
                    interp.set_reg(X11, ptr2);
                    interp.set_reg(X12, length);
                    interp.set_reg(X1, (routine.len() as u32 + 10) * 4);
                    interp.run(&routine, 50_000);
                    assert_eq!(
                        interp.reg(X10) as i32,
                        0,
                        "Expected equal for loop length={length}, align=({src_align},{dst_align})"
                    );
                }

                // Difference at middle
                if length > 0 {
                    let diff_pos = length / 2;
                    let ptr1 = 256 + src_align;
                    let ptr2 = 768 + dst_align;
                    let mut interp = RvInterpreter::new(1024);
                    for i in 0..length {
                        interp.mem[(ptr1 + i) as usize] = 0x42;
                        interp.mem[(ptr2 + i) as usize] = 0x42;
                    }
                    interp.mem[(ptr1 + diff_pos) as usize] = 0x80;
                    interp.mem[(ptr2 + diff_pos) as usize] = 0x20;

                    interp.set_reg(X10, ptr1);
                    interp.set_reg(X11, ptr2);
                    interp.set_reg(X12, length);
                    interp.set_reg(X1, (routine.len() as u32 + 10) * 4);
                    interp.run(&routine, 50_000);
                    let result = interp.reg(X10) as i32;
                    assert!(
                        result > 0,
                        "Expected positive for loop length={length}, \
                         align=({src_align},{dst_align}): got {result}"
                    );
                }
            }
        }
    }
}

// --- DFG Bytecopy optimizer tests ---

/// Build an LBU/SB sequence that copies `count` bytes from
/// base+src_start to base+dst_start, using registers t0..t4 as temps.
fn build_lbu_sb_copy(base_reg: u32, src_start: i32, dst_start: i32, count: u32) -> Vec<u32> {
    let mut code = Vec::new();
    let temps = [X5, X6, X7, X28, X29]; // t0..t4
    let chunk = temps.len();
    let mut remaining = count as usize;
    let mut offset = 0i32;

    while remaining > 0 {
        let n = remaining.min(chunk);
        for (k, &tmp) in temps.iter().enumerate().take(n) {
            code.push(rv_lbu(tmp, base_reg, src_start + offset + k as i32));
        }
        for (k, &tmp) in temps.iter().enumerate().take(n) {
            code.push(rv_sb(tmp, base_reg, dst_start + offset + k as i32));
        }
        offset += n as i32;
        remaining -= n;
    }
    code
}

/// Build an LBU/SB sequence that swaps `count` bytes between
/// base+region_a and base+region_b, using interleaved load/store groups.
fn build_lbu_sb_swap(base_reg: u32, region_a: i32, region_b: i32, count: u32) -> Vec<u32> {
    let mut code = Vec::new();
    let temps_a = [X5, X6, X7, X14]; // hold values from A
    let temps_b = [X28, X29, X12, X13]; // hold values from B
    let mut i = 0u32;
    while i < count {
        let n = (count - i).min(4) as usize;
        for (k, &ta) in temps_a.iter().enumerate().take(n) {
            code.push(rv_lbu(ta, base_reg, region_a + (i + k as u32) as i32));
        }
        for (k, &tb) in temps_b.iter().enumerate().take(n) {
            code.push(rv_lbu(tb, base_reg, region_b + (i + k as u32) as i32));
        }
        for (k, &tb) in temps_b.iter().enumerate().take(n) {
            code.push(rv_sb(tb, base_reg, region_a + (i + k as u32) as i32));
        }
        for (k, &ta) in temps_a.iter().enumerate().take(n) {
            code.push(rv_sb(ta, base_reg, region_b + (i + k as u32) as i32));
        }
        i += n as u32;
    }
    code
}

#[test]
fn test_dfg_scan_simple_copy() {
    let seq = build_lbu_sb_copy(X10, 0, 64, 16);
    let dfgs = scan_bytecopy_dfg(&seq);
    assert_eq!(dfgs.len(), 1, "Expected 1 DFG, got {}", dfgs.len());
    let d = &dfgs[0];
    assert_eq!(d.base_reg, X10);
    assert!(
        d.word_transfers.len() >= 3,
        "Expected >= 3 word transfers for 16 bytes, got {}",
        d.word_transfers.len()
    );
}

#[test]
fn test_dfg_scan_swap() {
    let seq = build_lbu_sb_swap(X10, -64, -32, 32);
    let dfgs = scan_bytecopy_dfg(&seq);
    assert_eq!(dfgs.len(), 1, "Expected 1 DFG, got {}", dfgs.len());
    let d = &dfgs[0];
    let (swaps, singles) = pair_swaps(&d.word_transfers);
    assert!(
        swaps.len() >= 4,
        "Expected >= 4 swap pairs for 32-byte swap, got {} swaps + {} singles",
        swaps.len(),
        singles.len()
    );
}

#[test]
fn test_dfg_scan_skips_short() {
    let seq = build_lbu_sb_copy(X10, 0, 64, 4);
    let dfgs = scan_bytecopy_dfg(&seq);
    assert!(dfgs.is_empty(), "Should skip short sequences");
}

#[test]
fn test_dfg_copy_correctness() {
    for count in [16u32, 20, 32, 48] {
        let base_reg = X10;
        let src = 0i32;
        let dst = 128i32;

        let seq = build_lbu_sb_copy(base_reg, src, dst, count);
        let dfgs = scan_bytecopy_dfg(&seq);
        assert!(!dfgs.is_empty(), "No DFG found for count={count}");
        let routine = generate_dfg_routine(&dfgs[0]);

        for align in 0..4u32 {
            let base_addr = 256 + align;

            // Reference: run original bytes
            let mut ref_interp = RvInterpreter::new(1024);
            for b in 0..count {
                ref_interp.mem[(base_addr + b) as usize] = (b as u8).wrapping_mul(37);
            }
            ref_interp.set_reg(base_reg, base_addr);
            ref_interp.run(&seq, 50_000);

            // Optimized
            let mut opt_interp = RvInterpreter::new(1024);
            for b in 0..count {
                opt_interp.mem[(base_addr + b) as usize] = (b as u8).wrapping_mul(37);
            }
            opt_interp.set_reg(base_reg, base_addr);
            opt_interp.run(&routine, 50_000);

            for b in 0..count {
                let addr = (base_addr + dst as u32 + b) as usize;
                assert_eq!(
                    opt_interp.mem[addr], ref_interp.mem[addr],
                    "copy count={count} align={align} byte={b}"
                );
            }
        }
    }
}

#[test]
fn test_dfg_swap_correctness() {
    for count in [16u32, 32] {
        let base_reg = X10;
        let region_a = -64i32;
        let region_b = -32i32;

        let seq = build_lbu_sb_swap(base_reg, region_a, region_b, count);
        let dfgs = scan_bytecopy_dfg(&seq);
        assert!(!dfgs.is_empty(), "No DFG found for swap count={count}");
        let routine = generate_dfg_routine(&dfgs[0]);

        for align in 0..4u32 {
            let base_addr = 512 + align;
            let mem_size = 1024;

            let init = |interp: &mut RvInterpreter| {
                for b in 0..count {
                    let addr_a = base_addr.wrapping_add(region_a as u32).wrapping_add(b) as usize;
                    let addr_b = base_addr.wrapping_add(region_b as u32).wrapping_add(b) as usize;
                    interp.mem[addr_a] = 0xAA_u8.wrapping_add(b as u8);
                    interp.mem[addr_b] = 0xBB_u8.wrapping_add(b as u8);
                }
                interp.set_reg(base_reg, base_addr);
            };

            let mut ref_interp = RvInterpreter::new(mem_size);
            init(&mut ref_interp);
            ref_interp.run(&seq, 50_000);

            let mut opt_interp = RvInterpreter::new(mem_size);
            init(&mut opt_interp);
            opt_interp.run(&routine, 50_000);

            for b in 0..count {
                let addr_a = base_addr.wrapping_add(region_a as u32).wrapping_add(b) as usize;
                let addr_b = base_addr.wrapping_add(region_b as u32).wrapping_add(b) as usize;
                assert_eq!(
                    opt_interp.mem[addr_a], ref_interp.mem[addr_a],
                    "swap count={count} align={align} A[{b}]"
                );
                assert_eq!(
                    opt_interp.mem[addr_b], ref_interp.mem[addr_b],
                    "swap count={count} align={align} B[{b}]"
                );
            }
        }
    }
}

#[test]
fn test_bytecopy_encode_jump_roundtrip() {
    let from = 0x10000u32;
    let targets = [0x20000, 0x10100, 0x0FF00, 0x300000, 0x10004];
    for &to in &targets {
        let (lui, jalr) = rv_encode_jump(from, to, X5);
        let decoded = rv_lui_jalr_target(lui, jalr);
        assert_eq!(
            decoded, to,
            "Jump roundtrip failed: from=0x{from:x}, to=0x{to:x}, got=0x{decoded:x}"
        );
        assert_eq!(rv_rd(jalr), X0, "JALR rd should be x0 for no-link jump");
        assert_eq!(rv_opcode(lui), 0x37, "First instruction should be LUI, not AUIPC");
    }
}

#[test]
fn test_bytecopy_imm_s_decoding() {
    for imm in [-2048, -64, -32, -1, 0, 1, 31, 63, 127, 2047] {
        let insn = rv_sb(X5, X10, imm);
        assert_eq!(rv_imm_s(insn), imm, "S-type imm roundtrip failed for {imm}");
    }
}

#[test]
fn test_dfg_end_to_end_patching() {
    let base_reg = X10;
    let src = 0i32;
    let dst = 64i32;
    let count = 16u32;

    let preamble = vec![rv_addi(X0, X0, 0)]; // 1 NOP
    let byte_seq = build_lbu_sb_copy(base_reg, src, dst, count);

    let mut program: Vec<u32> = Vec::new();
    program.extend(&preamble);
    program.extend(&byte_seq);
    // Sentinel: jump far out of bounds to stop execution
    program.push(rv_jal(X0, 0x10000));

    let pc_base = 0u32;

    let dfgs = scan_bytecopy_dfg(&program);
    assert!(!dfgs.is_empty(), "DFG scan should find the sequence");
    let dfg = &dfgs[0];

    let tmp = dfg.clobbered_regs[0];
    let mut routine = generate_dfg_routine(dfg);

    let routine_start_addr = pc_base + (program.len() as u32) * 4;
    let return_addr = pc_base + (dfg.end_idx as u32) * 4;
    let return_jump_addr = routine_start_addr + (routine.len() as u32) * 4;
    let (ret_lui, ret_jalr) = rv_encode_jump(return_jump_addr, return_addr, tmp);
    routine.push(ret_lui);
    routine.push(ret_jalr);

    let seq_addr = pc_base + (dfg.start_idx as u32) * 4;
    let (fwd_lui, fwd_jalr) = rv_encode_jump(seq_addr, routine_start_addr, tmp);

    let mut patched = program.clone();
    patched.extend(routine);
    patched[dfg.start_idx] = fwd_lui;
    patched[dfg.start_idx + 1] = fwd_jalr;
    let nop = rv_addi(X0, X0, 0);
    for item in patched.iter_mut().take(dfg.end_idx).skip(dfg.start_idx + 2) {
        *item = nop;
    }

    // Run patched
    let base_addr = 256u32;
    let mut opt_interp = RvInterpreter::new(1024);
    for b in 0..count {
        opt_interp.mem[(base_addr + b) as usize] = (b as u8).wrapping_mul(41);
    }
    opt_interp.set_reg(base_reg, base_addr);
    opt_interp.run(&patched, 50_000);

    // Run reference (original, no patching)
    let mut ref_interp = RvInterpreter::new(1024);
    for b in 0..count {
        ref_interp.mem[(base_addr + b) as usize] = (b as u8).wrapping_mul(41);
    }
    ref_interp.set_reg(base_reg, base_addr);
    ref_interp.run(&program, 50_000);

    for b in 0..count {
        let addr = (base_addr + dst as u32 + b) as usize;
        assert_eq!(
            opt_interp.mem[addr], ref_interp.mem[addr],
            "E2E mismatch at byte {b}"
        );
    }
}

#[test]
fn test_aligned_memcpy() {
    use crate::memcpy::rv_generate_aligned_memcpy;

    let lengths: Vec<u32> = vec![0, 1, 2, 3, 4, 5, 8, 12, 16, 32, 48, 64, 128];

    for &length in &lengths {
        let routine = rv_generate_aligned_memcpy(length);

        let src_base: u32 = 256;
        let dst_base: u32 = 768;

        let mut interp = RvInterpreter::new(1024);
        interp.mem.fill(0xAA);
        for i in 0..length {
            interp.mem[(src_base + i) as usize] =
                ((i.wrapping_mul(7).wrapping_add(13 + length)) & 0x7F) as u8;
        }

        interp.set_reg(X10, dst_base);
        interp.set_reg(X11, src_base);
        interp.set_reg(X1, (routine.len() as u32 + 10) * 4);

        interp.run(&routine, 10_000);

        for i in 0..length {
            let expected = interp.mem[(src_base + i) as usize];
            let actual = interp.mem[(dst_base + i) as usize];
            assert_eq!(
                actual, expected,
                "aligned memcpy mismatch at byte {} for length={}",
                i, length
            );
        }
    }
}

#[test]
fn test_aligned_memmove() {
    use crate::memcpy::rv_generate_aligned_memmove;

    let lengths: Vec<u32> = vec![0, 1, 2, 3, 4, 8, 16, 32, 64];

    for &length in &lengths {
        let routine = rv_generate_aligned_memmove(length);

        // Forward (dst < src, aligned)
        {
            let src_base: u32 = 512;
            let dst_base: u32 = 256;
            let mut interp = RvInterpreter::new(1024);
            interp.mem.fill(0xAA);
            for i in 0..length {
                interp.mem[(src_base + i) as usize] = (i & 0xFF) as u8;
            }
            interp.set_reg(X10, dst_base);
            interp.set_reg(X11, src_base);
            interp.set_reg(X1, (routine.len() as u32 + 10) * 4);
            interp.run(&routine, 10_000);
            for i in 0..length {
                assert_eq!(
                    interp.mem[(dst_base + i) as usize],
                    (i & 0xFF) as u8,
                    "aligned memmove fwd mismatch at {} for length={}",
                    i,
                    length
                );
            }
        }

        // Backward (overlapping, dst > src, aligned)
        if length >= 4 {
            let src_base: u32 = 256;
            let dst_base: u32 = 260;
            let mut interp = RvInterpreter::new(1024);
            interp.mem.fill(0xAA);
            let mut expected = vec![0u8; length as usize];
            for i in 0..length {
                let val = (i.wrapping_mul(3).wrapping_add(7) & 0xFF) as u8;
                interp.mem[(src_base + i) as usize] = val;
                expected[i as usize] = val;
            }
            interp.set_reg(X10, dst_base);
            interp.set_reg(X11, src_base);
            interp.set_reg(X1, (routine.len() as u32 + 10) * 4);
            interp.run(&routine, 10_000);
            for i in 0..length {
                assert_eq!(
                    interp.mem[(dst_base + i) as usize],
                    expected[i as usize],
                    "aligned memmove bwd mismatch at {} for length={}",
                    i,
                    length
                );
            }
        }
    }
}

#[test]
fn test_aligned_memcmp() {
    use crate::memcmp::rv_generate_aligned_memcmp;

    let lengths: Vec<u32> = vec![0, 1, 2, 3, 4, 8, 16, 32, 64];

    for &length in &lengths {
        let routine = rv_generate_aligned_memcmp(length);

        // Equal arrays
        {
            let ptr1: u32 = 256;
            let ptr2: u32 = 512;
            let mut interp = RvInterpreter::new(1024);
            for i in 0..length {
                let val = ((i * 7 + 3) & 0xFF) as u8;
                interp.mem[(ptr1 + i) as usize] = val;
                interp.mem[(ptr2 + i) as usize] = val;
            }
            interp.set_reg(X10, ptr1);
            interp.set_reg(X11, ptr2);
            interp.set_reg(X12, length);
            interp.set_reg(X1, (routine.len() as u32 + 10) * 4);
            interp.run(&routine, 10_000);
            assert_eq!(
                interp.reg(X10) as i32,
                0,
                "aligned memcmp should be 0 for equal data, length={}",
                length
            );
        }

        // Different arrays (last byte differs)
        if length > 0 {
            let ptr1: u32 = 256;
            let ptr2: u32 = 512;
            let mut interp = RvInterpreter::new(1024);
            for i in 0..length {
                let val = ((i * 7 + 3) & 0xFF) as u8;
                interp.mem[(ptr1 + i) as usize] = val;
                interp.mem[(ptr2 + i) as usize] = val;
            }
            interp.mem[(ptr1 + length - 1) as usize] = 0x80;
            interp.mem[(ptr2 + length - 1) as usize] = 0x10;
            interp.set_reg(X10, ptr1);
            interp.set_reg(X11, ptr2);
            interp.set_reg(X12, length);
            interp.set_reg(X1, (routine.len() as u32 + 10) * 4);
            interp.run(&routine, 10_000);
            let result = interp.reg(X10) as i32;
            assert!(
                result > 0,
                "aligned memcmp should be >0 when ptr1>ptr2, got {} for length={}",
                result,
                length
            );
        }
    }
}

#[test]
fn test_alignment_resolver() {
    use crate::call_site::rv_resolve_reg_alignment;

    // addi a0, sp, 16 → aligned
    let instrs = vec![rv_addi(X10, X2, 16)];
    assert!(rv_resolve_reg_alignment(&instrs, 1, X10));

    // addi a0, sp, 5 → NOT aligned
    let instrs = vec![rv_addi(X10, X2, 5)];
    assert!(!rv_resolve_reg_alignment(&instrs, 1, X10));

    // mv a0, sp (= addi a0, sp, 0) → aligned
    let instrs = vec![rv_addi(X10, X2, 0)];
    assert!(rv_resolve_reg_alignment(&instrs, 1, X10));

    // addi a3, sp, 8; addi a0, a3, 4 → aligned (8+4=12, 12%4==0)
    let instrs = vec![rv_addi(X13, X2, 8), rv_addi(X10, X13, 4)];
    assert!(rv_resolve_reg_alignment(&instrs, 2, X10));

    // addi a3, sp, 8; addi a0, a3, 3 → NOT aligned (offset 3)
    let instrs = vec![rv_addi(X13, X2, 8), rv_addi(X10, X13, 3)];
    assert!(!rv_resolve_reg_alignment(&instrs, 2, X10));

    // No write to reg → not aligned
    let instrs = vec![rv_addi(X11, X2, 8)];
    assert!(!rv_resolve_reg_alignment(&instrs, 1, X10));
}

// ---------------------------------------------------------------------------
// ELF routine constants extracted from openvm-client-eth binary.
//
// memcpy: musl memcpy at 0x002008f8, 259 instructions, 1036 bytes.
// Convention: a0=dst, a1=src, a2=length. Returns a0=dst.
// Handles all alignments internally via shift-based word copy.
// ---------------------------------------------------------------------------
#[cfg(test)]
const ELF_MEMCPY: &[u32] = &[
    0x0035f693, 0x0016b693, 0x00163713, 0x00e6e6b3, 0x0e069c63, 0x00158793, 0x00050813, 0x00058883,
    0x00158713, 0x00180693, 0x01180023, 0xfff60613, 0x0037f593, 0x00b035b3, 0x00c03833, 0x0105f8b3,
    0x00178793, 0x00070593, 0x00068813, 0xfc0898e3, 0x0036f593, 0x0c058263, 0x02000793, 0x22f66e63,
    0x00300793, 0x12f58663, 0x00200793, 0x1af58263, 0x00100793, 0x22f59263, 0x00072783, 0x00f68023,
    0x0087d593, 0x00b680a3, 0x0107d813, 0x00368593, 0x01068123, 0xffd60613, 0x01070693, 0x01000713,
    0xff46a803, 0x0187d793, 0x00881893, 0xff86a283, 0x00f8e7b3, 0x00f5a023, 0x01885793, 0x00829813,
    0xffc6a883, 0x00f867b3, 0x00f5a223, 0x0182d813, 0x00889293, 0x0006a783, 0x0102e833, 0x0105a423,
    0x0188d813, 0x00879893, 0x0108e833, 0x0105a623, 0x01058593, 0xff060613, 0x01068693, 0xfac762e3,
    0xff368713, 0x1900006f, 0x00050693, 0x00058713, 0x0036f593, 0xf40592e3, 0x01000593, 0x02b66c63,
    0x00f00593, 0x00072783, 0x00472803, 0x00872883, 0x00c72283, 0x00f6a023, 0x0106a223, 0x0116a423,
    0x0056a623, 0x01070713, 0xff060613, 0x01068693, 0xfcc5eae3, 0x00867593, 0x00058e63, 0x00072583,
    0x00472783, 0x00b6a023, 0x00f6a223, 0x00868693, 0x00870713, 0x00467593, 0x16058263, 0x00072583,
    0x00b6a023, 0x00468693, 0x00470713, 0x1500006f, 0x00072783, 0x00168593, 0x00f68023, 0xfff60613,
    0x01070693, 0x01200713, 0xff46a803, 0x0087d793, 0x01881893, 0xff86a283, 0x00f8e7b3, 0x00f5a023,
    0x00885793, 0x01829813, 0xffc6a883, 0x00f867b3, 0x00f5a223, 0x0082d813, 0x01889293, 0x0006a783,
    0x0102e833, 0x0105a423, 0x0088d813, 0x01879893, 0x0108e833, 0x0105a623, 0x01058593, 0xff060613,
    0x01068693, 0xfac762e3, 0xff168713, 0x0880006f, 0x00072783, 0x00f68023, 0x0087d813, 0x00268593,
    0x010680a3, 0xffe60613, 0x01070693, 0x01100713, 0xff46a803, 0x0107d793, 0x01081893, 0xff86a283,
    0x00f8e7b3, 0x00f5a023, 0x01085793, 0x01029813, 0xffc6a883, 0x00f867b3, 0x00f5a223, 0x0102d813,
    0x01089293, 0x0006a783, 0x0102e833, 0x0105a423, 0x0108d813, 0x01079893, 0x0108e833, 0x0105a623,
    0x01058593, 0xff060613, 0x01068693, 0xfac762e3, 0xff268713, 0x00058693, 0x01067593, 0x08059263,
    0x00867593, 0x10059863, 0x00467593, 0x02058863, 0x00070583, 0x00170783, 0x00270803, 0x00b68023,
    0x00f680a3, 0x00370583, 0x01068123, 0x00470713, 0x00468793, 0x00b681a3, 0x00078693, 0x00267593,
    0x00059863, 0x00167593, 0x02059663, 0x00008067, 0x00070583, 0x00170783, 0x00b68023, 0x00270713,
    0x00268593, 0x00f680a3, 0x00058693, 0x00167593, 0xfc058ee3, 0x00070583, 0x00b68023, 0x00008067,
    0x00070583, 0x00170783, 0x00270803, 0x00b68023, 0x00f680a3, 0x00370583, 0x01068123, 0x00470783,
    0x00570803, 0x00b681a3, 0x00670583, 0x00f68223, 0x010682a3, 0x00770783, 0x00b68323, 0x00870583,
    0x00970803, 0x00f683a3, 0x00a70783, 0x00b68423, 0x010684a3, 0x00b70583, 0x00f68523, 0x00c70783,
    0x00d70803, 0x00b685a3, 0x00e70583, 0x00f68623, 0x010686a3, 0x00f70783, 0x00b68723, 0x01070713,
    0x01068593, 0x00f687a3, 0x00058693, 0x00867593, 0xee058ce3, 0x00070583, 0x00170783, 0x00270803,
    0x00b68023, 0x00f680a3, 0x00370583, 0x01068123, 0x00470783, 0x00570803, 0x00b681a3, 0x00670583,
    0x00f68223, 0x010682a3, 0x00770783, 0x00b68323, 0x00870713, 0x00868593, 0x00f683a3, 0x00058693,
    0x00467593, 0xea0596e3, 0xed5ff06f,
];

// ---------------------------------------------------------------------------
// memmove: compiler_builtins memmove implementation at 0x004e7cc8,
// 258 instructions, 1032 bytes. Uses stack frame (32 bytes).
// Convention: a0=dst, a1=src, a2=length. Returns a0=dst.
// Handles overlapping regions and all alignments.
// ---------------------------------------------------------------------------
#[cfg(test)]
const ELF_MEMMOVE_IMPL: &[u32] = &[
    0xfe010113, 0x00812e23, 0x40b506b3, 0x0cc6f063, 0x00c508b3, 0x01000693, 0x00c58733, 0x08d66463,
    0x0038f793, 0xffc8f693, 0x40f002b3, 0x0316f463, 0x00c58833, 0xfff80813, 0x00088313, 0x00084383,
    0xfff30e13, 0xfe730fa3, 0xfff80813, 0x000e0313, 0xffc6e6e3, 0x00570733, 0x40f607b3, 0xffc7f393,
    0x00377313, 0x40700833, 0x40768e33, 0x10031a63, 0x02de7463, 0x00b785b3, 0xffc58593, 0x00068613,
    0x0005a883, 0xffc60293, 0xff162e23, 0xffc58593, 0x00028613, 0xfe5e66e3, 0x010688b3, 0x01070733,
    0x0037f613, 0x40c885b3, 0x0d15f663, 0xfff70713, 0x00074603, 0xfff88693, 0xfec88fa3, 0xfff70713,
    0x00068893, 0xfed5e6e3, 0x0ac0006f, 0x01000693, 0x08d66063, 0x40a006b3, 0x0036f693, 0x00d507b3,
    0x02f57463, 0x00068713, 0x00050813, 0x00058893, 0x0008c283, 0xfff70713, 0x00580023, 0x00180813,
    0x00188893, 0xfe0716e3, 0x00d585b3, 0x40d60633, 0xffc67713, 0x0035f893, 0x00e786b3, 0x08089463,
    0x00d7fe63, 0x00058813, 0x00082883, 0x0117a023, 0x00478793, 0x00480813, 0xfed7e8e3, 0x00e585b3,
    0x00367613, 0x00c68733, 0x00e6ea63, 0x0280006f, 0x00050693, 0x00c50733, 0x00e57e63, 0x0005c703,
    0xfff60613, 0x00e68023, 0x00168693, 0x00158593, 0xfe0616e3, 0x01c12403, 0x02010113, 0x00008067,
    0x40670eb3, 0x00010a23, 0x00100393, 0x00010923, 0x08731663, 0x00000393, 0x00000f13, 0x01410f93,
    0x1800006f, 0x00000813, 0x00400293, 0x00012623, 0x41128333, 0x00c10293, 0x00137393, 0x0112e2b3,
    0x06039c63, 0x00237313, 0x08031263, 0x00c12e83, 0x00389813, 0x00478293, 0x41158f33, 0x08d2fa63,
    0x410002b3, 0x0182fe13, 0x004f2283, 0x004f0393, 0x010edeb3, 0x00478313, 0x01c29f33, 0x01df6eb3,
    0x00878f93, 0x01d7a023, 0x00030793, 0x00038f13, 0x00028e93, 0xfcdfeae3, 0x0640006f, 0x000ecf83,
    0x001ecf03, 0x00177393, 0x01f10a23, 0x0e039663, 0x00000413, 0x1040006f, 0x0005c803, 0x01028023,
    0x00100813, 0x00237313, 0xf80302e3, 0x01058333, 0x00031303, 0x01028833, 0x00681023, 0x00c12e83,
    0x00389813, 0x00478293, 0x41158f33, 0xf6d2eae3, 0x000e8293, 0x000f0393, 0x00078313, 0x00010423,
    0x00100793, 0x00010323, 0x00f89c63, 0x00000893, 0x00000793, 0x00000e13, 0x00810e93, 0x01c0006f,
    0x0043c883, 0x0053c783, 0x00200e13, 0x01110423, 0x00879793, 0x00610e93, 0x0015ff13, 0x000f1663,
    0x00000393, 0x0200006f, 0x00438393, 0x01c383b3, 0x0003c883, 0x011e8023, 0x00614383, 0x00814883,
    0x01039393, 0x0113e8b3, 0x0102d2b3, 0x41000833, 0x0117e7b3, 0x01887813, 0x010797b3, 0x0057e7b3,
    0x00f32023, 0x00e585b3, 0x00367613, 0x00c68733, 0xe4e6e6e3, 0xe61ff06f, 0x01210f93, 0x00200393,
    0x007e83b3, 0x0003c383, 0x007f8023, 0x01214403, 0x01414f83, 0x01041413, 0x00331393, 0x008f1f13,
    0x01ff6fb3, 0x004e0f13, 0x01f46e33, 0x04df7663, 0x40700eb3, 0x40660fb3, 0x018ef613, 0x01f58eb3,
    0x000e0593, 0x005e8e33, 0xffce2e03, 0x00c595b3, 0x007e5fb3, 0x00bfe5b3, 0x00588fb3, 0xffc88893,
    0xfebfae23, 0x005885b3, 0xffce8e93, 0xfcbf6ae3, 0x005e8eb3, 0x0080006f, 0x00068593, 0x00000613,
    0x006e82b3, 0x00400893, 0x00012c23, 0x01810f13, 0x40688eb3, 0x006f68b3, 0x001ef313, 0xffc28293,
    0x00030863, 0x0002c603, 0x00c88023, 0x00100613, 0x002ef313, 0x00030a63, 0x00c282b3, 0x00029283,
    0x00c88633, 0x00561023, 0x01812603, 0x407008b3, 0x0188f893, 0x011e18b3, 0x00765633, 0x01166633,
    0xfec5ae23, 0xc95ff06f,
];

// ---------------------------------------------------------------------------
// memcmp: compiler_builtins byte-by-byte memcmp at 0x004e80d0,
// 12 instructions, 48 bytes.
// Convention: a0=ptr1, a1=ptr2, a2=length. Returns a0 = signed diff of
// first differing byte pair, or 0 if equal.
// ---------------------------------------------------------------------------
#[cfg(test)]
const ELF_MEMCMP_IMPL: &[u32] = &[
    0x02060063, 0x00054683, 0x0005c703, 0x00e69e63, 0xfff60613, 0x00158593, 0x00150513, 0xfe0614e3,
    0x00000513, 0x00008067, 0x40e68533, 0x00008067,
];

/// Helper: run ELF memcpy (a0=dst, a1=src, a2=length) and return resulting
/// destination bytes.
#[cfg(test)]
fn run_elf_memcpy(src: &[u8], dst_offset: u32, src_offset: u32, length: u32) -> Vec<u8> {
    let mem_size = 8192;
    let mut interp = RvInterpreter::new(mem_size);
    interp.mem.fill(0xAA);
    for (i, &b) in src.iter().enumerate() {
        interp.mem[src_offset as usize + i] = b;
    }
    interp.set_reg(X10, dst_offset);
    interp.set_reg(X11, src_offset);
    interp.set_reg(X12, length);
    interp.set_reg(X1, (ELF_MEMCPY.len() as u32 + 10) * 4);
    interp.run(ELF_MEMCPY, 100_000);
    interp.mem[dst_offset as usize..(dst_offset + length) as usize].to_vec()
}

/// Helper: run ELF memmove (a0=dst, a1=src, a2=length) and return resulting
/// destination bytes. Sets up stack at top of memory.
#[cfg(test)]
fn run_elf_memmove(initial_mem: &mut [u8], dst_offset: u32, src_offset: u32, length: u32) -> Vec<u8> {
    let mem_size = initial_mem.len();
    let stack_top = (mem_size - 64) as u32; // reserve 64 bytes for stack
    let stack_top = stack_top & !0xF; // 16-byte aligned
    let mut interp = RvInterpreter::new(mem_size);
    interp.mem.copy_from_slice(initial_mem);
    interp.set_reg(X2, stack_top); // SP
    interp.set_reg(X10, dst_offset);
    interp.set_reg(X11, src_offset);
    interp.set_reg(X12, length);
    interp.set_reg(X1, (ELF_MEMMOVE_IMPL.len() as u32 + 10) * 4);
    interp.run(ELF_MEMMOVE_IMPL, 500_000);
    // Copy resulting memory back so caller can inspect overlapping regions
    initial_mem.copy_from_slice(&interp.mem);
    interp.mem[dst_offset as usize..(dst_offset + length) as usize].to_vec()
}

/// Helper: run ELF memcmp (a0=ptr1, a1=ptr2, a2=length) and return result.
#[cfg(test)]
fn run_elf_memcmp(mem: &[u8], ptr1: u32, ptr2: u32, length: u32) -> i32 {
    let mut interp = RvInterpreter::new(mem.len());
    interp.mem[..mem.len()].copy_from_slice(mem);
    interp.set_reg(X10, ptr1);
    interp.set_reg(X11, ptr2);
    interp.set_reg(X12, length);
    interp.set_reg(X1, (ELF_MEMCMP_IMPL.len() as u32 + 10) * 4);
    interp.run(ELF_MEMCMP_IMPL, 10_000);
    interp.reg(X10) as i32
}

/// Test that our specialized memcpy routines produce the same result as the
/// original ELF memcpy for every length and alignment combination.
#[test]
fn test_elf_vs_optimized_memcpy() {
    let lengths: Vec<u32> = vec![1, 2, 3, 4, 5, 7, 8, 12, 15, 16, 31, 32, 48, 63, 64, 128];

    for &length in &lengths {
        let routine = rv_generate_specialized_memcpy(length);

        for src_align in 0..4u32 {
            for dst_align in 0..4u32 {
                let src_base = 2048 + src_align;
                let dst_base = 4096 + dst_align;

                // Prepare source data
                let src_data: Vec<u8> = (0..length)
                    .map(|i| ((i.wrapping_mul(7).wrapping_add(13 + length)) & 0xFF) as u8)
                    .collect();

                // Run ELF original
                let elf_result = run_elf_memcpy(&src_data, dst_base, src_base, length);

                // Run our optimized routine
                let mut interp = RvInterpreter::new(8192);
                interp.mem.fill(0xAA);
                for (i, &b) in src_data.iter().enumerate() {
                    interp.mem[src_base as usize + i] = b;
                }
                interp.set_reg(X10, dst_base);
                interp.set_reg(X11, src_base);
                interp.set_reg(X1, (routine.len() as u32 + 10) * 4);
                interp.run(&routine, 10_000);
                let opt_result: Vec<u8> =
                    interp.mem[dst_base as usize..(dst_base + length) as usize].to_vec();

                assert_eq!(
                    elf_result, opt_result,
                    "memcpy mismatch: length={}, src_align={}, dst_align={}",
                    length, src_align, dst_align
                );
            }
        }
    }
}

/// Test that our specialized memcpy routines produce the same result as the
/// original ELF memcpy for aligned arguments.
#[test]
fn test_elf_vs_aligned_memcpy() {
    use crate::memcpy::rv_generate_aligned_memcpy;

    let lengths: Vec<u32> = vec![4, 8, 12, 16, 32, 48, 64, 128];

    for &length in &lengths {
        let routine = rv_generate_aligned_memcpy(length);
        let src_base: u32 = 2048; // aligned
        let dst_base: u32 = 4096; // aligned

        let src_data: Vec<u8> = (0..length)
            .map(|i| ((i.wrapping_mul(7).wrapping_add(13 + length)) & 0xFF) as u8)
            .collect();

        let elf_result = run_elf_memcpy(&src_data, dst_base, src_base, length);

        let mut interp = RvInterpreter::new(8192);
        interp.mem.fill(0xAA);
        for (i, &b) in src_data.iter().enumerate() {
            interp.mem[src_base as usize + i] = b;
        }
        interp.set_reg(X10, dst_base);
        interp.set_reg(X11, src_base);
        interp.set_reg(X1, (routine.len() as u32 + 10) * 4);
        interp.run(&routine, 10_000);
        let opt_result: Vec<u8> =
            interp.mem[dst_base as usize..(dst_base + length) as usize].to_vec();

        assert_eq!(
            elf_result, opt_result,
            "aligned memcpy mismatch: length={}",
            length
        );
    }
}

/// Test that our specialized memmove routines produce the same result as the
/// original ELF memmove for non-overlapping and overlapping cases.
#[test]
fn test_elf_vs_optimized_memmove() {
    let lengths: Vec<u32> = vec![1, 2, 3, 4, 5, 7, 8, 12, 15, 16, 31, 32, 48, 63, 64, 128];
    let mem_size = 16384;

    for &length in &lengths {
        let routine = rv_generate_specialized_memmove(length);

        // Non-overlapping forward (dst < src)
        for src_align in 0..4u32 {
            for dst_align in 0..4u32 {
                let src_base = 4096 + src_align;
                let dst_base = 2048 + dst_align;

                let src_data: Vec<u8> = (0..length)
                    .map(|i| ((i.wrapping_mul(7).wrapping_add(13 + length)) & 0xFF) as u8)
                    .collect();

                // ELF memmove
                let mut elf_mem = vec![0xAA_u8; mem_size];
                for (i, &b) in src_data.iter().enumerate() {
                    elf_mem[src_base as usize + i] = b;
                }
                let elf_result = run_elf_memmove(&mut elf_mem, dst_base, src_base, length);

                // Our optimized memmove
                let mut interp = RvInterpreter::new(mem_size);
                interp.mem.fill(0xAA);
                for (i, &b) in src_data.iter().enumerate() {
                    interp.mem[src_base as usize + i] = b;
                }
                interp.set_reg(X10, dst_base);
                interp.set_reg(X11, src_base);
                interp.set_reg(X1, (routine.len() as u32 + 10) * 4);
                interp.run(&routine, 10_000);
                let opt_result: Vec<u8> =
                    interp.mem[dst_base as usize..(dst_base + length) as usize].to_vec();

                assert_eq!(
                    elf_result, opt_result,
                    "memmove fwd mismatch: length={}, src_align={}, dst_align={}",
                    length, src_align, dst_align
                );
            }
        }

        // Overlapping: dst > src
        if length >= 4 {
            for overlap in [1, 2, length / 2, length - 1] {
                if overlap == 0 || overlap >= length {
                    continue;
                }
                let src_base = 2048u32;
                let dst_base = src_base + (length - overlap);

                let src_data: Vec<u8> = (0..length)
                    .map(|i| ((i.wrapping_mul(7).wrapping_add(13 + length)) & 0xFF) as u8)
                    .collect();

                // ELF memmove
                let mut elf_mem = vec![0xAA_u8; mem_size];
                for (i, &b) in src_data.iter().enumerate() {
                    elf_mem[src_base as usize + i] = b;
                }
                let elf_result = run_elf_memmove(&mut elf_mem, dst_base, src_base, length);

                // Our optimized memmove
                let mut interp = RvInterpreter::new(mem_size);
                interp.mem.fill(0xAA);
                for (i, &b) in src_data.iter().enumerate() {
                    interp.mem[src_base as usize + i] = b;
                }
                interp.set_reg(X10, dst_base);
                interp.set_reg(X11, src_base);
                interp.set_reg(X1, (routine.len() as u32 + 10) * 4);
                interp.run(&routine, 10_000);
                let opt_result: Vec<u8> =
                    interp.mem[dst_base as usize..(dst_base + length) as usize].to_vec();

                assert_eq!(
                    elf_result, opt_result,
                    "memmove overlap mismatch: length={}, overlap={}",
                    length, overlap
                );
            }
        }
    }
}

/// Test that our specialized memmove routines produce the same result as the
/// original ELF memmove for aligned arguments.
#[test]
fn test_elf_vs_aligned_memmove() {
    use crate::memcpy::rv_generate_aligned_memmove;

    let lengths: Vec<u32> = vec![4, 8, 16, 32, 64, 128];
    let mem_size = 16384;

    for &length in &lengths {
        let routine = rv_generate_aligned_memmove(length);

        // Forward non-overlapping (aligned)
        let src_base: u32 = 4096;
        let dst_base: u32 = 2048;

        let src_data: Vec<u8> = (0..length)
            .map(|i| ((i.wrapping_mul(7).wrapping_add(13 + length)) & 0xFF) as u8)
            .collect();

        let mut elf_mem = vec![0xAA_u8; mem_size];
        for (i, &b) in src_data.iter().enumerate() {
            elf_mem[src_base as usize + i] = b;
        }
        let elf_result = run_elf_memmove(&mut elf_mem, dst_base, src_base, length);

        let mut interp = RvInterpreter::new(mem_size);
        interp.mem.fill(0xAA);
        for (i, &b) in src_data.iter().enumerate() {
            interp.mem[src_base as usize + i] = b;
        }
        interp.set_reg(X10, dst_base);
        interp.set_reg(X11, src_base);
        interp.set_reg(X1, (routine.len() as u32 + 10) * 4);
        interp.run(&routine, 10_000);
        let opt_result: Vec<u8> =
            interp.mem[dst_base as usize..(dst_base + length) as usize].to_vec();

        assert_eq!(
            elf_result, opt_result,
            "aligned memmove mismatch: length={}",
            length
        );

        // Overlapping backward (aligned)
        if length >= 8 {
            let src_base: u32 = 2048;
            let dst_base: u32 = src_base + 4; // 4-byte overlap offset (aligned)

            let mut elf_mem = vec![0xAA_u8; mem_size];
            for (i, &b) in src_data.iter().enumerate() {
                elf_mem[src_base as usize + i] = b;
            }
            let elf_result = run_elf_memmove(&mut elf_mem, dst_base, src_base, length);

            let mut interp = RvInterpreter::new(mem_size);
            interp.mem.fill(0xAA);
            for (i, &b) in src_data.iter().enumerate() {
                interp.mem[src_base as usize + i] = b;
            }
            interp.set_reg(X10, dst_base);
            interp.set_reg(X11, src_base);
            interp.set_reg(X1, (routine.len() as u32 + 10) * 4);
            interp.run(&routine, 10_000);
            let opt_result: Vec<u8> =
                interp.mem[dst_base as usize..(dst_base + length) as usize].to_vec();

            assert_eq!(
                elf_result, opt_result,
                "aligned memmove overlap mismatch: length={}",
                length
            );
        }
    }
}

/// Test that our specialized memcmp routines produce the same result sign as
/// the original ELF memcmp for all alignment combinations.
#[test]
fn test_elf_vs_optimized_memcmp() {
    let lengths: Vec<u32> = vec![1, 2, 3, 4, 5, 7, 8, 12, 15, 16, 31, 32, 48, 63, 64, 128];

    for &length in &lengths {
        let routine = rv_generate_specialized_memcmp(length);

        for src_align in 0..4u32 {
            for dst_align in 0..4u32 {
                let ptr1 = 2048 + src_align;
                let ptr2 = 4096 + dst_align;

                // Test equal buffers
                {
                    let mut mem = vec![0u8; 8192];
                    for i in 0..length {
                        let v = ((i.wrapping_mul(7).wrapping_add(13)) & 0xFF) as u8;
                        mem[ptr1 as usize + i as usize] = v;
                        mem[ptr2 as usize + i as usize] = v;
                    }

                    let elf_res = run_elf_memcmp(&mem, ptr1, ptr2, length);
                    assert_eq!(elf_res, 0, "ELF memcmp should return 0 for equal data");

                    let mut interp = RvInterpreter::new(8192);
                    interp.mem.copy_from_slice(&mem);
                    interp.set_reg(X10, ptr1);
                    interp.set_reg(X11, ptr2);
                    interp.set_reg(X12, length);
                    interp.set_reg(X1, (routine.len() as u32 + 10) * 4);
                    interp.run(&routine, 50_000);
                    let opt_res = interp.reg(X10) as i32;

                    assert_eq!(
                        opt_res, 0,
                        "opt memcmp should be 0 for equal data, length={}, align=({},{})",
                        length, src_align, dst_align
                    );
                }

                // Test with difference at various positions
                for diff_pos in [0, length / 2, length - 1] {
                    let mut mem = vec![0u8; 8192];
                    for i in 0..length {
                        mem[ptr1 as usize + i as usize] = 0x42;
                        mem[ptr2 as usize + i as usize] = 0x42;
                    }
                    mem[ptr1 as usize + diff_pos as usize] = 0x80;
                    mem[ptr2 as usize + diff_pos as usize] = 0x20;

                    let elf_res = run_elf_memcmp(&mem, ptr1, ptr2, length);

                    let mut interp = RvInterpreter::new(8192);
                    interp.mem.copy_from_slice(&mem);
                    interp.set_reg(X10, ptr1);
                    interp.set_reg(X11, ptr2);
                    interp.set_reg(X12, length);
                    interp.set_reg(X1, (routine.len() as u32 + 10) * 4);
                    interp.run(&routine, 50_000);
                    let opt_res = interp.reg(X10) as i32;

                    assert_eq!(
                        elf_res.signum(),
                        opt_res.signum(),
                        "memcmp sign mismatch: length={}, align=({},{}), diff_pos={}: \
                         elf={}, opt={}",
                        length, src_align, dst_align, diff_pos, elf_res, opt_res
                    );
                }
            }
        }
    }
}

/// Test that our aligned memcmp routines produce the same result sign as
/// the original ELF memcmp.
#[test]
fn test_elf_vs_aligned_memcmp() {
    use crate::memcmp::rv_generate_aligned_memcmp;

    let lengths: Vec<u32> = vec![4, 8, 16, 32, 64, 128];

    for &length in &lengths {
        let routine = rv_generate_aligned_memcmp(length);
        let ptr1: u32 = 2048;
        let ptr2: u32 = 4096;

        // Equal
        {
            let mut mem = vec![0u8; 8192];
            for i in 0..length {
                let v = ((i * 7 + 3) & 0xFF) as u8;
                mem[ptr1 as usize + i as usize] = v;
                mem[ptr2 as usize + i as usize] = v;
            }

            let elf_res = run_elf_memcmp(&mem, ptr1, ptr2, length);

            let mut interp = RvInterpreter::new(8192);
            interp.mem.copy_from_slice(&mem);
            interp.set_reg(X10, ptr1);
            interp.set_reg(X11, ptr2);
            interp.set_reg(X12, length);
            interp.set_reg(X1, (routine.len() as u32 + 10) * 4);
            interp.run(&routine, 10_000);
            let opt_res = interp.reg(X10) as i32;

            assert_eq!(elf_res, 0);
            assert_eq!(opt_res, 0, "aligned memcmp equal, length={}", length);
        }

        // Different
        {
            let mut mem = vec![0u8; 8192];
            for i in 0..length {
                mem[ptr1 as usize + i as usize] = 0x42;
                mem[ptr2 as usize + i as usize] = 0x42;
            }
            mem[ptr1 as usize + (length - 1) as usize] = 0x80;
            mem[ptr2 as usize + (length - 1) as usize] = 0x10;

            let elf_res = run_elf_memcmp(&mem, ptr1, ptr2, length);

            let mut interp = RvInterpreter::new(8192);
            interp.mem.copy_from_slice(&mem);
            interp.set_reg(X10, ptr1);
            interp.set_reg(X11, ptr2);
            interp.set_reg(X12, length);
            interp.set_reg(X1, (routine.len() as u32 + 10) * 4);
            interp.run(&routine, 10_000);
            let opt_res = interp.reg(X10) as i32;

            assert_eq!(
                elf_res.signum(),
                opt_res.signum(),
                "aligned memcmp diff, length={}: elf={}, opt={}",
                length, elf_res, opt_res
            );
        }
    }
}
