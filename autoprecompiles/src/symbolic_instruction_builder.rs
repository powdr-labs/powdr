use powdr_number::FieldElement;
use crate::SymbolicInstructionStatement;

// Helper for ALU-style (7-arg) instructions in range 512..=521
fn build_alu<T: FieldElement>(opcode: u32, rd_ptr: u32, rs1_ptr: u32, rs2: u32, rs2_as: u32) -> SymbolicInstructionStatement<T> {
    SymbolicInstructionStatement {
        opcode: opcode as usize,
        args: vec![
            T::from(rd_ptr),
            T::from(rs1_ptr),
            T::from(rs2),
            T::one(),
            T::from(rs2_as),
            T::zero(),
            T::zero(),
        ],
    }
}

// ALU instructions
pub fn add<T: FieldElement>(rd_ptr: u32, rs1_ptr: u32, rs2: u32, rs2_as: u32) -> SymbolicInstructionStatement<T> {
    build_alu(512, rd_ptr, rs1_ptr, rs2, rs2_as)
}
pub fn sub<T: FieldElement>(rd_ptr: u32, rs1_ptr: u32, rs2: u32, rs2_as: u32) -> SymbolicInstructionStatement<T> {
    build_alu(513, rd_ptr, rs1_ptr, rs2, rs2_as)
}
pub fn xor_<T: FieldElement>(rd_ptr: u32, rs1_ptr: u32, rs2: u32, rs2_as: u32) -> SymbolicInstructionStatement<T> {
    build_alu(514, rd_ptr, rs1_ptr, rs2, rs2_as)
}
pub fn or_<T: FieldElement>(rd_ptr: u32, rs1_ptr: u32, rs2: u32, rs2_as: u32) -> SymbolicInstructionStatement<T> {
    build_alu(515, rd_ptr, rs1_ptr, rs2, rs2_as)
}
pub fn and_<T: FieldElement>(rd_ptr: u32, rs1_ptr: u32, rs2: u32, rs2_as: u32) -> SymbolicInstructionStatement<T> {
    build_alu(516, rd_ptr, rs1_ptr, rs2, rs2_as)
}
pub fn sll<T: FieldElement>(rd_ptr: u32, rs1_ptr: u32, rs2: u32, rs2_as: u32) -> SymbolicInstructionStatement<T> {
    build_alu(517, rd_ptr, rs1_ptr, rs2, rs2_as)
}
pub fn srl<T: FieldElement>(rd_ptr: u32, rs1_ptr: u32, rs2: u32, rs2_as: u32) -> SymbolicInstructionStatement<T> {
    build_alu(518, rd_ptr, rs1_ptr, rs2, rs2_as)
}
pub fn sra<T: FieldElement>(rd_ptr: u32, rs1_ptr: u32, rs2: u32, rs2_as: u32) -> SymbolicInstructionStatement<T> {
    build_alu(519, rd_ptr, rs1_ptr, rs2, rs2_as)
}
pub fn slt<T: FieldElement>(rd_ptr: u32, rs1_ptr: u32, rs2: u32, rs2_as: u32) -> SymbolicInstructionStatement<T> {
    build_alu(520, rd_ptr, rs1_ptr, rs2, rs2_as)
}
pub fn sltu<T: FieldElement>(rd_ptr: u32, rs1_ptr: u32, rs2: u32, rs2_as: u32) -> SymbolicInstructionStatement<T> {
    build_alu(521, rd_ptr, rs1_ptr, rs2, rs2_as)
}

// Helper for Load/Store (7-arg) instructions in range 528..=535
fn build_ls<T: FieldElement>(opcode: u32, rd_rs2_ptr: u32, rs1_ptr: u32, imm: u32, mem_as: u32, needs_write: u32, imm_sign: u32) -> SymbolicInstructionStatement<T> {
    SymbolicInstructionStatement {
        opcode: opcode as usize,
        args: vec![
            T::from(rd_rs2_ptr),
            T::from(rs1_ptr),
            T::from(imm),
            T::one(),
            T::from(mem_as),
            T::from(needs_write),
            T::from(imm_sign),
        ],
    }
}

// Load/Store instructions
pub fn loadw<T: FieldElement>(rd_rs2_ptr: u32, rs1_ptr: u32, imm: u32, mem_as: u32, needs_write: u32, imm_sign: u32) -> SymbolicInstructionStatement<T> {
    build_ls(528, rd_rs2_ptr, rs1_ptr, imm, mem_as, needs_write, imm_sign)
}
pub fn loadbu<T: FieldElement>(rd_rs2_ptr: u32, rs1_ptr: u32, imm: u32, mem_as: u32, needs_write: u32, imm_sign: u32) -> SymbolicInstructionStatement<T> {
    build_ls(529, rd_rs2_ptr, rs1_ptr, imm, mem_as, needs_write, imm_sign)
}
pub fn loadhu<T: FieldElement>(rd_rs2_ptr: u32, rs1_ptr: u32, imm: u32, mem_as: u32, needs_write: u32, imm_sign: u32) -> SymbolicInstructionStatement<T> {
    build_ls(530, rd_rs2_ptr, rs1_ptr, imm, mem_as, needs_write, imm_sign)
}
pub fn storew<T: FieldElement>(rd_rs2_ptr: u32, rs1_ptr: u32, imm: u32, mem_as: u32, needs_write: u32, imm_sign: u32) -> SymbolicInstructionStatement<T> {
    build_ls(531, rd_rs2_ptr, rs1_ptr, imm, mem_as, needs_write, imm_sign)
}
pub fn storeh<T: FieldElement>(rd_rs2_ptr: u32, rs1_ptr: u32, imm: u32, mem_as: u32, needs_write: u32, imm_sign: u32) -> SymbolicInstructionStatement<T> {
    build_ls(532, rd_rs2_ptr, rs1_ptr, imm, mem_as, needs_write, imm_sign)
}
pub fn storeb<T: FieldElement>(rd_rs2_ptr: u32, rs1_ptr: u32, imm: u32, mem_as: u32, needs_write: u32, imm_sign: u32) -> SymbolicInstructionStatement<T> {
    build_ls(533, rd_rs2_ptr, rs1_ptr, imm, mem_as, needs_write, imm_sign)
}
pub fn loadb<T: FieldElement>(rd_rs2_ptr: u32, rs1_ptr: u32, imm: u32, mem_as: u32, needs_write: u32, imm_sign: u32) -> SymbolicInstructionStatement<T> {
    build_ls(534, rd_rs2_ptr, rs1_ptr, imm, mem_as, needs_write, imm_sign)
}
pub fn loadh<T: FieldElement>(rd_rs2_ptr: u32, rs1_ptr: u32, imm: u32, mem_as: u32, needs_write: u32, imm_sign: u32) -> SymbolicInstructionStatement<T> {
    build_ls(535, rd_rs2_ptr, rs1_ptr, imm, mem_as, needs_write, imm_sign)
}

// Helper for 5-arg instructions (branches, jumps, etc.)
fn build_5arg<T: FieldElement>(opcode: u32, a: u32, b: u32, c: u32, d: u32, e: u32) -> SymbolicInstructionStatement<T> {
    SymbolicInstructionStatement { opcode: opcode as usize, args: vec![T::from(a), T::from(b), T::from(c), T::from(d), T::from(e)] }
}

// Branch instructions
pub fn beq<T: FieldElement>(a: u32, b: u32, c: u32, d: u32, e: u32) -> SymbolicInstructionStatement<T> { build_5arg(544, a, b, c, d, e) }
pub fn bne<T: FieldElement>(a: u32, b: u32, c: u32, d: u32, e: u32) -> SymbolicInstructionStatement<T> { build_5arg(545, a, b, c, d, e) }
pub fn blt<T: FieldElement>(a: u32, b: u32, c: u32, d: u32, e: u32) -> SymbolicInstructionStatement<T> { build_5arg(549, a, b, c, d, e) }
pub fn bltu<T: FieldElement>(a: u32, b: u32, c: u32, d: u32, e: u32) -> SymbolicInstructionStatement<T> { build_5arg(550, a, b, c, d, e) }
pub fn bge<T: FieldElement>(a: u32, b: u32, c: u32, d: u32, e: u32) -> SymbolicInstructionStatement<T> { build_5arg(551, a, b, c, d, e) }
pub fn bgeu<T: FieldElement>(a: u32, b: u32, c: u32, d: u32, e: u32) -> SymbolicInstructionStatement<T> { build_5arg(552, a, b, c, d, e) }

// Jump and PC-relative instructions
pub fn jal<T: FieldElement>(a: u32, b: u32, c: u32, d: u32, e: u32) -> SymbolicInstructionStatement<T> { build_5arg(560, a, b, c, d, e) }
pub fn lui<T: FieldElement>(a: u32, b: u32, c: u32, d: u32, e: u32) -> SymbolicInstructionStatement<T> { build_5arg(561, a, b, c, d, e) }
pub fn jalr<T: FieldElement>(a: u32, b: u32, c: u32, d: u32, e: u32) -> SymbolicInstructionStatement<T> { build_5arg(565, a, b, c, d, e) }
pub fn auipc<T: FieldElement>(a: u32, b: u32, c: u32, d: u32, e: u32) -> SymbolicInstructionStatement<T> { build_5arg(576, a, b, c, d, e) }

// Multiply/Divide instructions
pub fn mul<T: FieldElement>(a: u32, b: u32, c: u32, d: u32, e: u32) -> SymbolicInstructionStatement<T> { build_5arg(592, a, b, c, d, e) }
pub fn mulh<T: FieldElement>(a: u32, b: u32, c: u32, d: u32, e: u32) -> SymbolicInstructionStatement<T> { build_5arg(593, a, b, c, d, e) }
pub fn mulhsu<T: FieldElement>(a: u32, b: u32, c: u32, d: u32, e: u32) -> SymbolicInstructionStatement<T> { build_5arg(594, a, b, c, d, e) }
pub fn mulhu<T: FieldElement>(a: u32, b: u32, c: u32, d: u32, e: u32) -> SymbolicInstructionStatement<T> { build_5arg(595, a, b, c, d, e) }
pub fn div_<T: FieldElement>(a: u32, b: u32, c: u32, d: u32, e: u32) -> SymbolicInstructionStatement<T> { build_5arg(596, a, b, c, d, e) }
pub fn divu<T: FieldElement>(a: u32, b: u32, c: u32, d: u32, e: u32) -> SymbolicInstructionStatement<T> { build_5arg(597, a, b, c, d, e) }
pub fn rem<T: FieldElement>(a: u32, b: u32, c: u32, d: u32, e: u32) -> SymbolicInstructionStatement<T> { build_5arg(598, a, b, c, d, e) }
pub fn remu<T: FieldElement>(a: u32, b: u32, c: u32, d: u32, e: u32) -> SymbolicInstructionStatement<T> { build_5arg(599, a, b, c, d, e) }

// Hints
pub fn hint_storew<T: FieldElement>(a: u32, b: u32, c: u32, d: u32, e: u32) -> SymbolicInstructionStatement<T> { build_5arg(608, a, b, c, d, e) }
pub fn hint_buffer<T: FieldElement>(a: u32, b: u32, c: u32, d: u32, e: u32) -> SymbolicInstructionStatement<T> { build_5arg(609, a, b, c, d, e) }
