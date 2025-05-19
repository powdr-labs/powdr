use openvm_instructions::instruction::Instruction;
use openvm_stark_backend::p3_field::PrimeField32;

pub fn openvm_instruction_formatter<F: PrimeField32>(instruction: &Instruction<F>) -> String {
    let Instruction {
        opcode,
        a,
        b,
        c,
        d,
        e,
        f,
        g,
    } = instruction;

    // Opcodes taken from:
    // https://github.com/openvm-org/openvm/blob/v1.0.0/extensions/rv32im/transpiler/src/instructions.rs
    match opcode.as_usize() {
        // Alu instructions, see:
        // https://github.com/openvm-org/openvm/blob/v1.0.0/extensions/rv32im/circuit/src/adapters/alu.rs#L197-L201
        512..=521 => {
            assert_eq!(d, &F::ONE);
            assert_eq!(f, &F::ZERO);
            assert_eq!(g, &F::ZERO);
            let opcode = match opcode.as_usize() {
                // Rv32BaseAluChip
                512 => "ADD",
                513 => "SUB",
                514 => "XOR",
                515 => "OR",
                516 => "AND",
                // Rv32ShiftChip
                517 => "SLL",
                518 => "SRL",
                519 => "SRA",
                // Rv32LessThanChip
                520 => "SLT",
                521 => "SLTU",
                _ => unreachable!(),
            };
            format!("{opcode} rd_ptr = {a}, rs1_ptr = {b}, rs2 = {c}, rs2_as = {e}")
        }

        // Load/Store instructions, see:
        // https://github.com/openvm-org/openvm/blob/v1.0.0/extensions/rv32im/circuit/src/adapters/loadstore.rs#L340-L346
        528..=535 => {
            assert_eq!(d, &F::ONE);
            let opcode = match opcode.as_usize() {
                528 => "LOADW",
                529 => "LOADBU",
                530 => "LOADHU",
                531 => "STOREW",
                532 => "STOREH",
                533 => "STOREB",
                534 => "LOADB",
                535 => "LOADH",
                _ => unreachable!(),
            };
            format!("{opcode} rd_rs2_ptr = {a}, rs1_ptr = {b}, imm = {c}, mem_as = {e}, needs_write = {f}, imm_sign = {g}")
        }

        544 => format!("BEQ {a} {b} {c} {d} {e}"),
        545 => format!("BNE {a} {b} {c} {d} {e}"),

        549 => format!("BLT {a} {b} {c} {d} {e}"),
        550 => format!("BLTU {a} {b} {c} {d} {e}"),
        551 => format!("BGE {a} {b} {c} {d} {e}"),
        552 => format!("BGEU {a} {b} {c} {d} {e}"),

        560 => format!("JAL {a} {b} {c} {d} {e}"),
        561 => format!("LUI {a} {b} {c} {d} {e}"),

        565 => format!("JALR {a} {b} {c} {d} {e}"),

        576 => format!("AUIPC {a} {b} {c} {d} {e}"),

        592 => format!("MUL {a} {b} {c} {d} {e}"),
        593 => format!("MULH {a} {b} {c} {d} {e}"),
        594 => format!("MULHSU {a} {b} {c} {d} {e}"),
        595 => format!("MULHU {a} {b} {c} {d} {e}"),

        596 => format!("DIV {a} {b} {c} {d} {e}"),
        597 => format!("DIVU {a} {b} {c} {d} {e}"),
        598 => format!("REM {a} {b} {c} {d} {e}"),
        599 => format!("REMU {a} {b} {c} {d} {e}"),

        608 => format!("HINT_STOREW {a} {b} {c} {d} {e}"),
        609 => format!("HINT_BUFFER {a} {b} {c} {d} {e}"),
        _ => format!("<opcode {opcode}> {a} {b} {c} {d} {e} {f} {g}"),
    }
}
