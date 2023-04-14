use std::{fs, path::Path};

use lazy_static::lazy_static;
use regex::Regex;

use crate::riscv::parser::{self, Argument, Register, Statement};

use super::parser::Constant;

/// Compiles riscv assembly file to POWDR assembly. Adds required library routines.
pub fn compile_file(file: &Path) {
    let output = compile_riscv_asm(&fs::read_to_string(file).unwrap());
    log::debug!("{output}");
}

/// Compiles riscv assembly to POWDR assembly. Adds required library routines.
pub fn compile_riscv_asm(data: &str) -> String {
    let statements = parser::parse_asm(data);
    let labels = parser::extract_labels(&statements);
    let label_references = parser::extract_label_references(&statements);
    let missing_labels = label_references.difference(&labels);

    let data = data.to_string()
        + &missing_labels
            .into_iter()
            .map(|label| library_routine(label))
            .collect::<Vec<_>>()
            .join("\n");
    let mut output = preamble();

    for s in parser::parse_asm(&data) {
        output += &process_statement(s);
    }
    output
}

fn preamble() -> String {
    r#"
reg pc[@pc];
reg X[<=];
reg Y[<=];
reg Z[<=];
"#
    .to_string()
        + &(0..32)
            .map(|i| format!("reg x{i};\n"))
            .collect::<Vec<_>>()
            .concat()
        + r#"
reg addr;

pil{
    x0 = 0;
}

pil{
// ============== iszero check for X =======================
    col witness XInv;
    col witness XIsZero;
    XIsZero = 1 - X * XInv;
    XIsZero * X = 0;
    XIsZero * (1 - XIsZero) = 0;

// =============== read-write memory =======================
    // Read-write memory. Columns are sorted by m_addr and
    // then by m_step. m_change is 1 if and only if m_addr changes
    // in the next row.
    col witness m_addr;
    col witness m_step;
    col witness m_change;
    col witness m_value;
    // If we have an operation at all (needed because this needs to be a permutation)
    col witness m_op;
    // If the operation is a write operation.
    col witness m_is_write;
    col witness m_is_read;

    // positive numbers (assumed to be much smaller than the field order)
    col fixed POSITIVE(i) { i + 1 };
    col fixed FIRST = [1] + [0]*;
    col fixed LAST(i) { FIRST(i + 1) };
    col fixed STEP(i) { i };

    m_change * (1 - m_change) = 0;

    // if m_change is zero, m_addr has to stay the same.
    (m_addr' - m_addr) * (1 - m_change) = 0;

    // Except for the last row, if m_change is 1, then m_addr has to increase,
    // if it is zero, m_step has to increase.
    (1 - LAST) { m_change * (m_addr' - m_addr) + (1 - m_change) * (m_step' - m_step) } in POSITIVE;

    m_op * (1 - m_op) = 0;
    m_is_write * (1 - m_is_write) = 0;
    m_is_read * (1 - m_is_read) = 0;
    // m_is_write can only be 1 if m_op is 1.
    m_is_write * (1 - m_op) = 0;
    m_is_read * (1 - m_op) = 0;
    m_is_read * m_is_write = 0;


    // If the next line is a read and we stay at the same address, then the
    // value cannot change.
    (1 - m_is_write') * (1 - m_change) * (m_value' - m_value) = 0;

    // If the next line is a read and we have an address change,
    // then the value is zero.
    (1 - m_is_write') * m_change * m_value' = 0;
}

// ============== memory instructions ==============

instr mstore X { { addr, STEP, X } is m_is_write { m_addr, m_step, m_value } }
instr mload -> X { { addr, STEP, X } is m_is_read { m_addr, m_step, m_value } }

// ============== control-flow instructions ==============

instr jump l: label { pc' = l }
instr call l: label { pc' = l, x1' = pc + 1, x6' = l }
instr ret { pc' = x1 }

instr branch_if_nonzero X, l: label { pc' = (1 - XIsZero) * l + XIsZero * (pc + 1) }
instr branch_if_zero X, l: label { pc' = XIsZero * l + (1 - XIsZero) * (pc + 1) }

// input X is required to be the difference of two 32-bit unsigend values.
// i.e. -2**32 < X < 2**32
instr branch_if_positive X, l: label {
    X + 2**32 - 1 = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000 + wrap_bit * 2**32,
    pc' = wrap_bit * l + (1 - wrap_bit) * (pc + 1)
}

// ================= logical instructions =================

instr is_equal_zero X -> Y { Y = XIsZero }

// ================= arith/bitwise instructions =================

// instr xor X, Y, Z {
//     {X, Y, Z} in 1 { binary.X, binary.Y, binary.RESULT, 1 }
// }
// we wanted better synatx: { binary(X, Y, Z) }
// maybe alternate syntax: instr xor a(Y), b(Z) -> X

// ================== wrapping instructions ==============

// Wraps a value in Y to 32 bits.
// Requires 0 <= Y < 2**33
instr wrap Y -> X { Y = X + wrap_bit * 2**32, X = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000 }
pil{
    col fixed bytes(i) { i & 0xff };
    col witness X_b1;
    col witness X_b2;
    col witness X_b3;
    col witness X_b4;
    { X_b1 } in { bytes };
    { X_b2 } in { bytes };
    { X_b3 } in { bytes };
    { X_b4 } in { bytes };
    col witness wrap_bit;
    wrap_bit * (1 - wrap_bit) = 0;
}

// ======================= assertions =========================

instr fail { 1 = 0 }

// Removes up to 16 bits beyond 32
// TODO is this really safe?
instr wrap16 Y -> X { Y = Y_b5 * 2**32 + Y_b6 * 2**40 + X, X = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000 }
pil{
    col witness Y_b5;
    col witness Y_b6;
    { Y_b5 } in { bytes };
    { Y_b6 } in { bytes };
}

// set the stack pointer.
// TODO other things to initialize?
x2 <=X= 0x10000;
    "#
}

lazy_static! {
    static ref LIBRARY_ROUTINES: Vec<(Regex, &'static str)> = vec![
        (
            Regex::new(r"^_ZN4core9panicking18panic_bounds_check17h[0-9a-f]{16}E$").unwrap(),
            "unimp"
        ),
        (
            Regex::new(r"^_ZN4core9panicking5panic17h[0-9a-f]{16}E$").unwrap(),
            "unimp"
        ),
        (
            Regex::new(r"^_ZN4core5slice5index24slice_end_index_len_fail17h[0-9a-f]{16}E$")
                .unwrap(),
            "unimp"
        ),
        (
            Regex::new(r"^memset@plt$").unwrap(),
            r#"
# a4: number of bytes
# a0: memory location
# a1: value
# We assume the value is zero and a4 is a multiple of 4
# TODO this is of course not always true
    beqz a4, ___end_memset
    sw a1, 0(a0)
    addi a4, a4, -4
    j memset@plt
___end_memset:
    ret
"#
        ),
    ];
}

fn library_routine(label: &str) -> String {
    for (pattern, routine) in LIBRARY_ROUTINES.iter() {
        if pattern.is_match(label) {
            return format!("{label}:\n{routine}");
        }
    }
    eprintln!("The RISCV assembly code references an external routine / label that has not been implemented yet:");
    eprintln!("{label}");
    panic!();
}

fn process_statement(s: Statement) -> String {
    match &s {
        Statement::Label(l) => format!("{}::\n", escape_label(l)),
        Statement::Directive(_, _) => String::new(), // ignore
        Statement::Instruction(instr, args) => {
            let s = process_instruction(instr, args);
            assert!(s.ends_with('\n'));
            "  ".to_string() + &s[..s.len() - 1].replace('\n', "\n  ") + "\n"
        }
    }
}

fn escape_label(l: &str) -> String {
    // TODO make this proper
    l.replace('.', "_dot_")
}

fn argument_to_number(x: &Argument) -> u32 {
    if let Argument::Constant(c) = x {
        constant_to_number(c)
    } else {
        panic!()
    }
}

fn constant_to_number(c: &Constant) -> u32 {
    match c {
        Constant::Number(n) => *n as u32,
        Constant::HiDataRef(_) => 0, // TODO
        Constant::LoDataRef(_) => 0, // TODO
    }
}

fn rri(args: &[Argument]) -> (Register, Register, u32) {
    match args {
        [Argument::Register(r1), Argument::Register(r2), n] => (*r1, *r2, argument_to_number(n)),
        _ => panic!(),
    }
}

fn rrr(args: &[Argument]) -> (Register, Register, Register) {
    match args {
        [Argument::Register(r1), Argument::Register(r2), Argument::Register(r3)] => (*r1, *r2, *r3),
        _ => panic!(),
    }
}

fn ri(args: &[Argument]) -> (Register, u32) {
    match args {
        [Argument::Register(r1), n] => (*r1, argument_to_number(n)),
        _ => panic!(),
    }
}

fn rr(args: &[Argument]) -> (Register, Register) {
    match args {
        [Argument::Register(r1), Argument::Register(r2)] => (*r1, *r2),
        _ => panic!(),
    }
}

fn rrl(args: &[Argument]) -> (Register, Register, String) {
    match args {
        [Argument::Register(r1), Argument::Register(r2), Argument::Symbol(l)] => {
            (*r1, *r2, escape_label(l))
        }
        _ => panic!(),
    }
}

fn rl(args: &[Argument]) -> (Register, String) {
    match args {
        [Argument::Register(r1), Argument::Symbol(l)] => (*r1, escape_label(l)),
        _ => panic!(),
    }
}

fn rro(args: &[Argument]) -> (Register, Register, u32) {
    match args {
        [Argument::Register(r1), Argument::RegOffset(r2, off)] => {
            (*r1, *r2, constant_to_number(off))
        }
        _ => panic!(),
    }
}

fn process_instruction(instr: &str, args: &[Argument]) -> String {
    match instr {
        "add" => {
            let (rd, r1, r2) = rrr(args);
            format!("{rd} <=X= wrap({r1} + {r2});\n")
        }
        "addi" => {
            let (rd, rs, imm) = rri(args);
            format!("{rd} <=X= wrap({rs} + {imm});\n")
        }
        "beq" => {
            let (r1, r2, label) = rrl(args);
            format!("branch_if_zero {r1} - {r2}, {label};\n")
        }
        "beqz" => {
            let (r1, label) = rl(args);
            format!("branch_if_zero {r1}, {label};\n")
        }
        "bgeu" => {
            let (r1, r2, label) = rrl(args);
            format!("branch_if_positive {r1} - {r2}, {label};\n")
        }
        "bltu" => {
            let (r1, r2, label) = rrl(args);
            format!("branch_if_positive {r2} - {r1}, {label};\n")
        }
        "bne" => {
            let (r1, r2, label) = rrl(args);
            format!("branch_if_nonzero {r1} - {r2}, {label};\n")
        }
        "bnez" => {
            let (r1, label) = rl(args);
            format!("branch_if_nonzero {r1}, {label};\n")
        }
        "j" => {
            if let [Argument::Symbol(label)] = args {
                format!("jump {};\n", escape_label(label))
            } else {
                panic!()
            }
        }
        "call" => {
            if let [Argument::Symbol(label)] = args {
                format!("call {};\n", escape_label(label))
            } else {
                panic!()
            }
        }
        "ecall" => {
            assert!(args.is_empty());
            "x10 <=X= ${ (\"input\", x10) };\n".to_string()
        }
        "li" => {
            let (rd, imm) = ri(args);
            format!("{rd} <=X= {imm};\n")
        }
        "lui" => {
            let (rd, imm) = ri(args);
            format!("{rd} <=X= {};\n", imm << 12)
        }
        "lw" => {
            let (rd, rs, off) = rro(args);
            format!("addr <=X= wrap({rs} + {off});\n") + &format!("{rd} <=X= mload();\n")
        }
        "sw" => {
            let (r1, r2, off) = rro(args);
            format!("addr <=X= wrap({r2} + {off});\n") + &format!("mstore {r1};\n")
        }
        "mv" => {
            let (rd, rs) = rr(args);
            format!("{rd} <=X= {rs};\n")
        }
        "ret" => {
            assert!(args.is_empty());
            "ret;\n".to_string()
        }
        "seqz" => {
            let (rd, rs) = rr(args);
            format!("{rd} <=Y= is_equal_zero {rs};\n")
        }
        "slli" => {
            let (rd, rs, amount) = rri(args);
            assert!(amount <= 31);
            if amount <= 16 {
                format!("{rd} <=X= wrap16({rs} * {});\n", 1 << amount)
            } else {
                todo!();
            }
        }
        "unimp" => "fail;\n".to_string(),
        "xor" => {
            todo!();
            // let (rd, r1, r2) = rrr(args);
            // format!("{rd} <=X= xor {r1}, {r2};\n")
        }
        _ => todo!("Unknown instruction: {instr}"),
    }
}
