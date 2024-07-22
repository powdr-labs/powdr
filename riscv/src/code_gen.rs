use std::fmt;

use itertools::Itertools;
use powdr_asm_utils::data_storage::SingleDataValue;
use powdr_asm_utils::utils::{escape_label, quote};
use powdr_number::{FieldElement, KnownField};

use crate::continuations::bootloader::{bootloader_and_shutdown_routine, bootloader_preamble};
use crate::runtime::Runtime;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Register {
    value: u8,
}

impl Register {
    pub fn new(value: u8) -> Self {
        Self { value }
    }

    pub fn is_zero(&self) -> bool {
        self.value == 0
    }
}

impl powdr_asm_utils::ast::Register for Register {}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "x{}", self.value)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum FunctionKind {
    HiDataRef,
    LoDataRef,
}

impl powdr_asm_utils::ast::FunctionOpKind for FunctionKind {}

impl fmt::Display for FunctionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionKind::HiDataRef => write!(f, "%hi"),
            FunctionKind::LoDataRef => write!(f, "%lo"),
        }
    }
}

pub enum Statement<'a, L: AsRef<str>, A: InstructionArgs> {
    DebugLoc { file: u64, line: u64, col: u64 },
    Label(L),
    Instruction { op: &'a str, args: A },
}

pub struct MemEntry {
    pub label: Option<String>,
    pub addr: u32,
    pub value: SingleDataValue,
}

pub struct SourceFileInfo<'a> {
    pub id: u32,
    pub dir: &'a str,
    pub file: &'a str,
}

/// A RISC-V program that can be translated to POWDR ASM.
pub trait RiscVProgram {
    /// Takes the listing of source files, to be used in the debug statements.
    fn take_source_files_info(&mut self) -> impl Iterator<Item = SourceFileInfo>;

    /// Takes the initial memory snapshot.
    fn take_initial_mem(&mut self) -> impl Iterator<Item = MemEntry>;

    /// Takes the executable statements and labels.
    fn take_executable_statements(
        &mut self,
    ) -> impl Iterator<Item = Statement<impl AsRef<str>, impl InstructionArgs>>;

    /// The name of the function that should be called to start the program.
    fn start_function(&self) -> impl AsRef<str>;
}

/// Translates a RISC-V program to POWDR ASM.
///
/// Will call each of the methods in the `RiscVProgram` just once.
pub fn translate_program<F: FieldElement>(
    program: impl RiscVProgram,
    runtime: &Runtime,
    with_bootloader: bool,
) -> String {
    // Do this in a separate function to avoid most of the code being generic on F.
    let (initial_mem, instructions, degree) =
        translate_program_impl(program, runtime, with_bootloader);

    riscv_machine(
        runtime,
        degree,
        &preamble::<F>(runtime, with_bootloader),
        initial_mem,
        instructions,
    )
}

fn translate_program_impl(
    mut program: impl RiscVProgram,
    runtime: &Runtime,
    with_bootloader: bool,
) -> (Vec<String>, Vec<String>, u64) {
    let mut initial_mem = Vec::new();
    let mut data_code = Vec::new();
    for MemEntry { label, addr, value } in program.take_initial_mem() {
        if let Some(label) = label {
            // This is a comment, so we don't need to escape the label.
            let comment = format!(" // data {label}");
            if with_bootloader && !matches!(value, SingleDataValue::LabelReference(_)) {
                &mut initial_mem
            } else {
                &mut data_code
            }
            .push(comment);
        }
        match value {
            SingleDataValue::Value(v) => {
                if with_bootloader {
                    // Instead of generating the data loading code, we store it
                    // in the variable that will be used as the initial memory
                    // snapshot, committed by the bootloader.
                    initial_mem.push(format!("(0x{addr:x}, 0x{v:x})"));
                } else {
                    // There is no bootloader to commit to memory, so we have to
                    // load it explicitly.
                    data_code.push(format!("mstore 0x{addr:x}, 0x{v:x};"));
                }
            }
            SingleDataValue::LabelReference(sym) => {
                // The label value is not known at this point, so we have to
                // load it via code, irrespectively of bootloader availability.
                //
                // TODO should be possible without temporary
                data_code.extend([
                    format!("tmp1 <== load_label({});", escape_label(&sym)),
                    format!("mstore 0x{addr:x}, tmp1;"),
                ]);
            }
            SingleDataValue::Offset(_, _) => {
                unimplemented!();
                /*
                object_code.push(format!("addr <=X= 0x{pos:x};"));

                I think this solution should be fine but hard to say without
                an actual code snippet that uses it.

                // TODO should be possible without temporary
                object_code.extend([
                    format!("tmp1 <== load_label({});", escape_label(a)),
                    format!("tmp2 <== load_label({});", escape_label(b)),
                    // TODO check if registers match
                    "mstore wrap(tmp1 - tmp2);".to_string(),
                ]);
                */
            }
        }
    }

    let submachines_init = runtime.submachines_init();
    let bootloader_and_shutdown_routine_lines = if with_bootloader {
        let bootloader_and_shutdown_routine = bootloader_and_shutdown_routine(&submachines_init);
        log::debug!("Adding Bootloader:\n{}", bootloader_and_shutdown_routine);
        bootloader_and_shutdown_routine
            .split('\n')
            .map(|l| l.to_string())
            .collect::<Vec<_>>()
    } else {
        submachines_init
    };

    let mut statements: Vec<String> = program
        .take_source_files_info()
        .map(
            |SourceFileInfo {
                 id,
                 dir,
                 file: name,
             }| { format!(".debug file {id} {} {};", quote(dir), quote(name)) },
        )
        .chain(bootloader_and_shutdown_routine_lines)
        .collect();
    if !data_code.is_empty() {
        statements.push("x1 <== jump(__data_init);".to_string());
    }
    statements.extend([
        format!("x1 <== jump({});", program.start_function().as_ref()),
        "return;".to_string(), // This is not "riscv ret", but "return from powdr asm function".
    ]);
    for s in program.take_executable_statements() {
        match s {
            Statement::DebugLoc { file, line, col } => {
                statements.push(format!(".debug loc {file} {line} {col};"))
            }
            Statement::Label(l) => statements.push(format!("{}:", escape_label(l.as_ref()))),
            Statement::Instruction { op, args } => {
                let processed_instr = match process_instruction(op, args) {
                    Ok(s) => s,
                    Err(e) => panic!("Failed to process instruction '{op}'. {e}"),
                };
                statements.extend(processed_instr.into_iter().map(|s| "  ".to_string() + &s))
            }
        }
    }

    if !data_code.is_empty() {
        statements.extend(
        ["// This is the data initialization routine.\n__data_init:".to_string()].into_iter()
        .chain(data_code)
        .chain([
            "// This is the end of the data initialization routine.\ntmp1 <== jump_dyn(x1);"
                .to_string(),
        ]));
    }
    statements.extend(runtime.ecall_handler());

    // The program ROM needs to fit the degree, so we use the next power of 2.
    let degree = statements.len().ilog2() + 1;
    let degree = std::cmp::max(degree, 18);
    log::info!("Inferred degree 2^{degree}");

    // In practice, these are the lengths of single proofs that we want to support.
    // Reasoning:
    // - 18: is the lower bound for the Binary and Shift machines.
    // - 20: revm's ROM does not fit in 2^19.
    // - >20: may be needed in the future.
    // This is an assert for now, but could be a compiler warning or error.
    // TODO note that if the degree is higher than 18 we might need mux machines for Binary and
    // Shift.
    assert!((18..=20).contains(&degree));
    let degree = 1 << degree;

    (initial_mem, statements, degree)
}

fn riscv_machine(
    runtime: &Runtime,
    degree: u64,
    preamble: &str,
    initial_memory: Vec<String>,
    program: Vec<String>,
) -> String {
    format!(
        r#"
{}
machine Main with degree: {degree} {{
{}

{}

let initial_memory: (fe, fe)[] = [
{}
];

    function main {{
{}
    }}
}}    
"#,
        runtime.submachines_import(),
        runtime.submachines_declare(),
        preamble,
        initial_memory
            .into_iter()
            .format_with(",\n", |line, f| f(&format_args!("\t\t{line}"))),
        program
            .into_iter()
            .format_with("\n", |line, f| f(&format_args!("\t\t{line}"))),
    )
}

fn preamble<T: FieldElement>(runtime: &Runtime, with_bootloader: bool) -> String {
    let bootloader_preamble_if_included = if with_bootloader {
        bootloader_preamble()
    } else {
        "".to_string()
    };

    for machine in ["binary", "shift", "bit2", "bit6", "bit7", "byte"] {
        assert!(
            runtime.has_submachine(machine),
            "RISC-V machine requires the `{machine}` submachine"
        );
    }

    let mul_instruction = mul_instruction::<T>(runtime);

    r#"
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg W[<=];
    reg tmp1;
    reg tmp2;
    reg tmp3;
    reg tmp4;
    reg lr_sc_reservation;
"#
        .to_string()
        // risc-v x* registers
        + &(0..32)
            .map(|i| format!("\t\treg x{i};\n"))
            .join("")
        // runtime extra registers
        + &runtime
            .submachines_extra_registers()
            .into_iter()
            .map(|s| format!("\t\t{s}\n"))
            .join("")
        + &bootloader_preamble_if_included
        + &memory(with_bootloader)
        + r#"
    // ============== Constraint on x0 =======================

    x0 = 0;

    // ============== iszero check for X =======================
    let XIsZero = std::utils::is_zero(X);

    // ============== control-flow instructions ==============

    instr load_label l: label -> X { X = l }

    instr jump l: label -> Y { pc' = l, Y = pc + 1}
    instr jump_dyn X -> Y { pc' = X, Y = pc + 1}

    instr branch_if_nonzero X, l: label { pc' = (1 - XIsZero) * l + XIsZero * (pc + 1) }
    instr branch_if_zero X, l: label { pc' = XIsZero * l + (1 - XIsZero) * (pc + 1) }

    // Skips Y instructions if X is zero
    instr skip_if_zero X, Y { pc' = pc + 1 + (XIsZero * Y) }

    // input X is required to be the difference of two 32-bit unsigend values.
    // i.e. -2**32 < X < 2**32
    instr branch_if_positive X, l: label {
        X + 2**32 - 1 = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000 + wrap_bit * 2**32,
        pc' = wrap_bit * l + (1 - wrap_bit) * (pc + 1)
    }
    // input X is required to be the difference of two 32-bit unsigend values.
    // i.e. -2**32 < X < 2**32
    instr is_positive X -> Y {
        X + 2**32 - 1 = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000 + wrap_bit * 2**32,
        Y = wrap_bit
    }

    // ================= logical instructions =================

    instr is_equal_zero X -> Y { Y = XIsZero }
    instr is_not_equal_zero X -> Y { Y = 1 - XIsZero }

    // ================= submachine instructions =================
"# + &runtime
        .submachines_instructions()
        .into_iter()
        .map(|s| format!("    {s}"))
        .join("\n")
        + r#"
    // Wraps a value in Y to 32 bits.
    // Requires 0 <= Y < 2**33
    instr wrap Y -> X { Y = X + wrap_bit * 2**32, X = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000 }
    // Requires -2**32 <= Y < 2**32
    instr wrap_signed Y -> X { Y + 2**32 = X + wrap_bit * 2**32, X = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000 }
    col witness X_b1;
    col witness X_b2;
    col witness X_b3;
    col witness X_b4;
    link => byte.check(X_b1);
    link => byte.check(X_b2);
    link => byte.check(X_b3);
    link => byte.check(X_b4);
    col witness wrap_bit;
    wrap_bit * (1 - wrap_bit) = 0;

    // Input is a 32 bit unsigned number. We check bit 7 and set all higher bits to that value.
    instr sign_extend_byte Y -> X {
        // wrap_bit is used as sign_bit here.
        Y = Y_7bit + wrap_bit * 0x80 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000,
        X = Y_7bit + wrap_bit * 0xffffff80
    }
    col witness Y_7bit;
    link => bit7.check(Y_7bit);

    // Input is a 32 bit unsigned number. We check bit 15 and set all higher bits to that value.
    instr sign_extend_16_bits Y -> X {
        Y_15bit = X_b1 + Y_7bit * 0x100,

        // wrap_bit is used as sign_bit here.
        Y = Y_15bit + wrap_bit * 0x8000 + X_b3 * 0x10000 + X_b4 * 0x1000000,
        X = Y_15bit + wrap_bit * 0xffff8000
    }
    col witness Y_15bit;

    // Input is a 32 but unsigned number (0 <= Y < 2**32) interpreted as a two's complement numbers.
    // Returns a signed number (-2**31 <= X < 2**31).
    instr to_signed Y -> X {
        // wrap_bit is used as sign_bit here.
        Y = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + Y_7bit * 0x1000000 + wrap_bit * 0x80000000,
        X = Y - wrap_bit * 2**32
    }

    // ======================= assertions =========================

    instr fail { 1 = 0 }

    // Removes up to 16 bits beyond 32
    // TODO is this really safe?
    instr wrap16 Y -> X { Y = Y_b5 * 2**32 + Y_b6 * 2**40 + X, X = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000 }
    col witness Y_b5;
    col witness Y_b6;
    col witness Y_b7;
    col witness Y_b8;
    link => byte.check(Y_b5);
    link => byte.check(Y_b6);
    link => byte.check(Y_b7);
    link => byte.check(Y_b8);

    col witness REM_b1;
    col witness REM_b2;
    col witness REM_b3;
    col witness REM_b4;
    link => byte.check(REM_b1);
    link => byte.check(REM_b2);
    link => byte.check(REM_b3);
    link => byte.check(REM_b4);

    // implements Z = Y / X and W = Y % X.
    instr divremu Y, X -> Z, W {
        // main division algorithm:
        // Y is the known dividend
        // X is the known divisor
        // Z is the unknown quotient
        // W is the unknown remainder
        // if X is zero, remainder is set to dividend, as per RISC-V specification:
        X * Z + W = Y,

        // remainder >= 0:
        W = REM_b1 + REM_b2 * 0x100 + REM_b3 * 0x10000 + REM_b4 * 0x1000000,

        // remainder < divisor, conditioned to X not being 0:
        (1 - XIsZero) * (X - W - 1 - Y_b5 - Y_b6 * 0x100 - Y_b7 * 0x10000 - Y_b8 * 0x1000000) = 0,

        // in case X is zero, we set quotient according to RISC-V specification
        XIsZero * (Z - 0xffffffff) = 0,

        // quotient is 32 bits:
        Z = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000
    }
"# + mul_instruction
}

fn mul_instruction<T: FieldElement>(runtime: &Runtime) -> &'static str {
    match T::known_field().expect("Unknown field!") {
        KnownField::Bn254Field => {
            // The BN254 field can fit any 64-bit number, so we can naively de-compose
            // Z * W into 8 bytes and put them together to get the upper and lower word.
            r#"
    // Multiply two 32-bits unsigned, return the upper and lower unsigned 32-bit
    // halves of the result.
    // X is the lower half (least significant bits)
    // Y is the higher half (most significant bits)
    instr mul Z, W -> X, Y {
        Z * W = X + Y * 2**32,
        X = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000,
        Y = Y_b5 + Y_b6 * 0x100 + Y_b7 * 0x10000 + Y_b8 * 0x1000000
    }
"#
        }
        KnownField::GoldilocksField => {
            assert!(
                runtime.has_submachine("split_gl"),
                "RISC-V machine with the goldilocks field requires the `split_gl` submachine"
            );
            // The Goldilocks field cannot fit some 64-bit numbers, so we have to use
            // the split machine. Note that it can fit a product of two 32-bit numbers.
            r#"
    // Multiply two 32-bits unsigned, return the upper and lower unsigned 32-bit
    // halves of the result.
    // X is the lower half (least significant bits)
    // Y is the higher half (most significant bits)
    instr mul Z, W -> X, Y link ~> (X, Y) = split_gl.split(Z * W);
"#
        }
    }
}

fn memory(with_bootloader: bool) -> String {
    let memory_machine = if with_bootloader {
        r#"
    std::machines::memory_with_bootloader_write::MemoryWithBootloaderWrite memory;
    instr mstore_bootloader Y, Z -> link ~> memory.mstore_bootloader(X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000, STEP, Z) {
        Y = (X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000) + wrap_bit * 2**32
    }
"#
    } else {
        r#"
    std::machines::memory::Memory memory;
"#
    };

    memory_machine.to_string()
        + r#"

    col fixed STEP(i) { i };

    // ============== memory instructions ==============

    /// Loads one word from an address Y, where Y can be between 0 and 2**33 (sic!),
    /// wraps the address to 32 bits and rounds it down to the next multiple of 4.
    /// Returns the loaded word and the remainder of the division by 4.
    instr mload Y -> X, Z 
        link ~> X = memory.mload(X_b4 * 0x1000000 + X_b3 * 0x10000 + X_b2 * 0x100 + X_b1 * 4, STEP)
        link => bit2.check(Z)
        link => bit6.check(X_b1) 
    {
        Y = wrap_bit * 2**32 + X_b4 * 0x1000000 + X_b3 * 0x10000 + X_b2 * 0x100 + X_b1 * 4 + Z
    }

    /// Stores Z at address Y % 2**32. Y can be between 0 and 2**33.
    /// Y should be a multiple of 4, but this instruction does not enforce it.
    instr mstore Y, Z -> link ~> memory.mstore(X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000, STEP, Z) {
        Y = (X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000) + wrap_bit * 2**32
    }
"#
}

pub trait InstructionArgs {
    type Error: fmt::Display;

    fn l(&self) -> Result<impl AsRef<str>, Self::Error>;
    fn r(&self) -> Result<Register, Self::Error>;
    fn rri(&self) -> Result<(Register, Register, u32), Self::Error>;
    /// Returns the usual rd, rs1, rs2
    fn rrr(&self) -> Result<(Register, Register, Register), Self::Error>;
    /// Special case used in amo* instructions, returning rd, rs2, rs1
    fn rrr2(&self) -> Result<(Register, Register, Register), Self::Error>;
    fn ri(&self) -> Result<(Register, u32), Self::Error>;
    fn rr(&self) -> Result<(Register, Register), Self::Error>;
    fn rrl(&self) -> Result<(Register, Register, impl AsRef<str>), Self::Error>;
    fn rl(&self) -> Result<(Register, impl AsRef<str>), Self::Error>;
    fn rro(&self) -> Result<(Register, Register, u32), Self::Error>;
    fn empty(&self) -> Result<(), Self::Error>;
}

fn only_if_no_write_to_zero(statement: String, reg: Register) -> Vec<String> {
    only_if_no_write_to_zero_vec(vec![statement], reg)
}

fn only_if_no_write_to_zero_vec(statements: Vec<String>, reg: Register) -> Vec<String> {
    if reg.is_zero() {
        vec![]
    } else {
        statements
    }
}

/// Push register into the stack
pub fn push_register(name: &str) -> [String; 2] {
    [
        "x2 <=X= wrap(x2 - 4);".to_string(),
        format!("mstore x2, {name};"),
    ]
}

/// Pop register from the stack
pub fn pop_register(name: &str) -> [String; 2] {
    [
        format!("{name}, tmp1 <== mload(x2);"),
        "x2 <=X= wrap(x2 + 4);".to_string(),
    ]
}

fn process_instruction<A: InstructionArgs>(instr: &str, args: A) -> Result<Vec<String>, A::Error> {
    Ok(match instr {
        // load/store registers
        "li" | "la" => {
            // The difference between "li" and "la" in RISC-V is that the former
            // is for loading values as is, and the later is for loading PC
            // relative values. But since we work on a higher abstraction level,
            // for us they are the same thing.
            if let Ok((rd, label)) = args.rl() {
                let label = escape_label(label.as_ref());
                only_if_no_write_to_zero(format!("{rd} <== load_label({label});"), rd)
            } else {
                let (rd, imm) = args.ri()?;
                only_if_no_write_to_zero(format!("{rd} <=X= {imm};"), rd)
            }
        }
        // TODO check if it is OK to clear the lower order bits
        "lui" => {
            let (rd, imm) = args.ri()?;
            only_if_no_write_to_zero(format!("{rd} <=X= {};", imm << 12), rd)
        }
        "mv" => {
            let (rd, rs) = args.rr()?;
            only_if_no_write_to_zero(format!("{rd} <=X= {rs};"), rd)
        }

        // Arithmetic
        "add" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero(format!("{rd} <== wrap({r1} + {r2});"), rd)
        }
        "addi" => {
            let (rd, rs, imm) = args.rri()?;
            only_if_no_write_to_zero(format!("{rd} <== wrap({rs} + {imm});"), rd)
        }
        "sub" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero(format!("{rd} <== wrap_signed({r1} - {r2});"), rd)
        }
        "neg" => {
            let (rd, r1) = args.rr()?;
            only_if_no_write_to_zero(format!("{rd} <== wrap_signed(0 - {r1});"), rd)
        }
        "mul" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero(format!("{rd}, tmp1 <== mul({r1}, {r2});"), rd)
        }
        "mulhu" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero(format!("tmp1, {rd} <== mul({r1}, {r2});"), rd)
        }
        "mulh" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero_vec(
                vec![
                    format!("tmp1 <== to_signed({r1});"),
                    format!("tmp2 <== to_signed({r2});"),
                    // tmp3 is 1 if tmp1 is non-negative
                    "tmp3 <== is_positive(tmp1 + 1);".into(),
                    // tmp4 is 1 if tmp2 is non-negative
                    "tmp4 <== is_positive(tmp2 + 1);".into(),
                    // If tmp1 is negative, convert to positive
                    "skip_if_zero 0, tmp3;".into(),
                    "tmp1 <=X= 0 - tmp1;".into(),
                    // If tmp2 is negative, convert to positive
                    "skip_if_zero 0, tmp4;".into(),
                    "tmp2 <=X= 0 - tmp2;".into(),
                    format!("tmp1, {rd} <== mul(tmp1, tmp2);"),
                    // Determine the sign of the result based on the signs of tmp1 and tmp2
                    "tmp3 <== is_not_equal_zero(tmp3 - tmp4);".into(),
                    // If the result should be negative, convert back to negative
                    "skip_if_zero tmp3, 2;".into(),
                    "tmp1 <== is_equal_zero(tmp1);".into(),
                    format!("{rd} <== wrap_signed(-{rd} - 1 + tmp1);"),
                ],
                rd,
            )
        }
        "mulhsu" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero_vec(
                vec![
                    format!("tmp1 <== to_signed({r1});"),
                    // tmp2 is 1 if tmp1 is non-negative
                    "tmp2 <== is_positive(tmp1 + 1);".into(),
                    // If negative, convert to positive
                    "skip_if_zero 0, tmp2;".into(),
                    "tmp1 <=X= 0 - tmp1;".into(),
                    format!("tmp1, {rd} <== mul(tmp1, {r2});"),
                    // If was negative before, convert back to negative
                    "skip_if_zero (1-tmp2), 2;".into(),
                    "tmp1 <== is_equal_zero(tmp1);".into(),
                    // If the lower bits are zero, return the two's complement,
                    // otherwise return one's complement.
                    format!("{rd} <== wrap_signed(-{rd} - 1 + tmp1);"),
                ],
                rd,
            )
        }
        "divu" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero(format!("{rd}, tmp1 <== divremu({r1}, {r2});"), rd)
        }
        "remu" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero(format!("tmp1, {rd} <== divremu({r1}, {r2});"), rd)
        }

        // bitwise
        "xor" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero(format!("{rd} <== xor({r1}, {r2});"), rd)
        }
        "xori" => {
            let (rd, r1, imm) = args.rri()?;
            only_if_no_write_to_zero(format!("{rd} <== xor({r1}, {imm});"), rd)
        }
        "and" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero(format!("{rd} <== and({r1}, {r2});"), rd)
        }
        "andi" => {
            let (rd, r1, imm) = args.rri()?;
            only_if_no_write_to_zero(format!("{rd} <== and({r1}, {imm});"), rd)
        }
        "or" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero(format!("{rd} <== or({r1}, {r2});"), rd)
        }
        "ori" => {
            let (rd, r1, imm) = args.rri()?;
            only_if_no_write_to_zero(format!("{rd} <== or({r1}, {imm});"), rd)
        }
        "not" => {
            let (rd, rs) = args.rr()?;
            only_if_no_write_to_zero(format!("{rd} <== wrap_signed(-{rs} - 1);"), rd)
        }

        // shift
        "slli" => {
            let (rd, rs, amount) = args.rri()?;
            assert!(amount <= 31);
            only_if_no_write_to_zero_vec(
                if amount <= 16 {
                    vec![format!("{rd} <== wrap16({rs} * {});", 1 << amount)]
                } else {
                    vec![
                        format!("tmp1 <== wrap16({rs} * {});", 1 << 16),
                        format!("{rd} <== wrap16(tmp1 * {});", 1 << (amount - 16)),
                    ]
                },
                rd,
            )
        }
        "sll" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero_vec(
                vec![
                    format!("tmp1 <== and({r2}, 0x1f);"),
                    format!("{rd} <== shl({r1}, tmp1);"),
                ],
                rd,
            )
        }
        "srli" => {
            // logical shift right
            let (rd, rs, amount) = args.rri()?;
            assert!(amount <= 31);
            only_if_no_write_to_zero(format!("{rd} <== shr({rs}, {amount});"), rd)
        }
        "srl" => {
            // logical shift right
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero_vec(
                vec![
                    format!("tmp1 <== and({r2}, 0x1f);"),
                    format!("{rd} <== shr({r1}, tmp1);"),
                ],
                rd,
            )
        }
        "srai" => {
            // arithmetic shift right
            // TODO see if we can implement this directly with a machine.
            // Now we are using the equivalence
            // a >>> b = (a >= 0 ? a >> b : ~(~a >> b))
            let (rd, rs, amount) = args.rri()?;
            assert!(amount <= 31);
            only_if_no_write_to_zero_vec(
                vec![
                    format!("tmp1 <== to_signed({rs});"),
                    format!("tmp1 <== is_positive(0 - tmp1);"),
                    format!("tmp1 <=X= tmp1 * 0xffffffff;"),
                    // Here, tmp1 is the full bit mask if rs is negative
                    // and zero otherwise.
                    format!("{rd} <== xor(tmp1, {rs});"),
                    format!("{rd} <== shr({rd}, {amount});"),
                    format!("{rd} <== xor(tmp1, {rd});"),
                ],
                rd,
            )
        }

        // comparison
        "seqz" => {
            let (rd, rs) = args.rr()?;
            only_if_no_write_to_zero(format!("{rd} <=Y= is_equal_zero({rs});"), rd)
        }
        "snez" => {
            let (rd, rs) = args.rr()?;
            only_if_no_write_to_zero(format!("{rd} <=Y= is_not_equal_zero({rs});"), rd)
        }
        "slti" => {
            let (rd, rs, imm) = args.rri()?;
            only_if_no_write_to_zero_vec(
                vec![
                    format!("tmp1 <== to_signed({rs});"),
                    format!("{rd} <=Y= is_positive({} - tmp1);", imm as i32),
                ],
                rd,
            )
        }
        "slt" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero_vec(
                vec![
                    format!("tmp1 <== to_signed({r1});"),
                    format!("tmp2 <== to_signed({r2});"),
                    format!("{rd} <=Y= is_positive(tmp2 - tmp1);"),
                ],
                rd,
            )
        }
        "sltiu" => {
            let (rd, rs, imm) = args.rri()?;
            only_if_no_write_to_zero(format!("{rd} <=Y= is_positive({imm} - {rs});"), rd)
        }
        "sltu" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero(format!("{rd} <=Y= is_positive({r2} - {r1});"), rd)
        }
        "sgtz" => {
            let (rd, rs) = args.rr()?;
            only_if_no_write_to_zero_vec(
                vec![
                    format!("tmp1 <== to_signed({rs});"),
                    format!("{rd} <=Y= is_positive(tmp1);"),
                ],
                rd,
            )
        }

        // branching
        "beq" => {
            let (r1, r2, label) = args.rrl()?;
            let label = escape_label(label.as_ref());
            vec![format!("branch_if_zero {r1} - {r2}, {label};")]
        }
        "beqz" => {
            let (r1, label) = args.rl()?;
            let label = escape_label(label.as_ref());
            vec![format!("branch_if_zero {r1}, {label};")]
        }
        "bgeu" => {
            let (r1, r2, label) = args.rrl()?;
            let label = escape_label(label.as_ref());
            // TODO does this fulfill the input requirements for branch_if_positive?
            vec![format!("branch_if_positive {r1} - {r2} + 1, {label};")]
        }
        "bgez" => {
            let (r1, label) = args.rl()?;
            let label = escape_label(label.as_ref());
            vec![
                format!("tmp1 <== to_signed({r1});"),
                format!("branch_if_positive tmp1 + 1, {label};"),
            ]
        }
        "bltu" => {
            let (r1, r2, label) = args.rrl()?;
            let label = escape_label(label.as_ref());
            vec![format!("branch_if_positive {r2} - {r1}, {label};")]
        }
        "blt" => {
            let (r1, r2, label) = args.rrl()?;
            let label = escape_label(label.as_ref());
            // Branch if r1 < r2 (signed).
            // TODO does this fulfill the input requirements for branch_if_positive?
            vec![
                format!("tmp1 <== to_signed({r1});"),
                format!("tmp2 <== to_signed({r2});"),
                format!("branch_if_positive tmp2 - tmp1, {label};"),
            ]
        }
        "bge" => {
            let (r1, r2, label) = args.rrl()?;
            let label = escape_label(label.as_ref());
            // Branch if r1 >= r2 (signed).
            // TODO does this fulfill the input requirements for branch_if_positive?
            vec![
                format!("tmp1 <== to_signed({r1});"),
                format!("tmp2 <== to_signed({r2});"),
                format!("branch_if_positive tmp1 - tmp2 + 1, {label};"),
            ]
        }
        "bltz" => {
            // branch if 2**31 <= r1 < 2**32
            let (r1, label) = args.rl()?;
            let label = escape_label(label.as_ref());
            vec![format!("branch_if_positive {r1} - 2**31 + 1, {label};")]
        }
        "blez" => {
            // branch less or equal zero
            let (r1, label) = args.rl()?;
            let label = escape_label(label.as_ref());
            vec![
                format!("tmp1 <== to_signed({r1});"),
                format!("branch_if_positive -tmp1 + 1, {label};"),
            ]
        }
        "bgtz" => {
            // branch if 0 < r1 < 2**31
            let (r1, label) = args.rl()?;
            let label = escape_label(label.as_ref());
            vec![
                format!("tmp1 <== to_signed({r1});"),
                format!("branch_if_positive tmp1, {label};"),
            ]
        }
        "bne" => {
            let (r1, r2, label) = args.rrl()?;
            let label = escape_label(label.as_ref());
            vec![format!("branch_if_nonzero {r1} - {r2}, {label};")]
        }
        "bnez" => {
            let (r1, label) = args.rl()?;
            let label = escape_label(label.as_ref());
            vec![format!("branch_if_nonzero {r1}, {label};")]
        }

        // jump and call
        "j" | "tail" => {
            let label = args.l()?;
            let label = escape_label(label.as_ref());
            vec![format!("tmp1 <== jump({label});",)]
        }
        "jr" => {
            let rs = args.r()?;
            vec![format!("tmp1 <== jump_dyn({rs});")]
        }
        "jal" => {
            if let Ok(label) = args.l() {
                let label = escape_label(label.as_ref());
                vec![format!("x1 <== jump({label});")]
            } else {
                let (rd, label) = args.rl()?;
                let label = escape_label(label.as_ref());
                let statement = if rd.is_zero() {
                    format!("tmp1 <== jump({label});")
                } else {
                    format!("{rd} <== jump({label});")
                };
                vec![statement]
            }
        }
        "jalr" => vec![if let Ok(rs) = args.r() {
            format!("x1 <== jump_dyn({rs});")
        } else {
            let (rd, rs, off) = args.rro()?;
            assert_eq!(off, 0, "jalr with non-zero offset is not supported");
            if rd.is_zero() {
                format!("tmp1 <== jump_dyn({rs});")
            } else {
                format!("{rd} <== jump_dyn({rs});")
            }
        }],
        "call" => {
            let label = args.l()?;
            let label = escape_label(label.as_ref());
            vec![format!("x1 <== jump({label});")]
        }
        "ecall" => {
            args.empty()?;
            // save ra/x1
            push_register("x1")
                .into_iter()
                // jump to to handler
                .chain(std::iter::once("x1 <== jump(__ecall_handler);".to_string()))
                // restore ra/x1
                .chain(pop_register("x1"))
                .collect()
        }
        "ebreak" => {
            args.empty()?;
            // we don't use ebreak for anything, ignore
            vec![]
        }
        "ret" => {
            args.empty()?;
            vec!["tmp1 <== jump_dyn(x1);".to_string()]
        }

        // memory access
        "lw" => {
            let (rd, rs, off) = args.rro()?;
            // TODO we need to consider misaligned loads / stores
            only_if_no_write_to_zero_vec(vec![format!("{rd}, tmp1 <== mload({rs} + {off});")], rd)
        }
        "lb" => {
            // load byte and sign-extend. the memory is little-endian.
            let (rd, rs, off) = args.rro()?;
            only_if_no_write_to_zero_vec(
                vec![
                    format!("{rd}, tmp2 <== mload({rs} + {off});"),
                    format!("{rd} <== shr({rd}, 8 * tmp2);"),
                    format!("{rd} <== sign_extend_byte({rd});"),
                ],
                rd,
            )
        }
        "lbu" => {
            // load byte and zero-extend. the memory is little-endian.
            let (rd, rs, off) = args.rro()?;
            only_if_no_write_to_zero_vec(
                vec![
                    format!("{rd}, tmp2 <== mload({rs} + {off});"),
                    format!("{rd} <== shr({rd}, 8 * tmp2);"),
                    format!("{rd} <== and({rd}, 0xff);"),
                ],
                rd,
            )
        }
        "lh" => {
            // Load two bytes and sign-extend.
            // Assumes the address is a multiple of two.
            let (rd, rs, off) = args.rro()?;
            only_if_no_write_to_zero_vec(
                vec![
                    format!("{rd}, tmp2 <== mload({rs} + {off});"),
                    format!("{rd} <== shr({rd}, 8 * tmp2);"),
                    format!("{rd} <== sign_extend_16_bits({rd});"),
                ],
                rd,
            )
        }
        "lhu" => {
            // Load two bytes and zero-extend.
            // Assumes the address is a multiple of two.
            let (rd, rs, off) = args.rro()?;
            only_if_no_write_to_zero_vec(
                vec![
                    format!("{rd}, tmp2 <== mload({rs} + {off});"),
                    format!("{rd} <== shr({rd}, 8 * tmp2);"),
                    format!("{rd} <== and({rd}, 0x0000ffff);"),
                ],
                rd,
            )
        }
        "sw" => {
            let (r2, r1, off) = args.rro()?;
            vec![format!("mstore {r1} + {off}, {r2};")]
        }
        "sh" => {
            // store half word (two bytes)
            // TODO this code assumes it is at least aligned on
            // a two-byte boundary

            let (r2, r1, off) = args.rro()?;
            vec![
                format!("tmp1, tmp2 <== mload({r1} + {off});"),
                "tmp3 <== shl(0xffff, 8 * tmp2);".to_string(),
                "tmp3 <== xor(tmp3, 0xffffffff);".to_string(),
                "tmp1 <== and(tmp1, tmp3);".to_string(),
                format!("tmp3 <== and({r2}, 0xffff);"),
                "tmp3 <== shl(tmp3, 8 * tmp2);".to_string(),
                "tmp1 <== or(tmp1, tmp3);".to_string(),
                format!("mstore {r1} + {off} - tmp2, tmp1;"),
            ]
        }
        "sb" => {
            // store byte
            let (r2, r1, off) = args.rro()?;
            vec![
                format!("tmp1, tmp2 <== mload({r1} + {off});"),
                "tmp3 <== shl(0xff, 8 * tmp2);".to_string(),
                "tmp3 <== xor(tmp3, 0xffffffff);".to_string(),
                "tmp1 <== and(tmp1, tmp3);".to_string(),
                format!("tmp3 <== and({r2}, 0xff);"),
                "tmp3 <== shl(tmp3, 8 * tmp2);".to_string(),
                "tmp1 <== or(tmp1, tmp3);".to_string(),
                format!("mstore {r1} + {off} - tmp2, tmp1;"),
            ]
        }
        "fence" | "nop" => vec![],
        "unimp" => vec!["fail;".to_string()],

        // atomic instructions
        insn if insn.starts_with("amoadd.w") => {
            let (rd, rs2, rs1) = args.rrr2()?;

            [
                vec![
                    format!("tmp1, tmp2 <== mload({rs1});"),
                    format!("tmp2 <== wrap(tmp1 + {rs2});"),
                    format!("mstore {rs1}, tmp2;"),
                ],
                only_if_no_write_to_zero(format!("{rd} <=X= tmp1;"), rd),
            ]
            .concat()
        }

        insn if insn.starts_with("lr.w") => {
            // Very similar to "lw":
            let (rd, rs) = args.rr()?;
            // TODO misaligned access should raise misaligned address exceptions
            let mut statements =
                only_if_no_write_to_zero_vec(vec![format!("{rd}, tmp1 <== mload({rs});")], rd);
            statements.push("lr_sc_reservation <=X= 1;".into());
            statements
        }

        insn if insn.starts_with("sc.w") => {
            // Some overlap with "sw", but also writes 0 to rd on success
            let (rd, rs2, rs1) = args.rrr2()?;
            // TODO: misaligned access should raise misaligned address exceptions
            let mut statements = vec![
                "skip_if_zero lr_sc_reservation, 1;".into(),
                format!("mstore {rs1}, {rs2};"),
            ];
            if !rd.is_zero() {
                statements.push(format!("{rd} <=X= (1 - lr_sc_reservation);"));
            }
            statements.push("lr_sc_reservation <=X= 0;".into());
            statements
        }

        _ => {
            panic!("Unknown instruction: {instr}");
        }
    })
}
