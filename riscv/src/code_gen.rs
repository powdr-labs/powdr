use std::{fmt, vec};

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

    pub fn addr(&self) -> u8 {
        self.value
    }
}

impl powdr_asm_utils::ast::Register for Register {}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.value < 32 {
            // 0 indexed
            write!(f, "x{}", self.value)
        } else if self.value < 36 {
            // 1 indexed
            write!(f, "tmp{}", self.value - 31 + 1)
        } else if self.value == 36 {
            write!(f, "lr_sc_reservation")
        } else {
            // 0 indexed
            write!(f, "xtra{}", self.value - 37)
        }
    }
}

impl From<&str> for Register {
    fn from(s: &str) -> Self {
        if let Some(prefix) = s.strip_prefix("xtra") {
            // 0 indexed
            let value: u8 = prefix.parse().expect("Invalid register");
            Self::new(value + 37)
        } else if let Some(prefix) = s.strip_prefix('x') {
            // 0 indexed
            let value = prefix.parse().expect("Invalid register");
            assert!(value < 32, "Invalid register");
            Self::new(value)
        } else if let Some(prefix) = s.strip_prefix("tmp") {
            // 1 indexed
            let value: u8 = prefix.parse().expect("Invalid register");
            assert!(value >= 1);
            assert!(value <= 4);
            Self::new(value - 1 + 32)
        } else if s == "lr_sc_reservation" {
            Self::new(36)
        } else {
            panic!("Invalid register")
        }
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

    let degree_log = degree.ilog2();
    riscv_machine(
        runtime,
        degree,
        &preamble::<F>(runtime, degree_log.into(), with_bootloader),
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
                    data_code.push(format!(
                        "set_reg {}, 0x{v:x};",
                        Register::from("tmp2").addr()
                    ));
                    data_code.push(format!(
                        "set_reg {}, 0x{addr:x};",
                        Register::from("tmp1").addr()
                    ));
                    data_code.push(format!(
                        "mstore {}, 0, 0, {};",
                        Register::from("tmp1").addr(),
                        Register::from("tmp2").addr()
                    ));
                }
            }
            SingleDataValue::LabelReference(sym) => {
                // The label value is not known at this point, so we have to
                // load it via code, irrespectively of bootloader availability.
                //
                // TODO should be possible without temporary
                data_code.extend([
                    format!(
                        "load_label {}, {};",
                        Register::from("tmp2").addr(),
                        escape_label(&sym)
                    ),
                    format!("set_reg {}, 0x{addr:x};", Register::from("tmp1").addr()),
                    format!(
                        "mstore {}, 0, 0, {};",
                        Register::from("tmp1").addr(),
                        Register::from("tmp2").addr()
                    ),
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
        statements.push("jump __data_init, 1;".to_string());
    }
    statements.extend([
        "set_reg 0, 0;".to_string(),
        format!("jump {}, 1;", program.start_function().as_ref()),
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
            [
                "// This is the data initialization routine.".to_string(),
                "__data_init:".to_string(),
            ]
            .into_iter()
            .chain(data_code)
            .chain([
                "// This is the end of the data initialization routine.".to_string(),
                format!("jump_dyn 1, {};", Register::from("tmp1").addr()),
            ]),
        );
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

fn preamble<T: FieldElement>(runtime: &Runtime, degree: u64, with_bootloader: bool) -> String {
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
"#
        .to_string()
        // runtime extra registers
        + &runtime
            .submachines_extra_registers()
            .into_iter()
            .map(|s| format!("\t\t{s}\n"))
            .join("")
        + &bootloader_preamble_if_included
        + &memory(with_bootloader)
        + r#"
    // =============== Register memory =======================
"# + format!("std::machines::memory::Memory_{} regs;", degree + 2)
        .as_str()
        + r#"
    // Get the value in register Y.
    instr get_reg Y -> X link ~> X = regs.mload(Y, STEP);

    // Set the value in register X to the value in register Y.
    instr set_reg X, Y -> link ~> regs.mstore(X, STEP, Y);

    // We still need these registers prover inputs.
    reg query_arg_1;
    reg query_arg_2;

    // Witness columns used in instuctions for intermediate values inside instructions.
    col witness val1_col;
    col witness val2_col;
    col witness val3_col;
    col witness val4_col;

    // We need to add these inline instead of using std::utils::is_zero
    // because when XX is not constrained, witgen will try to set XX,
    // XX_inv and XXIsZero to zero, which fails this constraint.
    // Therefore, we have to activate constrained whenever XXIsZero is used.
    // XXIsZero = 1 - XX * XX_inv
    col witness XX, XX_inv, XXIsZero;
    std::utils::force_bool(XXIsZero);
    XXIsZero * XX = 0;

    // ============== control-flow instructions ==============

    // Load the value of label `l` into register X.
    instr load_label X, l: label
        link ~> regs.mstore(X, STEP, val1_col)
    {
        val1_col = l
    }

    // Jump to `l` and store the return program counter in register W.
    instr jump l: label, W
        link ~> regs.mstore(W, STEP, val3_col)
    {
        pc' = l,
        val3_col = pc + 1
    }
    
    // Jump to the address in register X and store the return program counter in register W.
    instr jump_dyn X, W
        link ~> val1_col = regs.mload(X, STEP)
        link ~> regs.mstore(W, STEP, val3_col)
    {
        pc' = val1_col,
        val3_col = pc + 1
    }

    // Jump to `l` if val(X) - val(Y) is nonzero, where X and Y are register ids.
    instr branch_if_diff_nonzero X, Y, l: label
        link ~> val1_col = regs.mload(X, STEP)
        link ~> val2_col = regs.mload(Y, STEP + 1)
    {
        XXIsZero = 1 - XX * XX_inv,
        XX = val1_col - val2_col,
        pc' = (1 - XXIsZero) * l + XXIsZero * (pc + 1)
    }

    // Jump to `l` if val(X) - (val(Y) + Z) is zero, where X and Y are register ids and Z is a
    // constant offset.
    instr branch_if_zero X, Y, Z, l: label
        link ~> val1_col = regs.mload(X, STEP)
        link ~> val2_col = regs.mload(Y, STEP + 1)
    {
        XXIsZero = 1 - XX * XX_inv,
        XX = val1_col - (val2_col + Z),
        pc' = XXIsZero * l + (1 - XXIsZero) * (pc + 1)
    }

    // Skips W instructions if val(X) - val(Y) + Z is zero, where X and Y are register ids and Z is a
    // constant offset.
    instr skip_if_zero X, Y, Z, W
        link ~> val1_col = regs.mload(X, STEP)
        link ~> val2_col = regs.mload(Y, STEP + 1)
    {
        XXIsZero = 1 - XX * XX_inv,
        XX = val1_col - val2_col + Z,
        pc' = pc + 1 + (XXIsZero * W)
    }

    // Branches to `l` if V = val(X) - val(Y) + Z is positive, where X and Y are register ids and Z is a
    // constant offset.
    // V is required to be the difference of two 32-bit unsigned values.
    // i.e. -2**32 < V < 2**32.
    instr branch_if_positive X, Y, Z, l: label
        link ~> val1_col = regs.mload(X, STEP)
        link ~> val2_col = regs.mload(Y, STEP + 1)
    {
        (val1_col - val2_col + Z) + 2**32 - 1 = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000 + wrap_bit * 2**32,
        pc' = wrap_bit * l + (1 - wrap_bit) * (pc + 1)
    }

    // Stores 1 in register W if V = val(X) - val(Y) + Z is positive, where X and Y are register ids and Z is a
    // constant offset.
    // V is required to be the difference of two 32-bit unsigend values.
    // i.e. -2**32 < V < 2**32
    instr is_positive X, Y, Z, W
        link ~> val1_col = regs.mload(X, STEP)
        link ~> val2_col = regs.mload(Y, STEP + 1)
        link ~> regs.mstore(W, STEP + 2, wrap_bit)
    {
        (val1_col - val2_col + Z) + 2**32 - 1 = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000 + wrap_bit * 2**32
    }

    // Stores val(X) * Z + W in register Y.
    instr move_reg X, Y, Z, W
        link ~> val1_col = regs.mload(X, STEP)
        link ~> regs.mstore(Y, STEP + 1, val3_col)
    {
        val3_col = val1_col * Z + W
    }

    // ================= wrapping instructions =================

    // Computes V = val(X) + val(Y) + Z, wraps it in 32 bits, and stores the result in register W.
    // Requires 0 <= V < 2**33.
    instr add_wrap X, Y, Z, W
        link ~> val1_col = regs.mload(X, STEP)
        link ~> val2_col = regs.mload(Y, STEP + 1)
        link ~> regs.mstore(W, STEP + 2, val3_col)
    {
        val1_col + val2_col + Z = val3_col + wrap_bit * 2**32,
        val3_col = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000
    }

    // Computes V = val(X) - val(Y) + Z, wraps it in 32 bits, and stores the result in register W.
    // Requires -2**32 <= V < 2**32.
    instr sub_wrap_with_offset X, Y, Z, W
        link ~> val1_col = regs.mload(X, STEP)
        link ~> val2_col = regs.mload(Y, STEP + 1)
        link ~> regs.mstore(W, STEP + 2, val3_col)
    {
        (val1_col - val2_col + Z) + 2**32 = val3_col + wrap_bit * 2**32,
        val3_col = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000
    }

    // ================= logical instructions =================

    // Stores 1 in register W if the value in register X is zero,
    // otherwise stores 0.
    instr is_equal_zero X, W
        link ~> val1_col = regs.mload(X, STEP)
        link ~> regs.mstore(W, STEP + 2, XXIsZero)
    {
        XXIsZero = 1 - XX * XX_inv,
        XX = val1_col
    }

    // Stores 1 in register W if val(X) == val(Y), otherwise stores 0.
    instr is_not_equal X, Y, W
        link ~> val1_col = regs.mload(X, STEP)
        link ~> val2_col = regs.mload(Y, STEP + 1)
        link ~> regs.mstore(W, STEP + 2, val3_col)
    {
        XXIsZero = 1 - XX * XX_inv,
        XX = val1_col - val2_col,
        val3_col = 1 - XXIsZero
    }

    // ================= submachine instructions =================
"# + &runtime
        .submachines_instructions()
        .into_iter()
        .map(|s| format!("    {s}"))
        .join("\n")
        + r#"
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

    // Sign extends the value in register X and stores it in register Y.
    // Input is a 32 bit unsigned number. We check bit 7 and set all higher bits to that value.
    instr sign_extend_byte X, Y
        link ~> val1_col = regs.mload(X, STEP)
        link ~> regs.mstore(Y, STEP + 3, val3_col)
    {
        // wrap_bit is used as sign_bit here.
        val1_col = Y_7bit + wrap_bit * 0x80 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000,
        val3_col = Y_7bit + wrap_bit * 0xffffff80
    }
    col witness Y_7bit;
    link => bit7.check(Y_7bit);

    // Sign extends the value in register X and stores it in register Y.
    // Input is a 32 bit unsigned number. We check bit 15 and set all higher bits to that value.
    instr sign_extend_16_bits X, Y
        link ~> val1_col = regs.mload(X, STEP)
        link ~> regs.mstore(Y, STEP + 3, val3_col)
    {
        Y_15bit = X_b1 + Y_7bit * 0x100,

        // wrap_bit is used as sign_bit here.
        val1_col = Y_15bit + wrap_bit * 0x8000 + X_b3 * 0x10000 + X_b4 * 0x1000000,
        val3_col = Y_15bit + wrap_bit * 0xffff8000
    }
    col witness Y_15bit;

    // Converts the value in register X to a signed number and stores it in register Y.
    // Input is a 32 bit unsigned number (0 <= val(X) < 2**32) interpreted as a two's complement numbers.
    // Returns a signed number (-2**31 <= val(Y) < 2**31).
    instr to_signed X, Y
        link ~> val1_col = regs.mload(X, STEP)
        link ~> regs.mstore(Y, STEP + 1, val3_col)
    {
        // wrap_bit is used as sign_bit here.
        val1_col = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + Y_7bit * 0x1000000 + wrap_bit * 0x80000000,
        val3_col = val1_col - wrap_bit * 0x100000000
    }

    // ======================= assertions =========================

    instr fail { 1 = 0 }

    // Wraps V = val(X) * Y and stores it in register Z,
    // where X is a register and Y is a constant factor.
    // Removes up to 16 bits beyond 32
    // TODO is this really safe?
    instr wrap16 X, Y, Z
        link ~> val1_col = regs.mload(X, STEP)
        link ~> regs.mstore(Z, STEP + 3, val3_col)
    {
        (val1_col * Y) = Y_b5 * 2**32 + Y_b6 * 2**40 + val3_col,
        val3_col = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000
    }

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

    // Computes Q = val(Y) / val(X) and R = val(Y) % val(X) and stores them in registers Z and W.
    instr divremu Y, X, Z, W
        link ~> val1_col = regs.mload(Y, STEP)
        link ~> val2_col = regs.mload(X, STEP + 1)
        link ~> regs.mstore(Z, STEP + 2, val3_col)
        link ~> regs.mstore(W, STEP + 3, val4_col)
    {
        XXIsZero = 1 - XX * XX_inv,
        XX = val2_col,

        // if X is zero, remainder is set to dividend, as per RISC-V specification:
        val2_col * val3_col + val4_col = val1_col,

        // remainder >= 0:
        val4_col = REM_b1 + REM_b2 * 0x100 + REM_b3 * 0x10000 + REM_b4 * 0x1000000,

        // remainder < divisor, conditioned to val(X) not being 0:
        (1 - XXIsZero) * (val2_col - val4_col - 1 - Y_b5 - Y_b6 * 0x100 - Y_b7 * 0x10000 - Y_b8 * 0x1000000) = 0,

        // in case X is zero, we set quotient according to RISC-V specification
        XXIsZero * (val3_col - 0xffffffff) = 0,

        // quotient is 32 bits:
        val3_col = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000
    }
"# + mul_instruction
}

fn mul_instruction<T: FieldElement>(runtime: &Runtime) -> &'static str {
    match T::known_field().expect("Unknown field!") {
        KnownField::Bn254Field => {
            // The BN254 field can fit any 64-bit number, so we can naively de-compose
            // Z * W into 8 bytes and put them together to get the upper and lower word.
            r#"
    // Computes V = val(X) * val(Y) and
    // stores the lower 32 bits in register Z and the upper 32 bits in register W.
    instr mul X, Y, Z, W
        link ~> val1_col = regs.mload(X, STEP)
        link ~> val2_col = regs.mload(Y, STEP + 1)
        link ~> regs.mstore(Z, STEP + 2, val3_col)
        link ~> regs.mstore(W, STEP + 3, val4_col);
    {
        val1_col * val2_col = val3_col + val4_col * 2**32,
        val3_col = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000,
        val4_col = Y_b5 + Y_b6 * 0x100 + Y_b7 * 0x10000 + Y_b8 * 0x1000000
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
    // Computes V = val(X) * val(Y) and
    // stores the lower 32 bits in register Z and the upper 32 bits in register W.
    instr mul X, Y, Z, W
        link ~> val1_col = regs.mload(X, STEP)
        link ~> val2_col = regs.mload(Y, STEP + 1)
        link ~> (val3_col, val4_col) = split_gl.split(val1_col * val2_col)
        link ~> regs.mstore(Z, STEP + 2, val3_col)
        link ~> regs.mstore(W, STEP + 3, val4_col);
"#
        }
    }
}

fn memory(with_bootloader: bool) -> String {
    let memory_machine = if with_bootloader {
        r#"
    std::machines::memory_with_bootloader_write::MemoryWithBootloaderWrite memory;

    // Stores val(W) at address (V = val(X) - val(Z) + Y) % 2**32.
    // V can be between 0 and 2**33.
    instr mstore_bootloader X, Z, Y, W
        link ~> val1_col = regs.mload(X, STEP)
        link ~> val2_col = regs.mload(Z, STEP + 1)
        link ~> val3_col = regs.mload(W, STEP + 2)
        link ~> memory.mstore_bootloader(X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000, STEP + 3, val3_col)
    {
        val1_col - val2_col + Y = (X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000) + wrap_bit * 2**32
    }
"#
    } else {
        r#"
    std::machines::memory::Memory memory;
"#
    };

    memory_machine.to_string()
        + r#"

    // Increased by 4 in each step, because we do up to 4 register memory accesses per step
    col fixed STEP(i) { 4 * i };

    // ============== memory instructions ==============

    /// Loads one word from an address Y, where Y can be between 0 and 2**33 (sic!),
    /// wraps the address to 32 bits and rounds it down to the next multiple of 4.
    /// Writes the loaded word and the remainder of the division by 4 to registers Z and W,
    /// respectively.
    instr mload X, Y, Z, W
        link ~> val1_col = regs.mload(X, STEP)
        link ~> val3_col = memory.mload(X_b4 * 0x1000000 + X_b3 * 0x10000 + X_b2 * 0x100 + X_b1 * 4, STEP + 1)
        link ~> regs.mstore(Z, STEP + 2, val3_col)
        link ~> regs.mstore(W, STEP + 3, val4_col)
        link => bit2.check(val4_col)
        link => bit6.check(X_b1)
    {
        val1_col + Y = wrap_bit * 2**32 + X_b4 * 0x1000000 + X_b3 * 0x10000 + X_b2 * 0x100 + X_b1 * 4 + val4_col
    }

    // Stores val(W) at address (V = val(X) - val(Y) + Z) % 2**32.
    // V can be between 0 and 2**33.
    // V should be a multiple of 4, but this instruction does not enforce it.
    instr mstore X, Y, Z, W
        link ~> val1_col = regs.mload(X, STEP)
        link ~> val2_col = regs.mload(Y, STEP + 1)
        link ~> val3_col = regs.mload(W, STEP + 2)
        link ~> memory.mstore(X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000, STEP + 3, val3_col)
    {
        val1_col - val2_col + Z = (X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000) + wrap_bit * 2**32
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

fn only_if_no_write_to_zero(reg: Register, statement: String) -> Vec<String> {
    only_if_no_write_to_zero_vec(reg, vec![statement])
}

fn only_if_no_write_to_zero_vec(reg: Register, statements: Vec<String>) -> Vec<String> {
    if reg.is_zero() {
        vec![]
    } else {
        statements
    }
}

/// Push register into the stack

pub fn push_register(name: &str) -> Vec<String> {
    assert!(name.starts_with('x'), "Only x registers are supported");
    let reg = Register::from(name);
    vec![
        // x2 + x0 - 4 => x2
        format!("add_wrap 2, 0, -4, 2;",),
        format!("mstore 2, 0, 0, {};", reg.addr()),
    ]
}

/// Pop register from the stack
pub fn pop_register(name: &str) -> Vec<String> {
    assert!(name.starts_with('x'), "Only x registers are supported");
    let reg = Register::from(name);
    vec![
        format!(
            "mload 2, 0, {}, {};",
            reg.addr(),
            Register::from("tmp1").addr()
        ),
        "add_wrap 2, 0, 4, 2;".to_string(),
    ]
}

fn process_instruction<A: InstructionArgs>(instr: &str, args: A) -> Result<Vec<String>, A::Error> {
    let tmp1 = Register::from("tmp1");
    let tmp2 = Register::from("tmp2");
    let tmp3 = Register::from("tmp3");
    let tmp4 = Register::from("tmp4");
    let lr_sc_reservation = Register::from("lr_sc_reservation");
    let statements = match instr {
        // load/store registers
        "li" | "la" => {
            // The difference between "li" and "la" in RISC-V is that the former
            // is for loading values as is, and the later is for loading PC
            // relative values. But since we work on a higher abstraction level,
            // for us they are the same thing.
            if let Ok((rd, label)) = args.rl() {
                let label = escape_label(label.as_ref());
                only_if_no_write_to_zero(rd, format!("load_label {}, {label};", rd.addr()))
            } else {
                let (rd, imm) = args.ri()?;
                only_if_no_write_to_zero(rd, format!("set_reg {}, {imm};", rd.addr()))
            }
        }
        // TODO check if it is OK to clear the lower order bits
        "lui" => {
            let (rd, imm) = args.ri()?;
            only_if_no_write_to_zero(rd, format!("set_reg {}, {};", rd.addr(), imm << 12))
        }
        "mv" => {
            let (rd, rs) = args.rr()?;
            only_if_no_write_to_zero(rd, format!("move_reg {}, {}, 1, 0;", rs.addr(), rd.addr()))
        }

        // Arithmetic
        "add" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero(
                rd,
                format!(
                    "add_wrap {}, {}, {}, {};",
                    r1.addr(),
                    r2.addr(),
                    0,
                    rd.addr()
                ),
            )
        }
        "addi" => {
            let (rd, rs, imm) = args.rri()?;
            only_if_no_write_to_zero(
                rd,
                format!("add_wrap {}, 0, {imm}, {};", rs.addr(), rd.addr()),
            )
        }
        "sub" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero(
                rd,
                format!(
                    "sub_wrap_with_offset {}, {}, 0, {};",
                    r1.addr(),
                    r2.addr(),
                    rd.addr()
                ),
            )
        }
        "neg" => {
            let (rd, r1) = args.rr()?;
            only_if_no_write_to_zero(
                rd,
                format!("sub_wrap_with_offset 0, {}, 0, {};", r1.addr(), rd.addr()),
            )
        }
        "mul" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero(
                rd,
                format!(
                    "mul {}, {}, {}, {};",
                    r1.addr(),
                    r2.addr(),
                    rd.addr(),
                    tmp1.addr()
                ),
            )
        }
        "mulhu" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero(
                rd,
                format!(
                    "mul {}, {}, {}, {};",
                    r1.addr(),
                    r2.addr(),
                    tmp1.addr(),
                    rd.addr(),
                ),
            )
        }
        "mulh" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero_vec(
                rd,
                vec![
                    format!("to_signed {}, {};", r1.addr(), tmp1.addr()),
                    format!("to_signed {}, {};", r2.addr(), tmp2.addr()),
                    // tmp3 is 1 if tmp1 is non-negative
                    format!("is_positive {}, 0, 1, {};", tmp1.addr(), tmp3.addr()),
                    // tmp4 is 1 if tmp2 is non-negative
                    format!("is_positive {}, 0, 1, {};", tmp2.addr(), tmp4.addr()),
                    // If tmp1 is negative, convert to positive
                    format!("skip_if_zero 0, {}, 1, 1;", tmp3.addr()),
                    format!("move_reg {}, {}, -1, 0;", tmp1.addr(), tmp1.addr()),
                    // If tmp2 is negative, convert to positive
                    format!("skip_if_zero 0, {}, 1, 1;", tmp4.addr()),
                    format!("move_reg {}, {}, -1, 0;", tmp2.addr(), tmp2.addr()),
                    format!(
                        "mul {}, {}, {}, {};",
                        tmp1.addr(),
                        tmp2.addr(),
                        tmp1.addr(),
                        rd.addr()
                    ),
                    // Determine the sign of the result based on the signs of tmp1 and tmp2
                    format!(
                        "is_not_equal {}, {}, {};",
                        tmp3.addr(),
                        tmp4.addr(),
                        tmp3.addr()
                    ),
                    // If the result should be negative, convert back to negative
                    format!("skip_if_zero {}, 0, 0, 2;", tmp3.addr()),
                    format!("is_equal_zero {}, {};", tmp1.addr(), tmp1.addr()),
                    format!(
                        "sub_wrap_with_offset {}, {}, -1, {};",
                        tmp1.addr(),
                        rd.addr(),
                        rd.addr()
                    ),
                ],
            )
        }
        "mulhsu" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero_vec(
                rd,
                vec![
                    format!("to_signed {}, {};", r1.addr(), tmp1.addr()),
                    // tmp2 is 1 if tmp1 is non-negative
                    format!("is_positive {}, 0, 1, {};", tmp1.addr(), tmp2.addr()),
                    // If negative, convert to positive
                    format!("skip_if_zero 0, {}, 1, 1;", tmp2.addr()),
                    format!("move_reg {}, {}, -1, 0;", tmp1.addr(), tmp1.addr()),
                    format!(
                        "mul {}, {}, {}, {};",
                        tmp1.addr(),
                        r2.addr(),
                        tmp1.addr(),
                        rd.addr()
                    ),
                    // If was negative before, convert back to negative
                    format!("skip_if_zero 0, {}, 1, 2;", tmp2.addr()),
                    format!("is_equal_zero {}, {};", tmp1.addr(), tmp1.addr()),
                    // If the lower bits are zero, return the two's complement,
                    // otherwise return one's complement.
                    format!(
                        "sub_wrap_with_offset {}, {}, -1, {};",
                        tmp1.addr(),
                        rd.addr(),
                        rd.addr()
                    ),
                ],
            )
        }
        "divu" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero(
                rd,
                format!(
                    "divremu {}, {}, {}, {};",
                    r1.addr(),
                    r2.addr(),
                    rd.addr(),
                    tmp1.addr()
                ),
            )
        }
        "remu" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero(
                rd,
                format!(
                    "divremu {}, {}, {}, {};",
                    r1.addr(),
                    r2.addr(),
                    tmp1.addr(),
                    rd.addr()
                ),
            )
        }

        // bitwise
        "xor" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero(
                rd,
                format!("xor {}, {}, 0, {};", r1.addr(), r2.addr(), rd.addr()),
            )
        }
        "xori" => {
            let (rd, r1, imm) = args.rri()?;
            only_if_no_write_to_zero(rd, format!("xor {}, 0, {imm}, {};", r1.addr(), rd.addr()))
        }
        "and" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero(
                rd,
                format!("and {}, {}, 0, {};", r1.addr(), r2.addr(), rd.addr()),
            )
        }
        "andi" => {
            let (rd, r1, imm) = args.rri()?;
            only_if_no_write_to_zero(rd, format!("and {}, 0, {imm}, {};", r1.addr(), rd.addr()))
        }
        "or" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero(
                rd,
                format!("or {}, {}, 0, {};", r1.addr(), r2.addr(), rd.addr()),
            )
        }
        "ori" => {
            let (rd, r1, imm) = args.rri()?;
            only_if_no_write_to_zero(rd, format!("or {}, 0, {imm}, {};", r1.addr(), rd.addr()))
        }
        "not" => {
            let (rd, rs) = args.rr()?;
            only_if_no_write_to_zero(
                rd,
                format!("sub_wrap_with_offset 0, {}, -1, {};", rs.addr(), rd.addr()),
            )
        }

        // shift
        "slli" => {
            let (rd, rs, amount) = args.rri()?;
            assert!(amount <= 31);
            only_if_no_write_to_zero_vec(
                rd,
                if amount <= 16 {
                    vec![format!(
                        "wrap16 {}, {}, {};",
                        rs.addr(),
                        1 << amount,
                        rd.addr()
                    )]
                } else {
                    vec![
                        format!("wrap16 {}, {}, {};", rs.addr(), 1 << 16, tmp1.addr()),
                        format!(
                            "wrap16 {}, {}, {};",
                            tmp1.addr(),
                            1 << (amount - 16),
                            rd.addr()
                        ),
                    ]
                },
            )
        }
        "sll" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero_vec(
                rd,
                vec![
                    format!("and {}, 0, 0x1f, {};", r2.addr(), tmp1.addr()),
                    format!("shl {}, {}, 0, {};", r1.addr(), tmp1.addr(), rd.addr()),
                ],
            )
        }
        "srli" => {
            // logical shift right
            let (rd, rs, amount) = args.rri()?;
            assert!(amount <= 31);
            only_if_no_write_to_zero(
                rd,
                format!("shr {}, 0, {amount}, {};", rs.addr(), rd.addr()),
            )
        }
        "srl" => {
            // logical shift right
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero_vec(
                rd,
                vec![
                    format!("and {}, 0, 0x1f, {};", r2.addr(), tmp1.addr()),
                    format!("shr {}, {}, 0, {};", r1.addr(), tmp1.addr(), rd.addr()),
                ],
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
                rd,
                vec![
                    format!("to_signed {}, {};", rs.addr(), tmp1.addr()),
                    format!("is_positive 0, {}, 0, {};", tmp1.addr(), tmp1.addr()),
                    format!("move_reg {}, {}, 0xffffffff, 0;", tmp1.addr(), tmp1.addr()),
                    // Here, tmp1 is the full bit mask if rs is negative
                    // and zero otherwise.
                    format!("xor {}, {}, 0, {};", tmp1.addr(), rs.addr(), rd.addr()),
                    format!("shr {}, 0, {amount}, {};", rd.addr(), rd.addr()),
                    format!("xor {}, {}, 0, {};", tmp1.addr(), rd.addr(), rd.addr()),
                ],
            )
        }

        // comparison
        "seqz" => {
            let (rd, rs) = args.rr()?;
            only_if_no_write_to_zero(rd, format!("is_equal_zero {}, {};", rs.addr(), rd.addr()))
        }
        "snez" => {
            let (rd, rs) = args.rr()?;
            only_if_no_write_to_zero(rd, format!("is_not_equal {}, 0, {};", rs.addr(), rd.addr()))
        }
        "slti" => {
            let (rd, rs, imm) = args.rri()?;
            only_if_no_write_to_zero_vec(
                rd,
                vec![
                    format!("to_signed {}, {};", rs.addr(), tmp1.addr()),
                    format!(
                        "is_positive 0, {}, {}, {};",
                        tmp1.addr(),
                        imm as i32,
                        rd.addr()
                    ),
                ],
            )
        }
        "slt" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero_vec(
                rd,
                vec![
                    format!("to_signed {}, {};", r1.addr(), tmp1.addr()),
                    format!("to_signed {}, {};", r2.addr(), tmp2.addr()),
                    format!(
                        "is_positive {}, {}, 0, {};",
                        tmp2.addr(),
                        tmp1.addr(),
                        rd.addr()
                    ),
                ],
            )
        }
        "sltiu" => {
            let (rd, rs, imm) = args.rri()?;
            only_if_no_write_to_zero(
                rd,
                format!("is_positive 0, {}, {imm}, {};", rs.addr(), rd.addr()),
            )
        }
        "sltu" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero(
                rd,
                format!(
                    "is_positive {}, {}, 0, {};",
                    r2.addr(),
                    r1.addr(),
                    rd.addr()
                ),
            )
        }
        "sgtz" => {
            let (rd, rs) = args.rr()?;
            only_if_no_write_to_zero_vec(
                rd,
                vec![
                    format!("to_signed {}, {};", rs.addr(), tmp1.addr()),
                    format!("is_positive {}, 0, 0, {};", tmp1.addr(), rd.addr()),
                ],
            )
        }

        // branching
        "beq" => {
            let (r1, r2, label) = args.rrl()?;
            let label = escape_label(label.as_ref());
            vec![format!(
                "branch_if_zero {}, {}, 0, {label};",
                r1.addr(),
                r2.addr()
            )]
        }
        "beqz" => {
            let (r1, label) = args.rl()?;
            let label = escape_label(label.as_ref());
            vec![format!("branch_if_zero {}, 0, 0, {label};", r1.addr())]
        }
        "bgeu" => {
            let (r1, r2, label) = args.rrl()?;
            let label = escape_label(label.as_ref());
            // TODO does this fulfill the input requirements for branch_if_positive?
            vec![format!(
                "branch_if_positive {}, {}, 1, {label};",
                r1.addr(),
                r2.addr()
            )]
        }
        "bgez" => {
            let (r1, label) = args.rl()?;
            let label = escape_label(label.as_ref());
            vec![
                format!("to_signed {}, {};", r1.addr(), tmp1.addr()),
                format!("branch_if_positive {}, 0, 1, {label};", tmp1.addr()),
            ]
        }
        "bltu" => {
            let (r1, r2, label) = args.rrl()?;
            let label = escape_label(label.as_ref());
            vec![format!(
                "branch_if_positive {}, {}, 0, {label};",
                r2.addr(),
                r1.addr()
            )]
        }
        "blt" => {
            let (r1, r2, label) = args.rrl()?;
            let label = escape_label(label.as_ref());
            // Branch if r1 < r2 (signed).
            // TODO does this fulfill the input requirements for branch_if_positive?
            vec![
                format!("to_signed {}, {};", r1.addr(), tmp1.addr()),
                format!("to_signed {}, {};", r2.addr(), tmp2.addr()),
                format!(
                    "branch_if_positive {}, {}, 0, {label};",
                    tmp2.addr(),
                    tmp1.addr()
                ),
            ]
        }
        "bge" => {
            let (r1, r2, label) = args.rrl()?;
            let label = escape_label(label.as_ref());
            // Branch if r1 >= r2 (signed).
            // TODO does this fulfill the input requirements for branch_if_positive?
            vec![
                format!("to_signed {}, {};", r1.addr(), tmp1.addr()),
                format!("to_signed {}, {};", r2.addr(), tmp2.addr()),
                format!(
                    "branch_if_positive {}, {}, 1, {label};",
                    tmp1.addr(),
                    tmp2.addr()
                ),
            ]
        }
        "bltz" => {
            // branch if 2**31 <= r1 < 2**32
            let (r1, label) = args.rl()?;
            let label = escape_label(label.as_ref());
            vec![format!(
                "branch_if_positive {}, 0, -(2**31) + 1, {label};",
                r1.addr()
            )]
        }
        "blez" => {
            // branch less or equal zero
            let (r1, label) = args.rl()?;
            let label = escape_label(label.as_ref());
            vec![
                format!("to_signed {}, {};", r1.addr(), tmp1.addr()),
                format!("branch_if_positive 0, {}, 1, {label};", tmp1.addr()),
            ]
        }
        "bgtz" => {
            // branch if 0 < r1 < 2**31
            let (r1, label) = args.rl()?;
            let label = escape_label(label.as_ref());
            vec![
                format!("to_signed {}, {};", r1.addr(), tmp1.addr()),
                format!("branch_if_positive {}, 0, 0, {label};", tmp1.addr()),
            ]
        }
        "bne" => {
            let (r1, r2, label) = args.rrl()?;
            let label = escape_label(label.as_ref());
            vec![format!(
                "branch_if_diff_nonzero {}, {}, {label};",
                r1.addr(),
                r2.addr()
            )]
        }
        "bnez" => {
            let (r1, label) = args.rl()?;
            let label = escape_label(label.as_ref());
            vec![format!("branch_if_diff_nonzero {}, 0, {label};", r1.addr())]
        }

        // jump and call
        "j" | "tail" => {
            let label = args.l()?;
            let label = escape_label(label.as_ref());
            vec![format!("jump {label}, {};", tmp1.addr(),)]
        }
        "jr" => {
            let rs = args.r()?;
            vec![format!("jump_dyn {}, {};", rs.addr(), tmp1.addr())]
        }
        "jal" => {
            if let Ok(label) = args.l() {
                let label = escape_label(label.as_ref());
                vec![format!("jump {label}, 1;")]
            } else {
                let (rd, label) = args.rl()?;
                let label = escape_label(label.as_ref());
                if rd.is_zero() {
                    vec![format!("jump {label}, {};", tmp1.addr())]
                } else {
                    vec![format!("jump {label}, {};", rd.addr())]
                }
            }
        }
        "jalr" => {
            if let Ok(rs) = args.r() {
                vec![format!("jump_dyn {}, 1;", rs.addr())]
            } else {
                let (rd, rs, off) = args.rro()?;
                assert_eq!(off, 0, "jalr with non-zero offset is not supported");
                if rd.is_zero() {
                    vec![format!("jump_dyn {}, {};", rs.addr(), tmp1.addr())]
                } else {
                    vec![format!("jump_dyn {}, {};", rs.addr(), rd.addr())]
                }
            }
        }
        "call" => {
            let label = args.l()?;
            let label = escape_label(label.as_ref());
            vec![format!("jump {label}, 1;")]
        }
        "ecall" => {
            args.empty()?;
            // save ra/x1
            push_register("x1")
                .into_iter()
                // jump to to handler
                .chain(["jump __ecall_handler, 1;".to_string()])
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
            vec![format!("jump_dyn 1, {};", tmp1.addr())]
        }

        // memory access
        "lw" => {
            let (rd, rs, off) = args.rro()?;
            // TODO we need to consider misaligned loads / stores
            only_if_no_write_to_zero(
                rd,
                format!(
                    "mload {}, {off}, {}, {};",
                    rs.addr(),
                    rd.addr(),
                    tmp1.addr()
                ),
            )
        }
        "lb" => {
            // load byte and sign-extend. the memory is little-endian.
            let (rd, rs, off) = args.rro()?;
            only_if_no_write_to_zero_vec(
                rd,
                vec![
                    format!(
                        "mload {}, {off}, {}, {};",
                        rs.addr(),
                        tmp1.addr(),
                        tmp2.addr()
                    ),
                    format!("move_reg {}, {}, 8, 0;", tmp2.addr(), tmp2.addr()),
                    format!("shr {}, {}, 0, {};", tmp1.addr(), tmp2.addr(), tmp1.addr()),
                    format!("sign_extend_byte {}, {};", tmp1.addr(), rd.addr()),
                ],
            )
        }
        "lbu" => {
            // load byte and zero-extend. the memory is little-endian.
            let (rd, rs, off) = args.rro()?;
            only_if_no_write_to_zero_vec(
                rd,
                vec![
                    format!(
                        "mload {}, {off}, {}, {};",
                        rs.addr(),
                        tmp1.addr(),
                        tmp2.addr()
                    ),
                    format!("move_reg {}, {}, 8, 0;", tmp2.addr(), tmp2.addr()),
                    format!("shr {}, {}, 0, {};", tmp1.addr(), tmp2.addr(), tmp1.addr()),
                    format!("and {}, 0, 0xff, {};", tmp1.addr(), rd.addr()),
                ],
            )
        }
        "lh" => {
            // Load two bytes and sign-extend.
            // Assumes the address is a multiple of two.
            let (rd, rs, off) = args.rro()?;
            only_if_no_write_to_zero_vec(
                rd,
                vec![
                    format!(
                        "mload {}, {off}, {}, {};",
                        rs.addr(),
                        tmp1.addr(),
                        tmp2.addr()
                    ),
                    format!("move_reg {}, {}, 8, 0;", tmp2.addr(), tmp2.addr()),
                    format!("shr {}, {}, 0, {};", tmp1.addr(), tmp2.addr(), tmp1.addr()),
                    format!("sign_extend_16_bits {}, {};", tmp1.addr(), rd.addr()),
                ],
            )
        }
        "lhu" => {
            // Load two bytes and zero-extend.
            // Assumes the address is a multiple of two.
            let (rd, rs, off) = args.rro()?;
            only_if_no_write_to_zero_vec(
                rd,
                vec![
                    format!(
                        "mload {}, {off}, {}, {};",
                        rs.addr(),
                        tmp1.addr(),
                        tmp2.addr()
                    ),
                    format!("move_reg {}, {}, 8, 0;", tmp2.addr(), tmp2.addr()),
                    format!("shr {}, {}, 0, {};", tmp1.addr(), tmp2.addr(), tmp1.addr()),
                    format!("and {}, 0, 0x0000ffff, {};", tmp1.addr(), rd.addr()),
                ],
            )
        }
        "sw" => {
            let (r2, r1, off) = args.rro()?;
            vec![format!("mstore {}, 0, {off}, {};", r1.addr(), r2.addr())]
        }
        "sh" => {
            // store half word (two bytes)
            // TODO this code assumes it is at least aligned on
            // a two-byte boundary

            let (r2, r1, off) = args.rro()?;
            vec![
                format!(
                    "mload {}, {off}, {}, {};",
                    r1.addr(),
                    tmp1.addr(),
                    tmp2.addr()
                ),
                format!("set_reg {}, 0xffff;", tmp3.addr()),
                format!("move_reg {}, {}, 8, 0;", tmp2.addr(), tmp4.addr()),
                format!("shl {}, {}, 0, {};", tmp3.addr(), tmp4.addr(), tmp3.addr()),
                format!("xor {}, 0, 0xffffffff, {};", tmp3.addr(), tmp3.addr()),
                format!("and {}, {}, 0, {};", tmp1.addr(), tmp3.addr(), tmp1.addr()),
                format!("and {}, 0, 0xffff, {};", r2.addr(), tmp3.addr()),
                format!("shl {}, {}, 0, {};", tmp3.addr(), tmp4.addr(), tmp3.addr()),
                format!("or {}, {}, 0, {};", tmp1.addr(), tmp3.addr(), tmp1.addr()),
                format!(
                    "mstore {}, {}, {off}, {};",
                    r1.addr(),
                    tmp2.addr(),
                    tmp1.addr()
                ),
            ]
        }
        "sb" => {
            // store byte
            let (r2, r1, off) = args.rro()?;
            vec![
                format!(
                    "mload {}, {off}, {}, {};",
                    r1.addr(),
                    tmp1.addr(),
                    tmp2.addr()
                ),
                format!("set_reg {}, 0xff;", tmp3.addr()),
                format!("move_reg {}, {}, 8, 0;", tmp2.addr(), tmp4.addr()),
                format!("shl {}, {}, 0, {};", tmp3.addr(), tmp4.addr(), tmp3.addr()),
                format!("xor {}, 0, 0xffffffff, {};", tmp3.addr(), tmp3.addr()),
                format!("and {}, {}, 0, {};", tmp1.addr(), tmp3.addr(), tmp1.addr()),
                format!("and {}, 0, 0xff, {};", r2.addr(), tmp3.addr()),
                format!("shl {}, {}, 0, {};", tmp3.addr(), tmp4.addr(), tmp3.addr()),
                format!("or {}, {}, 0, {};", tmp1.addr(), tmp3.addr(), tmp1.addr()),
                format!(
                    "mstore {}, {}, {off}, {};",
                    r1.addr(),
                    tmp2.addr(),
                    tmp1.addr()
                ),
            ]
        }
        "fence" | "nop" => vec![],
        "unimp" => vec!["fail;".to_string()],

        // atomic instructions
        insn if insn.starts_with("amoadd.w") => {
            let (rd, rs2, rs1) = args.rrr2()?;

            [
                vec![
                    format!("mload {}, 0, {}, {};", rs1.addr(), tmp1.addr(), tmp2.addr()),
                    format!(
                        "add_wrap {}, {}, 0, {};",
                        tmp1.addr(),
                        rs2.addr(),
                        tmp2.addr()
                    ),
                    format!("mstore {}, 0, 0, {};", rs1.addr(), tmp2.addr()),
                ],
                only_if_no_write_to_zero(
                    rd,
                    format!("move_reg {}, {}, 1, 0;", tmp1.addr(), rd.addr()),
                ),
            ]
            .concat()
        }

        insn if insn.starts_with("lr.w") => {
            // Very similar to "lw":
            let (rd, rs) = args.rr()?;
            // TODO misaligned access should raise misaligned address exceptions
            [
                only_if_no_write_to_zero_vec(
                    rd,
                    vec![format!(
                        "mload {}, 0, {}, {};",
                        rs.addr(),
                        rd.addr(),
                        tmp1.addr()
                    )],
                ),
                vec![format!("set_reg {}, 1;", lr_sc_reservation.addr())],
            ]
            .concat()
        }

        insn if insn.starts_with("sc.w") => {
            // Some overlap with "sw", but also writes 0 to rd on success
            let (rd, rs2, rs1) = args.rrr2()?;
            // TODO: misaligned access should raise misaligned address exceptions
            [
                format!("skip_if_zero {}, 0, 0, 1;", lr_sc_reservation.addr()),
                format!("mstore {}, 0, 0, {};", rs1.addr(), rs2.addr()),
            ]
            .into_iter()
            .chain(only_if_no_write_to_zero_vec(
                rd,
                vec![format!(
                    "move_reg {}, {}, -1, 1;",
                    lr_sc_reservation.addr(),
                    rd.addr()
                )],
            ))
            .chain([format!("set_reg {}, 0;", lr_sc_reservation.addr())])
            .collect()
        }

        _ => {
            panic!("Unknown instruction: {instr}");
        }
    };
    for s in &statements {
        log::debug!("          {s}");
    }
    Ok(statements)
}
