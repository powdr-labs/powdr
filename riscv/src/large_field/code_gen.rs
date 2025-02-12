use std::vec;

use itertools::Itertools;
use powdr_isa_utils::SingleDataValue;
use powdr_isa_utils::{escape_label, quote};
use powdr_number::KnownField;

use crate::continuations::bootloader::{bootloader_and_shutdown_routine, bootloader_preamble};

use crate::code_gen::{
    InstructionArgs, MemEntry, Register, RiscVProgram, SourceFileInfo, Statement,
};
use crate::CompilerOptions;

use crate::large_field::runtime::Runtime;

/// Translates a RISC-V program to POWDR ASM
/// with constraints that work for a field >= the Goldilocks modulus.
///
/// Will call each of the methods in the `RiscVProgram` just once.
pub fn translate_program(program: impl RiscVProgram, options: CompilerOptions) -> String {
    let runtime = Runtime::new(options.libs);

    let prover_data_bounds = program.prover_data_bounds();

    // Do this in a separate function to avoid most of the code being generic on F.
    let (initial_mem, instructions) =
        translate_program_impl(program, options.field, &runtime, options.continuations);

    riscv_machine(
        options,
        &runtime,
        initial_mem,
        prover_data_bounds,
        instructions,
    )
}

fn translate_program_impl(
    mut program: impl RiscVProgram,
    field: KnownField,
    runtime: &Runtime,
    continuations: bool,
) -> (Vec<String>, Vec<String>) {
    let mut initial_mem = Vec::new();
    let mut data_code = Vec::new();
    for MemEntry { label, addr, value } in program.take_initial_mem() {
        if let Some(label) = label {
            // This is a comment, so we don't need to escape the label.
            let comment = format!(" // data {label}");
            if continuations && !matches!(value, SingleDataValue::LabelReference(_)) {
                &mut initial_mem
            } else {
                &mut data_code
            }
            .push(comment);
        }
        match value {
            SingleDataValue::Value(v) => {
                if continuations {
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

    let bootloader_and_shutdown_routine_lines = if continuations {
        let bootloader_and_shutdown_routine = bootloader_and_shutdown_routine(field);
        log::debug!("Adding Bootloader:\n{}", bootloader_and_shutdown_routine);
        bootloader_and_shutdown_routine
            .split('\n')
            .map(|l| l.to_string())
            .collect::<Vec<_>>()
    } else {
        vec![]
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
        format!(
            "jump {}, 1;",
            escape_label(program.start_function().as_ref())
        ),
        "return;".to_string(), // This is not "riscv ret", but "return from powdr asm function".
    ]);
    for s in program.take_executable_statements() {
        match s {
            Statement::DebugLoc { file, line, col } => {
                statements.push(format!(".debug loc {file} {line} {col};"))
            }
            Statement::Label(l) => statements.push(format!("{}:", escape_label(l.as_ref()))),
            Statement::Instruction { op, args } => {
                let processed_instr = match process_instruction(op, args, runtime) {
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

    (initial_mem, statements)
}

const RISCV_ASM_TEMPLATE: &str = include_str!("../riscv_gl.asm");

fn riscv_machine(
    options: CompilerOptions,
    runtime: &Runtime,
    initial_memory: Vec<String>,
    prover_data_bounds: (u32, u32),
    program: Vec<String>,
) -> String {
    for machine in [
        "binary",
        "shift",
        "bit2",
        "bit6",
        "bit7",
        "byte",
        "byte2",
        "byte_binary",
        "byte_shift",
        "byte_compare",
    ] {
        assert!(
            runtime.has_submachine(machine),
            "RISC-V machine requires the `{machine}` submachine"
        );
    }

    let initial_memory = initial_memory
        .into_iter()
        .format_with(",\n", |line, f| f(&format_args!("\t\t{line}")));
    let program = program
        .into_iter()
        .format_with("\n", |line, f| f(&format_args!("\t\t{line}")));
    let bootloader_instructions = if options.continuations {
        bootloader_preamble(options.field)
    } else {
        "".to_string()
    };
    let mul_instruction = mul_instruction(options.field, runtime);
    let invert_gl_instruction_body = invert_gl_instruction_body(options.field);
    let memory = memory(options.continuations);
    let submachine_instructions = runtime
        .submachines_instructions()
        .into_iter()
        .map(|s| format!("    {s}"))
        .join("\n");

    RISCV_ASM_TEMPLATE
        .to_string()
        .replace("{{IMPORTS}}", &runtime.submachines_import())
        .replace("{{MIN_DEGREE_LOG}}", &format!("{}", options.min_degree_log))
        .replace("{{MAX_DEGREE_LOG}}", &format!("{}", options.max_degree_log))
        // We're passing this as well because continuations requires
        // Main's max_degree to be a constant.
        // TODO We should fix that in the continuations code and remove this.
        .replace(
            "{{MAIN_MAX_DEGREE}}",
            &format!("{}", 1 << options.max_degree_log),
        )
        .replace("{{MEMORY_DECLARATION}}", &memory)
        .replace(
            "{{SUBMACHINE_DECLARATIONS}}",
            &runtime.submachines_declare(),
        )
        .replace("{{INITIAL_MEMORY}}", &format!("{initial_memory}"))
        .replace(
            "{{PROVER_DATA_START}}",
            &format!("{}", prover_data_bounds.0),
        )
        .replace("{{PROVER_DATA_END}}", &format!("{}", prover_data_bounds.1))
        .replace("{{PROGRAM}}", &format!("{program}"))
        .replace("{{BOOTLOADER_INSTRUCTIONS}}", &bootloader_instructions)
        .replace("{{MUL_INSTRUCTION}}", mul_instruction)
        .replace("{{INVERT_GL_INSTRUCTION_BODY}}", invert_gl_instruction_body)
        .replace("{{SUBMACHINE_INSTRUCTIONS}}", &submachine_instructions)
}

fn mul_instruction(field: KnownField, runtime: &Runtime) -> &'static str {
    match field {
        KnownField::Bn254Field => {
            // The BN254 field can fit any 64-bit number, so we can naively de-compose
            // Z * W into 8 bytes and put them together to get the upper and lower word.
            r#"
    // Computes V = val(X) * val(Y) and
    // stores the lower 32 bits in register Z and the upper 32 bits in register W.
    instr mul X, Y, Z, W
        link ~> tmp1_col = regs.mload(X, STEP)
        link ~> tmp2_col = regs.mload(Y, STEP + 1)
        link ~> regs.mstore(Z, STEP + 2, tmp3_col)
        link ~> regs.mstore(W, STEP + 3, tmp4_col)
    {
        tmp1_col * tmp2_col = tmp3_col + tmp4_col * 2**32,
        tmp3_col = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000,
        tmp4_col = Y_b5 + Y_b6 * 0x100 + Y_b7 * 0x10000 + Y_b8 * 0x1000000
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
        link ~> tmp1_col = regs.mload(X, STEP)
        link ~> tmp2_col = regs.mload(Y, STEP + 1)
        link ~> (tmp3_col, tmp4_col) = split_gl.split(tmp1_col * tmp2_col)
        link ~> regs.mstore(Z, STEP + 2, tmp3_col)
        link ~> regs.mstore(W, STEP + 3, tmp4_col);
"#
        }
        KnownField::BabyBearField | KnownField::KoalaBearField | KnownField::Mersenne31Field => {
            panic!()
        }
    }
}

fn invert_gl_instruction_body(field: KnownField) -> &'static str {
    match field {
        KnownField::GoldilocksField => "XX_inv * (tmp1_col + tmp2_col * 2**32) = 1",
        _ => " 1 = 0",
    }
}

fn memory(with_bootloader: bool) -> String {
    let memory_machine = if with_bootloader {
        r#"
    std::machines::large_field::memory_with_bootloader_write::MemoryWithBootloaderWrite memory(byte2, MIN_DEGREE, MAIN_MAX_DEGREE);

    // Stores val(W) at address (V = val(X) - val(Z) + Y) % 2**32.
    // V can be between 0 and 2**33.
    instr mstore_bootloader X, Z, Y, W
        link ~> tmp1_col = regs.mload(X, STEP)
        link ~> tmp2_col = regs.mload(Z, STEP + 1)
        link ~> tmp3_col = regs.mload(W, STEP + 2)
        link ~> memory.mstore_bootloader(X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000, STEP + 3, tmp3_col)
    {
        tmp1_col - tmp2_col + Y = (X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000) + wrap_bit * 2**32
    }
"#
    } else {
        r#"
    std::machines::large_field::memory::Memory memory(byte2, MIN_DEGREE, MAIN_MAX_DEGREE);
"#
    };

    memory_machine.to_string()
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

fn process_instruction<A: InstructionArgs>(
    instr: &str,
    args: A,
    runtime: &Runtime,
) -> Result<Vec<String>, A::Error> {
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
            only_if_no_write_to_zero(rd, format!("affine {}, {}, 1, 0;", rs.addr(), rd.addr()))
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
                    format!(
                        "is_diff_greater_than {}, 0, -1, {};",
                        tmp1.addr(),
                        tmp3.addr()
                    ),
                    // tmp4 is 1 if tmp2 is non-negative
                    format!(
                        "is_diff_greater_than {}, 0, -1, {};",
                        tmp2.addr(),
                        tmp4.addr()
                    ),
                    // If tmp1 is negative, convert to positive
                    format!("skip_if_equal 0, {}, 1, 1;", tmp3.addr()),
                    format!("affine {}, {}, -1, 0;", tmp1.addr(), tmp1.addr()),
                    // If tmp2 is negative, convert to positive
                    format!("skip_if_equal 0, {}, 1, 1;", tmp4.addr()),
                    format!("affine {}, {}, -1, 0;", tmp2.addr(), tmp2.addr()),
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
                    format!("skip_if_equal {}, 0, 0, 2;", tmp3.addr()),
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
                    format!(
                        "is_diff_greater_than {}, 0, -1, {};",
                        tmp1.addr(),
                        tmp2.addr()
                    ),
                    // If negative, convert to positive
                    format!("skip_if_equal 0, {}, 1, 1;", tmp2.addr()),
                    format!("affine {}, {}, -1, 0;", tmp1.addr(), tmp1.addr()),
                    format!(
                        "mul {}, {}, {}, {};",
                        tmp1.addr(),
                        r2.addr(),
                        tmp1.addr(),
                        rd.addr()
                    ),
                    // If was negative before, convert back to negative
                    format!("skip_if_equal 0, {}, 1, 2;", tmp2.addr()),
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
                    format!(
                        "is_diff_greater_than 0, {}, 0, {};",
                        tmp1.addr(),
                        tmp1.addr()
                    ),
                    format!("affine {}, {}, 0xffffffff, 0;", tmp1.addr(), tmp1.addr()),
                    // Here, tmp1 is the full bit mask if rs is negative
                    // and zero otherwise.
                    format!("xor {}, {}, 0, {};", tmp1.addr(), rs.addr(), rd.addr()),
                    format!("shr {}, 0, {amount}, {};", rd.addr(), rd.addr()),
                    format!("xor {}, {}, 0, {};", tmp1.addr(), rd.addr(), rd.addr()),
                ],
            )
        }
        "sra" => {
            // arithmetic shift right
            // TODO see if we can implement this directly with a machine.
            // Now we are using the equivalence
            // a >>> b = (a >= 0 ? a >> b : ~(~a >> b))
            let (rd, rs1, rs2) = args.rrr()?;
            only_if_no_write_to_zero_vec(
                rd,
                vec![
                    format!("to_signed {}, {};", rs1.addr(), tmp1.addr()),
                    format!(
                        "is_diff_greater_than 0, {}, 0, {};",
                        tmp1.addr(),
                        tmp1.addr()
                    ),
                    format!("affine {}, {}, 0xffffffff, 0;", tmp1.addr(), tmp1.addr()),
                    // Here, tmp1 is the full bit mask if rs is negative
                    // and zero otherwise.
                    format!("xor {}, {}, 0, {};", tmp1.addr(), rs1.addr(), tmp2.addr()),
                    format!("and {}, 0, 0x1f, {};", rs2.addr(), tmp3.addr()),
                    format!("shr {}, {}, 0, {};", tmp2.addr(), tmp3.addr(), rd.addr()),
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
                        "is_diff_greater_than 0, {}, -({}), {};",
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
                        "is_diff_greater_than {}, {}, 0, {};",
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
                format!(
                    "is_diff_greater_than 0, {}, -({imm}), {};",
                    rs.addr(),
                    rd.addr()
                ),
            )
        }
        "sltu" => {
            let (rd, r1, r2) = args.rrr()?;
            only_if_no_write_to_zero(
                rd,
                format!(
                    "is_diff_greater_than {}, {}, 0, {};",
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
                    format!("is_diff_greater_than {}, 0, 0, {};", tmp1.addr(), rd.addr()),
                ],
            )
        }

        // branching
        "beq" => {
            let (r1, r2, label) = args.rrl()?;
            let label = escape_label(label.as_ref());
            vec![format!(
                "branch_if_diff_equal {}, {}, 0, {label};",
                r1.addr(),
                r2.addr()
            )]
        }
        "beqz" => {
            let (r1, label) = args.rl()?;
            let label = escape_label(label.as_ref());
            vec![format!(
                "branch_if_diff_equal {}, 0, 0, {label};",
                r1.addr()
            )]
        }
        "bgeu" => {
            let (r1, r2, label) = args.rrl()?;
            let label = escape_label(label.as_ref());
            // TODO does this fulfill the input requirements for branch_if_diff_greater_than?
            vec![format!(
                "branch_if_diff_greater_than {}, {}, -1, {label};",
                r1.addr(),
                r2.addr()
            )]
        }
        "bgez" => {
            let (r1, label) = args.rl()?;
            let label = escape_label(label.as_ref());
            vec![
                format!("to_signed {}, {};", r1.addr(), tmp1.addr()),
                format!(
                    "branch_if_diff_greater_than {}, 0, -1, {label};",
                    tmp1.addr()
                ),
            ]
        }
        "bltu" => {
            let (r1, r2, label) = args.rrl()?;
            let label = escape_label(label.as_ref());
            vec![format!(
                "branch_if_diff_greater_than {}, {}, 0, {label};",
                r2.addr(),
                r1.addr()
            )]
        }
        "blt" => {
            let (r1, r2, label) = args.rrl()?;
            let label = escape_label(label.as_ref());
            // Branch if r1 < r2 (signed).
            // TODO does this fulfill the input requirements for branch_if_diff_greater_than?
            vec![
                format!("to_signed {}, {};", r1.addr(), tmp1.addr()),
                format!("to_signed {}, {};", r2.addr(), tmp2.addr()),
                format!(
                    "branch_if_diff_greater_than {}, {}, 0, {label};",
                    tmp2.addr(),
                    tmp1.addr()
                ),
            ]
        }
        "bge" => {
            let (r1, r2, label) = args.rrl()?;
            let label = escape_label(label.as_ref());
            // Branch if r1 >= r2 (signed).
            // TODO does this fulfill the input requirements for branch_if_diff_greater_than?
            vec![
                format!("to_signed {}, {};", r1.addr(), tmp1.addr()),
                format!("to_signed {}, {};", r2.addr(), tmp2.addr()),
                format!(
                    "branch_if_diff_greater_than {}, {}, -1, {label};",
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
                "branch_if_diff_greater_than {}, 0, 2**31 - 1, {label};",
                r1.addr()
            )]
        }
        "blez" => {
            // branch less or equal zero
            let (r1, label) = args.rl()?;
            let label = escape_label(label.as_ref());
            vec![
                format!("to_signed {}, {};", r1.addr(), tmp1.addr()),
                format!(
                    "branch_if_diff_greater_than 0, {}, -1, {label};",
                    tmp1.addr()
                ),
            ]
        }
        "bgtz" => {
            // branch if 0 < r1 < 2**31
            let (r1, label) = args.rl()?;
            let label = escape_label(label.as_ref());
            vec![
                format!("to_signed {}, {};", r1.addr(), tmp1.addr()),
                format!(
                    "branch_if_diff_greater_than {}, 0, 0, {label};",
                    tmp1.addr()
                ),
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
                // jump to handler
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
                    format!("affine {}, {}, 8, 0;", tmp2.addr(), tmp2.addr()),
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
                    format!("affine {}, {}, 8, 0;", tmp2.addr(), tmp2.addr()),
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
                    format!("affine {}, {}, 8, 0;", tmp2.addr(), tmp2.addr()),
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
                    format!("affine {}, {}, 8, 0;", tmp2.addr(), tmp2.addr()),
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
                format!("affine {}, {}, 8, 0;", tmp2.addr(), tmp4.addr()),
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
                format!("affine {}, {}, 8, 0;", tmp2.addr(), tmp4.addr()),
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
                    format!("affine {}, {}, 1, 0;", tmp1.addr(), rd.addr()),
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
                format!("skip_if_equal {}, 0, 0, 1;", lr_sc_reservation.addr()),
                format!("mstore {}, 0, 0, {};", rs1.addr(), rs2.addr()),
            ]
            .into_iter()
            .chain(only_if_no_write_to_zero_vec(
                rd,
                vec![format!(
                    "affine {}, {}, -1, 1;",
                    lr_sc_reservation.addr(),
                    rd.addr()
                )],
            ))
            .chain([format!("set_reg {}, 0;", lr_sc_reservation.addr())])
            .collect()
        }

        // possibly inlined system calls
        insn => {
            let Some(syscall_impl) = runtime.get_syscall_impl(insn) else {
                panic!("Unknown instruction: {instr}");
            };
            syscall_impl.statements.clone()
        }
    };
    for s in &statements {
        log::debug!("          {s}");
    }
    Ok(statements)
}
