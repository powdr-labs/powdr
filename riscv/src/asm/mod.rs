use std::collections::{BTreeMap, BTreeSet, HashSet};

use itertools::Itertools;
use parser::RiscParser;
use powdr_asm_utils::{
    ast::{BinaryOpKind, UnaryOpKind},
    data_parser::{self, DataObjects},
    data_storage::store_data_objects,
    parser::parse_asm,
    reachability::{self, symbols_in_args},
    utils::{argument_to_number, argument_to_symbol, expression_to_number},
    Architecture,
};
use powdr_number::FieldElement;

use crate::{
    code_gen::{
        self, FunctionKind, InstructionArgs, MemEntry, Register, RiscVProgram, SourceFileInfo,
    },
    Runtime,
};

mod disambiguator;
mod parser;

type Statement = powdr_asm_utils::ast::Statement<Register, FunctionKind>;
type Argument = powdr_asm_utils::ast::Argument<Register, FunctionKind>;
type Expression = powdr_asm_utils::ast::Expression<FunctionKind>;

struct AsmProgram {
    file_ids: Vec<(i64, String, String)>,
    mem_entries: Option<Vec<MemEntry>>,
    statements: Vec<Statement>,
}

const START_FUNCTION: &str = "__runtime_start";

impl RiscVProgram for AsmProgram {
    fn take_source_files_info(&mut self) -> impl Iterator<Item = SourceFileInfo> {
        self.file_ids.iter().map(|(id, dir, file)| SourceFileInfo {
            id: *id as u32,
            dir,
            file,
        })
    }

    fn take_initial_mem(&mut self) -> impl Iterator<Item = MemEntry> {
        std::mem::take(&mut self.mem_entries).unwrap().into_iter()
    }

    fn take_executable_statements(
        &mut self,
    ) -> impl Iterator<Item = code_gen::Statement<&str, &[Argument]>> {
        self.statements.iter().filter_map(process_statement)
    }

    fn start_function(&self) -> &str {
        START_FUNCTION
    }
}

impl InstructionArgs for &[Argument] {
    type Error = &'static str;

    fn l(&self) -> Result<&str, &'static str> {
        const ERR: &str = "Expected: label";
        match self {
            [l] => Ok(argument_to_symbol(l).ok_or(ERR)?),
            _ => Err(ERR),
        }
    }

    fn r(&self) -> Result<Register, &'static str> {
        match self {
            [Argument::Register(r1)] => Ok(*r1),
            _ => Err("Expected: register"),
        }
    }

    fn rri(&self) -> Result<(Register, Register, u32), &'static str> {
        const ERR: &str = "Expected: register, register, immediate";
        match self {
            [Argument::Register(r1), Argument::Register(r2), n] => {
                Ok((*r1, *r2, argument_to_number(n).ok_or(ERR)?))
            }
            _ => Err(ERR),
        }
    }

    fn rrr(&self) -> Result<(Register, Register, Register), &'static str> {
        match self {
            [Argument::Register(r1), Argument::Register(r2), Argument::Register(r3)
            | Argument::RegOffset(None | Some(Expression::Number(0)), r3)] => Ok((*r1, *r2, *r3)),
            _ => Err("Expected: register, register, register"),
        }
    }

    fn rrr2(&self) -> Result<(Register, Register, Register), &'static str> {
        // When reading from assembly, this is identical to rrr
        self.rrr()
    }

    fn ri(&self) -> Result<(Register, u32), &'static str> {
        const ERR: &str = "Expected: register, immediate";
        match self {
            [Argument::Register(r1), n] => Ok((*r1, argument_to_number(n).ok_or(ERR)?)),
            _ => Err(ERR),
        }
    }

    fn rr(&self) -> Result<(Register, Register), &'static str> {
        match self {
            [Argument::Register(r1), Argument::Register(r2)
            | Argument::RegOffset(None | Some(Expression::Number(0)), r2)] => Ok((*r1, *r2)),
            _ => Err("Expected: register, register"),
        }
    }

    fn rrl(&self) -> Result<(Register, Register, &str), &'static str> {
        const ERR: &str = "Expected: register, register, label";
        match self {
            [Argument::Register(r1), Argument::Register(r2), l] => {
                Ok((*r1, *r2, argument_to_symbol(l).ok_or(ERR)?))
            }
            _ => Err(ERR),
        }
    }

    fn rl(&self) -> Result<(Register, &str), &'static str> {
        const ERR: &str = "Expected: register, label";
        match self {
            [Argument::Register(r1), l] => Ok((*r1, argument_to_symbol(l).ok_or(ERR)?)),
            _ => Err(ERR),
        }
    }

    fn rro(&self) -> Result<(Register, Register, u32), &'static str> {
        const ERR: &str = "Expected: register, offset(register)";

        match self {
            [Argument::Register(r1), Argument::RegOffset(off, r2)] => Ok((
                *r1,
                *r2,
                expression_to_number(off.as_ref().unwrap_or(&Expression::Number(0))).ok_or(ERR)?,
            )),
            [Argument::Register(r1), Argument::Expression(off)] => {
                Ok((*r1, Register::new(0), expression_to_number(off).ok_or(ERR)?))
            }
            _ => Err(ERR),
        }
    }

    fn empty(&self) -> Result<(), &'static str> {
        match self {
            [] => Ok(()),
            _ => Err("Expected: no arguments"),
        }
    }
}

/// Compiles riscv assembly to a powdr assembly file. Adds required library routines.
pub fn compile<F: FieldElement>(
    assemblies: BTreeMap<String, String>,
    runtime: &Runtime,
    with_bootloader: bool,
) -> String {
    let asm_program = compile_internal(assemblies);

    code_gen::translate_program::<F>(asm_program, runtime, with_bootloader)
}

fn compile_internal(mut assemblies: BTreeMap<String, String>) -> AsmProgram {
    // stack grows towards zero
    let stack_start = 0x10000000;
    // data grows away from zero
    let data_start = 0x10000100;

    assert!(assemblies
        .insert("__runtime".to_string(), global_declarations(stack_start))
        .is_none());

    // TODO remove unreferenced files.
    let (mut statements, file_ids) = disambiguator::disambiguate(
        assemblies
            .into_iter()
            .map(|(name, contents)| (name, parse_asm(RiscParser::default(), &contents)))
            .collect(),
    );
    let DataObjects {
        sections: mut data_sections,
        adhoc_symbols: mut data_positions,
    } = data_parser::extract_data_objects(&statements);

    // Reduce to the code that is actually reachable from main
    // (and the objects that are referred from there)
    let data_labels = reachability::filter_reachable_from::<_, _, RiscvArchitecture>(
        START_FUNCTION,
        &mut statements,
        &mut data_sections,
    );

    // Replace dynamic references to code labels
    replace_dynamic_label_references(&mut statements, &data_labels);

    let mut mem_entries = Vec::new();
    store_data_objects(
        data_sections,
        data_start,
        &mut |label, addr, value| {
            mem_entries.push(MemEntry { label, addr, value });
        },
        &mut data_positions,
    );

    let statements = substitute_symbols_with_values(statements, &data_positions);

    AsmProgram {
        file_ids,
        mem_entries: Some(mem_entries),
        statements,
    }
}

/// Replace certain patterns of references to code labels by
/// special instructions. We ignore any references to data objects
/// because they will be handled differently.
fn replace_dynamic_label_references(statements: &mut Vec<Statement>, data_labels: &HashSet<&str>) {
    /*
    Find patterns of the form
    lui	a0, %hi(LABEL)
    addi	s10, a0, %lo(LABEL)
    -
    turn this into the pseudoinstruction
    li s10, LABEL
    which is then turned into

    s10 <== load_label(LABEL)

    It gets complicated by the fact that sometimes, labels
    and debugging directives occur between the two statements
    matching that pattern...
    */
    let instruction_indices = statements
        .iter()
        .enumerate()
        .filter_map(|(i, s)| match s {
            Statement::Instruction(_, _) => Some(i),
            _ => None,
        })
        .collect::<Vec<_>>();

    let mut to_delete = BTreeSet::default();
    for (i1, i2) in instruction_indices.into_iter().tuple_windows() {
        if let Some(r) =
            replace_dynamic_label_reference(&statements[i1], &statements[i2], data_labels)
        {
            to_delete.insert(i1);
            statements[i2] = r;
        }
    }

    let mut i = 0;
    statements.retain(|_| (!to_delete.contains(&i), i += 1).0);
}

fn replace_dynamic_label_reference(
    s1: &Statement,
    s2: &Statement,
    data_labels: &HashSet<&str>,
) -> Option<Statement> {
    let Statement::Instruction(instr1, args1) = s1 else {
        return None;
    };
    let Statement::Instruction(instr2, args2) = s2 else {
        return None;
    };
    if instr1.as_str() != "lui" || instr2.as_str() != "addi" {
        return None;
    };
    let [Argument::Register(r1), Argument::Expression(Expression::FunctionOp(FunctionKind::HiDataRef, expr1))] =
        &args1[..]
    else {
        return None;
    };
    // Maybe should try to reduce expr1 and expr2 before comparing deciding it is a pure symbol?
    let Expression::Symbol(label1) = expr1.as_ref() else {
        return None;
    };
    let [Argument::Register(r2), Argument::Register(r3), Argument::Expression(Expression::FunctionOp(FunctionKind::LoDataRef, expr2))] =
        &args2[..]
    else {
        return None;
    };
    let Expression::Symbol(label2) = expr2.as_ref() else {
        return None;
    };
    if r1 != r3 || label1 != label2 || data_labels.contains(label1.as_str()) {
        return None;
    }
    Some(Statement::Instruction(
        "li".to_string(),
        vec![
            Argument::Register(*r2),
            Argument::Expression(Expression::Symbol(label1.clone())),
        ],
    ))
}

fn substitute_symbols_with_values(
    mut statements: Vec<Statement>,
    data_positions: &BTreeMap<String, u32>,
) -> Vec<Statement> {
    for s in &mut statements {
        let Statement::Instruction(_name, args) = s else {
            continue;
        };
        for arg in args {
            arg.post_visit_expressions_mut(&mut |expression| match expression {
                Expression::Number(_) => {}
                Expression::Symbol(symb) => {
                    if let Some(pos) = data_positions.get(symb) {
                        *expression = Expression::Number(*pos as i64)
                    }
                }
                Expression::UnaryOp(op, subexpr) => {
                    if let Expression::Number(num) = subexpr.as_ref() {
                        let result = match op {
                            UnaryOpKind::BitwiseNot => !num,
                            UnaryOpKind::Negation => -num,
                        };
                        *expression = Expression::Number(result);
                    };
                }
                Expression::BinaryOp(op, subexprs) => {
                    if let (Expression::Number(a), Expression::Number(b)) =
                        (&subexprs[0], &subexprs[1])
                    {
                        let result = match op {
                            BinaryOpKind::Or => a | b,
                            BinaryOpKind::Xor => a ^ b,
                            BinaryOpKind::And => a & b,
                            BinaryOpKind::LeftShift => a << b,
                            BinaryOpKind::RightShift => a >> b,
                            BinaryOpKind::Add => a + b,
                            BinaryOpKind::Sub => a - b,
                            BinaryOpKind::Mul => a * b,
                            BinaryOpKind::Div => a / b,
                            BinaryOpKind::Mod => a % b,
                        };
                        *expression = Expression::Number(result);
                    }
                }
                Expression::FunctionOp(op, subexpr) => {
                    if let Expression::Number(num) = subexpr.as_ref() {
                        let result = match op {
                            FunctionKind::HiDataRef => num >> 12,
                            FunctionKind::LoDataRef => num & 0xfff,
                        };
                        *expression = Expression::Number(result);
                    };
                }
            });
        }
    }
    statements
}

fn process_statement(s: &Statement) -> Option<code_gen::Statement<&str, &[Argument]>> {
    match s {
        Statement::Label(l) => Some(code_gen::Statement::Label(l)),
        Statement::Directive(directive, args) => match (directive.as_str(), &args[..]) {
            (
                ".loc",
                [Argument::Expression(Expression::Number(file)), Argument::Expression(Expression::Number(line)), Argument::Expression(Expression::Number(column)), ..],
            ) => Some(code_gen::Statement::DebugLoc {
                file: *file as u64,
                line: *line as u64,
                col: *column as u64,
            }),
            (".file", _) => {
                // We ignore ".file" directives because they have been extracted to the top.
                None
            }
            (".size", _) => {
                // We ignore ".size" directives
                None
            }
            (".option", _) => {
                // We ignore ".option" directives
                None
            }
            _ if directive.starts_with(".cfi_") => None,
            _ => panic!(
                "Leftover directive in code: {directive} {}",
                args.iter().format(", ")
            ),
        },
        Statement::Instruction(instr, args) => {
            // TODO: maybe restore this debug info
            /*
            let stmt_str = format!("{s}");
            // remove indentation and trailing newline
            let stmt_str = &stmt_str[2..(stmt_str.len() - 1)];
            let mut ret = vec![format!("  .debug insn \"{stmt_str}\";")];
            */
            Some(code_gen::Statement::Instruction {
                op: instr,
                args: args.as_slice(),
            })
        }
    }
}

struct RiscvArchitecture {}

impl Architecture for RiscvArchitecture {
    fn instruction_ends_control_flow(instr: &str) -> bool {
        match instr {
            "li" | "lui" | "la" | "mv" | "add" | "addi" | "sub" | "neg" | "mul" | "mulh"
            | "mulhu" | "mulhsu" | "divu" | "remu" | "xor" | "xori" | "and" | "andi" | "or"
            | "ori" | "not" | "slli" | "sll" | "srli" | "srl" | "srai" | "seqz" | "snez"
            | "slt" | "slti" | "sltu" | "sltiu" | "sgtz" | "beq" | "beqz" | "bgeu" | "bltu"
            | "blt" | "bge" | "bltz" | "blez" | "bgtz" | "bgez" | "bne" | "bnez" | "jal"
            | "jalr" | "call" | "ecall" | "ebreak" | "lw" | "lb" | "lbu" | "lh" | "lhu" | "sw"
            | "sh" | "sb" | "nop" | "fence" | "amoadd.w" | "amoadd.w.aq" | "amoadd.w.rl"
            | "amoadd.w.aqrl" | "lr.w" | "lr.w.aq" | "lr.w.rl" | "lr.w.aqrl" | "sc.w"
            | "sc.w.aq" | "sc.w.rl" | "sc.w.aqrl" => false,
            "j" | "jr" | "tail" | "ret" | "unimp" => true,
            _ => {
                panic!("Unknown instruction: {instr}");
            }
        }
    }

    fn get_references<
        'a,
        R: powdr_asm_utils::ast::Register,
        F: powdr_asm_utils::ast::FunctionOpKind,
    >(
        instr: &str,
        args: &'a [powdr_asm_utils::ast::Argument<R, F>],
    ) -> Vec<&'a str> {
        // fence arguments are not symbols, they are like reserved
        // keywords affecting the instruction behavior
        if instr.starts_with("fence") {
            Vec::new()
        } else {
            symbols_in_args(args)
        }
    }
}

/// Maps an instruction in .insn syntax to Statement::Instruction() in the expected format.
///
/// See https://www.rowleydownload.co.uk/arm/documentation/gnu/as/RISC_002dV_002dFormats.html
pub fn map_insn_i(
    opcode6: Expression,
    func3: Expression,
    rd: Register,
    rs1: Register,
    simm12: Expression,
) -> Statement {
    let (Expression::Number(opcode6), Expression::Number(func3)) = (opcode6, func3) else {
        panic!("Only literal opcode and function are supported in .insn syntax");
    };

    // These are almost all instructions in RISC-V Instruction Set Manual that
    // we are supposed to implement and roughly fits the pattern of the I-type
    // instruction. Only "csr*i" instructions are missing.

    // First we try to match the instructions that uses the I-type encoding
    // ordinarily, i.e. where all fields are what they are supposed to be:
    let name = match (opcode6, func3) {
        (0b1100111, 0b000) => "jalr",
        (0b0000011, 0b000) => "lb",
        (0b0000011, 0b001) => "lh",
        (0b0000011, 0b010) => "lw",
        (0b0000011, 0b100) => "lbu",
        (0b0000011, 0b101) => "lhu",
        (0b0010011, 0b000) => "addi",
        (0b0010011, 0b010) => "slti",
        (0b0010011, 0b011) => "sltiu",
        (0b0010011, 0b100) => "xori",
        (0b0010011, 0b110) => "ori",
        (0b0010011, 0b111) => "andi",
        (0b1110011, 0b001) => "csrrw",
        (0b1110011, 0b010) => "csrrs",
        (0b1110011, 0b011) => "csrrc",
        // won't interpret "csr*i" instructions because it is too weird to
        // encode an immediate as a register
        opfunc => {
            // We now try the instructions that take certain liberties with the
            // I-type encoding, and don't use the standard arguments for it.
            let name = match opfunc {
                (0b0001111, 0b000) => "fence",
                (0b0001111, 0b001) => "fence.i",
                (0b1110011, 0b000) => {
                    let Expression::Number(simm12) = simm12 else {
                        panic!(
                            "Only literal simm12 is supported for ecall and ebreak instructions"
                        );
                    };
                    match simm12 {
                        0 => "ecall",
                        1 => "ebreak",
                        _ => panic!("unknown instruction"),
                    }
                }
                _ => panic!("unsupported .insn instruction"),
            };
            return Statement::Instruction(name.to_string(), Vec::new());
        }
    };

    let args = vec![
        Argument::Register(rd),
        Argument::Register(rs1),
        Argument::Expression(simm12),
    ];

    Statement::Instruction(name.to_string(), args)
}

fn global_declarations(stack_start: u32) -> String {
    [
        "__divdi3",
        "__udivdi3",
        "__udivti3",
        "__divdf3",
        "__muldf3",
        "__moddi3",
        "__umoddi3",
        "__umodti3",
        "__eqdf2",
        "__ltdf2",
        "__nedf2",
        "__unorddf2",
        "__floatundidf",
        "__extendsfdf2",
        "memcpy",
        "memmove",
        "memset",
        "memcmp",
        "bcmp",
        "strlen",
    ]
    .map(|n| format!(".globl {n}@plt\n.globl {n}\n.set {n}@plt, {n}\n"))
    .join("\n\n")
        + &[("__rust_alloc_error_handler", "__rg_oom")]
            .map(|(n, m)| format!(".globl {n}\n.set {n}, {m}\n"))
            .join("\n\n")
        +
        // some extra symbols expected by rust code:
        // - __rust_no_alloc_shim_is_unstable: compilation time acknowledgment
        //   that this feature is unstable.
        // - __rust_alloc_error_handler_should_panic: needed by the default
        //   alloc error handler, not sure why it's not present in the asm.
        //   https://github.com/rust-lang/rust/blob/ae9d7b0c6434b27e4e2effe8f05b16d37e7ef33f/library/alloc/src/alloc.rs#L415
        // - __stack_start: the start of the stack
        // - __global_pointer$: a RISC-V special symbol that we actually don't
        //   use, but we define for compatibility with programs that expect it.
        &format!(r".data
.globl __rust_alloc_error_handler_should_panic
__rust_alloc_error_handler_should_panic: .byte 0
.globl __rust_no_alloc_shim_is_unstable
__rust_no_alloc_shim_is_unstable: .byte 0
.globl __powdr_stack_start
.set __powdr_stack_start, {stack_start}
.globl __global_pointer$
.set __global_pointer$, 0

.text
")
}
