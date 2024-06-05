use std::{
    cell::Cell,
    collections::{BTreeMap, BTreeSet, HashSet},
};

use itertools::Itertools;
use parser::RiscParser;
use powdr_asm_utils::{
    ast::{BinaryOpKind, UnaryOpKind},
    data_parser,
    data_storage::store_data_objects,
    parser::parse_asm,
    reachability::{self, symbols_in_args},
    utils::{argument_to_escaped_symbol, argument_to_number, expression_to_number},
    Architecture,
};
use powdr_number::FieldElement;

use crate::{
    code_gen::{self, Args, FunctionKind, MemEntry, Register, RiscVProgram, SourceFileInfo},
    Runtime,
};

mod disambiguator;
mod parser;

type Statement = powdr_asm_utils::ast::Statement<Register, FunctionKind>;
type Argument = powdr_asm_utils::ast::Argument<Register, FunctionKind>;
type Expression = powdr_asm_utils::ast::Expression<FunctionKind>;

struct AsmProgram {
    file_ids: Vec<(i64, String, String)>,
    mem_entries: Cell<Option<Vec<MemEntry>>>,
    statements: Vec<Statement>,
}

impl<'a> RiscVProgram<'a> for AsmProgram {
    type InstructionArgs = [Argument];
    type Label = &'a str;

    fn source_files_info(&self) -> impl Iterator<Item = SourceFileInfo> {
        self.file_ids.iter().map(|(id, dir, file)| SourceFileInfo {
            id: *id as u32,
            dir,
            file,
        })
    }

    fn initial_mem(&self) -> impl Iterator<Item = MemEntry> {
        self.mem_entries.take().into_iter().flatten()
    }

    fn executable_statements(
        &'a self,
    ) -> impl Iterator<Item = code_gen::Statement<&'a str, Self::InstructionArgs>> {
        self.statements.iter().filter_map(process_statement)
    }

    fn start_function(&self) -> &str {
        "_start"
    }
}

impl Args for [Argument] {
    type Error = &'static str;

    fn l(&self) -> Result<String, &'static str> {
        const ERR: &str = "Expected: label";
        match self {
            [l] => Ok(argument_to_escaped_symbol(l).ok_or(ERR)?),
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
            [Argument::Register(r1), Argument::Register(r2), Argument::Register(r3)] => {
                Ok((*r1, *r2, *r3))
            }
            _ => Err("Expected: register, register, register"),
        }
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
            [Argument::Register(r1), Argument::Register(r2)] => Ok((*r1, *r2)),
            _ => Err("Expected: register, register"),
        }
    }

    fn rrl(&self) -> Result<(Register, Register, String), &'static str> {
        const ERR: &str = "Expected: register, register, label";
        match self {
            [Argument::Register(r1), Argument::Register(r2), l] => {
                Ok((*r1, *r2, argument_to_escaped_symbol(l).ok_or(ERR)?))
            }
            _ => Err(ERR),
        }
    }

    fn rl(&self) -> Result<(Register, String), &'static str> {
        const ERR: &str = "Expected: register, label";
        match self {
            [Argument::Register(r1), l] => Ok((*r1, argument_to_escaped_symbol(l).ok_or(ERR)?)),
            _ => Err(ERR),
        }
    }

    fn rro(&self) -> Result<(Register, Register, u32), &'static str> {
        if let [Argument::Register(r1), Argument::RegOffset(off, r2)] = self {
            if let Some(off) = expression_to_number(off.as_ref().unwrap_or(&Expression::Number(0)))
            {
                return Ok((*r1, *r2, off));
            }
        }
        if let [Argument::Register(r1), Argument::Expression(off)] = self {
            if let Some(off) = expression_to_number(off) {
                // If the register is not specified, it defaults to x0
                return Ok((*r1, Register::new(0), off));
            }
        }

        Err("Expected: register, offset(register)")
    }

    fn rrro(&self) -> Result<(Register, Register, Register, u32), &'static str> {
        if let [Argument::Register(r1), Argument::Register(r2), Argument::RegOffset(off, r3)] = self
        {
            if let Some(off) = expression_to_number(off.as_ref().unwrap_or(&Expression::Number(0)))
            {
                return Ok((*r1, *r2, *r3, off));
            }
        }
        if let [Argument::Register(r1), Argument::Register(r2), Argument::Expression(off)] = self {
            if let Some(off) = expression_to_number(off) {
                // If the register is not specified, it defaults to x0
                return Ok((*r1, *r2, Register::new(0), off));
            }
        }
        Err("Expected: register, register, offset(register)")
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
    let asm_program = compile_internal(assemblies, runtime);

    code_gen::translate_program::<F>(&asm_program, runtime, with_bootloader)
}

fn compile_internal(mut assemblies: BTreeMap<String, String>, runtime: &Runtime) -> AsmProgram {
    // stack grows towards zero
    let stack_start = 0x10000000;
    // data grows away from zero
    let data_start = 0x10000100;

    assert!(assemblies
        .insert(
            "__runtime".to_string(),
            runtime.global_declarations(stack_start)
        )
        .is_none());

    // TODO remove unreferenced files.
    let (mut statements, file_ids) = disambiguator::disambiguate(
        assemblies
            .into_iter()
            .map(|(name, contents)| (name, parse_asm(RiscParser::default(), &contents)))
            .collect(),
    );
    let mut data_sections = data_parser::extract_data_objects(&statements);

    // Reduce to the code that is actually reachable from main
    // (and the objects that are referred from there)
    let data_labels = reachability::filter_reachable_from::<_, _, RiscvArchitecture>(
        "_start",
        &mut statements,
        &mut data_sections,
    );

    // Replace dynamic references to code labels
    replace_dynamic_label_references(&mut statements, &data_labels);

    let mut mem_entries = Vec::new();
    let data_positions =
        store_data_objects(data_sections, data_start, &mut |label, addr, value| {
            mem_entries.push(MemEntry { label, addr, value });
        });

    let statements = substitute_symbols_with_values(statements, &data_positions);

    AsmProgram {
        file_ids,
        mem_entries: Cell::new(Some(mem_entries)),
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

fn process_statement(s: &Statement) -> Option<code_gen::Statement<&str, [Argument]>> {
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
