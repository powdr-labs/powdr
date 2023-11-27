use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::{self, Display},
};

use asm_utils::{
    ast::{BinaryOpKind, UnaryOpKind},
    data_parser::{self, DataValue},
    data_storage::{store_data_objects, SingleDataValue},
    parser::parse_asm,
    reachability::{self, symbols_in_args},
    utils::{
        argument_to_escaped_symbol, argument_to_number, escape_label, expression_to_number, quote,
    },
    Architecture,
};
use itertools::Itertools;

use crate::coprocessors::*;
use crate::disambiguator;
use crate::parser::RiscParser;
use crate::{Argument, Expression, Statement};

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

impl asm_utils::ast::Register for Register {}

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

impl asm_utils::ast::FunctionOpKind for FunctionKind {}

impl fmt::Display for FunctionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionKind::HiDataRef => write!(f, "%hi"),
            FunctionKind::LoDataRef => write!(f, "%lo"),
        }
    }
}

struct RiscvArchitecture {}

impl Architecture for RiscvArchitecture {
    fn instruction_ends_control_flow(instr: &str) -> bool {
        match instr {
            "li" | "lui" | "la" | "mv" | "add" | "addi" | "sub" | "neg" | "mul" | "mulhu"
            | "mulhsu" | "divu" | "remu" | "xor" | "xori" | "and" | "andi" | "or" | "ori"
            | "not" | "slli" | "sll" | "srli" | "srl" | "srai" | "seqz" | "snez" | "slt"
            | "slti" | "sltu" | "sltiu" | "sgtz" | "beq" | "beqz" | "bgeu" | "bltu" | "blt"
            | "bge" | "bltz" | "blez" | "bgtz" | "bgez" | "bne" | "bnez" | "jal" | "jalr"
            | "call" | "ecall" | "ebreak" | "lw" | "lb" | "lbu" | "lh" | "lhu" | "sw" | "sh"
            | "sb" | "nop" | "fence" | "fence.i" | "amoadd.w" | "amoadd.w.aq" | "amoadd.w.rl"
            | "amoadd.w.aqrl" | "lr.w" | "lr.w.aq" | "lr.w.rl" | "lr.w.aqrl" | "sc.w"
            | "sc.w.aq" | "sc.w.rl" | "sc.w.aqrl" => false,
            "j" | "jr" | "tail" | "ret" | "unimp" => true,
            _ => {
                panic!("Unknown instruction: {instr}");
            }
        }
    }

    fn get_references<'a, R: asm_utils::ast::Register, F: asm_utils::ast::FunctionOpKind>(
        instr: &str,
        args: &'a [asm_utils::ast::Argument<R, F>],
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

/// Compiles riscv assembly to a powdr assembly file. Adds required library routines.
pub fn compile(mut assemblies: BTreeMap<String, String>, coprocessors: &CoProcessors) -> String {
    // stack grows towards zero
    let stack_start = 0x10000;
    // registers "grow" away from zero
    //let reg_start = 0x10100;
    // data grows away from zero
    let data_start = 0x10200;

    assert!(assemblies
        .insert("__runtime".to_string(), runtime(coprocessors))
        .is_none());

    // TODO remove unreferenced files.
    let (mut statements, file_ids) = disambiguator::disambiguate(
        assemblies
            .into_iter()
            .map(|(name, contents)| (name, parse_asm(RiscParser::default(), &contents)))
            .collect(),
    );
    let (mut objects, mut object_order) = data_parser::extract_data_objects(&statements);
    assert_eq!(objects.keys().len(), object_order.len());

    // Reduce to the code that is actually reachable from main
    // (and the objects that are referred from there)
    reachability::filter_reachable_from::<_, _, RiscvArchitecture>(
        "__runtime_start",
        &mut statements,
        &mut objects,
    );

    // Replace dynamic references to code labels
    replace_dynamic_label_references(&mut statements, &objects);

    // Remove the riscv asm stub function, which is used
    // for compilation, and will not be called.
    statements = replace_coprocessor_stubs(statements, coprocessors).collect::<Vec<_>>();

    // Sort the objects according to the order of the names in object_order.
    // With the single exception: If there is large object, put that at the end.
    // The idea behind this is that there might be a single gigantic object representing the heap
    // and putting that at the end should keep memory addresses small.
    let mut large_objects = objects
        .iter()
        .filter(|(_name, data)| data.iter().map(|d| d.size()).sum::<usize>() > 0x2000);
    if let (Some((heap, _)), None) = (large_objects.next(), large_objects.next()) {
        let heap_pos = object_order.iter().position(|o| o == heap).unwrap();
        object_order.remove(heap_pos);
        object_order.push(heap.clone());
    };
    let sorted_objects = object_order
        .into_iter()
        .filter_map(|n| {
            let value = objects.get_mut(&n).map(std::mem::take);
            value.map(|v| (n, v))
        })
        .collect::<Vec<_>>();
    let (data_code, data_positions) = store_data_objects(
        &sorted_objects,
        data_start,
        &mut |addr, value| match value {
            SingleDataValue::Value(v) => {
                vec![format!("mstore 0x{addr:x}, 0x{v:x};")]
            }
            SingleDataValue::LabelReference(sym) => {
                // TODO should be possible without temporary
                vec![
                    format!("tmp1 <== load_label({});", escape_label(sym)),
                    format!("mstore 0x{addr:x}, tmp1;"),
                ]
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
        },
    );

    let program: Vec<String> = file_ids
        .into_iter()
        .map(|(id, dir, file)| format!("debug file {id} {} {};", quote(&dir), quote(&file)))
        .chain(["call __data_init;".to_string()])
        .chain(call_every_submachine(coprocessors))
        .chain([
            format!("// Set stack pointer\nx2 <=X= {stack_start};"),
            "call __runtime_start;".to_string(),
            "return;".to_string(), // This is not "riscv ret", but "return from powdr asm function".
        ])
        .chain(
            substitute_symbols_with_values(statements, &data_positions)
                .into_iter()
                .flat_map(|v| process_statement(v, coprocessors)),
        )
        .chain(["// This is the data initialization routine.\n__data_init::".to_string()])
        .chain(data_code)
        .chain(["// This is the end of the data initialization routine.\nret;".to_string()])
        .collect();

    // The program ROM needs to fit the degree, so we use the next power of 2.
    let degree = program.len().ilog2() + 1;
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

    riscv_machine(
        &coprocessors.machine_imports(),
        &preamble(degree, coprocessors),
        &coprocessors.declarations(),
        program,
    )
}

/// Replace certain patterns of references to code labels by
/// special instructions. We ignore any references to data objects
/// because they will be handled differently.
fn replace_dynamic_label_references(
    statements: &mut Vec<Statement>,
    data_objects: &BTreeMap<String, Vec<DataValue>>,
) {
    /*
    Find patterns of the form
    lui	a0, %hi(LABEL)
    addi	s10, a0, %lo(LABEL)
    -
    turn this into the pseudo-riscv-instruction
    load_dynamic s10, LABEL
    which is then turned into

    s10 <== load_label(LABEL)

    It gets more complicated by the fact that sometimes, labels
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
            replace_dynamic_label_reference(&statements[i1], &statements[i2], data_objects)
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
    data_objects: &BTreeMap<String, Vec<DataValue>>,
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
    if r1 != r3 || label1 != label2 || data_objects.contains_key(label1) {
        return None;
    }
    Some(Statement::Instruction(
        "load_dynamic".to_string(),
        vec![
            Argument::Register(*r2),
            Argument::Expression(Expression::Symbol(label1.clone())),
        ],
    ))
}

fn remove_matching_and_next<I: Iterator, F>(iter: I, predicate: F) -> impl Iterator<Item = I::Item>
where
    F: Fn(&I::Item) -> bool,
{
    iter.scan(false, move |filter_next, item| {
        let mut filter_current = *filter_next;
        *filter_next = predicate(&item);
        // if the predicate says this line should be filtered, then
        // the next one should be filtered as well.
        filter_current |= *filter_next;
        Some((filter_current, item))
    })
    .filter_map(|(filter, statement)| (!filter).then_some(statement))
}

fn replace_coprocessor_stubs<'a>(
    statements: impl IntoIterator<Item = Statement> + 'a,
    coprocessors: &'a CoProcessors,
) -> impl Iterator<Item = Statement> + 'a {
    let stub_names: Vec<&'a str> = coprocessors.runtime_names();

    remove_matching_and_next(statements.into_iter(), move |statement| -> bool {
        matches!(&statement, Statement::Label(label) if stub_names.contains(&label.as_str()))
    })
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

fn riscv_machine(
    machines: &[&str],
    preamble: &str,
    submachines: &[(&str, &str)],
    program: Vec<String>,
) -> String {
    format!(
        r#"
{}
machine Main {{
{}

{}

    function main {{
{}
    }}
}}    
"#,
        machines.join("\n"),
        submachines
            .iter()
            .map(|(instance, ty)| format!("\t\t{} {};", ty, instance))
            .collect::<Vec<_>>()
            .join("\n"),
        preamble,
        program
            .into_iter()
            .map(|line| format!("\t\t{line}"))
            .collect::<Vec<_>>()
            .join("\n")
    )
}

fn preamble(degree: u64, coprocessors: &CoProcessors) -> String {
    format!("degree {degree};")
        + r#"
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg W[<=];
"# + &coprocessors.registers()
        + &r#"
    reg tmp1;
    reg tmp2;
    reg tmp3;
    reg lr_sc_reservation;
    let reg_start = 0x10100; // Register area.
"#
        .to_owned()
        .to_string()
        + r#"

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

    // ============== memory instructions ==============

    let up_to_three = |i| i % 4;
    let six_bits = |i| i % 2**6;
    /// Loads one word from an address Y, where Y can be between 0 and 2**33 (sic!),
    /// wraps the address to 32 bits and rounds it down to the next multiple of 4.
    /// Returns the loaded word and the remainder of the division by 4.
    instr mload Y -> X, Z {
        // Z * (Z - 1) * (Z - 2) * (Z - 3) = 0,
        { Z } in { up_to_three },
        Y = wrap_bit * 2**32 + X_b4 * 0x1000000 + X_b3 * 0x10000 + X_b2 * 0x100 + X_b1 * 4 + Z,
        { X_b1 } in { six_bits },
        {
            X_b4 * 0x1000000 + X_b3 * 0x10000 + X_b2 * 0x100 + X_b1 * 4,
            STEP,
            X
        } is m_is_read { m_addr, m_step, m_value }
        // If we could access the shift machine here, we
        // could even do the following to complete the mload:
        // { W, X, Z} in { shr.value, shr.amount, shr.amount}
    }

    /// Stores Z at address Y % 2**32. Y can be between 0 and 2**33.
    /// Y should be a multiple of 4, but this instruction does not enforce it.
    instr mstore Y, Z {
        { X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000, STEP, Z } is m_is_write { m_addr, m_step, m_value },
        // Wrap the addr value
        Y = (X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000) + wrap_bit * 2**32
    }

    // ============== control-flow instructions ==============

    instr jump l: label { pc' = l }
    instr load_label l: label -> X { X = l }
    instr jump_dyn X { pc' = X }
    // instr jump_and_link_dyn X { pc' = X, x1' = pc + 1 }
    // instr call l: label { pc' = l, x1' = pc + 1 }
    // instr ret { pc' = x1 }

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

    // ================= coprocessor substitution instructions =================
"# + &coprocessors.instructions()
        + r#"
    // Wraps a value in Y to 32 bits.
    // Requires 0 <= Y < 2**33
    instr wrap Y -> X { Y = X + wrap_bit * 2**32, X = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000 }
    // Requires -2**32 <= Y < 2**32
    instr wrap_signed Y -> X { Y + 2**32 = X + wrap_bit * 2**32, X = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000 }
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

    // Input is a 32 bit unsigned number. We check bit 7 and set all higher bits to that value.
    instr sign_extend_byte Y -> X {
        // wrap_bit is used as sign_bit here.
        Y = Y_7bit + wrap_bit * 0x80 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000,
        X = Y_7bit + wrap_bit * 0xffffff80
    }
    col fixed seven_bit(i) { i & 0x7f };
    col witness Y_7bit;
    { Y_7bit } in { seven_bit };

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
    { Y_b5 } in { bytes };
    { Y_b6 } in { bytes };
    { Y_b7 } in { bytes };
    { Y_b8 } in { bytes };

    col witness REM_b1;
    col witness REM_b2;
    col witness REM_b3;
    col witness REM_b4;
    { REM_b1 } in { bytes };
    { REM_b2 } in { bytes };
    { REM_b3 } in { bytes };
    { REM_b4 } in { bytes };

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

fn runtime(coprocessors: &CoProcessors) -> String {
    r#"
.globl __udivdi3@plt
.globl __udivdi3
.set __udivdi3@plt, __udivdi3

.globl __umoddi3@plt
.globl __umoddi3
.set __umoddi3@plt, __umoddi3

.globl memcpy@plt
.globl memcpy
.set memcpy@plt, memcpy

.globl memmove@plt
.globl memmove
.set memmove@plt, memmove

.globl memset@plt
.globl memset
.set memset@plt, memset

.globl memcmp@plt
.globl memcmp
.set memcmp@plt, memcmp

.globl bcmp@plt
.globl bcmp
.set bcmp@plt, bcmp

.globl strlen@plt
.globl strlen
.set strlen@plt, strlen

.globl __rust_alloc
.set __rust_alloc, __rg_alloc

.globl __rust_dealloc
.set __rust_dealloc, __rg_dealloc

.globl __rust_realloc
.set __rust_realloc, __rg_realloc

.globl __rust_alloc_zeroed
.set __rust_alloc_zeroed, __rg_alloc_zeroed

.globl __rust_alloc_error_handler
.set __rust_alloc_error_handler, __rg_oom
"#
    .to_owned()
        + &coprocessors.runtime()
}

fn process_statement(s: Statement, coprocessors: &CoProcessors) -> Vec<String> {
    match &s {
        Statement::Label(l) => vec![format!("{}::", escape_label(l))],
        Statement::Directive(directive, args) => match (directive.as_str(), &args[..]) {
            (
                ".loc",
                [Argument::Expression(Expression::Number(file)), Argument::Expression(Expression::Number(line)), Argument::Expression(Expression::Number(column)), ..],
            ) => {
                vec![format!("  debug loc {file} {line} {column};")]
            }
            (".file", _) => {
                // We ignore ".file" directives because they have been extracted to the top.
                vec![]
            }
            _ if directive.starts_with(".cfi_") => vec![],
            _ => panic!(
                "Leftover directive in code: {directive} {}",
                args.iter().format(", ")
            ),
        },
        Statement::Instruction(instr, args) => {
            let stmt_str = format!("{s}");
            // remove indentation and trailing newline
            let stmt_str = &stmt_str[2..(stmt_str.len() - 1)];
            let mut ret = vec![format!("  debug insn \"{stmt_str}\";")];
            ret.extend(
                process_instruction(instr, args, coprocessors)
                    .into_iter()
                    .map(|s| "  ".to_string() + &s),
            );
            ret
        }
    }
}

fn r(args: &[Argument]) -> Register {
    match args {
        [Argument::Register(r1)] => *r1,
        _ => panic!(),
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
        [Argument::Register(r1), Argument::Register(r2), l] => {
            (*r1, *r2, argument_to_escaped_symbol(l))
        }
        _ => panic!(),
    }
}

fn rl(args: &[Argument]) -> (Register, String) {
    match args {
        [Argument::Register(r1), l] => (*r1, argument_to_escaped_symbol(l)),
        _ => panic!(),
    }
}

fn rro(args: &[Argument]) -> (Register, Register, u32) {
    match args {
        [Argument::Register(r1), Argument::RegOffset(off, r2)] => (
            *r1,
            *r2,
            expression_to_number(off.as_ref().unwrap_or(&Expression::Number(0))),
        ),
        _ => panic!(),
    }
}

fn rrro(args: &[Argument]) -> (Register, Register, Register, u32) {
    match args {
        [Argument::Register(r1), Argument::Register(r2), Argument::RegOffset(off, r3)] => (
            *r1,
            *r2,
            *r3,
            expression_to_number(off.as_ref().unwrap_or(&Expression::Number(0))),
        ),
        _ => panic!(),
    }
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

fn try_coprocessor_substitution(label: &str, coprocessors: &CoProcessors) -> Option<String> {
    coprocessors
        .substitutions()
        .iter()
        .find(|(l, _)| *l == label)
        .map(|(_, subst)| subst.to_string())
}

fn load_1(reg: Register) -> String {
    format!("tmp1, tmp2 <== mload(reg_mem + {reg});")
}

fn load_2(reg1: Register, reg2: Register) -> Vec<String> {
    vec![
        format!("tmp1, tmp2 <== mload(reg_mem + {reg1});"),
        format!("tmp2, tmp3 <== mload(reg_mem + {reg2});"),
    ]
}

fn store(reg: Register, value: impl Display) -> Vec<String> {
    if reg.is_zero() {
        vec![]
    } else {
        vec![format!("mstore(reg_mem + {reg}, {value});")]
    }
}

/// Returns instructions that load from rs, run the operation and store in rd.
/// op is a function that receives the column containing the loaded value as argument
/// and is supposed to return a single expression.
/// Returns the empty vector if rd is zero.
fn load_op_store_1(args: &[Argument], op: impl Fn(&str) -> String) -> Vec<String> {
    load_op_store_1_v(args, |out, a| vec![format!("{out} <== {};", op(a))])
}

fn load_op_store_1_v(args: &[Argument], op: impl Fn(&str, &str) -> Vec<String>) -> Vec<String> {
    let (rd, rs) = rr(args);
    if rd.is_zero() {
        vec![]
    } else {
        [vec![load_1(rs)], op("tmp1", "tmp1"), store(rd, "tmp1")].concat()
    }
}

fn load_op_store_1imm(args: &[Argument], op: impl Fn(&str, u32) -> String) -> Vec<String> {
    load_op_store_1imm_v(args, |out, a, imm| {
        vec![format!("{out} <== {};", op(a, imm))]
    })
}

fn load_op_store_1imm_v(
    args: &[Argument],
    op: impl Fn(&str, &str, u32) -> Vec<String>,
) -> Vec<String> {
    let (rd, r1, imm) = rri(args);
    if rd.is_zero() {
        vec![]
    } else {
        [vec![load_1(r1)], op("tmp1", "tmp1", imm), store(rd, "tmp1")].concat()
    }
}

fn load_op_1l(args: &[Argument], op: impl Fn(&str, &str) -> String) -> Vec<String> {
    load_op_1l_v(args, |a, label| vec![format!("{};", op(a, label))])
}

fn load_op_1l_v(args: &[Argument], op: impl Fn(&str, &str) -> Vec<String>) -> Vec<String> {
    let (r1, label) = rl(args);
    [vec![load_1(r1)], op("tmp1", &label)].concat()
}

fn load_op_2l(args: &[Argument], op: impl Fn(&str, &str, &str) -> String) -> Vec<String> {
    load_op_2l_v(args, |a, b, label| vec![format!("{};", op(a, b, label))])
}

fn load_op_2l_v(args: &[Argument], op: impl Fn(&str, &str, &str) -> Vec<String>) -> Vec<String> {
    let (r1, r2, label) = rrl(args);
    [load_2(r1, r2), op("tmp1", "tmp2", &label)].concat()
}

/// Returns instructions that load from r1 and r2, run the operation and store in rd.
/// op is a function that receives the columns containing the loaded values as arguments
/// and is supposed to return a single expression.
/// Returns the empty vector if rd is zero.
fn load_op_store_2(args: &[Argument], op: impl Fn(&str, &str) -> String) -> Vec<String> {
    load_op_store_2_v(args, |out, a, b| vec![format!("{out} <== {}", op(a, b))])
}

/// Returns instructions that load from r1 and r2, run the operation and store in rd.
/// op is a function that receives the columns containing the loaded values as arguments
/// and is supposed to return a vector of instructions where the value to be written
/// is stored in tmp1.
/// Returns the empty vector if rd is zero.
fn load_op_store_2_v(
    args: &[Argument],
    op: impl Fn(&str, &str, &str) -> Vec<String>,
) -> Vec<String> {
    let (rd, r1, r2) = rrr(args);
    if rd.is_zero() {
        vec![]
    } else {
        [
            load_2(r1, r2),
            op("tmp1", "tmp1", "tmp2"),
            store(rd, "tmp1"),
        ]
        .concat()
    }
}

fn process_instruction(instr: &str, args: &[Argument], coprocessors: &CoProcessors) -> Vec<String> {
    match instr {
        // load/store registers
        "li" | "la" => {
            let (rd, imm) = ri(args);
            store(rd, imm)
        }
        // TODO check if it is OK to clear the lower order bits
        "lui" => {
            let (rd, imm) = ri(args);
            store(rd, imm << 12)
        }
        "mv" => load_op_store_1(args, |arg| arg.to_string()),

        // Arithmetic
        "add" => load_op_store_2(args, |a, b| format!("wrap({a} + {b})")),
        "addi" => load_op_store_1imm(args, |a, imm| format!("wrap({a} + {imm})")),
        "sub" => load_op_store_2(args, |a, b| format!("wrap_signed({a} - {b})")),
        "neg" => load_op_store_1(args, |a| format!("wrap_signed(0 - {a})")),
        "mul" => load_op_store_2_v(args, |o, a, b| {
            vec![format!("{o}, tmp2 <== mul({a}, {b});")]
        }),
        "mulhu" => load_op_store_2_v(args, |o, a, b| {
            vec![format!("tmp2, {o} <== mul({a}, {b});")]
        }),
        "mulhsu" => {
            load_op_store_2_v(args, |o, a, b| {
                assert_eq!([a, b], ["tmp1", "tmp2"]);
                // TODO this might not work. It is horrible.
                vec![
                    format!("tmp1 <== to_signed({a});"),
                    // tmp3 is 1 if tmp1 is non-negative
                    "tmp3 <== is_positive(tmp1 + 1);".into(),
                    // If negative, convert to positive
                    "skip_if_zero 0, tmp3;".into(),
                    "tmp1 <=X= 0 - tmp1;".into(),
                    format!("tmp1, tmp2 <== mul(tmp1, tmp2);"),
                    // If was negative before, convert back to negative
                    "skip_if_zero (1-tmp3), 2;".into(),
                    "tmp1 <== is_equal_zero(tmp1);".into(),
                    // If the lower bits are zero, return the two's complement,
                    // otherwise return one's complement.
                    format!("{o} <== wrap_signed(-tmp2 - 1 + tmp1);"),
                ]
            })
        }
        "divu" => load_op_store_2_v(args, |o, a, b| {
            vec![format!("{o}, tmp2 <== divremu({a}, {b});")]
        }),
        "remu" => load_op_store_2_v(args, |o, a, b| {
            vec![format!("tmp2, {o} <== divremu({a}, {b});")]
        }),

        // bitwise
        "xor" | "and" | "or" => load_op_store_2(args, |a, b| format!("{instr}({a}, {b})")),
        "xori" => load_op_store_1imm(args, |a, imm| format!("xor({a}, {imm})")),
        "andi" => load_op_store_1imm(args, |a, imm| format!("and({a}, {imm})")),
        "ori" => load_op_store_1imm(args, |a, imm| format!("or({a}, {imm})")),
        "not" => load_op_store_1(args, |a| format!("wrap_signed(-{a} - 1)")),

        // shift
        "slli" => load_op_store_1imm_v(args, |o, a, amount| {
            assert!(amount <= 31);
            if amount <= 16 {
                vec![format!("{o} <= wrap16({a} * {});", 1 << amount)]
            } else {
                vec![
                    format!("tmp1 <== wrap16({a} * {});", 1 << 16),
                    format!("{o} <== wrap16(tmp1 * {});", 1 << (amount - 16)),
                ]
            }
        }),
        "sll" => load_op_store_2_v(args, |o, a, b| {
            vec![
                format!("{b} <== and({b}, 0x1f);"),
                format!("{o} <== shl({a}, {b});"),
            ]
        }),
        "srli" => {
            // logical shift right
            load_op_store_1imm(args, |a, amount| {
                assert!(amount <= 31);
                format!("shr({a} * {amount})")
            })
        }
        "srl" => {
            // logical shift right
            load_op_store_2_v(args, |o, a, b| {
                vec![
                    format!("{b} <== and({b}, 0x1f);"),
                    format!("{o} <== shr({a}, {b});"),
                ]
            })
        }
        "srai" => {
            // arithmetic shift right
            // TODO see if we can implement this directly with a machine.
            // Now we are using the equivalence
            // a >>> b = (a >= 0 ? a >> b : ~(~a >> b))
            todo!();
            // let (rd, rs, amount) = rri(args);
            // assert!(amount <= 31);
            // only_if_no_write_to_zero_vec(
            //     vec![
            //         format!("tmp1 <== to_signed({rs});"),
            //         format!("tmp1 <== is_positive(0 - tmp1);"),
            //         format!("tmp1 <=X= tmp1 * 0xffffffff;"),
            //         // Here, tmp1 is the full bit mask if rs is negative
            //         // and zero otherwise.
            //         format!("{rd} <== xor(tmp1, {rs});"),
            //         format!("{rd} <== shr({rd}, {amount});"),
            //         format!("{rd} <== xor(tmp1, {rd});"),
            //     ],
            //     rd,
            // )
        }

        // comparison
        "seqz" => load_op_store_1_v(args, |o, a| vec![format!("{o} <=Y= is_equal_zero({a});")]),
        "snez" => load_op_store_1_v(args, |o, a| {
            vec![format!("{o} <=Y= is_not_equal_zero({a});")]
        }),
        "slti" => load_op_store_1imm_v(args, |o, a, imm| {
            vec![
                format!("tmp1 <== to_signed({a});"),
                format!("{o} <=Y= is_positive({} - tmp1);", imm as i32),
            ]
        }),
        "slt" => load_op_store_2_v(args, |o, a, b| {
            vec![
                format!("{a} <== to_signed({a});"),
                format!("{b} <== to_signed({b});"),
                format!("{o} <=Y= is_positive({b} - {a});"),
            ]
        }),
        "sltiu" => load_op_store_1imm_v(args, |o, a, imm| {
            vec![format!("{o} <=Y= is_positive({imm} - {a});")]
        }),
        "sltu" => load_op_store_2_v(args, |o, a, b| {
            vec![format!("{o} <=Y= is_positive({b} - {a});")]
        }),
        "sgtz" => load_op_store_1_v(args, |o, a| {
            vec![
                format!("tmp1 <== to_signed({a});"),
                format!("{o} <=Y= is_positive(tmp1);"),
            ]
        }),

        // branching
        "beq" => load_op_2l(args, |a, b, l| format!("branch_if_zero {a} - {b}, {l}")),
        "beqz" => load_op_1l(args, |a, l| format!("branch_if_zero {a}, {l}")),
        "bgeu" => {
            // TODO does this fulfill the input requirements for branch_if_positive?
            load_op_2l(args, |a, b, l| {
                format!("branch_if_positive {a} - {b} + 1, {l}")
            })
        }
        "bgez" => load_op_1l_v(args, |a, l| {
            vec![
                format!("tmp1 <== to_signed({a});"),
                format!("branch_if_positive tmp1 + 1, {l};"),
            ]
        }),
        "bltu" => load_op_2l(args, |a, b, l| format!("branch_if_positive {a} - {b}, {l}")),
        "blt" => load_op_2l_v(args, |a, b, l| {
            // Branch if a < b (signed).
            // TODO does this fulfill the input requirements for branch_if_positive?
            vec![
                format!("{a} <== to_signed({a});"),
                format!("{b} <== to_signed({b});"),
                format!("branch_if_positive {b} - {a}, {l};"),
            ]
        }),
        "bge" => load_op_2l_v(args, |a, b, l| {
            // Branch if a >= b (signed).
            // TODO does this fulfill the input requirements for branch_if_positive?
            vec![
                format!("{a} <== to_signed({a});"),
                format!("{b} <== to_signed({b});"),
                format!("branch_if_positive {a} - {b} + 1, {l};"),
            ]
        }),
        "bltz" => load_op_1l(args, |a, l| {
            // branch if 2**31 <= a < 2**32
            format!("branch_if_positive {a} - 2**31 + 1, {l}")
        }),

        "blez" => load_op_1l_v(args, |a, l| {
            // branch less or equal zero
            vec![
                format!("{a} <== to_signed({a});"),
                format!("branch_if_positive -{a} + 1, {l};"),
            ]
        }),
        "bgtz" => load_op_1l_v(args, |a, l| {
            // branch if 0 < r1 < 2**31
            vec![
                format!("{a} <== to_signed({a});"),
                format!("branch_if_positive {a}, {l};"),
            ]
        }),
        "bne" => load_op_2l_v(args, |a, b, l| {
            vec![format!("branch_if_nonzero {a} - {b}, {l}")]
        }),
        "bnez" => load_op_1l(args, |a, l| format!("branch_if_nonzero {a}, {l}")),

        // jump and call
        "j" => {
            if let [label] = args {
                vec![format!("jump {};", argument_to_escaped_symbol(label))]
            } else {
                panic!()
            }
        }
        "jr" => vec![load_1(r(args)), format!("jump_dyn tmp1;")],
        "jal" => {
            let (_rd, _label) = rl(args);
            todo!();
        }
        "jalr" => {
            // TODO there is also a form that takes more arguments
            // instr jump_and_link_dyn X { pc' = X, x1' = pc + 1 }
            let rs = r(args);
            // TODO check that "pc + 2" is correct!
            [
                vec![load_1(rs)],
                store(Register::new(1), "pc + 2"),
                vec![format!("jump_dyn tmp1;")],
            ]
            .concat()
        }
        "call" | "tail" => {
            // Depending on what symbol is called, the call is replaced by a
            // powdr-asm call, or a call to a coprocessor if a special function
            // has been recognized.
            assert_eq!(args.len(), 1);
            let label = &args[0];
            let replacement = match label {
                Argument::Expression(Expression::Symbol(l)) => {
                    try_coprocessor_substitution(l, coprocessors)
                }
                _ => None,
            };
            match (replacement, instr) {
                (None, instr) => {
                    // instr call l: label { pc' = l, x1' = pc + 1 }
                    [
                        if instr == "call" {
                            store(Register::new(1), "pc + 2")
                        } else {
                            // tail
                            vec![]
                        },
                        vec![format!("jump {};", argument_to_escaped_symbol(label))],
                    ]
                    .concat()
                }
                // Both "call" and "tail" are pseudoinstructions that are
                // supposed to use x6 to calculate the high bits of the
                // destination address. Our implementation does not touch x6,
                // but no sane program would rely on this behavior, so we are
                // probably fine.
                (Some(replacement), "call") => vec![replacement],
                (Some(replacement), "tail") => vec![
                    replacement,
                    load_1(Register::new(1)),
                    "jump_dyn tmp1;".to_string(),
                ],
                (Some(_), _) => unreachable!(),
            }
        }
        "ecall" => {
            assert!(args.is_empty());
            let x10 = Register::new(10);
            [vec![load_1(x10)], store(x10, "${ (\"input\", tmp1) }")].concat()
        }
        "ebreak" => {
            assert!(args.is_empty());
            // TODO is it OK to just store this in tmp1?
            // Will it get optimized away?
            let x10 = Register::new(10);
            vec![
                load_1(x10),
                "tmp1 <=X= ${ (\"print_ch\", tmp1) };".to_string(),
            ]
        }
        "ret" => {
            assert!(args.is_empty());
            vec![load_1(Register::new(1)), "jump_dyn tmp1;".to_string()]
        }

        // memory access
        "lw" => {
            let (rd, rs, off) = rro(args);
            // TODO we need to consider misaligned loads / stores
            only_if_no_write_to_zero_vec(vec![format!("{rd}, tmp1 <== mload({rs} + {off});")], rd)
        }
        "lb" => {
            // load byte and sign-extend. the memory is little-endian.
            let (rd, rs, off) = rro(args);
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
            let (rd, rs, off) = rro(args);
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
            let (rd, rs, off) = rro(args);
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
            let (rd, rs, off) = rro(args);
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
            let (r1, r2, off) = rro(args);
            vec![format!("mstore {r2} + {off}, {r1};")]
        }
        "sh" => {
            // store half word (two bytes)
            // TODO this code assumes it is at least aligned on
            // a two-byte boundary

            let (rs, rd, off) = rro(args);
            vec![
                format!("tmp1, tmp2 <== mload({rd} + {off});"),
                "tmp3 <== shl(0xffff, 8 * tmp2);".to_string(),
                "tmp3 <== xor(tmp3, 0xffffffff);".to_string(),
                "tmp1 <== and(tmp1, tmp3);".to_string(),
                format!("tmp3 <== and({rs}, 0xffff);"),
                "tmp3 <== shl(tmp3, 8 * tmp2);".to_string(),
                "tmp1 <== or(tmp1, tmp3);".to_string(),
                format!("mstore {rd} + {off} - tmp2, tmp1;"),
            ]
        }
        "sb" => {
            // store byte
            let (rs, rd, off) = rro(args);
            vec![
                format!("tmp1, tmp2 <== mload({rd} + {off});"),
                "tmp3 <== shl(0xff, 8 * tmp2);".to_string(),
                "tmp3 <== xor(tmp3, 0xffffffff);".to_string(),
                "tmp1 <== and(tmp1, tmp3);".to_string(),
                format!("tmp3 <== and({rs}, 0xff);"),
                "tmp3 <== shl(tmp3, 8 * tmp2);".to_string(),
                "tmp1 <== or(tmp1, tmp3);".to_string(),
                format!("mstore {rd} + {off} - tmp2, tmp1;"),
            ]
        }
        "nop" => vec![],
        "unimp" => vec!["fail;".to_string()],

        // Special instruction that is inserted to allow dynamic label references
        "load_dynamic" => {
            let (rd, label) = rl(args);
            only_if_no_write_to_zero(format!("{rd} <== load_label({label});"), rd)
        }

        // atomic and synchronization
        "fence" | "fence.i" => vec![],

        insn if insn.starts_with("amoadd.w") => {
            let (rd, rs2, rs1, off) = rrro(args);
            assert_eq!(off, 0);

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
            let (rd, rs, off) = rro(args);
            assert_eq!(off, 0);
            // TODO misaligned access should raise misaligned address exceptions
            let mut statments =
                only_if_no_write_to_zero_vec(vec![format!("{rd}, tmp1 <== mload({rs});")], rd);
            statments.push("lr_sc_reservation <=X= 1;".into());
            statments
        }

        insn if insn.starts_with("sc.w") => {
            // Some overlap with "sw", but also writes 0 to rd on success
            let (rd, rs2, rs1, off) = rrro(args);
            assert_eq!(off, 0);
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
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_remove_matching_and_next_integers() {
        assert_eq!(
            remove_matching_and_next([0, 1, 2, 0, 2, 0, 0, 3, 2, 1].iter(), |&&i| { i == 0 })
                .copied()
                .collect::<Vec<_>>(),
            vec![2, 2, 1]
        );
    }

    #[test]
    fn test_remove_matching_and_next_strings() {
        assert_eq!(
            remove_matching_and_next(
                [
                    "croissant",
                    "pain au chocolat",
                    "chausson aux pommes",
                    "croissant" // corner case: if the label is at the end of the program
                ]
                .iter(),
                |&&s| { s == "croissant" }
            )
            .copied()
            .collect::<Vec<_>>(),
            vec!["chausson aux pommes"]
        );
    }
}
