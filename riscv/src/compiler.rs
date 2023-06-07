use std::collections::{BTreeMap, BTreeSet};

use itertools::Itertools;

use crate::data_parser::{self, DataValue};
use crate::parser::{self, Argument, Register, Statement, UnaryOpKind};
use crate::{disambiguator, reachability};

use super::parser::Expression;

/// Compiles riscv assembly to POWDR assembly. Adds required library routines.
pub fn compile_riscv_asm(mut assemblies: BTreeMap<String, String>) -> String {
    // stack grows towards zero
    let stack_start = 0x10000;
    // data grows away from zero
    let data_start = 0x20000;

    assert!(assemblies
        .insert("__runtime".to_string(), runtime().to_string())
        .is_none());

    let mut statements = disambiguator::disambiguate(
        assemblies
            .into_iter()
            .map(|(name, contents)| (name, parser::parse_asm(&contents)))
            .collect(),
    );
    let (mut objects, object_order) = data_parser::extract_data_objects(&statements);

    // Reduce to the code that is actually reachable from main
    // (and the objects that are referred from there)
    reachability::filter_reachable_from("__runtime_start", &mut statements, &mut objects);

    // Replace dynamic references to code labels
    replace_dynamic_label_references(&mut statements, &objects);

    // Sort the objects according to the order of the names in object_order.
    let sorted_objects = object_order
        .into_iter()
        .filter_map(|n| {
            let value = objects.get_mut(&n).map(std::mem::take);
            value.map(|v| (n, v))
        })
        .collect::<Vec<_>>();
    let (data_code, data_positions) = store_data_objects(&sorted_objects, data_start);

    preamble()
        + &vec!["call __data_init;".to_string()]
            .into_iter()
            .chain([
                format!("// Set stack pointer\nx2 <=X= {stack_start};"),
                "jump __runtime_start;".to_string(),
            ])
            .chain(
                substitute_symbols_with_values(statements, &data_positions)
                    .into_iter()
                    .flat_map(process_statement),
            )
            .chain(["// This is the data initialization routine.\n__data_init::".to_string()])
            .chain(data_code)
            .chain(["// This is the end of the data initialization routine.\nret;".to_string()])
            .enumerate()
            .map(|(i, line)| {
                if i % 10 == 0 {
                    format!("// PC: {i}\n{line}")
                } else {
                    line
                }
            })
            .join("\n")
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

    s10 <=X= load_label(LABEL)

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
    let Statement::Instruction(instr1, args1) = s1 else { return None; };
    let Statement::Instruction(instr2, args2) = s2 else { return None; };
    if instr1.as_str() != "lui" || instr2.as_str() != "addi" {
        return None;
    };
    let [Argument::Register(r1), Argument::Expression(Expression::UnaryOp(UnaryOpKind::HiDataRef, expr1))] = &args1[..] else { return None; };
    // Maybe should try to reduce expr1 and expr2 before comparing deciding it is a pure symbol?
    let Expression::Symbol(label1) = &expr1[0] else { return None; };
    let [Argument::Register(r2), Argument::Register(r3), Argument::Expression(Expression::UnaryOp(UnaryOpKind::LoDataRef, expr2))] = &args2[..] else { return None; };
    let Expression::Symbol(label2) = &expr2[0] else { return None; };
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

fn store_data_objects<'a>(
    objects: impl IntoIterator<Item = &'a (String, Vec<DataValue>)> + Copy,
    mut memory_start: u32,
) -> (Vec<String>, BTreeMap<String, u32>) {
    memory_start = ((memory_start + 7) / 8) * 8;
    let mut current_pos = memory_start;
    let mut positions = BTreeMap::new();
    for (name, data) in objects.into_iter() {
        // TODO check if we need to use multiples of four.
        let size: u32 = data
            .iter()
            .map(|d| next_multiple_of_four(d.size()) as u32)
            .sum();
        positions.insert(name.clone(), current_pos);
        current_pos += size;
    }

    let code = objects
        .into_iter()
        .filter(|(_, data)| !data.is_empty())
        .flat_map(|(name, data)| {
            let mut object_code = vec![];
            let mut pos = positions[name];
            for item in data {
                match &item {
                    DataValue::Zero(_length) => {
                        // We can assume memory to be zero-initialized,
                        // so we do nothing.
                    }
                    DataValue::Direct(bytes) => {
                        for i in (0..bytes.len()).step_by(4) {
                            let v = (0..4)
                                .map(|j| {
                                    (bytes.get(i + j).cloned().unwrap_or_default() as u32)
                                        << (j * 8)
                                })
                                .reduce(|a, b| a | b)
                                .unwrap();
                            // We can assume memory to be zero-initialized.
                            if v != 0 {
                                object_code.extend([
                                    format!("addr <=X= 0x{:x};", pos + i as u32),
                                    format!("mstore 0x{v:x};"),
                                ]);
                            }
                        }
                    }
                    DataValue::Reference(sym) => {
                        object_code.push(format!("addr <=X= 0x{pos:x};"));
                        if let Some(p) = positions.get(sym) {
                            object_code.push(format!("mstore 0x{p:x};"));
                        } else {
                            // code reference
                            // TODO should be possible without temporary
                            object_code.extend([
                                format!("tmp1 <=X= load_label({});", escape_label(sym)),
                                "mstore tmp1;".to_string(),
                            ]);
                        }
                    }
                }
                pos += item.size() as u32;
            }
            if let Some(first_line) = object_code.first_mut() {
                *first_line = format!("// data {name}\n") + first_line;
            }
            object_code
        })
        .collect();
    (code, positions)
}

fn next_multiple_of_four(x: usize) -> usize {
    ((x + 3) / 4) * 4
}

fn substitute_symbols_with_values(
    mut statements: Vec<Statement>,
    data_positions: &BTreeMap<String, u32>,
) -> Vec<Statement> {
    for s in &mut statements {
        let Statement::Instruction(_name, args) = s else { continue; };
        for arg in args {
            arg.post_visit_expressions_mut(&mut |expression| match expression {
                Expression::Number(_) => {}
                Expression::Symbol(symb) => {
                    if let Some(pos) = data_positions.get(symb) {
                        *expression = Expression::Number(*pos as i64)
                    }
                }
                Expression::UnaryOp(op, subexpr) => {
                    if let Expression::Number(num) = subexpr[0] {
                        let result = match op {
                            UnaryOpKind::HiDataRef => num >> 12,
                            UnaryOpKind::LoDataRef => num & 0xfff,
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
                            parser::BinaryOpKind::Or => a | b,
                            parser::BinaryOpKind::Xor => a ^ b,
                            parser::BinaryOpKind::And => a & b,
                            parser::BinaryOpKind::LeftShift => a << b,
                            parser::BinaryOpKind::RightShift => a >> b,
                            parser::BinaryOpKind::Add => a + b,
                            parser::BinaryOpKind::Sub => a - b,
                            parser::BinaryOpKind::Mul => a * b,
                            parser::BinaryOpKind::Div => a / b,
                            parser::BinaryOpKind::Mod => a % b,
                        };
                        *expression = Expression::Number(result);
                    }
                }
            });
        }
    }
    statements
}

fn preamble() -> String {
    r#"
degree 262144;
reg pc[@pc];
reg X[<=];
reg Y[<=];
reg Z[<=];
reg tmp1;
reg tmp2;
reg tmp3;
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
instr load_label l: label -> X { X = l }
instr jump_dyn X { pc' = X }
instr jump_and_link_dyn X { pc' = X, x1' = pc + 1 }
instr call l: label { pc' = l, x1' = pc + 1 }
// TODO x6 actually stores some relative address, but only part of it.
instr tail l: label { pc' = l, x6' = l }
instr ret { pc' = x1 }

instr branch_if_nonzero X, l: label { pc' = (1 - XIsZero) * l + XIsZero * (pc + 1) }
instr branch_if_zero X, l: label { pc' = XIsZero * l + (1 - XIsZero) * (pc + 1) }

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

// ================= binary/bitwise instructions =================

instr and Y, Z -> X {
    {Y, Z, X, 0} in binary_RESET { binary_A, binary_B, binary_C, binary_operation }
}

instr or Y, Z -> X {
    {Y, Z, X, 1} in binary_RESET { binary_A, binary_B, binary_C, binary_operation }
}

instr xor Y, Z -> X {
    {Y, Z, X, 2} in binary_RESET { binary_A, binary_B, binary_C, binary_operation }
}

pil{
	macro is_nonzero(X) { match X { 0 => 0, _ => 1, } };
	macro is_zero(X) { 1 - is_nonzero(X) };

	col fixed binary_RESET(i) { is_zero((i % 4) - 3) };
	col fixed binary_FACTOR(i) { 1 << (((i + 1) % 4) * 8) };

	col fixed binary_P_A(i) { i % 256 };
	col fixed binary_P_B(i) { (i >> 8) % 256 };
    col fixed binary_P_operation(i) { (i / (256 * 256)) % 3 };
	col fixed binary_P_C(i) {
        match binary_P_operation(i) {
            0 => binary_P_A(i) & binary_P_B(i),
            1 => binary_P_A(i) | binary_P_B(i),
            2 => binary_P_A(i) ^ binary_P_B(i),
        } & 0xff
    };

	col witness binary_A_byte;
	col witness binary_B_byte;
	col witness binary_C_byte;

	col witness binary_A;
	col witness binary_B;
	col witness binary_C;
    col witness binary_operation;

	binary_A' = binary_A * (1 - binary_RESET) + binary_A_byte * binary_FACTOR;
	binary_B' = binary_B * (1 - binary_RESET) + binary_B_byte * binary_FACTOR;
	binary_C' = binary_C * (1 - binary_RESET) + binary_C_byte * binary_FACTOR;
	(binary_operation' - binary_operation) * (1 - binary_RESET) = 0;

	{binary_operation', binary_A_byte, binary_B_byte, binary_C_byte} in {binary_P_operation, binary_P_A, binary_P_B, binary_P_C};
}

// ================= shift instructions =================

instr shl Y, Z -> X {
    {Y, Z, X, 0} in shift_RESET { shift_A, shift_B, shift_C, shift_operation }
}

instr shr Y, Z -> X {
    {Y, Z, X, 1} in shift_RESET { shift_A, shift_B, shift_C, shift_operation }
}

pil{
	col fixed shift_RESET(i) { is_zero((i % 4) - 3) };
	col fixed shift_FACTOR_ROW(i) { (i + 1) % 4 };
	col fixed shift_FACTOR(i) { 1 << (((i + 1) % 4) * 8) };

	col fixed shift_P_A(i) { i % 256 };
	col fixed shift_P_B(i) { (i / 256) % 32 };
    col fixed shift_P_ROW(i) { (i / (256 * 32)) % 4 };
    col fixed shift_P_operation(i) { (i / (256 * 32 * 4)) % 2 };
	col fixed shift_P_C(i) {
        match shift_P_operation(i) {
            0 => (shift_P_A(i) << (shift_P_B(i) + (shift_P_ROW(i) * 8))),
            1 => (shift_P_A(i) << (shift_P_ROW(i) * 8)) >> shift_P_B(i),
        } & 0xffffffff
    };

	col witness shift_A_byte;
	col witness shift_C_part;

	col witness shift_A;
	col witness shift_B;
	col witness shift_C;
    col witness shift_operation;

	shift_A' = shift_A * (1 - shift_RESET) + shift_A_byte * shift_FACTOR;
	(shift_B' - shift_B) * (1 - shift_RESET) = 0;
	shift_C' = shift_C * (1 - shift_RESET) + shift_C_part;
	(shift_operation' - shift_operation) * (1 - shift_RESET) = 0;

    // TODO this way, we cannot prove anything that shifts by more than 31 bits.
	{shift_operation', shift_A_byte, shift_B', shift_FACTOR_ROW, shift_C_part} in {shift_P_operation, shift_P_A, shift_P_B, shift_P_ROW, shift_P_C};
}

// ================== wrapping instructions ==============

// Wraps a value in Y to 32 bits.
// Requires 0 <= Y < 2**33
instr wrap Y -> X { Y = X + wrap_bit * 2**32, X = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000 }
// Requires -2**32 <= Y < 2**32
instr wrap_signed Y -> X { Y + 2**32 = X + wrap_bit * 2**32, X = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000 }
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

// Input is a 32 bit unsigned number. We check the 7th bit and set all higher bits to that value.
instr sign_extend_byte Y -> X {
    // wrap_bit is used as sign_bit here.
    Y = Y_7bit + wrap_bit * 0x80 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000,
    X = Y_7bit + wrap_bit * 0xffffff80
}
pil{
    col fixed seven_bit(i) { i & 0x7f };
    col witness Y_7bit;
    { Y_7bit } in { seven_bit };
}

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
pil{
    col witness Y_b5;
    col witness Y_b6;
    { Y_b5 } in { bytes };
    { Y_b6 } in { bytes };
}

// Removes up to 32 bits beyond 32
// TODO is this really safe?
instr mul Y, Z -> X {
    Y * Z = X + Y_b5 * 2**32 + Y_b6 * 2**40 + Y_b7 * 2**48 + Y_b8 * 2**56,
    X = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000
}
// implements (Y * Z) >> 32
instr mulhu Y, Z -> X {
    Y * Z = X * 2**32 + Y_b5 + Y_b6 * 0x100 + Y_b7 * 0x10000 + Y_b8 * 0x1000000,
    X = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000
}
pil{
    col witness Y_b7;
    col witness Y_b8;
    { Y_b7 } in { bytes };
    { Y_b8 } in { bytes };
}
    "#
}

fn runtime() -> &'static str {
    r#"
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
}

fn process_statement(s: Statement) -> Vec<String> {
    match &s {
        Statement::Label(l) => vec![format!("{}::", escape_label(l))],
        Statement::Directive(_, _) => panic!(""),
        Statement::Instruction(instr, args) => process_instruction(instr, args)
            .into_iter()
            .map(|s| "  ".to_string() + &s)
            .collect(),
    }
}

fn escape_label(l: &str) -> String {
    // TODO make this proper
    l.replace('.', "_dot_").replace('/', "_slash_")
}

fn argument_to_number(x: &Argument) -> u32 {
    if let Argument::Expression(expr) = x {
        expression_to_number(expr)
    } else {
        panic!("Expected numeric expression, got {x}")
    }
}

fn expression_to_number(expr: &Expression) -> u32 {
    if let Expression::Number(n) = expr {
        *n as u32
    } else {
        panic!("Constant expression could not be fully resolved to a number during preprocessing: {expr}");
    }
}

fn argument_to_escaped_symbol(x: &Argument) -> String {
    if let Argument::Expression(Expression::Symbol(symb)) = x {
        escape_label(symb)
    } else {
        panic!("Expected a symbol, got {x}");
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
        [Argument::Register(r1), Argument::RegOffset(r2, off)] => {
            (*r1, *r2, expression_to_number(off))
        }
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

fn process_instruction(instr: &str, args: &[Argument]) -> Vec<String> {
    match instr {
        // load/store registers
        "li" => {
            let (rd, imm) = ri(args);
            only_if_no_write_to_zero(format!("{rd} <=X= {imm};"), rd)
        }
        // TODO check if it is OK to clear the lower order bits
        "lui" => {
            let (rd, imm) = ri(args);
            only_if_no_write_to_zero(format!("{rd} <=X= {};", imm << 12), rd)
        }
        "la" => {
            let (rd, addr) = ri(args);
            only_if_no_write_to_zero(format!("{rd} <=X= {};", addr), rd)
        }
        "mv" => {
            let (rd, rs) = rr(args);
            only_if_no_write_to_zero(format!("{rd} <=X= {rs};"), rd)
        }

        // Arithmetic
        "add" => {
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero(format!("{rd} <=X= wrap({r1} + {r2});"), rd)
        }
        "addi" => {
            let (rd, rs, imm) = rri(args);
            only_if_no_write_to_zero(format!("{rd} <=X= wrap({rs} + {imm});"), rd)
        }
        "sub" => {
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero(format!("{rd} <=X= wrap_signed({r1} - {r2});"), rd)
        }
        "neg" => {
            let (rd, r1) = rr(args);
            only_if_no_write_to_zero(format!("{rd} <=X= wrap_signed(0 - {r1});"), rd)
        }
        "mul" => {
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero(format!("{rd} <=X= mul({r1}, {r2});"), rd)
        }
        "mulhu" => {
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero(format!("{rd} <=X= mulhu({r1}, {r2});"), rd)
        }

        // bitwise
        "xor" => {
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero(format!("{rd} <=X= xor({r1}, {r2});"), rd)
        }
        "xori" => {
            let (rd, r1, imm) = rri(args);
            only_if_no_write_to_zero(format!("{rd} <=X= xor({r1}, {imm});"), rd)
        }
        "and" => {
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero(format!("{rd} <=X= and({r1}, {r2});"), rd)
        }
        "andi" => {
            let (rd, r1, imm) = rri(args);
            only_if_no_write_to_zero(format!("{rd} <=X= and({r1}, {imm});"), rd)
        }
        "or" => {
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero(format!("{rd} <=X= or({r1}, {r2});"), rd)
        }
        "ori" => {
            let (rd, r1, imm) = rri(args);
            only_if_no_write_to_zero(format!("{rd} <=X= or({r1}, {imm});"), rd)
        }
        "not" => {
            let (rd, rs) = rr(args);
            only_if_no_write_to_zero(format!("{rd} <=X= wrap_signed(-{rs} - 1);"), rd)
        }

        // shift
        "slli" => {
            let (rd, rs, amount) = rri(args);
            assert!(amount <= 31);
            only_if_no_write_to_zero_vec(
                if amount <= 16 {
                    vec![format!("{rd} <=X= wrap16({rs} * {});", 1 << amount)]
                } else {
                    vec![
                        format!("tmp1 <=X= wrap16({rs} * {});", 1 << 16),
                        format!("{rd} <=X= wrap16(tmp1 * {});", 1 << (amount - 16)),
                    ]
                },
                rd,
            )
        }
        "sll" => {
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero_vec(
                vec![
                    format!("tmp1 <=X= and({r2}, 0x1f);"),
                    format!("{rd} <=X= shl({r1}, tmp1);"),
                ],
                rd,
            )
        }
        "srli" => {
            // logical shift right
            let (rd, rs, amount) = rri(args);
            assert!(amount <= 31);
            only_if_no_write_to_zero(format!("{rd} <=X= shr({rs}, {amount});"), rd)
        }
        "srl" => {
            // logical shift right
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero_vec(
                vec![
                    format!("tmp1 <=X= and({r2}, 0x1f);"),
                    format!("{rd} <=X= shr({r1}, tmp1);"),
                ],
                rd,
            )
        }
        "srai" => {
            // arithmetic shift right
            // TODO see if we can implement this directly with a machine.
            // Now we are using the equivalence
            // a >>> b = (a >= 0 ? a >> b : ~(~a >> b))
            let (rd, rs, amount) = rri(args);
            assert!(amount <= 31);
            only_if_no_write_to_zero_vec(
                vec![
                    format!("tmp1 <=X= to_signed({rs});"),
                    format!("tmp1 <=Y= is_positive(0 - tmp1);"),
                    format!("tmp1 <=X= tmp1 * 0xffffffff;"),
                    // Here, tmp1 is the full bit mask if rs is negative
                    // and zero otherwise.
                    format!("{rd} <=X= xor(tmp1, {rs});"),
                    format!("{rd} <=X= shr({rd}, {amount});"),
                    format!("{rd} <=X= xor(tmp1, {rd});"),
                ],
                rd,
            )
        }

        // comparison
        "seqz" => {
            let (rd, rs) = rr(args);
            only_if_no_write_to_zero(format!("{rd} <=Y= is_equal_zero({rs});"), rd)
        }
        "snez" => {
            let (rd, rs) = rr(args);
            only_if_no_write_to_zero(format!("{rd} <=Y= is_not_equal_zero({rs});"), rd)
        }
        "slti" => {
            let (rd, rs, imm) = rri(args);
            only_if_no_write_to_zero_vec(
                vec![
                    format!("tmp1 <=X= to_signed({rs});"),
                    format!("{rd} <=Y= is_positive({} - tmp1);", imm as i32),
                ],
                rd,
            )
        }
        "slt" => {
            let (rd, r1, r2) = rrr(args);
            vec![
                format!("tmp1 <=X= to_signed({r1});"),
                format!("tmp2 <=X= to_signed({r2});"),
                format!("{rd} <=Y= is_positive(tmp2 - tmp1);"),
            ]
        }
        "sltiu" => {
            let (rd, rs, imm) = rri(args);
            only_if_no_write_to_zero(format!("{rd} <=Y= is_positive({imm} - {rs});"), rd)
        }
        "sltu" => {
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero(format!("{rd} <=Y= is_positive({r2} - {r1});"), rd)
        }
        "sgtz" => {
            let (rd, rs) = rr(args);
            vec![
                format!("tmp1 <=X= to_signed({rs});"),
                format!("{rd} <=Y= is_positive(tmp1);"),
            ]
        }

        // branching
        "beq" => {
            let (r1, r2, label) = rrl(args);
            vec![format!("branch_if_zero {r1} - {r2}, {label};")]
        }
        "beqz" => {
            let (r1, label) = rl(args);
            vec![format!("branch_if_zero {r1}, {label};")]
        }
        "bgeu" => {
            let (r1, r2, label) = rrl(args);
            // TODO does this fulfill the input requirements for branch_if_positive?
            vec![format!("branch_if_positive {r1} - {r2} + 1, {label};")]
        }
        "bgez" => {
            let (r1, label) = rl(args);
            vec![
                format!("tmp1 <=X= to_signed({r1});"),
                format!("branch_if_positive tmp1 + 1, {label};"),
            ]
        }
        "bltu" => {
            let (r1, r2, label) = rrl(args);
            vec![format!("branch_if_positive {r2} - {r1}, {label};")]
        }
        "blt" => {
            let (r1, r2, label) = rrl(args);
            // Branch if r1 < r2 (signed).
            // TODO does this fulfill the input requirements for branch_if_positive?
            vec![
                format!("tmp1 <=X= to_signed({r1});"),
                format!("tmp2 <=X= to_signed({r2});"),
                format!("branch_if_positive tmp2 - tmp1, {label};"),
            ]
        }
        "bge" => {
            let (r1, r2, label) = rrl(args);
            // Branch if r1 >= r2 (signed).
            // TODO does this fulfill the input requirements for branch_if_positive?
            vec![
                format!("tmp1 <=X= to_signed({r1});"),
                format!("tmp2 <=X= to_signed({r2});"),
                format!("branch_if_positive tmp1 - tmp2 + 1, {label};"),
            ]
        }
        "bltz" => {
            // branch if 2**31 <= r1 < 2**32
            let (r1, label) = rl(args);
            vec![format!("branch_if_positive {r1} - 2**31 + 1, {label};")]
        }

        "blez" => {
            // branch less or equal zero
            let (r1, label) = rl(args);
            vec![
                format!("tmp1 <=X= to_signed({r1});"),
                format!("branch_if_positive -tmp1 + 1, {label};"),
            ]
        }
        "bgtz" => {
            // branch if 0 < r1 < 2**31
            let (r1, label) = rl(args);
            vec![
                format!("tmp1 <=X= to_signed({r1});"),
                format!("branch_if_positive tmp1, {label};"),
            ]
        }
        "bne" => {
            let (r1, r2, label) = rrl(args);
            vec![format!("branch_if_nonzero {r1} - {r2}, {label};")]
        }
        "bnez" => {
            let (r1, label) = rl(args);
            vec![format!("branch_if_nonzero {r1}, {label};")]
        }

        // jump and call
        "j" => {
            if let [label] = args {
                vec![format!("jump {};", argument_to_escaped_symbol(label))]
            } else {
                panic!()
            }
        }
        "jr" => {
            let rs = r(args);
            vec![format!("jump_dyn {rs};")]
        }
        "jal" => {
            let (_rd, _label) = rl(args);
            todo!();
        }
        "jalr" => {
            // TODO there is also a form that takes more arguments
            let rs = r(args);
            vec![format!("jump_and_link_dyn {rs};")]
        }
        "call" => {
            if let [label] = args {
                vec![format!("call {};", argument_to_escaped_symbol(label))]
            } else {
                panic!()
            }
        }
        "ecall" => {
            assert!(args.is_empty());
            vec!["x10 <=X= ${ (\"input\", x10) };".to_string()]
        }
        "ebreak" => {
            assert!(args.is_empty());
            // This is using x0 on purpose, because we do not want to introduce
            // nondeterminism with this.
            vec!["x0 <=X= ${ (\"print_char\", x10) };\n".to_string()]
        }
        "tail" => {
            if let [label] = args {
                vec![format!("tail {};", argument_to_escaped_symbol(label))]
            } else {
                panic!()
            }
        }
        "ret" => {
            assert!(args.is_empty());
            vec!["ret;".to_string()]
        }

        // memory access
        "lw" => {
            let (rd, rs, off) = rro(args);
            // TODO we need to consider misaligned loads / stores
            only_if_no_write_to_zero_vec(
                vec![
                    format!("addr <=X= wrap({rs} + {off});"),
                    format!("{rd} <=X= mload();"),
                ],
                rd,
            )
        }
        "lb" => {
            // load byte and sign-extend. the memory is little-endian.
            let (rd, rs, off) = rro(args);
            only_if_no_write_to_zero_vec(
                vec![
                    format!("tmp1 <=X= wrap({rs} + {off});"),
                    "addr <=X= and(tmp1, 0xfffffffc);".to_string(),
                    "tmp2 <=X= and(tmp1, 0x3);".to_string(),
                    format!("{rd} <=X= mload();"),
                    format!("{rd} <=X= shr({rd}, 8 * tmp2);"),
                    format!("{rd} <=X= sign_extend_byte({rd});"),
                ],
                rd,
            )
        }
        "lbu" => {
            // load byte and zero-extend. the memory is little-endian.
            let (rd, rs, off) = rro(args);
            only_if_no_write_to_zero_vec(
                vec![
                    format!("tmp1 <=X= wrap({rs} + {off});"),
                    "addr <=X= and(tmp1, 0xfffffffc);".to_string(),
                    "tmp2 <=X= and(tmp1, 0x3);".to_string(),
                    format!("{rd} <=X= mload();"),
                    format!("{rd} <=X= shr({rd}, 8 * tmp2);"),
                    format!("{rd} <=X= and({rd}, 0xff);"),
                ],
                rd,
            )
        }
        "sw" => {
            let (r1, r2, off) = rro(args);
            vec![
                format!("addr <=X= wrap({r2} + {off});"),
                format!("mstore {r1};"),
            ]
        }
        "sh" => {
            // store half word (two bytes)
            // TODO this code assumes it is at least aligned on
            // a two-byte boundary

            let (rs, rd, off) = rro(args);
            vec![
                format!("tmp1 <=X= wrap({rd} + {off});"),
                "addr <=X= and(tmp1, 0xfffffffc);".to_string(),
                "tmp2 <=X= and(tmp1, 0x3);".to_string(),
                "tmp1 <=X= mload();".to_string(),
                "tmp3 <=X= shl(0xffff, 8 * tmp2);".to_string(),
                "tmp3 <=X= xor(tmp3, 0xffffffff);".to_string(),
                "tmp1 <=X= and(tmp1, tmp3);".to_string(),
                format!("tmp3 <=X= and({rs}, 0xffff);"),
                "tmp3 <=X= shl(tmp3, 8 * tmp2);".to_string(),
                "tmp1 <=X= or(tmp1, tmp3);".to_string(),
                "mstore tmp1;".to_string(),
            ]
        }
        "sb" => {
            // store byte
            let (rs, rd, off) = rro(args);
            vec![
                format!("tmp1 <=X= wrap({rd} + {off});"),
                "addr <=X= and(tmp1, 0xfffffffc);".to_string(),
                "tmp2 <=X= and(tmp1, 0x3);".to_string(),
                "tmp1 <=X= mload();".to_string(),
                "tmp3 <=X= shl(0xff, 8 * tmp2);".to_string(),
                "tmp3 <=X= xor(tmp3, 0xffffffff);".to_string(),
                "tmp1 <=X= and(tmp1, tmp3);".to_string(),
                format!("tmp3 <=X= and({rs}, 0xff);"),
                "tmp3 <=X= shl(tmp3, 8 * tmp2);".to_string(),
                "tmp1 <=X= or(tmp1, tmp3);".to_string(),
                "mstore tmp1;".to_string(),
            ]
        }
        "nop" => vec![],
        "unimp" => vec!["fail;".to_string()],

        // Special instruction that is inserted to allow dynamic label references
        "load_dynamic" => {
            let (rd, label) = rl(args);
            only_if_no_write_to_zero(format!("{rd} <=X= load_label({label});"), rd)
        }

        _ => {
            panic!("Unknown instruction: {instr}");
        }
    }
}
