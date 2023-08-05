use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
};

use asm_utils::{
    ast::{BinaryOpKind, UnaryOpKind},
    data_parser::{self, DataValue},
    parser::parse_asm,
    reachability,
};
use itertools::Itertools;

use crate::disambiguator;
use crate::parser::RiscParser;
use crate::{Argument, Expression, Statement};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Register {
    value: u8,
}

pub fn machine_decls() -> Vec<&'static str> {
    vec![
        r#"
// ================= binary/bitwise instructions =================

machine Binary(latch, operation_id) {

    degree 262144;

    operation and<0> A, B -> C;

    operation or<1> A, B -> C;

    operation xor<2> A, B -> C;

    constraints{
        col witness operation_id;

        macro is_nonzero(X) { match X { 0 => 0, _ => 1, } };
        macro is_zero(X) { 1 - is_nonzero(X) };

        col fixed latch(i) { is_zero((i % 4) - 3) };
        col fixed FACTOR(i) { 1 << (((i + 1) % 4) * 8) };

        col fixed P_A(i) { i % 256 };
        col fixed P_B(i) { (i >> 8) % 256 };
        col fixed P_operation(i) { (i / (256 * 256)) % 3 };
        col fixed P_C(i) {
            match P_operation(i) {
                0 => P_A(i) & P_B(i),
                1 => P_A(i) | P_B(i),
                2 => P_A(i) ^ P_B(i),
            } & 0xff
        };

        col witness A_byte;
        col witness B_byte;
        col witness C_byte;

        col witness A;
        col witness B;
        col witness C;

        A' = A * (1 - latch) + A_byte * FACTOR;
        B' = B * (1 - latch) + B_byte * FACTOR;
        C' = C * (1 - latch) + C_byte * FACTOR;

        {operation_id', A_byte, B_byte, C_byte} in {P_operation, P_A, P_B, P_C};
    }
}
"#,
        r#"
// ================= shift instructions =================

machine Shift(latch, operation_id) {
    degree 262144;

    operation shl<0> A, B -> C;

    operation shr<1> A, B -> C;

    constraints{
        col witness operation_id;

        col fixed latch(i) { is_zero((i % 4) - 3) };
        col fixed FACTOR_ROW(i) { (i + 1) % 4 };
        col fixed FACTOR(i) { 1 << (((i + 1) % 4) * 8) };

        col fixed P_A(i) { i % 256 };
        col fixed P_B(i) { (i / 256) % 32 };
        col fixed P_ROW(i) { (i / (256 * 32)) % 4 };
        col fixed P_operation(i) { (i / (256 * 32 * 4)) % 2 };
        col fixed P_C(i) {
            match P_operation(i) {
                0 => (P_A(i) << (P_B(i) + (P_ROW(i) * 8))),
                1 => (P_A(i) << (P_ROW(i) * 8)) >> P_B(i),
            } & 0xffffffff
        };

        col witness A_byte;
        col witness C_part;

        col witness A;
        col witness B;
        col witness C;

        A' = A * (1 - latch) + A_byte * FACTOR;
        (B' - B) * (1 - latch) = 0;
        C' = C * (1 - latch) + C_part;

        // TODO this way, we cannot prove anything that shifts by more than 31 bits.
        {operation_id', A_byte, B', FACTOR_ROW, C_part} in {P_operation, P_A, P_B, P_ROW, P_C};
    }
}
"#,
    ]
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

#[derive(Default)]
pub struct Risc {}

impl asm_utils::compiler::Compiler for Risc {
    /// Compiles riscv assembly to POWDR assembly. Adds required library routines.
    fn compile(mut assemblies: BTreeMap<String, String>) -> String {
        // stack grows towards zero
        let stack_start = 0x10000;
        // data grows away from zero
        let data_start = 0x10100;

        assert!(assemblies
            .insert("__runtime".to_string(), runtime().to_string())
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
        reachability::filter_reachable_from("__runtime_start", &mut statements, &mut objects);

        // Replace dynamic references to code labels
        replace_dynamic_label_references(&mut statements, &objects);

        // Remove the riscv asm stub function, which is used
        // for compilation, and will not be called.
        statements = replace_coprocessor_stubs(statements).collect::<Vec<_>>();

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
        let (data_code, data_positions) = store_data_objects(&sorted_objects, data_start);

        riscv_machine(
            &machine_decls(),
            &preamble(),
            &[("binary", "Binary"), ("shift", "Shift")],
            file_ids
                .into_iter()
                .map(|(id, dir, file)| format!("debug file {id} {} {};", quote(&dir), quote(&file)))
                .chain(["call __data_init;".to_string()])
                .chain(call_every_submachine())
                .chain([
                    format!("// Set stack pointer\nx2 <=X= {stack_start};"),
                    "call __runtime_start;".to_string(),
                    "return;".to_string(), // This is not "riscv ret", but "return from powdr asm function".
                ])
                .chain(
                    substitute_symbols_with_values(statements, &data_positions)
                        .into_iter()
                        .flat_map(process_statement),
                )
                .chain(["// This is the data initialization routine.\n__data_init::".to_string()])
                .chain(data_code)
                .chain(["// This is the end of the data initialization routine.\nret;".to_string()])
                .collect(),
        )
    }
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

fn replace_coprocessor_stubs(
    statements: impl IntoIterator<Item = Statement>,
) -> impl Iterator<Item = Statement> {
    let stub_names: Vec<&str> = COPROCESSOR_SUBSTITUTIONS
        .iter()
        .map(|(name, _)| *name)
        .collect();

    remove_matching_and_next(statements.into_iter(), move |statement| -> bool {
        matches!(&statement, Statement::Label(label) if stub_names.contains(&label.as_str()))
    })
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
                                format!("tmp1 <== load_label({});", escape_label(sym)),
                                "mstore tmp1;".to_string(),
                            ]);
                        }
                    }
                    DataValue::Offset(_, _) => {
                        unimplemented!()

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

fn call_every_submachine() -> Vec<String> {
    // TODO This is a hacky snippet to ensure that every submachine in the RISCV machine
    // is called at least once. This is needed for witgen until it can do default blocks
    // automatically.
    // https://github.com/powdr-labs/powdr/issues/548
    vec![
        "x10 <== and(x10, x10);".to_string(),
        "x10 <== shl(x10, x10);".to_string(),
        "x10 <=X= 0;".to_string(),
    ]
}

fn next_multiple_of_four(x: usize) -> usize {
    ((x + 3) / 4) * 4
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
            .map(|i| format!("\t\treg x{i};\n"))
            .collect::<Vec<_>>()
            .concat()
        + r#"
    reg addr;

    constraints {
        x0 = 0;
    }

    constraints{
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

    // ================= coprocessor substitution instructions =================

    instr poseidon Y, Z -> X {
        // Dummy code, to be replaced with actual poseidon code.
        X = 0
    }

    // ================= binary/bitwise instructions =================

    instr and Y, Z -> X = binary.and

    instr or Y, Z -> X = binary.or

    instr xor Y, Z -> X = binary.xor

    // ================= shift instructions =================

    instr shl Y, Z -> X = shift.shl

    instr shr Y, Z -> X = shift.shr

    // ================== wrapping instructions ==============

    // Wraps a value in Y to 32 bits.
    // Requires 0 <= Y < 2**33
    instr wrap Y -> X { Y = X + wrap_bit * 2**32, X = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000 }
    // Requires -2**32 <= Y < 2**32
    instr wrap_signed Y -> X { Y + 2**32 = X + wrap_bit * 2**32, X = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000 }
    constraints{
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
    constraints{
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
    constraints {
        col witness Y_b5;
        col witness Y_b6;
        col witness Y_b7;
        col witness Y_b8;
        { Y_b5 } in { bytes };
        { Y_b6 } in { bytes };
        { Y_b7 } in { bytes };
        { Y_b8 } in { bytes };

        col witness remainder; 

        col witness REM_b1;
        col witness REM_b2;
        col witness REM_b3;
        col witness REM_b4;
        { REM_b1 } in { bytes };
        { REM_b2 } in { bytes };
        { REM_b3 } in { bytes };
        { REM_b4 } in { bytes };
    }

    // implements Z = Y / X, stores remainder in `remainder`.
    instr divu Y, X -> Z {
        // Y is the known dividend
        // X is the known divisor
        // Z is the unknown quotient
        // main division algorithm;
        // if X is zero, remainder is set to dividend, as per RISC-V specification:
        X * Z + remainder = Y,

        // remainder >= 0:
        remainder = REM_b1 + REM_b2 * 0x100 + REM_b3 * 0x10000 + REM_b4 * 0x1000000,

        // remainder < divisor, conditioned to X not being 0:
        (1 - XIsZero) * (X - remainder - 1 - Y_b5 - Y_b6 * 0x100 - Y_b7 * 0x10000 - Y_b8 * 0x1000000) = 0,

        // in case X is zero, we set quotient according to RISC-V specification
        XIsZero * (Z - 0xffffffff) = 0,

        // quotient is 32 bits:
        Z = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000
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
"#
}

fn runtime() -> &'static str {
    r#"
.globl __udivdi3@plt
.globl __udivdi3
.set __udivdi3@plt, __udivdi3

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

.globl poseidon_coprocessor
poseidon_coprocessor:
    ret
"#
}

fn process_statement(s: Statement) -> Vec<String> {
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
                args.iter().map(|s| s.to_string()).join(", ")
            ),
        },
        Statement::Instruction(instr, args) => process_instruction(instr, args)
            .into_iter()
            .map(|s| "  ".to_string() + &s)
            .collect(),
    }
}

fn quote(s: &str) -> String {
    // TODO more things to quote
    format!("\"{}\"", s.replace('\\', "\\\\").replace('\"', "\\\""))
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

static COPROCESSOR_SUBSTITUTIONS: &[(&str, &str)] =
    &[("poseidon_coprocessor", "x10 <== poseidon(x10, x11);")];

fn try_coprocessor_substitution(label: &str) -> Option<String> {
    COPROCESSOR_SUBSTITUTIONS
        .iter()
        .find(|(l, _)| *l == label)
        .map(|&(_, subst)| subst.to_string())
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
            only_if_no_write_to_zero(format!("{rd} <== wrap({r1} + {r2});"), rd)
        }
        "addi" => {
            let (rd, rs, imm) = rri(args);
            only_if_no_write_to_zero(format!("{rd} <== wrap({rs} + {imm});"), rd)
        }
        "sub" => {
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero(format!("{rd} <== wrap_signed({r1} - {r2});"), rd)
        }
        "neg" => {
            let (rd, r1) = rr(args);
            only_if_no_write_to_zero(format!("{rd} <== wrap_signed(0 - {r1});"), rd)
        }
        "mul" => {
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero(format!("{rd} <== mul({r1}, {r2});"), rd)
        }
        "mulhu" => {
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero(format!("{rd} <== mulhu({r1}, {r2});"), rd)
        }
        "divu" => {
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero(format!("{rd} <=Z= divu({r1}, {r2});"), rd)
        }

        // bitwise
        "xor" => {
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero(format!("{rd} <== xor({r1}, {r2});"), rd)
        }
        "xori" => {
            let (rd, r1, imm) = rri(args);
            only_if_no_write_to_zero(format!("{rd} <== xor({r1}, {imm});"), rd)
        }
        "and" => {
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero(format!("{rd} <== and({r1}, {r2});"), rd)
        }
        "andi" => {
            let (rd, r1, imm) = rri(args);
            only_if_no_write_to_zero(format!("{rd} <== and({r1}, {imm});"), rd)
        }
        "or" => {
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero(format!("{rd} <== or({r1}, {r2});"), rd)
        }
        "ori" => {
            let (rd, r1, imm) = rri(args);
            only_if_no_write_to_zero(format!("{rd} <== or({r1}, {imm});"), rd)
        }
        "not" => {
            let (rd, rs) = rr(args);
            only_if_no_write_to_zero(format!("{rd} <== wrap_signed(-{rs} - 1);"), rd)
        }

        // shift
        "slli" => {
            let (rd, rs, amount) = rri(args);
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
            let (rd, r1, r2) = rrr(args);
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
            let (rd, rs, amount) = rri(args);
            assert!(amount <= 31);
            only_if_no_write_to_zero(format!("{rd} <== shr({rs}, {amount});"), rd)
        }
        "srl" => {
            // logical shift right
            let (rd, r1, r2) = rrr(args);
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
            let (rd, rs, amount) = rri(args);
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
                    format!("tmp1 <== to_signed({rs});"),
                    format!("{rd} <=Y= is_positive({} - tmp1);", imm as i32),
                ],
                rd,
            )
        }
        "slt" => {
            let (rd, r1, r2) = rrr(args);
            vec![
                format!("tmp1 <== to_signed({r1});"),
                format!("tmp2 <== to_signed({r2});"),
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
                format!("tmp1 <== to_signed({rs});"),
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
                format!("tmp1 <== to_signed({r1});"),
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
                format!("tmp1 <== to_signed({r1});"),
                format!("tmp2 <== to_signed({r2});"),
                format!("branch_if_positive tmp2 - tmp1, {label};"),
            ]
        }
        "bge" => {
            let (r1, r2, label) = rrl(args);
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
            let (r1, label) = rl(args);
            vec![format!("branch_if_positive {r1} - 2**31 + 1, {label};")]
        }

        "blez" => {
            // branch less or equal zero
            let (r1, label) = rl(args);
            vec![
                format!("tmp1 <== to_signed({r1});"),
                format!("branch_if_positive -tmp1 + 1, {label};"),
            ]
        }
        "bgtz" => {
            // branch if 0 < r1 < 2**31
            let (r1, label) = rl(args);
            vec![
                format!("tmp1 <== to_signed({r1});"),
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
        "call" | "tail" => {
            // Depending on what symbol is called, the call is replaced by a
            // powdr asm call, or a call to a coprocessor if a special function
            // has been recognized.
            assert_eq!(args.len(), 1);
            let label = &args[0];
            let replacement = match label {
                Argument::Expression(Expression::Symbol(l)) => try_coprocessor_substitution(l),
                _ => None,
            };
            match (replacement, instr) {
                (Some(replacement), "call") => vec![replacement],
                (Some(replacement), "tail") => vec![replacement, "ret;".to_string()],
                (Some(_), _) => panic!(),
                (None, _) => vec![format!("{instr} {};", argument_to_escaped_symbol(label))],
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
                    format!("addr <== wrap({rs} + {off});"),
                    format!("{rd} <== mload();"),
                ],
                rd,
            )
        }
        "lb" => {
            // load byte and sign-extend. the memory is little-endian.
            let (rd, rs, off) = rro(args);
            only_if_no_write_to_zero_vec(
                vec![
                    format!("tmp1 <== wrap({rs} + {off});"),
                    "addr <== and(tmp1, 0xfffffffc);".to_string(),
                    "tmp2 <== and(tmp1, 0x3);".to_string(),
                    format!("{rd} <== mload();"),
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
                    format!("tmp1 <== wrap({rs} + {off});"),
                    "addr <== and(tmp1, 0xfffffffc);".to_string(),
                    "tmp2 <== and(tmp1, 0x3);".to_string(),
                    format!("{rd} <== mload();"),
                    format!("{rd} <== shr({rd}, 8 * tmp2);"),
                    format!("{rd} <== and({rd}, 0xff);"),
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
                    format!("tmp1 <== wrap({rs} + {off});"),
                    "addr <== and(tmp1, 0xfffffffc);".to_string(),
                    "tmp2 <== and(tmp1, 0x3);".to_string(),
                    format!("{rd} <== mload();"),
                    format!("{rd} <== shr({rd}, 8 * tmp2);"),
                    format!("{rd} <== and({rd}, 0x0000ffff);"),
                ],
                rd,
            )
        }
        "sw" => {
            let (r1, r2, off) = rro(args);
            vec![
                format!("addr <== wrap({r2} + {off});"),
                format!("mstore {r1};"),
            ]
        }
        "sh" => {
            // store half word (two bytes)
            // TODO this code assumes it is at least aligned on
            // a two-byte boundary

            let (rs, rd, off) = rro(args);
            vec![
                format!("tmp1 <== wrap({rd} + {off});"),
                "addr <== and(tmp1, 0xfffffffc);".to_string(),
                "tmp2 <== and(tmp1, 0x3);".to_string(),
                "tmp1 <== mload();".to_string(),
                "tmp3 <== shl(0xffff, 8 * tmp2);".to_string(),
                "tmp3 <== xor(tmp3, 0xffffffff);".to_string(),
                "tmp1 <== and(tmp1, tmp3);".to_string(),
                format!("tmp3 <== and({rs}, 0xffff);"),
                "tmp3 <== shl(tmp3, 8 * tmp2);".to_string(),
                "tmp1 <== or(tmp1, tmp3);".to_string(),
                "mstore tmp1;".to_string(),
            ]
        }
        "sb" => {
            // store byte
            let (rs, rd, off) = rro(args);
            vec![
                format!("tmp1 <== wrap({rd} + {off});"),
                "addr <== and(tmp1, 0xfffffffc);".to_string(),
                "tmp2 <== and(tmp1, 0x3);".to_string(),
                "tmp1 <== mload();".to_string(),
                "tmp3 <== shl(0xff, 8 * tmp2);".to_string(),
                "tmp3 <== xor(tmp3, 0xffffffff);".to_string(),
                "tmp1 <== and(tmp1, tmp3);".to_string(),
                format!("tmp3 <== and({rs}, 0xff);"),
                "tmp3 <== shl(tmp3, 8 * tmp2);".to_string(),
                "tmp1 <== or(tmp1, tmp3);".to_string(),
                "mstore tmp1;".to_string(),
            ]
        }
        "nop" => vec![],
        "unimp" => vec!["fail;".to_string()],

        // Special instruction that is inserted to allow dynamic label references
        "load_dynamic" => {
            let (rd, label) = rl(args);
            only_if_no_write_to_zero(format!("{rd} <== load_label({label});"), rd)
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
