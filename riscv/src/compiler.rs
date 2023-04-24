use std::collections::{BTreeMap, BTreeSet};

use crate::{
    data_parser::{self, DataValue},
    disambiguator,
    parser::{self, Argument, Register, Statement},
};

use super::parser::Constant;

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
    let mut objects = data_parser::extract_data_objects(&statements);

    // Reduce to the code that is actually reachable from main
    // (and the objects that are referred from there)
    filter_reachable_from("main", &mut statements, &mut objects);

    // Replace dynamic references to code labels
    replace_dynamic_label_references(&mut statements, &objects);

    let (data_code, data_positions) = store_data_objects(objects, data_start);
    let mut output = preamble()
        + &data_code
        + &format!("// Set stack pointer\nx2 <=X= {stack_start};\n")
        + "jump main;\n";

    let statements = insert_data_positions(statements, &data_positions);

    for s in statements {
        output += &process_statement(s);
    }
    output
}

fn filter_reachable_from(
    label: &str,
    statements: &mut Vec<Statement>,
    objects: &mut BTreeMap<String, Vec<DataValue>>,
) {
    let label_offsets = parser::extract_label_offsets(statements);
    let mut queued_labels: BTreeSet<&str> = vec![label].into_iter().collect();
    let mut referenced_labels: BTreeSet<&str> = vec![label].into_iter().collect();
    let mut processed_labels = BTreeSet::<&str>::new();
    // Labels that are included in a basic block that starts with a different label.
    let mut secondary_labels = BTreeSet::<&str>::new();
    let mut label_queue = vec![label];
    while let Some(l) = label_queue.pop() {
        if objects.contains_key(l) {
            // We record but do not process references to objects
            continue;
        }
        if !processed_labels.insert(l) {
            continue;
        }
        let offset = *label_offsets.get(l).unwrap_or_else(|| {
            eprintln!("The RISCV assembly code references an external routine / label that is not available:");
            eprintln!("{l}");
            panic!();
        });
        let (referenced_labels_in_block, seen_labels_in_block) =
            basic_block_references_starting_from(&statements[offset..]);
        assert!(!secondary_labels.contains(l));
        secondary_labels.extend(seen_labels_in_block.clone());
        secondary_labels.remove(l);
        processed_labels.extend(seen_labels_in_block);

        for referenced in &referenced_labels_in_block {
            if !queued_labels.contains(referenced) && !processed_labels.contains(referenced) {
                label_queue.push(referenced);
                queued_labels.insert(referenced);
            }
        }
        referenced_labels.extend(referenced_labels_in_block);
    }
    objects.retain(|name, _value| referenced_labels.contains(name.as_str()));
    let code = processed_labels
        .difference(&secondary_labels)
        .flat_map(|l| {
            let offset = *label_offsets.get(l).unwrap();
            basic_block_code_starting_from(&statements[offset..])
        })
        .collect();
    *statements = code;
}

fn basic_block_references_starting_from(statements: &[Statement]) -> (Vec<&str>, Vec<&str>) {
    let mut seen_labels = vec![];
    let mut referenced_labels = BTreeSet::<&str>::new();
    for s in statements {
        if let Statement::Label(l) = s {
            seen_labels.push(l.as_str());
        } else {
            referenced_labels.extend(parser::referenced_labels(s))
        }
        if parser::ends_control_flow(s) {
            break;
        }
    }
    (referenced_labels.into_iter().collect(), seen_labels)
}

fn basic_block_code_starting_from(statements: &[Statement]) -> Vec<Statement> {
    let mut code = vec![];
    for s in statements {
        if let Statement::Directive(_, _) = s {
            panic!("Included directive in code block: {s}");
        }
        code.push(s.clone());
        if parser::ends_control_flow(s) {
            break;
        }
    }
    code
}

/// Replace certain patterns of references to code labels by
/// special instructions. We ignore any references to data objects
/// because they will be handled differently.
fn replace_dynamic_label_references(
    statements: &mut Vec<Statement>,
    data_objects: &BTreeMap<String, Vec<DataValue>>,
) {
    let mut replacement = vec![];
    /*
    Find patterns of the form
    lui	a0, %hi(LABEL)
    addi	s10, a0, %lo(LABEL)
    -
    turn this into the pseudo-riscv-instruction
    load_dynamic s10, LABEL
    which is then turned into

    s10 <=X= load_label(LABEL)
    */
    // TODO This is really hacky, should be rustified
    let mut i = 0;
    while i < statements.len() {
        let s1 = &statements[i];
        let s2 = &statements.get(i + 1);
        if s2.is_none() {
            replacement.push(s1.clone());
            i += 1;
        } else if let Some(r) = replace_dynamic_label_reference(s1, s2.unwrap(), data_objects) {
            replacement.push(r);
            i += 2;
        } else {
            // TODO avoid clone
            replacement.push(s1.clone());
            i += 1;
        }
    }

    *statements = replacement;
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
    let [Argument::Register(r1), Argument::Constant(Constant::HiDataRef(label1))] = &args1[..] else { return None; };
    let [Argument::Register(r2), Argument::Register(r3), Argument::Constant(Constant::LoDataRef(label2))] = &args2[..] else { return None; };
    if r1 != r3 || label1 != label2 || data_objects.contains_key(label1) {
        return None;
    }
    Some(Statement::Instruction(
        "load_dynamic".to_string(),
        vec![Argument::Register(*r2), Argument::Symbol(label1.clone())],
    ))
}

fn store_data_objects(
    objects: BTreeMap<String, Vec<DataValue>>,
    mut memory_start: u32,
) -> (String, BTreeMap<String, u32>) {
    memory_start = ((memory_start + 7) / 8) * 8;
    let mut current_pos = memory_start;
    let mut positions = BTreeMap::new();
    for (name, data) in &objects {
        // TODO check if we need to use multiples of four.
        let size: u32 = data
            .iter()
            .map(|d| next_multiple_of_four(d.size()) as u32)
            .sum();
        positions.insert(name.clone(), current_pos);
        current_pos += size;
    }

    let mut code = String::new();
    for (name, data) in objects {
        code += &format!("// data {name}\n");
        let mut pos = positions[&name];
        for item in data {
            match &item {
                DataValue::Direct(bytes) => {
                    for i in 0..((bytes.len() + 3) / 4) {
                        let v = (0..4)
                            .map(|j| {
                                (bytes.get(i * 4 + j).cloned().unwrap_or_default() as u32)
                                    << (j * 8)
                            })
                            .reduce(|a, b| a | b)
                            .unwrap();
                        code += &format!("addr <=X= 0x{pos:x};\nmstore 0x{v:x};\n");
                    }
                }
                DataValue::Reference(sym) => {
                    code += &format!("addr <=X= 0x{pos:x};\n");
                    if let Some(p) = positions.get(sym) {
                        code += &format!("mstore 0x{p:x};\n");
                    } else {
                        // code reference
                        // TODO should be possible without temporary
                        code += &format!(
                            "tmp1 <=X= load_label({});\nmstore tmp1;\n",
                            escape_label(sym)
                        );
                    }
                }
            }
            pos += next_multiple_of_four(item.size()) as u32;
        }
    }
    (code, positions)
}

fn next_multiple_of_four(x: usize) -> usize {
    ((x + 3) / 4) * 4
}

fn insert_data_positions(
    mut statements: Vec<Statement>,
    data_positions: &BTreeMap<String, u32>,
) -> Vec<Statement> {
    for s in &mut statements {
        let Statement::Instruction(_name, args) = s else { continue; };
        for arg in args {
            match arg {
                Argument::RegOffset(_, offset) => replace_data_reference(offset, data_positions),
                Argument::Constant(c) => replace_data_reference(c, data_positions),
                _ => {}
            }
        }
    }
    statements
}

fn replace_data_reference(constant: &mut Constant, data_positions: &BTreeMap<String, u32>) {
    match constant {
        Constant::Number(_) => {}
        Constant::HiDataRef(data) => {
            if let Some(pos) = data_positions.get(data) {
                *constant = Constant::Number((pos >> 12) as i64)
            }
            // Otherwise, it references a code label
        }
        Constant::LoDataRef(data) => {
            if let Some(pos) = data_positions.get(data) {
                *constant = Constant::Number((pos & 0xfff) as i64)
            }
            // Otherwise, it references a code label
        }
    }
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

// Input is a 32 but unsined number (0 <= Y < 2**32) interpreted as a two's complement numbers.
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
    // // TODO rust alloc calls the global allocator - not sure why this is not automatic.
    // (Regex::new(r"^__rust_alloc$").unwrap(), "j __rg_alloc"),
    // (Regex::new(r"^__rust_realloc$").unwrap(), "j __rg_realloc"),
    // (Regex::new(r"^__rust_dealloc$").unwrap(), "j __rg_dealloc"),
    // (

    /* Source code:
    // TODO c is usually a "c int"
    pub unsafe extern "C" fn memset(s: *mut u8, c: u8, n: usize) -> *mut u8 {
        // We only access u32 because then we do not have to deal with
        // un-aligned memory access.
        // TODO this does not really enforce that the pointers are u32-aligned.
        let mut value = c as u32;
        value = value | (value << 8) | (value << 16) | (value << 24);
        let mut i: isize = 0;
        while i + 3 < n as isize {
            *((s.offset(i)) as *mut u32) = value;
            i += 4;
        }
        if i < n {
            let dest_value = (s.offset(i)) as *mut u32;
            let mask = (1 << ((((n as isize) - i) * 8) as u32)) - 1;
            *dest_value = (*dest_value & !mask) | (value & mask);
        }
        s
    }

    pub unsafe extern "C" fn memcpy(dest: *mut u8, src: *const u8, n: usize) -> *mut u8 {
        // We only access u32 because then we do not have to deal with
        // un-aligned memory access.
        // TODO this does not really enforce that the pointers are u32-aligned.
        let mut i: isize = 0;
        while i + 3 < n as isize {
            *((dest.offset(i)) as *mut u32) = *((src.offset(i)) as *mut u32);
            i += 4;
        }
        if i < n as isize {
            let value = *((src.offset(i)) as *mut u32);
            let dest_value = (dest.offset(i)) as *mut u32;
            let mask = (1 << (((n as isize - i) * 8) as u32)) - 1;
            *dest_value = (*dest_value & !mask) | (value & mask);
        }
        dest
    }
    */
    r#"
.globl rust_begin_unwind
rust_begin_unwind:
    unimp

.globl memset@plt
memset@plt:
    li	a3, 4
    blt	a2, a3, __memset_LBB5_5
    li	a5, 0
    lui	a3, 4112
    addi	a3, a3, 257
    mul	a6, a1, a3
__memset_LBB5_2:
    add	a7, a0, a5
    addi	a3, a5, 4
    addi	a4, a5, 7
    sw	a6, 0(a7)
    mv	a5, a3
    blt	a4, a2, __memset_LBB5_2
    bge	a3, a2, __memset_LBB5_6
__memset_LBB5_4:
    lui	a4, 16
    addi	a4, a4, 257
    mul	a1, a1, a4
    add	a3, a3, a0
    slli	a2, a2, 3
    lw	a4, 0(a3)
    li	a5, -1
    sll	a2, a5, a2
    not	a5, a2
    and	a2, a2, a4
    and	a1, a1, a5
    or	a1, a1, a2
    sw	a1, 0(a3)
    ret
__memset_LBB5_5:
    li	a3, 0
    blt	a3, a2, __memset_LBB5_4
__memset_LBB5_6:
    ret

.globl memcpy@plt
memcpy@plt:
    li	a3, 4
    blt	a2, a3, __memcpy_LBB2_5
    li	a4, 0
__memcpy_LBB2_2:
    add	a3, a1, a4
    lw	a6, 0(a3)
    add	a7, a0, a4
    addi	a3, a4, 4
    addi	a5, a4, 7
    sw	a6, 0(a7)
    mv	a4, a3
    blt	a5, a2, __memcpy_LBB2_2
    bge	a3, a2, __memcpy_LBB2_6
__memcpy_LBB2_4:
    add	a1, a1, a3
    lw	a1, 0(a1)
    add	a3, a3, a0
    slli	a2, a2, 3
    lw	a4, 0(a3)
    li	a5, -1
    sll	a2, a5, a2
    not	a5, a2
    and	a2, a2, a4
    and	a1, a1, a5
    or	a1, a1, a2
    sw	a1, 0(a3)
    ret
__memcpy_LBB2_5:
    li	a3, 0
    blt	a3, a2, __memcpy_LBB2_4
__memcpy_LBB2_6:
    ret

.globl memcmp@plt
memcmp@plt:
	beqz	a2, .LBB270_3
.LBB270_1:
	lbu	a3, 0(a0)
	lbu	a4, 0(a1)
	bne	a3, a4, .LBB270_4
	addi	a1, a1, 1
	addi	a2, a2, -1
	addi	a0, a0, 1
	bnez	a2, .LBB270_1
.LBB270_3:
	li	a0, 0
	ret
.LBB270_4:
	sub	a0, a3, a4
	ret

"#
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
        Constant::HiDataRef(n) | Constant::LoDataRef(n) => {
            panic!("Data reference was not erased during preprocessing: {n}");
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
        // load/store registers
        "li" => {
            let (rd, imm) = ri(args);
            format!("{rd} <=X= {imm};\n")
        }
        // TODO check if it is OK to clear the lower order bits
        "lui" => {
            let (rd, imm) = ri(args);
            format!("{rd} <=X= {};\n", imm << 12)
        }
        "mv" => {
            let (rd, rs) = rr(args);
            format!("{rd} <=X= {rs};\n")
        }

        // Arithmetic
        "add" => {
            let (rd, r1, r2) = rrr(args);
            format!("{rd} <=X= wrap({r1} + {r2});\n")
        }
        "addi" => {
            let (rd, rs, imm) = rri(args);
            format!("{rd} <=X= wrap({rs} + {imm});\n")
        }
        "sub" => {
            let (rd, r1, r2) = rrr(args);
            format!("{rd} <=X= wrap_signed({r1} - {r2});\n")
        }
        "neg" => {
            let (rd, r1) = rr(args);
            format!("{rd} <=X= wrap_signed(0 - {r1});\n")
        }
        "mul" => {
            let (rd, r1, r2) = rrr(args);
            format!("{rd} <=X= mul({r1}, {r2});\n")
        }
        "mulhu" => {
            let (rd, r1, r2) = rrr(args);
            format!("{rd} <=X= mulhu({r1}, {r2});\n")
        }

        // bitwise
        "xor" => {
            let (rd, r1, r2) = rrr(args);
            format!("{rd} <=X= xor({r1}, {r2});\n")
        }
        "xori" => {
            let (rd, r1, imm) = rri(args);
            format!("{rd} <=X= xor({r1}, {imm});\n")
        }
        "and" => {
            let (rd, r1, r2) = rrr(args);
            format!("{rd} <=X= and({r1}, {r2});\n")
        }
        "andi" => {
            let (rd, r1, imm) = rri(args);
            format!("{rd} <=X= and({r1}, {imm});\n")
        }
        "or" => {
            let (rd, r1, r2) = rrr(args);
            format!("{rd} <=X= or({r1}, {r2});\n")
        }
        "ori" => {
            let (rd, r1, imm) = rri(args);
            format!("{rd} <=X= or({r1}, {imm});\n")
        }
        "not" => {
            let (rd, rs) = rr(args);
            format!("{rd} <=X= wrap_signed(-{rs} - 1);\n")
        }

        // shift
        "slli" => {
            let (rd, rs, amount) = rri(args);
            assert!(amount <= 31);
            if amount <= 16 {
                format!("{rd} <=X= wrap16({rs} * {});\n", 1 << amount)
            } else {
                format!("tmp1 <=X= wrap16({rs} * {});\n", 1 << 16)
                    + &format!("{rd} <=X= wrap16(tmp1 * {});\n", 1 << (amount - 16))
            }
        }
        "sll" => {
            let (rd, r1, r2) = rrr(args);
            format!("tmp1 <=X= and({r2}, 0x1f);\n{rd} <=X= shl({r1}, tmp1);\n")
        }
        "srli" => {
            // logical shift right
            let (rd, rs, amount) = rri(args);
            assert!(amount <= 31);
            format!("{rd} <=X= shr({rs}, {amount});\n")
        }
        "srl" => {
            // logical shift right
            let (rd, r1, r2) = rrr(args);
            format!("tmp1 <=X= and({r2}, 0x1f);\n{rd} <=X= shr({r1}, tmp1);\n")
        }

        // comparison
        "seqz" => {
            let (rd, rs) = rr(args);
            format!("{rd} <=Y= is_equal_zero({rs});\n")
        }
        "snez" => {
            let (rd, rs) = rr(args);
            format!("{rd} <=Y= is_not_equal_zero({rs});\n")
        }
        "slti" => {
            let (rd, rs, imm) = rri(args);
            format!("tmp1 <=X= to_signed({rs});\n")
                + &format!("{rd} <=Y= is_positive({imm} - tmp1);\n")
        }
        "sltiu" => {
            let (rd, rs, imm) = rri(args);
            format!("{rd} <=Y= is_positive({imm} - {rs});\n")
        }
        "sltu" => {
            let (rd, r1, r2) = rrr(args);
            format!("{rd} <=Y= is_positive({r2} - {r1});\n")
        }

        // branching
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
            // TODO does this fulfill the input requirements for branch_if_positive?
            format!("branch_if_positive {r1} - {r2} + 1, {label};\n")
        }
        "bgez" => {
            let (r1, label) = rl(args);
            format!("tmp1 <=X= to_signed({r1});\n")
                + &format!("branch_if_positive {r1} + 1, {label};\n")
        }
        "bltu" => {
            let (r1, r2, label) = rrl(args);
            format!("branch_if_positive {r2} - {r1}, {label};\n")
        }
        "blt" => {
            let (r1, r2, label) = rrl(args);
            // Branch if r1 < r2 (signed).
            // TODO does this fulfill the input requirements for branch_if_positive?
            format!("tmp1 <=X= to_signed({r1});\n")
                + &format!("tmp2 <=X= to_signed({r2});\n")
                + &format!("branch_if_positive tmp2 - tmp1, {label};\n")
        }
        "bge" => {
            let (r1, r2, label) = rrl(args);
            // Branch if r1 >= r2 (signed).
            // TODO does this fulfill the input requirements for branch_if_positive?
            format!("tmp1 <=X= to_signed({r1});\n")
                + &format!("tmp2 <=X= to_signed({r2});\n")
                + &format!("branch_if_positive tmp1 - tmp2 + 1, {label};\n")
        }
        "bltz" => {
            // branch if 2**31 <= r1 < 2**32
            let (r1, label) = rl(args);
            format!("branch_if_positive {r1} - 2**31 + 1, {label};\n")
        }

        "blez" => {
            // branch less or equal zero
            let (r1, label) = rl(args);
            format!("tmp1 <=X= to_signed({r1});\n")
                + &format!("branch_if_positive -tmp1 + 1, {label};\n")
        }
        "bgtz" => {
            // branch if 0 < r1 < 2**31
            let (r1, label) = rl(args);
            format!("tmp1 <=X= to_signed({r1});\n")
                + &format!("branch_if_positive tmp1, {label};\n")
        }
        "bne" => {
            let (r1, r2, label) = rrl(args);
            format!("branch_if_nonzero {r1} - {r2}, {label};\n")
        }
        "bnez" => {
            let (r1, label) = rl(args);
            format!("branch_if_nonzero {r1}, {label};\n")
        }

        // jump and call
        "j" => {
            if let [Argument::Symbol(label)] = args {
                format!("jump {};\n", escape_label(label))
            } else {
                panic!()
            }
        }
        "jr" => {
            let rs = r(args);
            format!("jump_dyn {rs};\n")
        }
        "jal" => {
            let (_rd, _label) = rl(args);
            todo!();
        }
        "jalr" => {
            // TODO there is also a form that takes more arguments
            let rs = r(args);
            format!("jump_and_link_dyn {rs};\n")
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
        "tail" => {
            if let [Argument::Symbol(label)] = args {
                format!("tail {};\n", escape_label(label))
            } else {
                panic!()
            }
        }
        "ret" => {
            assert!(args.is_empty());
            "ret;\n".to_string()
        }

        // memory access
        "lw" => {
            let (rd, rs, off) = rro(args);
            // TODO we need to consider misaligned loads / stores
            format!("addr <=X= wrap({rs} + {off});\n") + &format!("{rd} <=X= mload();\n")
        }
        "lb" => {
            // load byte and sign-extend. the memory is little-endian.
            let (rd, rs, off) = rro(args);
            format!("tmp1 <=X= wrap({rs} + {off});\n")
                + "addr <=X= and(tmp1, 0xfffffffc);\n"
                + "tmp2 <=X= and(tmp1, 0x3);\n"
                + &format!("{rd} <=X= mload();\n")
                + &format!("{rd} <=X= shr({rd}, 8 * tmp2);\n")
                + &format!("{rd} <=X= sign_extend_byte({rd});\n")
        }
        "lbu" => {
            // load byte and zero-extend. the memory is little-endian.
            let (rd, rs, off) = rro(args);
            format!("tmp1 <=X= wrap({rs} + {off});\n")
                + "addr <=X= and(tmp1, 0xfffffffc);\n"
                + "tmp2 <=X= and(tmp1, 0x3);\n"
                + &format!("{rd} <=X= mload();\n")
                + &format!("{rd} <=X= shr({rd}, 8 * tmp2);\n")
                + &format!("{rd} <=X= and({rd}, 0xff);\n")
        }
        "sw" => {
            let (r1, r2, off) = rro(args);
            format!("addr <=X= wrap({r2} + {off});\n") + &format!("mstore {r1};\n")
        }
        "sh" => {
            // store half word (two bytes)
            // TODO this code assumes it is at least aligned on
            // a two-byte boundary

            let (rs, rd, off) = rro(args);
            format!("tmp1 <=X= wrap({rd} + {off});\n")
                + "addr <=X= and(tmp1, 0xfffffffc);\n"
                + "tmp2 <=X= and(tmp1, 0x3);\n"
                + "tmp1 <=X= mload();\n"
                + "tmp3 <=X= shl(0xffff, 8 * tmp2);\n"
                + "tmp3 <=X= xor(tmp3, 0xffffffff);\n"
                + "tmp1 <=X= and(tmp1, tmp3);\n"
                + &format!("tmp3 <=X= and({rs}, 0xffff);\n")
                + "tmp3 <=X= shl(tmp3, 8 * tmp2);\n"
                + "tmp1 <=X= or(tmp1, tmp3);\n"
                + "mstore tmp1;\n"
        }
        "sb" => {
            // store byte
            let (rs, rd, off) = rro(args);
            format!("tmp1 <=X= wrap({rd} + {off});\n")
                + "addr <=X= and(tmp1, 0xfffffffc);\n"
                + "tmp2 <=X= and(tmp1, 0x3);\n"
                + "tmp1 <=X= mload();\n"
                + "tmp3 <=X= shl(0xff, 8 * tmp2);\n"
                + "tmp3 <=X= xor(tmp3, 0xffffffff);\n"
                + "tmp1 <=X= and(tmp1, tmp3);\n"
                + &format!("tmp3 <=X= and({rs}, 0xff);\n")
                + "tmp3 <=X= shl(tmp3, 8 * tmp2);\n"
                + "tmp1 <=X= or(tmp1, tmp3);\n"
                + "mstore tmp1;\n"
        }
        "unimp" => "fail;\n".to_string(),

        // Special instruction that is inserted to allow dynamic label references
        "load_dynamic" => {
            let (r1, label) = rl(args);
            format!("{r1} <=X= load_label({label});\n")
        }

        _ => {
            panic!("Unknown instruction: {instr}");
        }
    }
}
