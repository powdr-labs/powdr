mod common;

use lean_compiler::{compile_program, ProgramSource};
use test_log::test;

/// Compile a program, extract basic blocks, and snapshot each one.
fn compile_and_snapshot(program: &str, test_prefix: &str) {
    let bytecode = compile_program(&ProgramSource::Raw(program.to_string()));
    let blocks = common::extract_basic_blocks(&bytecode);
    assert!(!blocks.is_empty(), "no basic blocks extracted");
    for (i, bb) in blocks.into_iter().enumerate() {
        let test_name = format!("{test_prefix}_block_{i}");
        common::assert_machine_output(bb.into(), "compiled_programs", &test_name);
    }
}

#[test]
fn simple_arithmetic() {
    compile_and_snapshot(
        r#"
def main():
    a = 3
    b = 5
    c = a + b
    d = a * b
    return
"#,
        "simple_arithmetic",
    );
}

#[test]
fn conditional() {
    compile_and_snapshot(
        r#"
def main():
    x = 10
    y: Mut = 0
    if x == 10:
        y = x + 1
    else:
        y = x * 2
    return
"#,
        "conditional",
    );
}

#[test]
fn loop_unrolled() {
    compile_and_snapshot(
        r#"
def main():
    acc: Mut = 0
    for i in unroll(0, 5):
        acc = acc + i
    return
"#,
        "loop_unrolled",
    );
}

#[test]
fn function_call() {
    compile_and_snapshot(
        r#"
def main():
    a = NONRESERVED_PROGRAM_INPUT_START
    b = a + 8
    c = add_arrays(a, b)
    return

def add_arrays(x, y):
    result = Array(8)
    for i in unroll(0, 8):
        result[i] = x[i] + y[i]
    return result
"#,
        "function_call",
    );
}
