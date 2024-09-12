use powdr_jit_compiler::codegen::Compiler;
use powdr_number::GoldilocksField;
use powdr_pil_analyzer::analyze_string;

use pretty_assertions::assert_eq;

fn compile(input: &str, syms: &[&str]) -> String {
    let analyzed = analyze_string::<GoldilocksField>(input);
    let mut compiler = Compiler::new(&analyzed);
    for s in syms {
        compiler.request_symbol(s).unwrap();
    }
    compiler.compiled_symbols()
}

#[test]
fn empty_code() {
    let result = compile("", &[]);
    assert_eq!(result, "");
}

#[test]
fn simple_fun() {
    let result = compile("let c: int -> int = |i| i;", &["c"]);
    assert_eq!(
        result,
        "fn c(i: num_bigint::BigInt) -> num_bigint::BigInt { i }\n"
    );
}

#[test]
fn constant() {
    let result = compile("let c: int -> int = |i| i; let d = c(20);", &["c", "d"]);
    assert_eq!(
        result,
        "fn c(i: num_bigint::BigInt) -> num_bigint::BigInt { i }\n"
    );
}
