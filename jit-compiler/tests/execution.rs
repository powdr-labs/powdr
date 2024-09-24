use test_log::test;

use powdr_number::GoldilocksField;
use powdr_pil_analyzer::analyze_string;

fn compile(input: &str, symbol: &str) -> fn(u64) -> u64 {
    let analyzed = analyze_string::<GoldilocksField>(input).unwrap();
    powdr_jit_compiler::compile(&analyzed, &[symbol]).unwrap()[symbol]
}

#[test]
fn identity_function() {
    let f = compile("let c: int -> int = |i| i;", "c");

    assert_eq!(f(10), 10);
}

#[test]
fn sqrt() {
    let f = compile(
        "
        let sqrt_rec: int, int -> int = |y, x|
        if y * y <= x && (y + 1) * (y + 1) > x {
            y
        } else {
            sqrt_rec((y + x / y) / 2, x)
        };

        let sqrt: int -> int = |x| sqrt_rec(x, x);",
        "sqrt",
    );

    assert_eq!(f(9), 3);
    assert_eq!(f(100), 10);
    assert_eq!(f(8), 2);
    assert_eq!(f(101), 10);
    assert_eq!(f(99), 9);
    assert_eq!(f(0), 0);
}

#[test]
#[should_panic = "Only (int -> int) functions and columns are supported, but requested c: int -> bool"]
fn invalid_function() {
    let _ = compile("let c: int -> bool = |i| true;", "c");
}
