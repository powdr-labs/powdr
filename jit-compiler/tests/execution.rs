use powdr_jit_compiler::LoadedFunction;
use test_log::test;

use powdr_number::GoldilocksField;
use powdr_pil_analyzer::analyze_string;

fn compile(input: &str, symbol: &str) -> LoadedFunction {
    let analyzed = analyze_string::<GoldilocksField>(input).unwrap();
    powdr_jit_compiler::compile(&analyzed, &[symbol])
        .map_err(|e| {
            eprintln!("{e}");
            e
        })
        .unwrap()[symbol]
        .clone()
}

#[test]
fn identity_function() {
    let f = compile("let c: int -> int = |i| i;", "c");

    assert_eq!(f.call(10), 10);
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

    assert_eq!(f.call(9), 3);
    assert_eq!(f.call(100), 10);
    assert_eq!(f.call(8), 2);
    assert_eq!(f.call(101), 10);
    assert_eq!(f.call(99), 9);
    assert_eq!(f.call(0), 0);
}

#[test]
fn match_expr() {
    let f = compile(
        r#"let f: int -> int = |x| match (x, ("abc", x + 3)) {
            (0, _) => 1,
            (1, ("ab", _)) => 2,
            (1, ("abc", t)) => t,
            (a, (_, b)) => a + b,
        };"#,
        "f",
    );

    assert_eq!(f.call(0), 1);
    assert_eq!(f.call(1), 2);
    assert_eq!(f.call(2), 3);
    assert_eq!(f.call(3), 0);
}

#[test]
#[should_panic = "Only (int -> int) functions and columns are supported, but requested c: int -> bool"]
fn invalid_function() {
    let _ = compile("let c: int -> bool = |i| true;", "c");
}
