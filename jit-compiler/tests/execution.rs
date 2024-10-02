use powdr_jit_compiler::LoadedFunction;
use test_log::test;

use powdr_number::GoldilocksField;
use powdr_pil_analyzer::analyze_string;

fn compile(input: &str, symbol: &str) -> LoadedFunction {
    let analyzed = analyze_string::<GoldilocksField>(input).unwrap();
    powdr_jit_compiler::compile(&analyzed, &[symbol])
        .map_err(|e| {
            eprintln!("Error jit-compiling:\n{e}");
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
#[should_panic = "Only (int -> int) functions and columns are supported, but requested c: int -> bool"]
fn invalid_function() {
    let _ = compile("let c: int -> bool = |i| true;", "c");
}

#[test]
fn assigned_functions() {
    let input = r#"
        namespace std::array;
            let len = 8;
        namespace main;
            let a: int -> int = |i| i + 1;
            let b: int -> int = |i| i + 2;
            let t: bool = "" == "";
            let c = if t { a } else { b };
            let d = |i| c(i);
        "#;
    let c = compile(input, "main::c");

    assert_eq!(c.call(0), 1);
    assert_eq!(c.call(1), 2);
    assert_eq!(c.call(2), 3);
    assert_eq!(c.call(3), 4);

    let d = compile(input, "main::d");
    assert_eq!(d.call(0), 1);
}

#[test]
fn gigantic_number() {
    let f = compile("let c: int -> int = |i| (i * 0x1000000000000000000000000000000000000000000000000000000000000000000000000000000000) >> (81 * 4);", "c");

    assert_eq!(f.call(10), 10);
}

#[test]
fn simple_field() {
    let input = "
        namespace std::convert;
            let fe = 99;
            let int = 100;
        namespace std::array;
            let len = 8;
        namespace main;
            let a: fe[] = [1, 2, 3];
            let k: int -> int = |i| i;
            let q: col = |i| a[i % std::array::len(a)];
            let r: col = |i| std::convert::fe(k(i));
        ";
    let q = compile(input, "main::q");

    assert_eq!(q.call(0), 1);
    assert_eq!(q.call(1), 2);
    assert_eq!(q.call(2), 3);
    assert_eq!(q.call(3), 1);

    let r = compile(input, "main::r");
    assert_eq!(r.call(0), 0);
    assert_eq!(r.call(1), 1);
    assert_eq!(r.call(2), 2);
    assert_eq!(r.call(3), 3);
}
