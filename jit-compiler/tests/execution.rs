use powdr_jit_compiler::{CompiledPIL, FixedColFunction};
use test_log::test;

use powdr_number::GoldilocksField;
use powdr_pil_analyzer::analyze_string;

fn compile(input: &str, symbols: &[&str]) -> CompiledPIL {
    let analyzed = analyze_string::<GoldilocksField>(input).unwrap();
    powdr_jit_compiler::compile(&analyzed, symbols)
        .map_err(|e| {
            eprintln!("Error jit-compiling:\n{e}");
            e
        })
        .unwrap()
}

fn compile_fun(input: &str, symbol: &str) -> FixedColFunction {
    compile(input, &[symbol])
        .get_fixed_column(symbol)
        .unwrap()
        .clone()
}

#[test]
fn identity_function() {
    let f = compile_fun("let c: int -> int = |i| i;", "c");

    assert_eq!(f.call(10), 10);
}

#[test]
fn sqrt() {
    let f = compile_fun(
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
    let _ = compile_fun("let c: int -> bool = |i| true;", "c");
}

#[test]
fn builtin_panic() {
    let input = r#"
        namespace std::check;
            let panic = [""];
        namespace main;
            let a: int -> int = |i| std::check::panic("test");
        "#;
    compile(input, &["main::a"]);
    // We de not call `a` because handling the panic is not yet properly implemented.
    // It currently causes an unhandled panic inside an `extern "C"` function, which results in
    // direct termination, so we cannot test it here.
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
    let obj = compile(input, &["main::c", "main::d"]);
    let c = obj.get_fixed_column("main::c").unwrap();

    assert_eq!(c.call(0), 1);
    assert_eq!(c.call(1), 2);
    assert_eq!(c.call(2), 3);
    assert_eq!(c.call(3), 4);

    let d = obj.get_fixed_column("main::d").unwrap();
    assert_eq!(d.call(0), 1);
}

#[test]
fn gigantic_number() {
    let f = compile_fun("let c: int -> int = |i| (i * 0x1000000000000000000000000000000000000000000000000000000000000000000000000000000000) >> (81 * 4);", "c");

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
    let obj = compile(input, &["main::q", "main::r"]);
    let q = obj.get_fixed_column("main::q").unwrap();

    assert_eq!(q.call(0), 1);
    assert_eq!(q.call(1), 2);
    assert_eq!(q.call(2), 3);
    assert_eq!(q.call(3), 1);

    let r = obj.get_fixed_column("main::r").unwrap();
    assert_eq!(r.call(0), 0);
    assert_eq!(r.call(1), 1);
    assert_eq!(r.call(2), 2);
    assert_eq!(r.call(3), 3);
}

#[test]
fn degree_builtin() {
    let input = r#"
        namespace std::prover;
            let degree = [""];
        namespace main;
            let a: int -> int = |i| std::prover::degree();
        "#;
    let compiled_pil = compile(input, &["main::a"]);
    compiled_pil.set_degree(128);

    let a = compiled_pil.get_fixed_column("main::a").unwrap();
    assert_eq!(a.call(0), 128);
    compiled_pil.set_degree(256);
    assert_eq!(a.call(0), 256);
}

#[test]
fn match_number() {
    let f = compile_fun(
        r#"let f: int -> int = |x| match x {
            0 => 1,
            1 => 2,
            2 => 3,
            _ => 0,
        };"#,
        "f",
    );

    assert_eq!(f.call(0), 1);
    assert_eq!(f.call(1), 2);
    assert_eq!(f.call(2), 3);
    assert_eq!(f.call(3), 0);
}

#[test]
fn match_negative() {
    let f = compile_fun(
        r#"let f: int -> int = |x| match -x {
            -0 => 1,
            -1 => 2,
            -2 => 3,
            _ => 9,
        };"#,
        "f",
    );

    assert_eq!(f.call(0), 1);
    assert_eq!(f.call(1), 2);
    assert_eq!(f.call(2), 3);
    assert_eq!(f.call(3), 9);
}

#[test]
fn match_string() {
    let f = compile_fun(
        r#"let f: int -> int = |x| match "abc" {
            "ab" => 1,
            "abc" => 2,
            _ => 0,
        };"#,
        "f",
    );

    assert_eq!(f.call(0), 2);
    assert_eq!(f.call(1), 2);
}

#[test]
fn match_tuples() {
    let f = compile_fun(
        r#"let f: int -> int = |x| match (x, ("abc", x + 3)) {
            (0, _) => 1,
            (1, ("ab", _)) => 2,
            (1, ("abc", t)) => t,
            (a, (_, b)) => a + b,
        };"#,
        "f",
    );

    assert_eq!(f.call(0), 1);
    assert_eq!(f.call(1), 4);
    assert_eq!(f.call(2), 7);
    assert_eq!(f.call(3), 9);
}

#[test]
fn match_array() {
    let f = compile_fun(
        r#"let f: int -> int = |y| match (y, [1, 3, 3, 4]) {
            (0, _) => 1,
            (1, [1, 3]) => 20,
            (1, [.., 2, 4]) => 20,
            (1, [.., x, 4]) => x - 1,
            (2, [x, .., 0]) => 22,
            (2, [x, .., 4]) => x + 2,
            (3, [1, 3, 3, 4, ..]) => 4,
            (4, [1, 3, 3, 4]) => 5,
            (5, [..]) => 6,
            _ => 7
        };"#,
        "f",
    );

    assert_eq!(f.call(0), 1);
    assert_eq!(f.call(1), 2);
    assert_eq!(f.call(2), 3);
    assert_eq!(f.call(3), 4);
    assert_eq!(f.call(4), 5);
    assert_eq!(f.call(5), 6);
    assert_eq!(f.call(6), 7);
}

#[test]
fn match_enum() {
    let f = compile_fun(
        r#"
enum Slice { S(int[], int, int) }
let slice_pop: Slice -> (Slice, Option<int>) = |s| match s {
    Slice::S(_, _, 0) => (s, Option::None),
    Slice::S(arr, start, l) => (Slice::S(arr, start, l - 1), Option::Some(arr[start + l - 1])),
};
let f: int -> int = |y| {
    let (s, last) = slice_pop(Slice::S([1, 2, y], 0, 3));
    match last {
        Option::Some(x) => x,
        Option::None => 0,
    }
};
"#,
        "f",
    );
    assert_eq!(f.call(0), 0);
    assert_eq!(f.call(1), 1);
    assert_eq!(f.call(2), 2);
}

#[test]
fn let_simple() {
    let f = compile_fun(
        r#"let f: int -> int = |x| {
            let a = 1;
            let b = a + 9;
            b - 9 + x
        };"#,
        "f",
    );

    assert_eq!(f.call(0), 1);
    assert_eq!(f.call(1), 2);
    assert_eq!(f.call(2), 3);
    assert_eq!(f.call(3), 4);
}

#[test]
fn let_complex() {
    let f = compile_fun(
        r#"let f: int -> int = |x| {
            let (a, b, (_, d)) = (1, 2, ("abc", [x, 5]));
            a + b + d[0] + d[1]
        };"#,
        "f",
    );

    assert_eq!(f.call(0), 8);
    assert_eq!(f.call(1), 9);
}

#[test]
fn enums() {
    let input = r#"
        namespace std::array;
            let len = 8;
        namespace main;
            enum Op<T> { Some(T), None }
            enum Items { Multiple(int[]), Single(int) }
            let a = |x| match x {
                Op::Some(i) => i,
                Op::None => 0,
            };
            let b = |x| match x {
                Items::Multiple(i) => i[0],
                Items::Single(i) => i,
            };
            let c = |i| match i {
                0 => a(Op::Some(i)),
                1 => a(Op::None),
                2 => b(Items::Multiple([1, 2, i])),
                _ => 99,
            };
        "#;
    let c = compile_fun(input, "main::c");

    assert_eq!(c.call(0), 0);
    assert_eq!(c.call(1), 0);
    assert_eq!(c.call(2), 1);
    assert_eq!(c.call(3), 99);
}

#[test]
fn clone_locals() {
    let f = compile_fun(
        r#"
        let a: int -> (int -> int) = |i| |_| i;
        let f: int -> int = |i| a(7)(i);
        "#,
        "f",
    );

    assert_eq!(f.call(0), 7);
    assert_eq!(f.call(1), 7);
    assert_eq!(f.call(2), 7);
}

#[test]
fn closures() {
    let input = "
        namespace std::convert;
            let fe = 99;
        namespace main;
            let eval_on: (int -> int), int -> int = |f, x| f(x);
            let q: col = |i| std::convert::fe(eval_on(|j| i + j, i));
        ";
    let q = compile_fun(input, "main::q");

    assert_eq!(q.call(0), 0);
    assert_eq!(q.call(1), 2);
    assert_eq!(q.call(2), 4);
    assert_eq!(q.call(3), 6);
}

#[test]
fn generic() {
    let input = "
        namespace std::convert;
            let fe = 99;
            let int = 100;
        namespace std::array;
            let len = 8;
        namespace main;
            let<T1, T2>
                fold: int, (int -> T1), T2, (T2, T1 -> T2) -> T2 = |length, f, initial, folder|
                    if length <= 0 {
                        initial
                    } else {
                        folder(fold((length - 1), f, initial, folder), f((length - 1)))
                    };
            let<T: Add + FromLiteral> sum: T[] -> T = |arr| fold(std::array::len(arr), |i| arr[i], 0, |acc, e| acc + e);
            let a: int[] = [1, 2, 3];
            let b: fe[] = [4, 5, 6];
            let q: col = |i| std::convert::fe(i + std::convert::int(sum(b)) + sum(a));
        ";
    let q = compile_fun(input, "main::q");

    assert_eq!(q.call(0), 21);
    assert_eq!(q.call(1), 22);
    assert_eq!(q.call(2), 23);
    assert_eq!(q.call(3), 24);
}
