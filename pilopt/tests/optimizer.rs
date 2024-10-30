use powdr_number::GoldilocksField;
use powdr_pil_analyzer::analyze_string;

use pretty_assertions::assert_eq;

use powdr_pilopt::optimize;

#[test]
fn replace_fixed() {
    let input = r#"namespace N(65536);
    col fixed one = [1]*;
    col fixed zero = [0]*;
    col witness X;
    col witness Y;
    X * one = X * zero - zero + Y;
    one * Y = zero * Y + 7 * X;
"#;
    let expectation = r#"namespace N(65536);
    col witness X;
    col witness Y;
    N::X = N::Y;
    N::Y = 7 * N::X;
"#;
    let optimized = optimize(analyze_string::<GoldilocksField>(input).unwrap()).to_string();
    assert_eq!(optimized, expectation);
}

#[test]
fn replace_lookup() {
    let input = r#"namespace N(65536);
    col fixed one = [1]*;
    col fixed zero = [0]*;
    col fixed two = [2]*;
    col fixed cnt(i) { i };
    col witness X;
    col witness Y;
    col witness W;
    col witness Z;
    col witness A;
    (1 - A) $ [ X, Y, A ] in [ zero, one, cnt ];
    [ Y, W, Z, A ] in (1 + A) $ [ cnt, zero, two, one ];
    [ W, Z ] in (1 + A) $ [ zero, one ];
"#;
    let expectation = r#"namespace N(65536);
    col fixed cnt(i) { i };
    col witness X;
    col witness Y;
    col witness Z;
    col witness A;
    1 - N::A $ [N::A] in [N::cnt];
    [N::Y] in 1 + N::A $ [N::cnt];
    (1 - N::A) * N::X = 0;
    (1 - N::A) * N::Y = 1;
    N::Z = (1 + N::A) * 2;
    N::A = 1 + N::A;
    N::Z = 1 + N::A;
"#;
    let optimized = optimize(analyze_string::<GoldilocksField>(input).unwrap()).to_string();
    assert_eq!(optimized, expectation);
}

#[test]
fn intermediate() {
    let input = r#"namespace N(65536);
        col witness x;
        col intermediate = x;
        intermediate = intermediate;
    "#;
    let expectation = r#"namespace N(65536);
    col witness x;
    col intermediate = N::x;
    N::intermediate = N::intermediate;
"#;
    let optimized = optimize(analyze_string::<GoldilocksField>(input).unwrap()).to_string();
    assert_eq!(optimized, expectation);
}

#[test]
fn zero_sized_array() {
    let input = r#"
        namespace std::array(65536);
            let<T> len: T[] -> int = [];
        namespace N(65536);
            col witness x[1];
            col witness y[0];
            let t: col = |i| std::array::len(y);
            x[0] = t;
    "#;
    let expectation = r#"namespace std::array(65536);
    let<T> len: T[] -> int = [];
namespace N(65536);
    col witness x[1];
    col witness y[0];
    col fixed t(i) { std::array::len::<expr>(N::y) };
    N::x[0] = N::t;
"#;
    let optimized = optimize(analyze_string::<GoldilocksField>(input).unwrap()).to_string();
    assert_eq!(optimized, expectation);
}

#[test]
fn remove_duplicates() {
    let input = r#"namespace N(65536);
        col witness x;
        col fixed cnt(i) { i };

        x * (x - 1) = 0;
        x * (x - 1) = 0;
        x * (x - 1) = 0;

        [ x ] in [ cnt ];
        [ x ] in [ cnt ];
        [ x ] in [ cnt ];

        [ x + 1 ] in [ cnt ];
        [ x ] in [ cnt + 1 ];
    "#;
    let expectation = r#"namespace N(65536);
    col witness x;
    col fixed cnt(i) { i };
    N::x * (N::x - 1) = 0;
    [N::x] in [N::cnt];
    [N::x + 1] in [N::cnt];
    [N::x] in [N::cnt + 1];
"#;
    let optimized = optimize(analyze_string::<GoldilocksField>(input).unwrap()).to_string();
    assert_eq!(optimized, expectation);
}

#[test]
fn remove_unreferenced() {
    let input = r#"namespace N(65536);
        col witness x;
        col fixed cnt(i) { inc(i) };
        let inc = |x| x + 1;
        // these are removed
        col witness k;
        col k2 = k;
        let rec: -> int = || rec();
        let a: int -> int = |i| b(i + 1);
        let b: int -> int = |j| 8;
        // identity
        [ x ] in [ cnt ];

    "#;
    let expectation = r#"namespace N(65536);
    col witness x;
    col fixed cnt(i) { N::inc(i) };
    let inc: int -> int = |x| x + 1_int;
    [N::x] in [N::cnt];
"#;
    let optimized = optimize(analyze_string::<GoldilocksField>(input).unwrap()).to_string();
    assert_eq!(optimized, expectation);
}

#[test]
fn remove_unreferenced_parts_of_arrays() {
    let input = r#"namespace N(65536);
        col witness x[5];
        let inte: inter[5] = x;
        x[2] = inte[4];
    "#;
    let expectation = r#"namespace N(65536);
    col witness x[5];
    col inte[5] = [N::x[0], N::x[1], N::x[2], N::x[3], N::x[4]];
    N::x[2] = N::inte[4];
"#;
    let optimized = optimize(analyze_string::<GoldilocksField>(input).unwrap());
    assert_eq!(optimized.intermediate_count(), 5);
    assert_eq!(optimized.to_string(), expectation);
}

#[test]
fn remove_unreferenced_keep_enums() {
    let input = r#"namespace N(65536);
        enum X { A, B, C }
        enum Y { D, E, F(R[]) }
        enum R { T }
        let t: X[] -> int = |r| 1;
        // This references Y::F but even after type checking, the type
        // Y is not mentioned anywhere.
        let f: col = |i| if i == 0 { t([]) } else { (|x| 1)(Y::F([])) };
        let x;
        x = f;
    "#;
    let expectation = r#"namespace N(65536);
    enum X {
        A,
        B,
        C,
    }
    enum Y {
        D,
        E,
        F(N::R[]),
    }
    enum R {
        T,
    }
    let t: N::X[] -> int = |r| 1_int;
    col fixed f(i) { if i == 0_int { N::t([]) } else { (|x| 1_int)(N::Y::F([])) } };
    col witness x;
    N::x = N::f;
"#;
    let optimized = optimize(analyze_string::<GoldilocksField>(input).unwrap()).to_string();
    assert_eq!(optimized, expectation);
}
