use powdr_number::GoldilocksField;
use powdr_pil_analyzer::analyze_string;
use test_log::test;

use pretty_assertions::assert_eq;

#[test]
fn new_witness_column() {
    let input = r#"namespace N(16);
    let even: col = |i| i * 2;
    let new_wit = constr || { let x; x };
    let new_wit_arr = constr || { let x; [x, x] };
    let x;
    let y;
    let z = new_wit();
    z = y;
    z $ [z] in [even];
    let t = new_wit_arr();
    t[0] = t[1];
    "#;
    let expected = r#"namespace N(16);
    col fixed even(i) { i * 2 };
    let new_wit: -> expr = (constr || {
        let x: col;
        x
    });
    let new_wit_arr: -> expr[] = (constr || {
        let x: col;
        [x, x]
    });
    col witness x;
    col witness y;
    let z: expr = N.new_wit();
    col witness x_1;
    N.x_1 = N.y;
    N.x_1 $ [N.x_1] in [N.even];
    let t: expr[] = N.new_wit_arr();
    col witness x_2;
    N.x_2 = N.x_2;
"#;
    let formatted = analyze_string::<GoldilocksField>(input).to_string();
    assert_eq!(formatted, expected);
}

#[test]
fn new_witness_column_name_clash() {
    let input = r#"namespace N(16);
    let new_wit = constr || { let x; x };
    new_wit() = new_wit() + new_wit();
    "#;
    let expected = r#"namespace N(16);
    let new_wit: -> expr = (constr || {
        let x: col;
        x
    });
    col witness x;
    col witness x_1;
    col witness x_2;
    N.x = N.x_1 + N.x_2;
"#;
    let formatted = analyze_string::<GoldilocksField>(input).to_string();
    assert_eq!(formatted, expected);
}

#[test]
fn create_constraints() {
    let input = r#"namespace N(16);
    let force_bool: expr -> Constr = |c| c * (1 - c) = 0;
    let new_bool: -> expr = constr || { let x; force_bool(x); x };
    let is_zero: expr -> expr = constr |x| {
        let x_is_zero;
        force_bool(x_is_zero);
        let x_inv;
        x_is_zero = 1 - x * x_inv;
        x_is_zero * x = 0;
        x_is_zero
    };
    let x;
    let x_is_zero = is_zero(x);
    let y;
    y = x_is_zero + 2;
    "#;
    let expected = r#"namespace N(16);
    let force_bool: expr -> std::prelude::Constr = (|c| c * (1 - c) = 0);
    let new_bool: -> expr = (constr || {
        let x: col;
        N.force_bool(x);
        x
    });
    let is_zero: expr -> expr = (constr |x| {
        let x_is_zero: col;
        N.force_bool(x_is_zero);
        let x_inv: col;
        x_is_zero = 1 - x * x_inv;
        x_is_zero * x = 0;
        x_is_zero
    });
    col witness x;
    let x_is_zero: expr = N.is_zero(N.x);
    col witness y;
    col witness x_is_zero_1;
    col witness x_inv;
    N.x_is_zero_1 * (1 - N.x_is_zero_1) = 0;
    N.x_is_zero_1 = 1 - N.x * N.x_inv;
    N.x_is_zero_1 * N.x = 0;
    N.y = N.x_is_zero_1 + 2;
"#;
    let formatted = analyze_string::<GoldilocksField>(input).to_string();
    assert_eq!(formatted, expected);
}

#[test]
pub fn degree() {
    let input = r#"
        namespace std::convert;
            let expr = [];
        namespace std::prover;
            let degree = [];
        namespace Main(8);
            let d = std::prover::degree();
            let w;
            w = std::convert::expr(d);
    "#;
    let formatted = analyze_string::<GoldilocksField>(input).to_string();
    let expected = r#"namespace std::convert;
    let expr = [];
namespace std::prover;
    let degree = [];
namespace Main(8);
    let d: int = std::prover::degree();
    col witness w;
    Main.w = 8;
"#;
    assert_eq!(formatted, expected);
}

#[test]
#[should_panic = "Error: DataNotAvailable"]
pub fn degree_unset() {
    let input = r#"
        namespace std::convert;
            let expr = [];
        namespace std::prover;
            let degree = [];
        namespace Main;
            let d = std::prover::degree();
            let w;
            w = std::convert::expr(d);
    "#;
    analyze_string::<GoldilocksField>(input);
}

#[test]
pub fn constructed_constraints() {
    let input = r#"
        namespace Main(1024);
            let x;
            let y;
            let z;
            Constr::Identity(x, y);
            Constr::Lookup((Option::Some(1), Option::None), [(x, y), (3, z)]);
            Constr::Permutation((Option::None, Option::Some(x)), [(x, y), (3, z)]);
            Constr::Connection([(x, z), (y, 3)]);
    "#;
    let formatted = analyze_string::<GoldilocksField>(input).to_string();
    let expected = r#"namespace Main(1024);
    col witness x;
    col witness y;
    col witness z;
    Main.x = Main.y;
    1 $ [Main.x, 3] in [Main.y, Main.z];
    [Main.x, 3] is Main.x $ [Main.y, Main.z];
    [Main.x, Main.y] connect [Main.z, 3];
"#;
    assert_eq!(formatted, expected);
}

#[test]
fn next() {
    let input = r#"namespace N(16);
        col witness x;
        col witness y;
        x * y = 1';
        x * y = (1 + x)';
    "#;
    let formatted = analyze_string::<GoldilocksField>(input).to_string();
    let expected = r#"namespace N(16);
    col witness x;
    col witness y;
    N.x * N.y = 1;
    N.x * N.y = 1 + N.x';
"#;
    assert_eq!(formatted, expected);
}

#[test]
#[should_panic = "Double application of \\\"'\\\" on: N.x"]
fn double_next() {
    let input = r#"namespace N(16);
        col witness x;
        col witness y;
        x * y = (1 + x')';
    "#;
    analyze_string::<GoldilocksField>(input).to_string();
}

#[test]
fn new_fixed_column() {
    let input = r#"namespace N(16);
        let f = constr || {
            let even: col = |i| i * 2;
            even
        };
        let ev = f();
        let x;
        x = ev;
    "#;
    let formatted = analyze_string::<GoldilocksField>(input).to_string();
    let expected = r#"namespace N(16);
    let f: -> expr = (constr || {
        let even: col = (|i| i * 2);
        even
    });
    let ev: expr = N.f();
    col witness x;
    col fixed even(i) { i * 2 };
    N.x = N.even;
"#;
    assert_eq!(formatted, expected);
}

#[test]
#[should_panic = "Error creating fixed column N.fi: Lambda expression must not reference outer variables."]
fn new_fixed_column_as_closure() {
    let input = r#"namespace N(16);
        let f = constr |j| {
            let fi: col = |i| (i + j) * 2;
            fi
        };
        let ev = f(2);
        let x;
        x = ev;
    "#;
    analyze_string::<GoldilocksField>(input);
}

#[test]
fn set_hint() {
    let input = r#"
    namespace std::prover;
        let set_hint = 8;
        let eval = 8;
        enum Query { Hint(fe), None, }
    namespace N(16);
        let x;
        let y;
        std::prover::set_hint(x, query |_| std::prover::Query::Hint(1));
        std::prover::set_hint(y, |i| std::prover::Query::Hint(std::prover::eval(x)));
    "#;
    let expected = r#"namespace std::prover;
    let set_hint = 8;
    let eval = 8;
    enum Query {
        Hint(fe),
        None,
    }
namespace N(16);
    col witness x(_) query std::prover::Query::Hint(1);
    col witness y(i) query std::prover::Query::Hint(std::prover::eval(N.x));
"#;
    let formatted = analyze_string::<GoldilocksField>(input).to_string();
    assert_eq!(formatted, expected);
}

// TODO test with two variables
