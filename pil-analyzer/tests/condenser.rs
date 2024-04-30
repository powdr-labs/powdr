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
    z { z } in { even };
    let t = new_wit_arr();
    t[0] = t[1];
    "#;
    let expected = r#"namespace N(16);
    col fixed even(i) { (i * 2) };
    let new_wit: -> expr = (constr || {
        let x;
        x
    });
    let new_wit_arr: -> expr[] = (constr || {
        let x;
        [x, x]
    });
    col witness x;
    col witness y;
    let z: expr = N.new_wit();
    col witness x_1;
    N.x_1 = N.y;
    N.x_1 { N.x_1 } in { N.even };
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
        let x;
        x
    });
    col witness x;
    col witness x_1;
    col witness x_2;
    N.x = (N.x_1 + N.x_2);
"#;
    let formatted = analyze_string::<GoldilocksField>(input).to_string();
    assert_eq!(formatted, expected);
}

#[test]
fn create_constrainst() {
    let input = r#"namespace N(16);
    let force_bool: expr -> constr = |c| c * (1 - c) = 0;
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
    let force_bool: expr -> constr = (|c| ((c * (1 - c)) = 0));
    let new_bool: -> expr = (constr || {
        let x;
        N.force_bool(x);
        x
    });
    let is_zero: expr -> expr = (constr |x| {
        let x_is_zero;
        N.force_bool(x_is_zero);
        let x_inv;
        (x_is_zero = (1 - (x * x_inv)));
        ((x_is_zero * x) = 0);
        x_is_zero
    });
    col witness x;
    let x_is_zero: expr = N.is_zero(N.x);
    col witness y;
    col witness x_is_zero_1;
    col witness x_inv;
    (N.x_is_zero_1 * (1 - N.x_is_zero_1)) = 0;
    N.x_is_zero_1 = (1 - (N.x * N.x_inv));
    (N.x_is_zero_1 * N.x) = 0;
    N.y = (N.x_is_zero_1 + 2);
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
    let expected = r#"namespace std::convert(8);
    let expr = [];
namespace std::prover(8);
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
