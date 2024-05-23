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
    col fixed even(i) { i * 2 };
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
        let x;
        N.force_bool(x);
        x
    });
    let is_zero: expr -> expr = (constr |x| {
        let x_is_zero;
        N.force_bool(x_is_zero);
        let x_inv;
        x_is_zero = 1 - x * x_inv;
        x_is_zero * x = 0;
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

#[test]
pub fn constructed_constraints() {
    let input = r#"
        namespace Main(1024);
            let x;
            let y;
            let z;
            Constr::Identity(x, y);
            Constr::Lookup(Option::Some(1), [x, 3], Option::None, [y, z]);
            Constr::Permutation(Option::None, [x, 3], Option::Some(x), [y, z]);
            Constr::Connection([x, y], [z, 3]);
    "#;
    let formatted = analyze_string::<GoldilocksField>(input).to_string();
    let expected = r#"namespace Main(1024);
    col witness x;
    col witness y;
    col witness z;
    Main.x = Main.y;
    1 { Main.x, 3 } in { Main.y, Main.z };
    { Main.x, 3 } is Main.x { Main.y, Main.z };
    { Main.x, Main.y } connect { Main.z, 3 };
"#;
    assert_eq!(formatted, expected);
}

#[test]
#[should_panic = "There are pre-existing constraints or witness columns"]
pub fn capture_stage_non_fresh_wit() {
    let input = r#"
        namespace std::prover;
            let capture_stage: (-> int) -> Constr[] = 9;

        namespace Main(1024);
            col witness x;
            let f = constr || { x = 2; 1024 };
            std::prover::capture_stage(f);
    "#;
    analyze_string::<GoldilocksField>(input);
}

#[test]
#[should_panic = "There are pre-existing constraints or witness columns"]
pub fn capture_stage_non_fresh_inter() {
    let input = r#"
        namespace std::prover;
            let capture_stage: (-> int) -> Constr[] = 9;

        namespace Main(1024);
            col x = 3;
            let f = constr || { x = 2; 1024 };
            std::prover::capture_stage(f);
    "#;
    analyze_string::<GoldilocksField>(input);
}

#[test]
#[should_panic = "There are pre-existing constraints or witness columns"]
pub fn capture_stage_non_fresh_constr() {
    let input = r#"
        namespace std::prover;
            let capture_stage: (-> int) -> Constr[] = 9;

        namespace Main(1024);
            4 = 5;
            let f = constr || 1024;
            std::prover::capture_stage(f);
    "#;
    analyze_string::<GoldilocksField>(input);
}

#[test]
#[should_panic = "returned degree 2048, but the degree has already been set to 1024."]
pub fn capture_stage_set_different_degree() {
    let input = r#"
        namespace std::prover;
            let capture_stage: (-> int) -> Constr[] = 9;

        namespace Main(1024);
            let f = constr || 2048;
            std::prover::capture_stage(f);
    "#;
    analyze_string::<GoldilocksField>(input);
}

#[test]
pub fn capture_stage_working() {
    let input = r#"
        namespace std::prover;
            let capture_stage: (-> int) -> Constr[] = 9;

        namespace Main;
            let f = constr || { let x; let y; x + y = 2; x = 8; 1024 };
            // Just use the second constraint.
            std::prover::capture_stage(f)[1];
            let w;
            w = 10;
    "#;
    let formatted = analyze_string::<GoldilocksField>(input).to_string();
    let expected = "namespace std::prover(1024);
    let capture_stage: (-> int) -> std::prelude::Constr[] = 9;
namespace Main(1024);
    let f: -> int = (constr || {
        let x;
        let y;
        x + y = 2;
        x = 8;
        1024
    });
    col witness x;
    col witness y;
    Main.x = 8;
    col witness stage(1) w;
    Main.w = 10;
";
    assert_eq!(formatted, expected);
}
