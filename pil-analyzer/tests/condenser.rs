use powdr_number::GoldilocksField;
use powdr_pil_analyzer::analyze_string;
use test_log::test;

use pretty_assertions::assert_eq;

#[test]
fn new_witness_column() {
    let input = r#"namespace N(16);
    let even: col = |i| i * 2;
    let new_wit = || { let x; x };
    let x;
    let y;
    let z = new_wit();
    z = y;
    z { z } in { even };
    "#;
    let expected = r#"namespace N(16);
    col fixed even(i) { (i * 2) };
    let new_wit: -> expr = (|| {
        let x;
        x
    });
    col witness x;
    col witness y;
    let z: expr = N.new_wit();
    col witness x_1;
    N.x_1 = N.y;
    N.x_1 { N.x_1 } in { N.even };
"#;
    let formatted = analyze_string::<GoldilocksField>(input).to_string();
    assert_eq!(formatted, expected);
}

#[test]
fn new_witness_column_name_clash() {
    let input = r#"namespace N(16);
    let new_wit = || { let x; x };
    new_wit() = new_wit() + new_wit();
    "#;
    let expected = r#"namespace N(16);
    let new_wit: -> expr = (|| {
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
