use powdr_number::GoldilocksField;
use powdr_pil_analyzer::analyze_string;
use test_log::test;

use pretty_assertions::assert_eq;

#[test]
fn new_witness_column() {
    let input = r#"namespace N(16);
    let new_wit = constr || { let x; x };
    let x;
    let y;
    let z = new_wit();
    z = y;
    "#;
    let expected = r#"namespace N(16);
    let new_wit: -> expr = (constr || {
        let x;
        x
    });
    col witness x;
    col witness y;
    let z: expr = N.new_wit();
    col witness x_1;
    N.x_1 = N.y;
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
