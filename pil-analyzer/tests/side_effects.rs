use powdr_number::GoldilocksField;
use powdr_pil_analyzer::analyze_string;
use test_log::test;

#[test]
#[should_panic = "Tried to create a witness column in a pure context: let x;"]
fn new_wit_in_pure() {
    let input = r#"namespace N(16);
    let new_col = || { let x; x };
    "#;
    analyze_string::<GoldilocksField>(input);
}

#[test]
#[should_panic = "Tried to create a fixed column in a pure context: let x: col = |i| i;"]
fn new_fixed_in_pure() {
    let input = r#"namespace N(16);
    let new_col = || { let x: col = |i| i; x };
    "#;
    analyze_string::<GoldilocksField>(input);
}

#[test]
#[should_panic = "Expected type () but got type std::prelude::Constr"]
fn constr_in_pure() {
    let input = r#"namespace N(16);
    let new_col = |x| { x = 7; [] };
    "#;
    analyze_string::<GoldilocksField>(input);
}

#[test]
fn return_constr_in_pure() {
    let input = r#"namespace N(16);
    let new_col = |x| x = 7;
    "#;
    analyze_string::<GoldilocksField>(input);
}

#[test]
#[should_panic = "Referenced a query function inside a pure context: std::prover::eval"]
fn call_eval_in_pure() {
    let input = r#"
    namespace std::prover(16);
        let eval = [];
    namespace N(16);
        let val_of_x = |x| std::prover::eval(x);
    "#;
    analyze_string::<GoldilocksField>(input);
}

#[test]
fn call_eval_in_query() {
    let input = r#"
    namespace std::prover(16);
        let eval = [];
    namespace N(16);
        let val_of_x = query |x| std::prover::eval(x);
    "#;
    analyze_string::<GoldilocksField>(input);
}

#[test]
#[should_panic = "Referenced a constr function inside a query context: N::new_wit"]
fn call_constr_in_query() {
    let input = r#"
    namespace std::prover(16);
        let eval = [];
    namespace N(16);
        let new_wit = constr || { let x; x };
        let val_of_x = query |x| std::prover::eval(new_wit());
    "#;
    analyze_string::<GoldilocksField>(input);
}

#[test]
#[should_panic = "Used a constr lambda function inside a pure context"]
fn constr_lambda_in_pure() {
    let input = r#"namespace N(16);
    let f = |x| {
        let new_wit = constr || { x = 7; 0};
        8
    };
    "#;
    analyze_string::<GoldilocksField>(input);
}

#[test]
#[should_panic = "Expected type () but got type std::prelude::Constr"]
fn reset_context() {
    let input = r#"namespace N(16);
    let new_col = |x| { x = 7; [] };
    "#;
    analyze_string::<GoldilocksField>(input);
}

#[test]
#[should_panic = "Used a constr lambda function inside a pure context"]
fn fixed_with_constr_type() {
    let input = "let x: col = constr |i| 2;";
    analyze_string::<GoldilocksField>(input);
}

#[test]
fn set_hint() {
    let input = r#"
    namespace std::prover;
        enum Query { Hint(fe), None, }
    namespace N(16);
        let x;
        std::prelude::set_hint(x, query |i| std::prelude::Query::Hint(1));
    "#;
    analyze_string::<GoldilocksField>(input);
}

#[test]
fn set_hint_can_use_query() {
    let input = r#"
    namespace std::prover;
        let eval = 7;
        enum Query { Hint(fe), None, }
    namespace N(16);
        let x;
        let y;
        std::prelude::set_hint(x, query |_| std::prelude::Query::Hint(std::prover::eval(y)));
    "#;
    analyze_string::<GoldilocksField>(input);
}

#[test]
fn set_hint_pure() {
    let input = r#"
    namespace std::prover;
        enum Query { Hint(fe), None, }
    namespace N(16);
        let x;
        std::prelude::set_hint(x, |i| std::prelude::Query::Hint(1));
    "#;
    analyze_string::<GoldilocksField>(input);
}

#[test]
#[should_panic = "Used a constr lambda function inside a query context"]
fn set_hint_constr() {
    let input = r#"
    namespace std::prover;
        enum Query { Hint(fe), None, }
    namespace N(16);
        let x;
        std::prelude::set_hint(x, constr |i| std::prelude::Query::Hint(1));
    "#;
    analyze_string::<GoldilocksField>(input);
}

#[test]
fn query_in_constr() {
    let input = r#"namespace N(16);
    query |i| { };
    let f = constr || { query |i| { } };
    f();
    "#;
    analyze_string::<GoldilocksField>(input);
}
