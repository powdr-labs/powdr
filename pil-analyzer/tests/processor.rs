use std::collections::BTreeSet;

use powdr_ast::{
    analyzed::{Analyzed, Expression, FunctionValueDefinition},
    parsed::{LambdaExpression, TypedExpression},
};
use powdr_number::GoldilocksField;

use powdr_pil_analyzer::analyze_string;

use pretty_assertions::assert_eq;

fn extract_expression<'a, T>(analyzed: &'a Analyzed<T>, name: &str) -> &'a Expression {
    match analyzed.definitions[name].1.as_ref().unwrap() {
        FunctionValueDefinition::Expression(TypedExpression { e, .. }) => e,
        _ => panic!(),
    }
}

fn outer_vars_of_lambda(expr: &Expression) -> &BTreeSet<u64> {
    match expr {
        Expression::LambdaExpression(
            _,
            LambdaExpression {
                outer_var_references,
                ..
            },
        ) => outer_var_references,
        _ => panic!(),
    }
}

#[test]
fn determine_outer_var_refs() {
    let input = "
        let f: int, int -> int = |i, _| i;
        let g: int, int -> int = |_, i| i;
        let h: int -> (int -> (int, int)) = |i| |j| (f(i, j), g(i, j));
        let k: int, int -> (int -> (int, int)) = |k, i| |j| (f(i, j), g(i, j));
        ";

    let analyzed = analyze_string::<GoldilocksField>(input).unwrap();
    assert!(outer_vars_of_lambda(extract_expression(&analyzed, "f")).is_empty());
    assert!(outer_vars_of_lambda(extract_expression(&analyzed, "g")).is_empty());
    let h = extract_expression(&analyzed, "h");
    assert!(outer_vars_of_lambda(h).is_empty());
    let h_body = match h {
        Expression::LambdaExpression(_, LambdaExpression { body, .. }) => body,
        _ => panic!(),
    };
    assert_eq!(outer_vars_of_lambda(h_body), &[0].into_iter().collect());
    let k = extract_expression(&analyzed, "k");
    assert!(outer_vars_of_lambda(k).is_empty());
    let k_body = match k {
        Expression::LambdaExpression(_, LambdaExpression { body, .. }) => body,
        _ => panic!(),
    };
    assert_eq!(outer_vars_of_lambda(k_body), &[1].into_iter().collect());
}
