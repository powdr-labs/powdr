use powdr_ast::analyzed::types::{format_type_scheme_around_name, TypeScheme};
use powdr_number::GoldilocksField;
use powdr_parser::{parse_type_name, parse_type_var_bounds};
use powdr_pil_analyzer::analyze_string;

fn parse_type_scheme(vars: &str, ty: &str) -> TypeScheme {
    let vars = parse_type_var_bounds(vars).unwrap();
    let ty = parse_type_name::<GoldilocksField>(ty).unwrap();
    TypeScheme {
        vars,
        ty: ty.into(),
    }
}

#[test]
fn type_scheme_simplify_type_vars_basic() {
    let ts = parse_type_scheme("A, B, C", "B -> (C -> (A, B))").simplify_type_vars();
    assert_eq!(
        format_type_scheme_around_name("x", &Some(ts)),
        "<T1, T2, T3> x: T2 -> (T3 -> (T1, T2))"
    );
}

#[test]
fn type_scheme_simplify_type_vars() {
    // Test conflicts between the old and new names.
    let ts = parse_type_scheme("T2: FromLiteral + Sum, T1", "T2 -> T1[]").simplify_type_vars();
    assert_eq!(
        format_type_scheme_around_name("x", &Some(ts)),
        "<T1: FromLiteral + Sum, T2> x: T1 -> T2[]"
    );
}

#[test]
#[should_panic = "Expected algebraic expression, got (|i| i)"]
fn use_fun_in_expr_context() {
    let input = r#"namespace N(16);
    let id = |i| i;
    let w;
    w = id;
"#;
    analyze_string::<GoldilocksField>(input);
}
