use asmopt::optimize;
use powdr_analysis::analyze;
use powdr_importer::load_dependencies_and_resolve_str;

#[test]
fn base() {
    let input = r#"
"#;
    let expectation = r#"
"#;
    let analyzed = analyze(load_dependencies_and_resolve_str(input)).unwrap();
    let optimized = optimize(analyzed).to_string();
    assert_eq!(optimized, expectation);
}
