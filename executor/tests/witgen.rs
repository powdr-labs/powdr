use powdr_number::GoldilocksField;
use powdr_pil_analyzer::analyze_string;
use powdr_pipeline::Pipeline;

#[test]
fn two_machines_conflicting_public() {
    env_logger::init();

    // This test *should* fail, because two machines access the same
    // public, but assign different values to it.
    // The current implementation ignores scalar publics though, so this
    // is not caught.

    let src = r#"
    namespace Machine1(4);
        col witness y;
        [ 42, y ] in [ Machine2.x, Machine2.y ];

        // y will be 43 on all rows
        y - :public = 0;

    namespace Machine2(8);
        col witness x, y;
        y = x + 1;

        // x will be 42 on all rows
        x - :public = 0;
    "#;
    let witness = Pipeline::<GoldilocksField>::default()
        .from_pil_string(src.to_string())
        .compute_witness()
        .unwrap();
}
