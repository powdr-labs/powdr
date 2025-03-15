use powdr_number::{FieldElement, GoldilocksField};
use powdr_pipeline::pipeline::Columns;
use powdr_pipeline::Pipeline;
use std::sync::Arc;
use test_log::test;

fn run_witgen_pil<T: FieldElement>(pil: &str) -> Arc<Columns<T>> {
    Pipeline::default()
        .with_backend(powdr_pipeline::BackendType::Mock, None)
        .from_pil_string(pil.to_string())
        .compute_witness()
        .unwrap()
}

#[test]
#[should_panic = "Publics are referenced by more than one machine: Machine1::pub"]
fn two_machines_conflicting_public() {
    // This test *should* fail, because two machines access the same
    // public, but assign different values to it.
    let src = r#"
    namespace Machine1(4);
        col witness y;
        [ 42, y ] in [ Machine2.x, Machine2.y ];
        // y will be 43 on all rows
        public pub = y(3);
        y - pub = 0;
    namespace Machine2(8);
        col witness x, y;
        y = x + 1;
        // x will be 42 on all rows
        x - Machine1::pub = 0;
    "#;
    run_witgen_pil::<GoldilocksField>(src);
}
