use powdr_number::{FieldElement, GoldilocksField};
use powdr_pipeline::pipeline::Columns;
use powdr_pipeline::Pipeline;
use test_log::test;

fn run_witgen_pil<T: FieldElement>(pil: &str) -> Columns<T> {
    Pipeline::default()
        .from_pil_string(pil.to_string())
        .compute_witness()
        .unwrap()
        .0
        .clone()
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
        public pub = y(3);
        let even: col = |i| if i % 2 == 0 { 1 } else { 0 };
        pub * even = 8;
    namespace Machine2(8);
        col witness x, y;
        y = x + 1;
        let odd: col = |i| if i % 2 == 0 { 1 } else { 0 };
        Machine1::pub * odd * x = 9;
    "#;
    run_witgen_pil::<GoldilocksField>(src);
}
