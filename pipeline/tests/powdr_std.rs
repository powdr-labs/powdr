use std::rc::Rc;

use powdr_ast::analyzed::Analyzed;
use powdr_number::{FieldElement, GoldilocksField};
use powdr_pil_analyzer::evaluator::{self, SymbolLookup};
use powdr_pipeline::{
    test_util::{gen_estark_proof, gen_halo2_proof, verify_test_file},
    Pipeline, Stage,
};
use test_log::test;

#[test]
fn poseidon_bn254_test() {
    let f = "std/poseidon_bn254_test.asm";
    gen_halo2_proof(f, Default::default());
}

#[test]
fn poseidon_gl_test() {
    let f = "std/poseidon_gl_test.asm";
    verify_test_file::<GoldilocksField>(f, Default::default(), vec![]);
    gen_estark_proof(f, Default::default());
}

#[test]
fn split_bn254_test() {
    let f = "std/split_bn254_test.asm";
    gen_halo2_proof(f, Default::default());
}

#[test]
fn split_gl_test() {
    let f = "std/split_gl_test.asm";
    verify_test_file::<GoldilocksField>(f, Default::default(), vec![]);
    gen_estark_proof(f, Default::default());
}

/// Returns the analyzed PIL containing only the std library.
pub fn std_analyzed<T: FieldElement>() -> Analyzed<T> {
    // airgen needs a main machine.
    let code = "machine Main { }".to_string();
    let mut pipeline = Pipeline::default().from_asm_string(code, None);
    pipeline.advance_to(Stage::AnalyzedPil).unwrap();
    pipeline.analyzed_pil().unwrap()
}

/// Evaluates a function call.
pub fn evaluate_function<'a, T: FieldElement>(
    analyzed: &'a Analyzed<T>,
    function: &'a str,
    arguments: Vec<Rc<evaluator::Value<'a, T, evaluator::NoCustom>>>,
) -> evaluator::Value<'a, T, evaluator::NoCustom> {
    let symbols = evaluator::Definitions(&analyzed.definitions);
    let function = symbols.lookup(function).unwrap();
    evaluator::evaluate_function_call(function, arguments, &symbols).unwrap()
}

/// Evaluates a function call assuming inputs and outputs are integers.
pub fn evaluate_integer_function<'a, T: FieldElement>(
    analyzed: &Analyzed<T>,
    function: &'a str,
    arguments: Vec<num_bigint::BigInt>,
) -> num_bigint::BigInt {
    let arguments = arguments
        .into_iter()
        .map(|x| Rc::new(evaluator::Value::Integer(x)))
        .collect();
    if let evaluator::Value::Integer(x) = evaluate_function(analyzed, function, arguments) {
        x
    } else {
        panic!("Expected integer.");
    }
}

#[test]
fn ff_inverse() {
    let test_inputs = vec![
        (1, 11),
        (1, 7),
        (2, 7),
        (3, 7),
        (4, 7),
        (5, 7),
        (6, 7),
        (2, 17),
        (3, 17),
        (9, 17),
        (15, 17),
        (16, 17),
    ];
    let analyzed = std_analyzed::<GoldilocksField>();
    for (x, modulus) in test_inputs {
        let x = num_bigint::BigInt::from(x);
        let modulus = num_bigint::BigInt::from(modulus);
        let result = evaluate_integer_function(
            &analyzed,
            "std::math::ff::inverse",
            vec![x.clone(), modulus.clone()],
        );
        assert_eq!((result * x) % modulus, 1.into());
    }
}

#[test]
fn ff_add_sub_mul_div() {
    let inputs = vec![
        (1, 0, 11),
        (1, 6, 7),
        (6, 6, 7),
        (0, 0, 17),
        (0, 16, 17),
        (16, 16, 17),
        (3, 8, 17),
        (16, 1, 17),
        (5, 9, 17),
        (3, 14, 17),
    ];
    let analyzed = std_analyzed::<GoldilocksField>();
    for (x, y, modulus) in inputs {
        let x = num_bigint::BigInt::from(x);
        let y = num_bigint::BigInt::from(y);
        let modulus = num_bigint::BigInt::from(modulus);
        let result = evaluate_integer_function(
            &analyzed,
            "std::math::ff::add",
            vec![x.clone(), y.clone(), modulus.clone()],
        );
        assert_eq!((x.clone() + y.clone()) % modulus.clone(), result.into());

        let result = evaluate_integer_function(
            &analyzed,
            "std::math::ff::sub",
            vec![x.clone(), y.clone(), modulus.clone()],
        );
        assert_eq!(
            (x.clone() - y.clone() + modulus.clone()) % modulus.clone(),
            result.into()
        );

        let result = evaluate_integer_function(
            &analyzed,
            "std::math::ff::mul",
            vec![x.clone(), y.clone(), modulus.clone()],
        );
        assert_eq!((x.clone() * y.clone()) % modulus.clone(), result.into());

        if y != 0.into() {
            let result = evaluate_integer_function(
                &analyzed,
                "std::math::ff::div",
                vec![x.clone(), y.clone(), modulus.clone()],
            );
            assert_eq!(x, (result * y) % modulus);
        }
    }
}
