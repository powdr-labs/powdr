use powdr_ast::{analyzed::Analyzed, WitgenCallback};

use crate::circuit_builder::PowdrCircuit;

use halo2_proofs::{dev::MockProver, halo2curves::bn256::Fr};
use powdr_number::{FieldElement, KnownField};

// Can't depend on compiler::pipeline::GeneratedWitness because of circular dependencies...
pub fn mock_prove<T: FieldElement>(
    pil: &Analyzed<T>,
    constants: &[(String, Vec<T>)],
    witness: &[(String, Vec<T>)],
    witgen_callback: Box<dyn WitgenCallback<T>>,
) -> Result<(), String> {
    if !matches!(T::known_field(), Some(KnownField::Bn254Field)) {
        panic!("powdr modulus doesn't match halo2 modulus. Make sure you are using Bn254");
    }

    // double the row count in order to make space for the cells introduced by the backend
    // TODO: use a precise count of the extra rows needed to avoid using so many rows

    let circuit_row_count_log = usize::BITS - pil.degree().leading_zeros();
    let expanded_row_count_log = circuit_row_count_log + 1;

    let circuit = PowdrCircuit::new(pil, constants, witgen_callback).with_witness(witness);
    let mock_prover = MockProver::<Fr>::run(
        expanded_row_count_log,
        &circuit,
        vec![circuit.instance_column()],
    )
    .unwrap();
    mock_prover.verify().map_err(|errs| {
        // Using err.emit() is the only way to get a pretty error message,
        // so we print it here even though the error might be caught by the caller.
        for err in errs {
            err.emit(&mock_prover);
            eprintln!();
        }
        "Circuit was not satisfied".to_string()
    })
}

#[cfg(test)]
mod test {
    use powdr_number::Bn254Field;
    use powdr_pipeline::{test_util::resolve_test_file, Pipeline};
    use test_log::test;

    use super::*;

    #[allow(clippy::print_stdout)]
    fn mock_prove_asm(file_name: &str, inputs: &[Bn254Field]) {
        let mut pipeline = Pipeline::default()
            .from_file(resolve_test_file(file_name))
            .with_prover_inputs(inputs.to_vec());

        let pil = pipeline.compute_optimized_pil().unwrap();
        let fixed_cols = pipeline.compute_fixed_cols().unwrap();
        let witness = pipeline.compute_witness().unwrap();
        mock_prove(&pil, &fixed_cols, &witness, Box::new(|_, _, _| panic!())).unwrap();
    }

    #[test]
    fn simple_pil_halo2() {
        let content = "namespace Global(8); pol fixed z = [1, 2]*; pol witness a; a = z + 1;";

        let mut pipeline = Pipeline::<Bn254Field>::default().from_pil_string(content.to_string());

        let pil = pipeline.compute_optimized_pil().unwrap();
        let fixed_cols = pipeline.compute_fixed_cols().unwrap();
        let witness = pipeline.compute_witness().unwrap();
        mock_prove(&pil, &fixed_cols, &witness, Box::new(|_, _, _| panic!())).unwrap();
    }

    #[test]
    fn simple_sum() {
        let inputs = [165, 5, 11, 22, 33, 44, 55].map(From::from);
        mock_prove_asm("asm/simple_sum.asm", &inputs);
    }

    #[test]
    fn palindrome() {
        let inputs = [3, 11, 22, 11].map(From::from);
        mock_prove_asm("asm/palindrome.asm", &inputs);
    }
}
