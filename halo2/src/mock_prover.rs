use ast::analyzed::Analyzed;
use polyexen::plaf::PlafDisplayBaseTOML;

use super::circuit_builder::analyzed_to_circuit;
use halo2_proofs::{dev::MockProver, halo2curves::bn256::Fr};
use number::{BigInt, FieldElement};

// Can't depend on compiler::pipeline::GeneratedWitness because of circular dependencies...
pub fn mock_prove<T: FieldElement>(
    pil: &Analyzed<T>,
    constants: &[(String, Vec<T>)],
    witness: &[(String, Vec<T>)],
) {
    if polyexen::expr::get_field_p::<Fr>() != T::modulus().to_arbitrary_integer() {
        panic!("powdr modulus doesn't match halo2 modulus. Make sure you are using Bn254");
    }

    let circuit = analyzed_to_circuit(pil, constants, witness);

    // double the row count in order to make space for the cells introduced by the backend
    // TODO: use a precise count of the extra rows needed to avoid using so many rows

    let circuit_row_count_log = usize::BITS - circuit.plaf.info.num_rows.leading_zeros();

    let expanded_row_count_log = circuit_row_count_log + 1;

    log::debug!("{}", PlafDisplayBaseTOML(&circuit.plaf));

    let inputs = vec![];

    let mock_prover = MockProver::<Fr>::run(expanded_row_count_log, &circuit, inputs).unwrap();
    mock_prover.assert_satisfied();
}

#[cfg(test)]
mod test {
    use compiler::{pipeline::Pipeline, test_util::resolve_test_file};
    use number::Bn254Field;
    use test_log::test;

    use super::*;

    #[allow(clippy::print_stdout)]
    fn mock_prove_asm(file_name: &str, inputs: &[Bn254Field]) {
        let result = Pipeline::default()
            .from_file(resolve_test_file(file_name))
            .with_prover_inputs(inputs.to_vec())
            .generated_witness()
            .unwrap();
        mock_prove(
            &result.pil,
            &result.constants,
            result.witness.as_ref().unwrap(),
        );
    }

    #[test]
    fn simple_pil_halo2() {
        let content = "namespace Global(8); pol fixed z = [1, 2]*; pol witness a; a = z + 1;";

        let result = Pipeline::<Bn254Field>::default()
            .from_pil_string(content.to_string())
            .generated_witness()
            .unwrap();
        mock_prove(
            &result.pil,
            &result.constants,
            result.witness.as_ref().unwrap(),
        );
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
