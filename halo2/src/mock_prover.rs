use ast::analyzed::Analyzed;
use polyexen::plaf::PlafDisplayBaseTOML;

use super::circuit_builder::analyzed_to_circuit;
use halo2_proofs::{dev::MockProver, halo2curves::bn256::Fr};
use number::{BigInt, FieldElement};

pub fn mock_prove<T: FieldElement>(
    pil: &Analyzed<T>,
    fixed: &[(String, Vec<T>)],
    witness: &[(String, Vec<T>)],
) {
    if polyexen::expr::get_field_p::<Fr>() != T::modulus().to_arbitrary_integer() {
        panic!("powdr modulus doesn't match halo2 modulus. Make sure you are using Bn254");
    }

    let circuit = analyzed_to_circuit(pil, fixed, witness);

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
    use std::{fs, path::PathBuf};

    use analysis::analyze;
    use number::Bn254Field;
    use parser::parse_asm;
    use test_log::test;

    use super::*;

    #[allow(clippy::print_stdout)]
    fn mock_prove_asm(file_name: &str, inputs: &[Bn254Field]) {
        // read and compile PIL.

        let location = format!(
            "{}/../test_data/asm/{file_name}",
            env!("CARGO_MANIFEST_DIR")
        );

        let contents = fs::read_to_string(&location).unwrap();
        let parsed = parse_asm::<Bn254Field>(Some(&location), &contents).unwrap();
        let resolved = importer::resolve(Some(PathBuf::from(location)), parsed).unwrap();
        let analysed = analyze(resolved).unwrap();
        let graph = airgen::compile(analysed);
        let pil = linker::link(graph).unwrap();

        let query_callback = |query: &str| -> Option<Bn254Field> {
            let items = query.split(',').map(|s| s.trim()).collect::<Vec<_>>();
            match items[0] {
                "\"input\"" => {
                    assert_eq!(items.len(), 2);
                    let index = items[1].parse::<usize>().unwrap();
                    let value = inputs.get(index).cloned();
                    if let Some(value) = value {
                        log::trace!("Input query: Index {index} -> {value}");
                    }
                    value
                }
                "\"print\"" => {
                    log::info!("Print: {}", items[1..].join(", "));
                    Some(0.into())
                }
                "\"print_ch\"" => {
                    print!("{}", items[1].parse::<u8>().unwrap() as char);
                    Some(0.into())
                }
                "\"hint\"" => {
                    assert_eq!(items.len(), 2);
                    Some(Bn254Field::from_str(items[1]))
                }
                _ => None,
            }
        };

        let analyzed = pil_analyzer::analyze_string(&format!("{pil}"));

        let fixed = executor::constant_evaluator::generate(&analyzed);
        let witness =
            executor::witgen::WitnessGenerator::new(&analyzed, &fixed, query_callback).generate();

        let fixed = to_owned_values(fixed);

        mock_prove(&analyzed, &fixed, &witness);
    }

    #[test]
    fn simple_pil_halo2() {
        let content = "namespace Global(8); pol fixed z = [0]*; pol witness a; a = 0;";
        let analyzed: Analyzed<Bn254Field> = pil_analyzer::analyze_string(content);
        let fixed = executor::constant_evaluator::generate(&analyzed);

        let query_callback = |_: &str| -> Option<Bn254Field> { None };

        let witness =
            executor::witgen::WitnessGenerator::new(&analyzed, &fixed, query_callback).generate();

        let fixed = to_owned_values(fixed);

        mock_prove(&analyzed, &fixed, &witness);
    }

    #[test]
    fn simple_sum() {
        let inputs = [165, 5, 11, 22, 33, 44, 55].map(From::from);
        mock_prove_asm("simple_sum.asm", &inputs);
    }

    #[test]
    fn palindrome() {
        let inputs = [3, 11, 22, 11].map(From::from);
        mock_prove_asm("palindrome.asm", &inputs);
    }

    fn to_owned_values<T: FieldElement>(values: Vec<(&str, Vec<T>)>) -> Vec<(String, Vec<T>)> {
        values
            .into_iter()
            .map(|(s, fields)| (s.to_string(), fields.clone()))
            .collect::<Vec<_>>()
    }
}
