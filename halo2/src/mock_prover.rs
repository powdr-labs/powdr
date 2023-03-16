use std::fs;

use polyexen::plaf::PlafDisplayBaseTOML;

use super::circuit_builder::analyzed_to_circuit;
use halo2_proofs::{dev::MockProver, halo2curves::bn256::Fr};
use number::{BigInt, Bn254Field, FieldElement};

// Follow dependency installation instructions from https://github.com/ed255/polyexen-demo

//const MAX_PUBLIC_INPUTS: usize = 12;

pub fn mock_prove_asm(file_name: &str, inputs: &[Bn254Field]) {
    // read and compile PIL.

    let contents = fs::read_to_string(file_name).unwrap();
    let pil = pilgen::compile::<Bn254Field>(Some(file_name), &contents).unwrap_or_else(|err| {
        eprintln!("Error parsing .asm file:");
        err.output_to_stderr();
        panic!();
    });

    let analyzed = pil_analyzer::analyze_string(&format!("{pil}"));

    mock_prove(analyzed, inputs);
}

pub fn mock_prove(analyzed: pil_analyzer::Analyzed<Bn254Field>, inputs: &[Bn254Field]) {
    // define how query information is retrieved.

    let query_callback = |query: &str| -> Option<Bn254Field> {
        let items = query.split(',').map(|s| s.trim()).collect::<Vec<_>>();
        assert_eq!(items.len(), 2);
        match items[0] {
            "\"input\"" => {
                let index = items[1].parse::<usize>().unwrap();
                let value = inputs.get(index).cloned();
                if let Some(value) = value {
                    log::trace!("Input query: Index {index} -> {value}");
                }
                value
            }
            _ => None,
        }
    };

    assert_eq!(
        polyexen::expr::get_field_p::<Fr>(),
        Bn254Field::modulus().to_arbitrary_integer(),
        "powdr modulus doesn't match halo2 modulus"
    );

    let circuit = analyzed_to_circuit(&analyzed, Some(query_callback));

    let k = 1 + f32::log2(circuit.plaf.info.num_rows as f32).ceil() as u32;

    log::debug!("{}", PlafDisplayBaseTOML(&circuit.plaf));

    /*
        let inputs: Vec<_> = inputs
            .iter()
            .map(|n| {
                Fr::from_bytes(
                    &n.to_biguint()
                        .unwrap()
                        .to_bytes_le()
                        .into_iter()
                        .chain(std::iter::repeat(0))
                        .take(32)
                        .collect::<Vec<_>>()
                        .try_into()
                        .unwrap(),
                )
                .unwrap()
            })
            .chain(std::iter::repeat(Fr::zero()))
            .take(MAX_PUBLIC_INPUTS)
            .collect();

    */

    let mock_prover = MockProver::<Fr>::run(k, &circuit, vec![]).unwrap();
    mock_prover.assert_satisfied();
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn simple_pil_halo2() {
        let content = "namespace Global(8); pol fixed z = [0]*; pol witness a; a = 0;";
        let analyzed = pil_analyzer::analyze_string(content);
        super::mock_prove(analyzed, &vec![]);
    }

    #[test]
    fn fibonacci() {
        let inputs = [165, 5, 11, 22, 33, 44, 55].map(From::from);
        mock_prove_asm("../test_data/asm/simple_sum.asm", &inputs);
    }

    #[test]
    fn palindrome() {
        let inputs = [3, 11, 22, 11].map(From::from);
        mock_prove_asm("../test_data/asm/palindrome.asm", &inputs);
    }
}
