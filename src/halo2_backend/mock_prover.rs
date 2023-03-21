use std::fs;

use itertools::Itertools;
use num_bigint::BigInt;
use polyexen::plaf::PlafDisplayBaseTOML;

use super::circuit_builder::analyzed_to_circuit;
use crate::number::AbstractNumberType;
use crate::{analyzer, asm_compiler};
use halo2_proofs::{dev::MockProver, halo2curves::bn256::Fr};

// Follow dependency installation instrucions from https://github.com/ed255/polyexen-demo

const MAX_PUBLIC_INPUTS: usize = 12;

pub fn mock_prove_asm(file_name: &str, inputs: Vec<AbstractNumberType>, verbose: bool) {
    // read and compile PIL.

    let contents = fs::read_to_string(file_name).unwrap();
    let pil = asm_compiler::compile(Some(file_name), &contents).unwrap_or_else(|err| {
        eprintln!("Error parsing .asm file:");
        err.output_to_stderr();
        panic!();
    });
    let analyzed = &analyzer::analyze_string(&format!("{pil}"));

    // define how query information is retrieved.

    let query_callback = |query: &str| -> Option<AbstractNumberType> {
        let items = query.split(',').map(|s| s.trim()).collect::<Vec<_>>();
        let mut it = items.iter();
        let _current_step = it.next().unwrap();
        let current_pc = it.next().unwrap();
        assert!(it.clone().len() % 3 == 0);
        for (pc_check, input, index) in it.tuples() {
            if pc_check == current_pc {
                assert_eq!(*input, "\"input\"");
                let index: usize = index.parse().unwrap();
                return inputs.get(index).cloned();
            }
        }
        None
    };

    let modulus = polyexen::expr::get_field_p::<Fr>();

    let int_to_field = |n: &BigInt| {
        let n = if let Some(n) = n.to_biguint() {
            n
        } else {
            &modulus - (-n).to_biguint().unwrap()
        };
        assert!(n < modulus);
        n
    };

    let circuit = analyzed_to_circuit(
        analyzed,
        Some(query_callback),
        &inputs,
        polyexen::expr::get_field_p::<Fr>(),
        MAX_PUBLIC_INPUTS,
        verbose,
        &int_to_field,
    );

    let k = 1 + f32::log2(circuit.plaf.info.num_rows as f32).ceil() as u32;

    if verbose {
        println!("{}", PlafDisplayBaseTOML(&circuit.plaf));
    }
    
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

    let mock_prover = MockProver::<Fr>::run(k, &circuit, vec![inputs]).unwrap();
    mock_prover.assert_satisfied();

    println!("cool, works");
}
