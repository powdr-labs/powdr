use std::io;

use halo2_curves::bn256::{Bn256, G1Affine};
use powdr_ast::analyzed::Analyzed;
use powdr_executor::witgen::WitgenCallback;
use snark_verifier::system::halo2::{compile, Config};

use crate::{aggregation, circuit_builder::PowdrCircuit, degree_bits, generate_setup};

use halo2_proofs::{
    dev::MockProver,
    halo2curves::bn256::Fr,
    plonk::VerifyingKey,
    poly::{commitment::Params, kzg::commitment::ParamsKZG},
};
use powdr_number::{FieldElement, KnownField};

// Can't depend on compiler::pipeline::GeneratedWitness because of circular dependencies...
pub fn mock_prove<T: FieldElement>(
    pil: &Analyzed<T>,
    constants: &[(String, Vec<T>)],
    witness: &[(String, Vec<T>)],
    witgen_callback: WitgenCallback<T>,
) -> Result<(), String> {
    if !matches!(T::known_field(), Some(KnownField::Bn254Field)) {
        panic!("powdr modulus doesn't match halo2 modulus. Make sure you are using Bn254");
    }

    // double the row count in order to make space for the cells introduced by the backend
    // TODO: use a precise count of the extra rows needed to avoid using so many rows

    let circuit_row_count_log = usize::BITS - pil.degree().leading_zeros();
    let expanded_row_count_log = circuit_row_count_log + 1;

    let circuit = PowdrCircuit::new(pil, constants)
        .with_witness(witness)
        .with_witgen_callback(witgen_callback);
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

pub fn mock_prove_aggr<T: FieldElement>(
    pil: &Analyzed<T>,
    constants: &[(String, Vec<T>)],
    witness: &[(String, Vec<T>)],
    witgen_callback: WitgenCallback<T>,
    params: ParamsKZG<Bn256>,
    vkey_app: VerifyingKey<G1Affine>,
    proof: Vec<u8>,
) -> Result<(), String> {
    if !matches!(T::known_field(), Some(KnownField::Bn254Field)) {
        panic!("powdr modulus doesn't match halo2 modulus. Make sure you are using Bn254");
    }

    log::info!("Starting proof aggregation...");

    log::info!("Generating circuit for app snark...");

    let circuit_app = PowdrCircuit::new(pil, constants)
        .with_witness(witness)
        .with_witgen_callback(witgen_callback);

    if circuit_app.has_publics() {
        unimplemented!("Public inputs are not supported yet");
    }

    log::info!("Generating VK for app snark...");

    let mut params_app = params.clone();
    params_app.downsize(degree_bits(pil.degree()));

    log::info!("Generating circuit for compression snark...");
    let protocol_app = compile(
        &params_app,
        &vkey_app,
        Config::kzg().with_num_instance(vec![]),
    );

    log::info!("Generating aggregated proof...");

    let snark = aggregation::Snark::new(protocol_app, vec![], proof);
    let agg_circuit_with_proof = aggregation::AggregationCircuit::new(&params, [snark]);

    let mock_prover = MockProver::<Fr>::run(
        20,
        &agg_circuit_with_proof,
        agg_circuit_with_proof.instances(),
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

pub struct Halo2Mock<'a, F: FieldElement> {
    pil: &'a Analyzed<F>,
    fixed: &'a [(String, Vec<F>)],
    vkey: Option<VerifyingKey<G1Affine>>,
    setup: ParamsKZG<Bn256>,
}

impl<'a, T: FieldElement> Halo2Mock<'a, T> {
    pub fn new(
        pil: &'a Analyzed<T>,
        fixed: &'a [(String, Vec<T>)],
        setup: Option<&mut dyn io::Read>,
        vkey: Option<VerifyingKey<G1Affine>>,
    ) -> Result<Self, io::Error> {
        let setup = setup
            .map(|mut setup| ParamsKZG::<Bn256>::read(&mut setup))
            .transpose()?
            // TODO: what's the degree of the aggregation circuit?
            .unwrap_or_else(|| generate_setup(pil.degree()));
        Ok(Self {
            pil,
            fixed,
            vkey,
            setup,
        })
    }

    pub fn prove(
        &self,
        witness: &[(String, Vec<T>)],
        prev_proof: Option<Vec<u8>>,
        witgen_callback: WitgenCallback<T>,
    ) -> Result<(), String> {
        if let Some(prev_proof) = prev_proof {
            mock_prove_aggr(
                self.pil,
                self.fixed,
                witness,
                witgen_callback,
                self.setup.clone(),
                self.vkey.clone().unwrap(),
                prev_proof,
            )?;
        } else {
            mock_prove(self.pil, self.fixed, witness, witgen_callback)?;
        }

        Ok(())
    }
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
        let witgen_callback = pipeline.witgen_callback().unwrap();
        mock_prove(&pil, &fixed_cols, &witness, witgen_callback).unwrap();
    }

    #[test]
    fn simple_pil_halo2() {
        let content = "namespace Global(8); pol fixed z = [1, 2]*; pol witness a; a = z + 1;";

        let mut pipeline = Pipeline::<Bn254Field>::default().from_pil_string(content.to_string());

        let pil = pipeline.compute_optimized_pil().unwrap();
        let fixed_cols = pipeline.compute_fixed_cols().unwrap();
        let witness = pipeline.compute_witness().unwrap();
        let witgen_callback = pipeline.witgen_callback().unwrap();
        mock_prove(&pil, &fixed_cols, &witness, witgen_callback).unwrap();
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
