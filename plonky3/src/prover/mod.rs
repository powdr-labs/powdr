//! A plonky3 prover using FRI and Poseidon

mod types;

use std::io::Read;

use powdr_ast::analyzed::Analyzed;

use powdr_executor::witgen::WitgenCallback;

use p3_air::BaseAir;
use p3_uni_stark::{prove, verify, Proof};
use powdr_number::{FieldElement, KnownField};
use rand::{distributions::Standard, thread_rng, Rng};

use crate::circuit_builder::{cast_to_goldilocks, PowdrCircuit, Val};

use self::types::*;

pub fn generate_setup() -> Vec<Val> {
    let num_rounds = 2 * HALF_NUM_FULL_ROUNDS + NUM_PARTIAL_ROUNDS;
    let num_constants = WIDTH * num_rounds;

    thread_rng()
        .sample_iter(Standard)
        .take(num_constants)
        .collect()
}

#[derive(Clone)]
pub struct Plonky3Prover<'a, T> {
    /// The analyzed PIL
    analyzed: &'a Analyzed<T>,
    /// The value of the fixed columns
    fixed: &'a [(String, Vec<T>)],
    /// The constants which uniquely determine the prover, loosely interpreted as a setup
    setup: Constants,
    /// The constants which uniquely determine the prover, loosely interpreted as a verification key
    vkey: Option<Constants>,
}

impl<'a, T> Plonky3Prover<'a, T> {
    pub fn write_setup(&self, output: &mut dyn std::io::Write) {
        serde_json::to_writer(output, &self.setup.values).unwrap();
    }

    pub fn write_vkey(&self, output: &mut dyn std::io::Write) {
        let vk = self.verification_key();
        serde_json::to_writer(output, &vk.values).unwrap();
    }

    pub fn verification_key(&self) -> &Constants {
        &self.setup
    }

    pub fn new(
        analyzed: &'a Analyzed<T>,
        fixed: &'a [(String, Vec<T>)],
        constants: Option<&mut dyn std::io::Read>,
    ) -> Result<Self, std::io::Error> {
        let constants = constants
            .map(serde_json::from_reader)
            .transpose()?
            .unwrap_or_else(generate_setup);

        Ok(Self {
            analyzed,
            fixed,
            setup: Constants::new(constants),
            vkey: None,
        })
    }

    pub fn add_verification_key(&mut self, vkey: &mut dyn Read) {
        self.vkey = Some(Constants::new(serde_json::de::from_reader(vkey).unwrap()));
    }
}

impl<'a, T: FieldElement> Plonky3Prover<'a, T> {
    pub fn prove_ast(
        &self,
        witness: &[(String, Vec<T>)],
        witgen_callback: WitgenCallback<T>,
    ) -> Result<Vec<u8>, String> {
        assert_eq!(T::known_field(), Some(KnownField::GoldilocksField));

        let circuit = PowdrCircuit::new(self.analyzed, self.fixed)
            .with_witgen_callback(witgen_callback)
            .with_witness(witness);
        let publics = circuit.instance_column();

        let trace = circuit.preprocessed_trace().unwrap();

        let (config, perm) = self.setup.to_config_and_perm(self.analyzed.degree());

        let mut challenger = Challenger::new(perm.clone());

        let proof = prove(&config, &circuit, &mut challenger, trace, &publics);

        let mut challenger = Challenger::new(perm);

        verify(&config, &circuit, &mut challenger, &proof, &publics).unwrap();
        Ok(serde_json::to_vec(&proof).unwrap())
    }

    pub fn verify(&self, proof: &[u8], instances: &[Vec<T>]) -> Result<(), String> {
        let proof: Proof<_> = serde_json::from_slice(proof)
            .map_err(|e| format!("Failed to deserialize proof: {e}"))?;
        let publics = instances
            .iter()
            .flatten()
            .map(|v| cast_to_goldilocks(*v))
            .collect();

        let (config, perm) = self
            .vkey
            .as_ref()
            .unwrap()
            .to_config_and_perm(self.analyzed.degree());

        let mut challenger = Challenger::new(perm);

        verify(
            &config,
            &PowdrCircuit::new(self.analyzed, self.fixed),
            &mut challenger,
            &proof,
            &publics,
        )
        .map_err(|e| format!("Failed to verify proof: {e:?}"))
    }
}

#[cfg(test)]
mod tests {
    use powdr_number::GoldilocksField;
    use powdr_pipeline::Pipeline;

    use crate::Plonky3Prover;

    /// Prove and verify execution using a trivial PCS (coefficients of the polynomials)
    fn run_test_goldilocks_trivial_pcs(pil: &str) {
        let mut pipeline = Pipeline::<GoldilocksField>::default().from_pil_string(pil.to_string());

        let pil = pipeline.compute_optimized_pil().unwrap();
        let fixed_cols = pipeline.compute_fixed_cols().unwrap();
        let witness_callback = pipeline.witgen_callback().unwrap();
        let witness = pipeline.compute_witness().unwrap();

        let proof = Plonky3Prover::new(&pil, &fixed_cols, None)
            .unwrap()
            .prove_ast(&witness, witness_callback);

        assert!(proof.is_ok());
    }

    #[test]
    fn publics() {
        let content = "namespace Global(8); pol witness x; x * (x - 1) = 0; public out = x(7);";
        run_test_goldilocks_trivial_pcs(content);
    }

    #[test]
    #[should_panic = "assertion failed: width >= 1"]
    fn empty() {
        let content = "namespace Global(8);";
        run_test_goldilocks_trivial_pcs(content);
    }

    #[test]
    #[should_panic = "not implemented"]
    fn challenge() {
        let content = r#"
        let N: int = 8;
        namespace std::prover(N);
            let challenge = [];
            enum Query {
                Hint(int)
            }
        
        namespace Global(N); 
            let beta: expr = std::prover::challenge(0, 42); 
            col witness stage(0) x;
            col witness stage(1) y;
            x + beta = y + beta;
        "#;
        run_test_goldilocks_trivial_pcs(content);
    }

    #[test]
    fn polynomial_identity() {
        let content = "namespace Global(8); pol fixed z = [1, 2]*; pol witness a; a = z + 1;";
        run_test_goldilocks_trivial_pcs(content);
    }

    #[test]
    #[should_panic = "not implemented"]
    fn lookup() {
        let content = "namespace Global(8); pol fixed z = [0, 1]*; pol witness a; a in z;";
        run_test_goldilocks_trivial_pcs(content);
    }
}
