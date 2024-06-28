//! A plonky3 prover using FRI and Poseidon

mod params;

use std::sync::Arc;

use powdr_ast::analyzed::Analyzed;

use powdr_executor::witgen::WitgenCallback;

use p3_uni_stark::{prove, verify, Proof};
use powdr_number::{FieldElement, KnownField};

use crate::circuit_builder::{cast_to_goldilocks, PowdrCircuit};

use self::params::{get_challenger, get_config};

#[derive(Clone)]
pub struct Plonky3Prover<T> {
    /// The analyzed PIL
    analyzed: Arc<Analyzed<T>>,
}

impl<T> Plonky3Prover<T> {
    pub fn new(analyzed: Arc<Analyzed<T>>) -> Self {
        Self { analyzed }
    }
}

impl<T: FieldElement> Plonky3Prover<T> {
    pub fn prove(
        &self,
        witness: &[(String, Vec<T>)],
        witgen_callback: WitgenCallback<T>,
    ) -> Result<Vec<u8>, String> {
        assert_eq!(T::known_field(), Some(KnownField::GoldilocksField));

        let circuit = PowdrCircuit::new(&self.analyzed)
            .with_witgen_callback(witgen_callback)
            .with_witness(witness);

        let publics = circuit.get_public_values();

        let trace = circuit.generate_trace_rows();

        let config = get_config(self.analyzed.degree());

        let mut challenger = get_challenger();

        let proof = prove(&config, &circuit, &mut challenger, trace, &publics);

        let mut challenger = get_challenger();

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

        let config = get_config(self.analyzed.degree());

        let mut challenger = get_challenger();

        verify(
            &config,
            &PowdrCircuit::new(&self.analyzed),
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

    /// Prove and verify execution
    fn run_test_goldilocks(pil: &str) {
        run_test_goldilocks_publics(pil, None)
    }

    fn run_test_goldilocks_publics(pil: &str, malicious_publics: Option<Vec<GoldilocksField>>) {
        let mut pipeline = Pipeline::<GoldilocksField>::default().from_pil_string(pil.to_string());

        let pil = pipeline.compute_optimized_pil().unwrap();
        let witness_callback = pipeline.witgen_callback().unwrap();
        let witness = pipeline.compute_witness().unwrap();

        let prover = Plonky3Prover::new(pil);
        let proof = prover.prove(&witness, witness_callback);

        assert!(proof.is_ok());

        if let Some(publics) = malicious_publics {
            prover.verify(&proof.unwrap(), &[publics]).unwrap()
        }
    }

    #[test]
    fn publics() {
        let content = "namespace Global(8); pol witness x; x * (x - 1) = 0; public out = x(7);";
        run_test_goldilocks(content);
    }

    #[test]
    #[should_panic = r#"called `Result::unwrap()` on an `Err` value: "Failed to verify proof: OodEvaluationMismatch""#]
    fn public_inputs_malicious() {
        let content = r#"
        namespace Add(8);
            col witness x;
            col witness y;
            col witness z;
            y - 1 = 0;
            x = 0;
            x + y = z;

            public outz = z(7);
        "#;
        let malicious_publics = Some(vec![GoldilocksField::from(0)]);
        run_test_goldilocks_publics(content, malicious_publics);
    }

    #[test]
    fn add() {
        let content = r#"
        namespace Add(8);
            col witness x;
            col witness y;
            col witness z;
            x + y = z;
        "#;
        run_test_goldilocks(content);
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
            x = y + beta;
        "#;
        run_test_goldilocks(content);
    }

    #[test]
    #[should_panic = "not implemented"]
    fn polynomial_identity() {
        let content = "namespace Global(8); pol fixed z = [1, 2]*; pol witness a; a = z + 1;";
        run_test_goldilocks(content);
    }

    #[test]
    #[should_panic = "not implemented"]
    fn lookup() {
        let content = "namespace Global(8); pol fixed z = [0, 1]*; pol witness a; a in z;";
        run_test_goldilocks(content);
    }
}
