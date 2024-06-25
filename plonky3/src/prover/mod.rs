//! A plonky3 prover using FRI and Poseidon

mod params;

use p3_goldilocks::Goldilocks;
use powdr_ast::analyzed::Analyzed;

use powdr_executor::witgen::WitgenCallback;

use p3_uni_stark::{prove, verify, Proof};
use powdr_number::{FieldElement, KnownField};

use crate::circuit_builder::{cast_to_goldilocks, PowdrCircuit};

use self::params::{get_challenger, get_config};

#[derive(Clone)]
pub struct Plonky3Prover<'a, T> {
    /// The analyzed PIL
    analyzed: &'a Analyzed<T>,
}

impl<'a, T> Plonky3Prover<'a, T> {
    pub fn new(analyzed: &'a Analyzed<T>) -> Self {
        Self { analyzed }
    }
}

impl<'a, T: FieldElement> Plonky3Prover<'a, T> {
    pub fn prove(
        &self,
        witness: &[(String, Vec<T>)],
        witgen_callback: WitgenCallback<T>,
        malicious_publics: Option<Vec<Goldilocks>>,
    ) -> Result<Vec<u8>, String> {
        assert_eq!(T::known_field(), Some(KnownField::GoldilocksField));

        let circuit = PowdrCircuit::new(self.analyzed)
            .with_witgen_callback(witgen_callback)
            .with_witness(witness);

        let prover_publics = publics_from_witness(self.analyzed, witness);

        let verifier_publics = malicious_publics.unwrap_or(
            publics_from_witness(self.analyzed, witness)
        );

        let trace = circuit.generate_trace_rows();

        let config = get_config(self.analyzed.degree());

        let mut challenger = get_challenger();

        let proof = prove(&config, &circuit, &mut challenger, trace, &prover_publics);

        let mut challenger = get_challenger();

        verify(&config, &circuit, &mut challenger, &proof, &verifier_publics).unwrap();
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
            &PowdrCircuit::new(self.analyzed),
            &mut challenger,
            &proof,
            &publics,
        )
        .map_err(|e| format!("Failed to verify proof: {e:?}"))
    }
}

fn publics_from_witness<T: FieldElement>(analyzed: &Analyzed<T>, witness: &[(String, Vec<T>)]) -> Vec<Goldilocks> {
    let publics = analyzed
        .public_declarations
        .values()
        .map(|public_declaration| {
            // index into correct witness value, extract public_declaration.index
            let pub_idx = witness.iter().position(|name| name.0 == 
                public_declaration.referenced_poly_name()).unwrap();
            let pub_val = witness[pub_idx].1[public_declaration.index as usize];
            cast_to_goldilocks(pub_val)
        }).collect::<Vec<Goldilocks>>();
    // order of publics should be deterministic
    publics
    }

#[cfg(test)]
mod tests {
    use p3_goldilocks::Goldilocks;
    use powdr_number::GoldilocksField;
    use powdr_pipeline::Pipeline;

    use crate::{circuit_builder::cast_to_goldilocks, Plonky3Prover};

    /// Prove and verify execution
    fn run_test_goldilocks(pil: &str) {
        let mut pipeline = Pipeline::<GoldilocksField>::default().from_pil_string(pil.to_string());

        let pil = pipeline.compute_optimized_pil().unwrap();
        let witness_callback = pipeline.witgen_callback().unwrap();
        let witness = pipeline.compute_witness().unwrap();

        let proof = Plonky3Prover::new(&pil).prove(&witness, witness_callback, None);

        assert!(proof.is_ok());
    }

    fn run_test_goldilocks_publics(pil:& str,  publics: Vec<Goldilocks>) {
        let mut pipeline = Pipeline::<GoldilocksField>::default().from_pil_string(pil.to_string());

        let pil = pipeline.compute_optimized_pil().unwrap();
        let witness_callback = pipeline.witgen_callback().unwrap();
        let witness = pipeline.compute_witness().unwrap();

        let proof = Plonky3Prover::new(&pil).prove(&witness, witness_callback, Some(publics));

        assert!(proof.is_ok());
    }

    #[test]
    // #[should_panic = "not implemented"]
    fn publics() {
        let content = "namespace Global(8); pol witness x; x * (x - 1) = 0; public out = x(7);";
        run_test_goldilocks(content);
    }

    #[test]
    #[should_panic = "called `Result::unwrap()` on an `Err` value: OodEvaluationMismatch"]
    fn public_inputs() {
        let content = r#"
        namespace Add(8);
            col witness x;
            col witness y;
            col witness z;
            x + y = z;

            public outx = x(7);
            public outy = y(7);
            public outz = z(7);
        "#;
        let publics = vec![
            cast_to_goldilocks(GoldilocksField::from(0)),
            cast_to_goldilocks(GoldilocksField::from(1)),
            cast_to_goldilocks(GoldilocksField::from(1)),
            ];
        run_test_goldilocks_publics(content, publics)
    }

    #[test]
    #[should_panic = "assertion failed: width >= 1"]
    fn empty() {
        let content = "namespace Global(8);";
        run_test_goldilocks(content);
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
            x + beta = y + beta;
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
