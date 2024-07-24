//! A plonky3 prover using FRI and Poseidon

mod folder;
mod params;
mod prover;
mod verifier;

use p3_matrix::dense::RowMajorMatrix;

use params::Proof;
use prover::prove;

use std::sync::Arc;
use verifier::verify;

use params::{Challenger, StarkProvingKey, StarkVerifyingKey};
use powdr_ast::analyzed::Analyzed;

use powdr_executor::witgen::WitgenCallback;

use p3_uni_stark::StarkGenericConfig;
use powdr_number::{FieldElement, KnownField};

use crate::circuit_builder::{cast_to_goldilocks, PowdrCircuit};

use self::params::{get_challenger, get_config, Config};

pub struct Plonky3Prover<T> {
    /// The analyzed PIL
    analyzed: Arc<Analyzed<T>>,
    /// The value of the fixed columns
    fixed: Arc<Vec<(String, Vec<T>)>>,
    /// Proving key
    proving_key: Option<StarkProvingKey<Config>>,
    /// Verifying key
    verifying_key: Option<StarkVerifyingKey<Config>>,
}

impl<T> Plonky3Prover<T> {
    pub fn new(analyzed: Arc<Analyzed<T>>, fixed: Arc<Vec<(String, Vec<T>)>>) -> Self {
        Self {
            analyzed,
            fixed,
            proving_key: None,
            verifying_key: None,
        }
    }

    pub fn set_verifying_key(&mut self, rdr: &mut dyn std::io::Read) {
        self.verifying_key = Some(serde_json::from_reader(rdr).unwrap());
    }

    pub fn get_verifying_key(&self) -> Option<&StarkVerifyingKey<Config>> {
        self.verifying_key.as_ref()
    }
}

impl<T: FieldElement> Plonky3Prover<T> {
    pub fn setup(&mut self) {
        // get fixed columns
        let fixed = &self.fixed;

        // get selector columns for public values
        let publics = self
            .analyzed
            .get_publics()
            .into_iter()
            .map(|(name, _, row_id)| {
                let selector = (0..self.analyzed.degree())
                    .map(move |i| T::from(i == row_id as u64))
                    .collect::<Vec<T>>();
                (name, selector)
            })
            .collect::<Vec<(String, Vec<T>)>>();

        // get the config
        let config = get_config(self.analyzed.degree());

        // commit to the fixed columns
        let pcs = config.pcs();
        let domain = <_ as p3_commit::Pcs<_, Challenger>>::natural_domain_for_degree(
            pcs,
            self.analyzed.degree() as usize,
        );
        // write fixed into matrix row by row
        let matrix = RowMajorMatrix::new(
            (0..self.analyzed.degree())
                .flat_map(|i| {
                    fixed
                        .iter()
                        .map(move |(_, values)| cast_to_goldilocks(values[i as usize]))
                        .chain(
                            publics
                                .iter()
                                .map(move |(_, values)| cast_to_goldilocks(values[i as usize])),
                        )
                })
                .collect(),
            self.fixed.len() + publics.len(),
        );

        let evaluations = vec![(domain, matrix)];

        // commit to the evaluations
        let (fixed_commit, fixed_data) =
            <_ as p3_commit::Pcs<_, Challenger>>::commit(pcs, evaluations);

        let proving_key = StarkProvingKey {
            fixed_commit,
            fixed_data,
        };
        let verifying_key = StarkVerifyingKey { fixed_commit };

        self.proving_key = Some(proving_key);
        self.verifying_key = Some(verifying_key);
    }

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

        let proving_key = self.proving_key.as_ref();

        let proof = prove(
            &config,
            proving_key,
            &circuit,
            &mut challenger,
            trace,
            &publics,
        );

        let mut challenger = get_challenger();

        let verifying_key = self.verifying_key.as_ref();

        verify(
            &config,
            verifying_key,
            &circuit,
            &mut challenger,
            &proof,
            &publics,
        )
        .unwrap();
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

        let verifying_key = self.verifying_key.as_ref();

        verify(
            &config,
            verifying_key,
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
    use test_log::test;

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
        let fixed = pipeline.compute_fixed_cols().unwrap();

        let mut prover = Plonky3Prover::new(pil, fixed);
        prover.setup();
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
    #[should_panic = "assertion failed: width >= 1"]
    // The above failure is from a debug assertion, so we can only run this
    // test if debug assertions are activated.
    #[cfg(debug_assertions)]
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
    fn fixed() {
        let content = r#"
        namespace Add(8);
            col witness x;
            col fixed y = [1, 0]*;
            x * y = y;
        "#;
        run_test_goldilocks(content);
    }

    #[test]
    #[should_panic = "not implemented"]
    fn challenge() {
        let content = r#"
        let N: int = 8;
        
        namespace Global(N); 
            let beta: expr = std::prelude::challenge(0, 42);
            col witness stage(0) x;
            col witness stage(1) y;
            x = y + beta;
        "#;
        run_test_goldilocks(content);
    }

    #[test]
    fn polynomial_identity() {
        let content = "namespace Global(8); pol fixed z = [1, 2]*; pol witness a; a = z + 1;";
        run_test_goldilocks(content);
    }

    #[test]
    #[should_panic = "not implemented"]
    fn lookup() {
        let content = "namespace Global(8); pol fixed z = [0, 1]*; pol witness a; [a] in [z];";
        run_test_goldilocks(content);
    }
}
