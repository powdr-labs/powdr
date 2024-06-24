//! A plonky3 prover using FRI and Poseidon

mod params;

use p3_matrix::dense::RowMajorMatrix;

use params::{Challenger, StarkProvingKey, StarkVerifyingKey};
use powdr_ast::analyzed::Analyzed;

use powdr_executor::witgen::WitgenCallback;

use p3_air::BaseAir;

use p3_uni_stark::{prove, verify, Proof, StarkGenericConfig};
use powdr_number::{FieldElement, KnownField};

use crate::circuit_builder::{cast_to_goldilocks, PowdrCircuit};

use self::params::{get_challenger, get_config};

pub struct Plonky3Prover<'a, T> {
    /// The analyzed PIL
    analyzed: &'a Analyzed<T>,
    /// The value of the fixed columns
    fixed: &'a [(String, Vec<T>)],
    /// Proving key
    proving_key: Option<StarkProvingKey>,
    /// Verifying key
    verifying_key: Option<StarkVerifyingKey>,
}

impl<'a, T> Plonky3Prover<'a, T> {
    pub fn new(analyzed: &'a Analyzed<T>, fixed: &'a [(String, Vec<T>)]) -> Self {
        Self {
            analyzed,
            fixed,
            proving_key: None,
            verifying_key: None,
        }
    }
}

impl<'a, T: FieldElement> Plonky3Prover<'a, T> {
    pub fn setup(&mut self) {
        // get fixed columns
        let fixed = self.fixed;

        // get the config
        let config = get_config(self.analyzed.degree());

        // commit to the fixed columns
        let pcs = config.pcs();
        let domain = <_ as p3_commit::Pcs<_, Challenger>>::natural_domain_for_degree(
            pcs,
            self.analyzed.degree() as usize,
        );
        // write fixed into matrix row by row. I'm not sure ordering them like this makes a difference
        let matrix = RowMajorMatrix::new(
            (0..self.analyzed.degree())
                .flat_map(|i| {
                    fixed
                        .iter()
                        .map(move |(_, values)| cast_to_goldilocks(values[i as usize]))
                })
                .collect(),
            self.fixed.len(),
        );
        let evaluations = vec![(domain, matrix)];

        // commit to the evaluations
        let (commit, data) = <_ as p3_commit::Pcs<_, Challenger>>::commit(pcs, evaluations);

        let proving_key = StarkProvingKey {
            // commit,
            data,
        };
        let verifying_key = StarkVerifyingKey { commit };

        self.proving_key = Some(proving_key);
        self.verifying_key = Some(verifying_key);
    }

    pub fn prove(
        &self,
        witness: &[(String, Vec<T>)],
        witgen_callback: WitgenCallback<T>,
    ) -> Result<Vec<u8>, String> {
        assert_eq!(T::known_field(), Some(KnownField::GoldilocksField));

        let circuit = PowdrCircuit::new(self.analyzed, self.fixed)
            .with_witgen_callback(witgen_callback)
            .with_witness(witness);

        let publics = vec![];

        let trace = circuit.preprocessed_trace().unwrap();

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

    /// Prove and verify execution
    fn run_test_goldilocks(pil: &str) {
        let mut pipeline = Pipeline::<GoldilocksField>::default().from_pil_string(pil.to_string());

        let pil = pipeline.compute_optimized_pil().unwrap();
        let fixed_cols = pipeline.compute_fixed_cols().unwrap();
        let witness_callback = pipeline.witgen_callback().unwrap();
        let witness = pipeline.compute_witness().unwrap();

        let proof = Plonky3Prover::new(&pil, &fixed_cols).prove(&witness, witness_callback);

        assert!(proof.is_ok());
    }

    #[test]
    #[should_panic = "not implemented"]
    fn publics() {
        let content = "namespace Global(8); pol witness x; x * (x - 1) = 0; public out = x(7);";
        run_test_goldilocks(content);
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
