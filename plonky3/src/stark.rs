//! A plonky3 prover using FRI and Poseidon

use p3_matrix::dense::RowMajorMatrix;

use core::fmt;
use std::iter::{once, repeat};
use std::sync::Arc;

use powdr_ast::analyzed::Analyzed;

use powdr_executor::witgen::WitgenCallback;

use p3_uni_stark::{
    prove_with_key, verify_with_key, Proof, StarkGenericConfig, StarkProvingKey, StarkVerifyingKey,
};

use crate::{
    circuit_builder::{generate_matrix, PowdrCircuit},
    params::{Challenger, Commitment, FieldElementMap, Plonky3Field, ProverData},
};

pub struct Plonky3Prover<T: FieldElementMap>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    /// The analyzed PIL
    analyzed: Arc<Analyzed<T>>,
    /// The value of the fixed columns
    fixed: Arc<Vec<(String, Vec<T>)>>,
    /// Proving key
    proving_key: Option<StarkProvingKey<T::Config>>,
    /// Verifying key
    verifying_key: Option<StarkVerifyingKey<T::Config>>,
}

pub enum VerificationKeyExportError {
    NoVerificationKey,
}

impl fmt::Display for VerificationKeyExportError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NoVerificationKey => write!(f, "No verification key set"),
        }
    }
}

impl<T: FieldElementMap> Plonky3Prover<T>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    pub fn new(analyzed: Arc<Analyzed<T>>, fixed: Arc<Vec<(String, Vec<T>)>>) -> Self {
        Self {
            analyzed,
            fixed,
            proving_key: None,
            verifying_key: None,
        }
    }

    pub fn set_verifying_key(&mut self, rdr: &mut dyn std::io::Read) {
        self.verifying_key = Some(bincode::deserialize_from(rdr).unwrap());
    }

    pub fn export_verifying_key(&self) -> Result<Vec<u8>, VerificationKeyExportError> {
        Ok(bincode::serialize(
            self.verifying_key
                .as_ref()
                .ok_or(VerificationKeyExportError::NoVerificationKey)?,
        )
        .unwrap())
    }

    /// Returns preprocessed matrix based on the fixed inputs [`Plonky3Prover<T>`].
    /// This is used when running the setup phase
    pub fn get_preprocessed_matrix(&self) -> RowMajorMatrix<Plonky3Field<T>> {
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
            .collect::<Vec<_>>();

        let fixed_with_public_selectors = self
            .fixed
            .iter()
            .chain(publics.iter())
            .map(|(name, values)| (name, values.as_ref()));

        generate_matrix(fixed_with_public_selectors)
    }
}

impl<T: FieldElementMap> Plonky3Prover<T>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
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

        if fixed.is_empty() && publics.is_empty() {
            return;
        }

        // get the config
        let config = T::get_config();

        // commit to the fixed columns
        let pcs = config.pcs();
        let domain = <_ as p3_commit::Pcs<_, Challenger<T>>>::natural_domain_for_degree(
            pcs,
            self.analyzed.degree() as usize,
        );
        // write fixed into matrix row by row
        let matrix = RowMajorMatrix::new(
            (0..self.analyzed.degree())
                .flat_map(|i| {
                    fixed
                        .iter()
                        .chain(publics.iter())
                        .map(move |(_, values)| values[i as usize].into_p3_field())
                })
                .collect(),
            self.fixed.len() + publics.len(),
        );

        let evaluations = vec![(domain, matrix)];

        // commit to the evaluations
        let (fixed_commit, fixed_data) =
            <_ as p3_commit::Pcs<_, Challenger<T>>>::commit(pcs, evaluations);

        let proving_key = StarkProvingKey {
            preprocessed_commit: fixed_commit.clone(),
            preprocessed_data: fixed_data,
        };
        let verifying_key = StarkVerifyingKey {
            preprocessed_commit: fixed_commit,
        };

        self.proving_key = Some(proving_key);
        self.verifying_key = Some(verifying_key);
    }

    pub fn prove(
        &self,
        witness: &[(String, Vec<T>)],
        witgen_callback: WitgenCallback<T>,
    ) -> Result<Vec<u8>, String> {
        let stage_0_trace =
            generate_matrix(witness.iter().map(|(name, value)| (name, value.as_ref())));

        let circuit = PowdrCircuit::new(&self.analyzed)
            .with_witgen_callback(witgen_callback)
            .with_phase_0_witness(witness);

        #[cfg(debug_assertions)]
        let circuit = circuit.with_preprocessed(self.get_preprocessed_matrix());

        let stage_0_publics = circuit.public_values_so_far();

        let config = T::get_config();

        let mut challenger = T::get_challenger();

        let proving_key = self.proving_key.as_ref();

        let proof = prove_with_key(
            &config,
            proving_key,
            &circuit,
            &mut challenger,
            stage_0_trace,
            &circuit,
            &stage_0_publics,
        );

        let mut challenger = T::get_challenger();

        let verifying_key = self.verifying_key.as_ref();

        let empty_public = vec![];
        let public_values = once(&stage_0_publics)
            .chain(repeat(&empty_public))
            .take(self.analyzed.stage_count())
            .collect();

        verify_with_key(
            &config,
            verifying_key,
            &circuit,
            &mut challenger,
            &proof,
            public_values,
        )
        .unwrap();
        Ok(bincode::serialize(&proof).unwrap())
    }

    pub fn verify(&self, proof: &[u8], instances: &[Vec<T>]) -> Result<(), String> {
        let proof: Proof<_> =
            bincode::deserialize(proof).map_err(|e| format!("Failed to deserialize proof: {e}"))?;
        let publics = instances
            .iter()
            .flatten()
            .map(|v| v.into_p3_field())
            .collect();

        let config = T::get_config();

        let mut challenger = T::get_challenger();

        let verifying_key = self.verifying_key.as_ref();

        let empty_public = vec![];
        let public_values = once(&publics)
            .chain(repeat(&empty_public))
            .take(self.analyzed.stage_count())
            .collect();

        verify_with_key(
            &config,
            verifying_key,
            &PowdrCircuit::new(&self.analyzed),
            &mut challenger,
            &proof,
            public_values,
        )
        .map_err(|e| format!("Failed to verify proof: {e:?}"))
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use powdr_executor::constant_evaluator::get_uniquely_sized_cloned;
    use powdr_number::{BabyBearField, GoldilocksField, Mersenne31Field};
    use powdr_pipeline::Pipeline;
    use test_log::test;

    use crate::{Commitment, FieldElementMap, Plonky3Prover, ProverData};

    /// Prove and verify execution over all supported fields
    fn run_test(pil: &str) {
        run_test_publics::<GoldilocksField>(pil, None);
        run_test_publics::<BabyBearField>(pil, None);
        run_test_publics::<Mersenne31Field>(pil, None);
    }

    fn run_test_publics<F: FieldElementMap>(pil: &str, malicious_publics: Option<Vec<F>>)
    where
        ProverData<F>: Send,
        Commitment<F>: Send,
    {
        let mut pipeline = Pipeline::<F>::default().from_pil_string(pil.to_string());

        let pil = pipeline.compute_optimized_pil().unwrap();
        let witness_callback = pipeline.witgen_callback().unwrap();
        let witness = pipeline.compute_witness().unwrap();
        let fixed = pipeline.compute_fixed_cols().unwrap();
        let fixed = Arc::new(get_uniquely_sized_cloned(&fixed).unwrap());

        let mut prover = Plonky3Prover::new(pil, fixed);
        prover.setup();
        let proof = prover.prove(&witness, witness_callback);

        assert!(proof.is_ok());

        if let Some(publics) = malicious_publics {
            prover.verify(&proof.unwrap(), &[publics]).unwrap()
        }
    }

    #[test]
    fn add_baby_bear() {
        let content = r#"
        namespace Add(8);
            col witness x;
            col witness y;
            col witness z;
            x + y = z;
        "#;
        run_test_publics::<BabyBearField>(content, None);
    }

    #[test]
    fn public_values() {
        let content = "namespace Global(8); pol witness x; x * (x - 1) = 0; public out = x(7);";
        run_test(content);
    }

    #[test]
    #[should_panic = "not implemented: Unexpected expression: :oldstate"]
    fn public_reference() {
        let content = r#"
        namespace Global(8);
            col witness x;
            col witness y;
            public oldstate = x(0);
            x = 0;
            y = 1 + :oldstate;
        "#;
        run_test(content);
    }

    #[test]
    #[should_panic = "fri err: InvalidPowWitness"]
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
        let gl_malicious_publics = Some(vec![GoldilocksField::from(0)]);
        run_test_publics(content, gl_malicious_publics);

        let bb_malicious_publics = Some(vec![BabyBearField::from(0)]);
        run_test_publics(content, bb_malicious_publics);

        let m31_malicious_publics = Some(vec![Mersenne31Field::from(0)]);
        run_test_publics(content, m31_malicious_publics);
    }

    #[test]
    #[should_panic = "assertion `left == right` failed: Not a power of two: 0\n  left: 0\n right: 1"]
    fn empty() {
        let content = "namespace Global(8);";
        run_test(content);
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
        run_test(content);
    }

    #[test]
    fn fixed() {
        let content = r#"
        namespace Add(8);
            col witness x;
            col fixed y = [1, 0]*;
            x * y = y;
        "#;
        run_test(content);
    }

    #[test]
    fn challenge() {
        let content = r#"
        let N: int = 8;
        
        namespace Global(N); 
            let alpha: expr = std::prelude::challenge(0, 41);
            let beta: expr = std::prelude::challenge(0, 42);
            col witness x;
            col witness stage(1) y;
            x = y + beta * alpha;
        "#;
        run_test(content);
    }

    #[test]
    #[should_panic = "no entry found for key"]
    fn stage_1_public() {
        // this currently fails because we try to extract the public values from the stage 0 witness only
        let content = r#"
        let N: int = 8;
        
        namespace Global(N); 
            let alpha: expr = std::prelude::challenge(0, 41);
            let beta: expr = std::prelude::challenge(0, 42);
            col witness stage(0) x;
            col witness stage(1) y;
            x = y + beta * alpha;

            public out = y(N - 1);
        "#;
        let malicious_publics = Some(vec![GoldilocksField::from(0)]);
        run_test_goldilocks_publics(content, malicious_publics);
    }

    #[test]
    fn polynomial_identity() {
        let content = "namespace Global(8); pol fixed z = [1, 2]*; pol witness a; a = z + 1;";
        run_test(content);
    }

    #[test]
    #[should_panic = "not implemented"]
    fn lookup() {
        let content = "namespace Global(8); pol fixed z = [0, 1]*; pol witness a; [a] in [z];";
        run_test(content);
    }
}
