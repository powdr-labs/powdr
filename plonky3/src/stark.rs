//! A plonky3 prover using FRI and Poseidon

use itertools::Itertools;
use p3_matrix::dense::RowMajorMatrix;
use powdr_executor::constant_evaluator::VariablySizedColumn;

use core::fmt;
use std::collections::BTreeMap;
use std::sync::Arc;

use powdr_ast::analyzed::Analyzed;

use powdr_executor::witgen::WitgenCallback;

use crate::{
    prove, verify, Proof, StarkProvingKey, StarkVerifyingKey, TableProvingKey,
    TableProvingKeyCollection,
};

use p3_uni_stark::StarkGenericConfig;

use crate::{
    circuit_builder::PowdrCircuit,
    params::{Challenger, Commitment, FieldElementMap, ProverData},
};

pub struct Plonky3Prover<T: FieldElementMap>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    /// The analyzed PIL
    analyzed: Arc<Analyzed<T>>,
    /// The value of the fixed columns
    fixed: Arc<Vec<(String, VariablySizedColumn<T>)>>,
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
    pub fn new(
        analyzed: Arc<Analyzed<T>>,
        fixed: Arc<Vec<(String, VariablySizedColumn<T>)>>,
    ) -> Self {
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

    // /// Returns preprocessed matrix based on the fixed inputs [`Plonky3Prover<T>`].
    // /// This is used in debug mode
    // pub fn get_preprocessed_matrix(&self) -> RowMajorMatrix<Plonky3Field<T>> {
    //     let publics = self
    //         .analyzed
    //         .get_publics()
    //         .into_iter()
    //         .map(|(name, _, row_id, _)| {
    //             let selector = (0..self.analyzed.degree())
    //                 .map(move |i| T::from(i == row_id as u64))
    //                 .collect::<Vec<T>>();
    //             (name, selector)
    //         })
    //         .collect::<Vec<_>>();

    //     let fixed_with_public_selectors = self
    //         .fixed
    //         .iter()
    //         .chain(publics.iter())
    //         .map(|(name, values)| (name, values.as_ref()));

    //     generate_matrix(fixed_with_public_selectors)
    // }
}

impl<T: FieldElementMap> Plonky3Prover<T>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    pub fn setup(&mut self) {
        // get fixed columns
        let fixed_by_table = self
            .fixed
            .iter()
            .into_group_map_by(|(name, _)| name.split("::").next().unwrap());

        let preprocessed: BTreeMap<String, TableProvingKeyCollection<T::Config>> =
            powdr_backend_utils::split_pil(&self.analyzed)
                .iter()
                .filter_map(|(namespace, pil)| {
                    // if we have neither fixed columns nor publics, we don't need to commit to anything
                    if pil.constant_count() + pil.publics_count() == 0 {
                        None
                    } else {
                        let fixed = fixed_by_table.get(namespace.as_str());
                        Some((
                            namespace.to_string(),
                            pil.committed_polys_in_source_order()
                                .find_map(|(s, _)| s.degree)
                                .unwrap()
                                .iter()
                                .map(|size| {
                                    // get selector columns for public values
                                    let publics = pil
                                        .get_publics()
                                        .into_iter()
                                        .map(|(name, _, row_id, _)| {
                                            let selector = (0..size)
                                                .map(move |i| T::from(i == row_id as u64))
                                                .collect::<Vec<T>>();
                                            (name, selector)
                                        })
                                        .collect::<Vec<(String, Vec<T>)>>();

                                    // get the config
                                    let config = T::get_config();

                                    // commit to the fixed columns
                                    let pcs = config.pcs();
                                    let domain =
                                <_ as p3_commit::Pcs<_, Challenger<T>>>::natural_domain_for_degree(
                                    pcs,
                                    size as usize,
                                );
                                    // write fixed into matrix row by row
                                    let matrix = RowMajorMatrix::new(
                                        (0..size)
                                            .flat_map(|i| {
                                                fixed
                                                    .iter()
                                                    .flat_map(|columns| {
                                                        columns.iter().map(|(name, column)| {
                                                            (
                                                                name,
                                                                column.get_by_size(size).unwrap(),
                                                            )
                                                        })
                                                    })
                                                    .collect_vec()
                                                    .into_iter()
                                                    .chain(publics.iter().map(|(name, values)| {
                                                        (name, values.as_ref())
                                                    }))
                                                    .map(move |(_, values)| {
                                                        values[i as usize].into_p3_field()
                                                    })
                                            })
                                            .collect(),
                                        fixed.map(|f| f.len()).unwrap_or_default() + publics.len(),
                                    );

                                    let evaluations = vec![(domain, matrix)];

                                    // commit to the evaluations
                                    (size as usize, {
                                        let (commitment, prover_data) =
                                            <_ as p3_commit::Pcs<_, Challenger<T>>>::commit(
                                                pcs,
                                                evaluations,
                                            );
                                        TableProvingKey {
                                            commitment,
                                            prover_data,
                                        }
                                    })
                                })
                                .collect::<BTreeMap<usize, _>>(),
                        ))
                    }
                })
                .collect();

        let verifying_key = StarkVerifyingKey {
            preprocessed: preprocessed
                .iter()
                .map(|(table, data)| {
                    (
                        table.clone(),
                        data.iter()
                            .map(|(size, table_proving_key)| {
                                (*size, table_proving_key.commitment.clone())
                            })
                            .collect(),
                    )
                })
                .collect(),
        };
        let proving_key = StarkProvingKey { preprocessed };

        self.proving_key = Some(proving_key);
        self.verifying_key = Some(verifying_key);
    }

    pub fn prove(
        &self,
        witness: &[(String, Vec<T>)],
        witgen_callback: WitgenCallback<T>,
    ) -> Result<Vec<u8>, String> {
        let circuit = PowdrCircuit::new((*self.analyzed).clone())
            .with_witgen_callback(witgen_callback)
            .with_phase_0_witness(witness);

        let mut challenger = T::get_challenger();

        let proving_key = self.proving_key.as_ref();

        let proof = prove(proving_key, &circuit, witness, &mut challenger, &circuit);

        let mut challenger = T::get_challenger();

        let verifying_key = self.verifying_key.as_ref();

        let public_values = circuit.public_values_so_far();

        let public_values = public_values
            .into_iter()
            .map(|(name, values)| {
                (
                    name,
                    values
                        .into_iter()
                        .map(|v| {
                            v.into_iter()
                                .map(|v| v.expect("public value should be known"))
                                .collect()
                        })
                        .collect(),
                )
            })
            .collect();

        verify(
            verifying_key,
            &circuit,
            &mut challenger,
            &proof,
            public_values,
        )
        .unwrap();
        Ok(bincode::serialize(&proof).unwrap())
    }

    // verify the proof given the instances for each table, for each stage
    pub fn verify(
        &self,
        proof: &[u8],
        instances: BTreeMap<String, Vec<Vec<T>>>,
    ) -> Result<(), String> {
        let proof: Proof<T::Config> =
            bincode::deserialize(proof).map_err(|e| format!("Failed to deserialize proof: {e}"))?;

        let mut challenger = T::get_challenger();

        let verifying_key = self.verifying_key.as_ref();

        verify(
            verifying_key,
            &PowdrCircuit::new((*self.analyzed).clone()),
            &mut challenger,
            &proof,
            instances,
        )
        .map_err(|e| format!("Failed to verify proof: {e:?}"))
    }
}

#[cfg(test)]
mod tests {

    use std::iter::once;

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

        let mut prover = Plonky3Prover::new(pil, fixed);
        prover.setup();

        let proof = prover.prove(&witness, witness_callback);

        assert!(proof.is_ok());

        if let Some(publics) = malicious_publics {
            let publics = once(("Main".to_string(), vec![publics])).collect();
            prover.verify(&proof.unwrap(), publics).unwrap()
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
        namespace Main(8);
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
    #[should_panic = "No tables to prove"]
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
    fn two_tables() {
        // This test is a bit contrived but witgen wouldn't allow a more direct example
        let content = r#"
        namespace Add(8);
            col witness x;
            col witness y;
            col witness z;
            x = 0;
            y = 0;
            x + y = z;
            1 $ [ x, y, z ] in 1 $ [ Mul::x, Mul::y, Mul::z ];

        namespace Mul(16);
            col witness x;
            col witness y;
            col witness z;
            x * y = z;
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
        run_test_publics::<GoldilocksField>(content, malicious_publics);
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
