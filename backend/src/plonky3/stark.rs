//! A plonky3 prover using FRI and Poseidon

use itertools::Itertools;
use p3_commit::Pcs;
use p3_matrix::dense::RowMajorMatrix;
use powdr_backend_utils::{machine_fixed_columns, machine_witness_columns};
use powdr_executor::constant_evaluator::VariablySizedColumn;
use serde::{Deserialize, Serialize};

use core::fmt;
use std::collections::BTreeMap;
use std::sync::Arc;

use powdr_ast::analyzed::Analyzed;

use powdr_executor::witgen::WitgenCallback;

use powdr_plonky3::{
    prove, verify, Challenger, Commitment, ConstraintSystem, FieldElementMap, PowdrCircuit, Proof,
    ProverData, StarkProvingKey, StarkVerifyingKey, TableProvingKey, TableProvingKeyCollection,
};

use p3_uni_stark::StarkGenericConfig;

pub struct Plonky3Prover<T: FieldElementMap>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    /// The analyzed PIL
    analyzed: Arc<Analyzed<T>>,
    /// The split analyzed PIL
    split: BTreeMap<String, (Analyzed<T>, ConstraintSystem<T>)>,
    /// The value of the fixed columns
    fixed: Arc<Vec<(String, VariablySizedColumn<T>)>>,
    /// Proving key
    proving_key: Option<StarkProvingKey<T::Config>>,
    /// Verifying key
    verifying_key: Option<StarkVerifyingKey<T::Config>>,
}

pub enum KeyExportError {
    NoProvingKey,
    NoVerificationKey,
}

impl fmt::Display for KeyExportError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NoProvingKey => write!(f, "No proving key set"),
            Self::NoVerificationKey => write!(f, "No verification key set"),
        }
    }
}

impl<T: FieldElementMap> Plonky3Prover<T>
where
    ProverData<T>: Send + Serialize + for<'a> Deserialize<'a>,
    Commitment<T>: Send,
{
    pub fn new(
        analyzed: Arc<Analyzed<T>>,
        fixed: Arc<Vec<(String, VariablySizedColumn<T>)>>,
    ) -> Self {
        Self {
            split: powdr_backend_utils::split_pil(&analyzed)
                .into_iter()
                .map(|(name, pil)| {
                    let constraint_system = ConstraintSystem::from(&pil);
                    (name, (pil, constraint_system))
                })
                .collect(),
            analyzed,
            fixed,
            proving_key: None,
            verifying_key: None,
        }
    }

    pub fn set_proving_key(&mut self, rdr: &mut dyn std::io::Read) {
        self.proving_key = Some(bincode::deserialize_from(rdr).unwrap());
    }

    pub fn set_verifying_key(&mut self, rdr: &mut dyn std::io::Read) {
        self.verifying_key = Some(bincode::deserialize_from(rdr).unwrap());
    }

    pub fn export_proving_key(
        &self,
        writer: &mut dyn std::io::Write,
    ) -> Result<(), KeyExportError> {
        let pk = self
            .proving_key
            .as_ref()
            .ok_or(KeyExportError::NoProvingKey)?;
        bincode::serialize_into(writer, pk).unwrap();
        Ok(())
    }

    pub fn export_verifying_key(&self) -> Result<Vec<u8>, KeyExportError> {
        Ok(bincode::serialize(
            self.verifying_key
                .as_ref()
                .ok_or(KeyExportError::NoVerificationKey)?,
        )
        .unwrap())
    }
}

impl<T: FieldElementMap> Plonky3Prover<T>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    pub fn setup(&mut self) {
        let preprocessed: BTreeMap<String, TableProvingKeyCollection<T::Config>> = self
            .split
            .iter()
            .filter_map(|(namespace, (pil, _))| {
                // if we have neither fixed columns nor publics, we don't need to commit to anything
                if pil.constant_count() + pil.publics_count() == 0 {
                    None
                } else {
                    let fixed_columns = machine_fixed_columns(&self.fixed, pil);
                    Some((
                        namespace.to_string(),
                        pil.committed_polys_in_source_order()
                            .find_map(|(s, _)| s.degree)
                            .unwrap()
                            .iter()
                            .map(|size| {
                                // get selector columns for the public inputs, as closures
                                let publics = pil
                                    .get_publics()
                                    .into_iter()
                                    .map(|(_, _, _, row_id, _)| {
                                        move |i| T::from(i == row_id as u64)
                                    })
                                    .collect::<Vec<_>>();

                                // get the config
                                let config = T::get_config();

                                // commit to the fixed columns
                                let pcs = config.pcs();
                                let domain = pcs.natural_domain_for_degree(size as usize);
                                let fixed_columns = &fixed_columns[&size];

                                // generate the preprocessed matrix row by row
                                let matrix = RowMajorMatrix::new(
                                    (0..size)
                                        .flat_map(|i| {
                                            fixed_columns
                                                .iter()
                                                .map(move |(_, column)| column[i as usize])
                                                .chain(publics.iter().map(move |f| f(i)))
                                                .map(|value| value.into_p3_field())
                                        })
                                        .collect(),
                                    fixed_columns.len() + publics.len(),
                                );

                                let evaluations = vec![(domain, matrix)];

                                // commit to the evaluations
                                let (commitment, prover_data) =
                                    <_ as p3_commit::Pcs<_, Challenger<T>>>::commit(
                                        pcs,
                                        evaluations,
                                    );
                                (
                                    size as usize,
                                    TableProvingKey {
                                        commitment,
                                        prover_data,
                                    },
                                )
                            })
                            .collect(),
                    ))
                }
            })
            .collect();

        let verifying_key = StarkVerifyingKey {
            preprocessed: preprocessed
                .iter()
                .map(|(table_name, data)| {
                    (
                        table_name.clone(),
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
        let mut witness_by_machine = self
            .split
            .iter()
            .filter_map(|(machine, (pil, _))| {
                let witness_columns = machine_witness_columns(witness, pil, machine);
                if witness_columns[0].1.is_empty() {
                    // Empty machines can be removed entirely.
                    None
                } else {
                    Some((
                        machine.clone(),
                        machine_witness_columns(witness, pil, machine),
                    ))
                }
            })
            .collect::<BTreeMap<_, _>>();

        let circuit = PowdrCircuit::new(&self.split).with_witgen_callback(witgen_callback);

        let mut challenger = T::get_challenger();

        let proving_key = self.proving_key.as_ref();

        let proof = prove(
            proving_key,
            &circuit,
            &mut witness_by_machine,
            &mut challenger,
        );

        let mut challenger = T::get_challenger();

        let verifying_key = self.verifying_key.as_ref();

        let public_values = circuit.public_values_so_far(&witness_by_machine);

        // extract the full map of public values by unwrapping all the options
        let public_values = public_values
            .into_iter()
            .map(|(name, values)| {
                (
                    name,
                    values
                        .into_iter()
                        .map(|v| {
                            v.into_iter()
                                .map(|v| {
                                    v.expect("all public values should be known after execution")
                                })
                                .collect()
                        })
                        .collect(),
                )
            })
            .collect();

        verify(
            verifying_key,
            &circuit
                .split
                .iter()
                .map(|(name, (_, constraints))| (name, constraints))
                .collect(),
            &mut challenger,
            &proof,
            public_values,
        )
        .unwrap();
        Ok(bincode::serialize(&proof).unwrap())
    }

    // verify the proof given the instances for each table, for each stage
    pub fn verify(&self, proof: &[u8], instances: &[T]) -> Result<(), String> {
        let proof: Proof<_> =
            bincode::deserialize(proof).map_err(|e| format!("Failed to deserialize proof: {e}"))?;

        let mut challenger = T::get_challenger();

        let verifying_key = self.verifying_key.as_ref();

        let stage_count = self.analyzed.stage_count();

        let mut instance_map: BTreeMap<String, Vec<Vec<T>>> = self
            .split
            .keys()
            .map(|name| (name.clone(), vec![vec![]; stage_count]))
            .collect();

        self.analyzed
            .get_publics()
            .iter()
            .zip_eq(instances.iter())
            .map(|((_, poly_name, _, _, stage), value)| {
                let namespace = poly_name.split("::").next().unwrap();
                (namespace, stage, value)
            })
            .for_each(|(namespace, stage, value)| {
                instance_map.get_mut(namespace).unwrap()[*stage as usize].push(*value);
            });

        verify(
            verifying_key,
            &self
                .split
                .iter()
                .map(|(name, (_, constraints))| (name, constraints))
                .collect(),
            &mut challenger,
            &proof,
            instance_map,
        )
        .map_err(|e| format!("Failed to verify proof: {e:?}"))
    }
}

#[cfg(test)]
mod tests {

    use powdr_number::{BabyBearField, GoldilocksField, Mersenne31Field};
    use powdr_pipeline::{BackendType, Pipeline};
    use test_log::test;

    use powdr_plonky3::{Commitment, FieldElementMap, ProverData};

    /// Prove and verify execution over all supported fields
    fn run_test(pil: &str) {
        run_test_publics(pil, &None);
    }

    fn run_test_publics(pil: &str, malicious_publics: &Option<Vec<usize>>) {
        run_test_publics_aux::<GoldilocksField>(pil, malicious_publics);
        run_test_publics_aux::<BabyBearField>(pil, malicious_publics);
        run_test_publics_aux::<Mersenne31Field>(pil, malicious_publics);
    }

    fn run_test_publics_aux<F: FieldElementMap>(pil: &str, malicious_publics: &Option<Vec<usize>>)
    where
        ProverData<F>: Send + serde::Serialize + for<'a> serde::Deserialize<'a>,
        Commitment<F>: Send,
    {
        let mut pipeline = Pipeline::<F>::default()
            .with_backend(BackendType::Plonky3, None)
            .from_pil_string(pil.to_string());

        let proof = pipeline.compute_proof().unwrap().clone();

        if let Some(publics) = malicious_publics {
            pipeline
                .verify(
                    &proof,
                    &[publics
                        .iter()
                        .map(|i| F::from(*i as u64))
                        .collect::<Vec<_>>()],
                )
                .unwrap()
        }
    }

    #[test]
    fn public_values() {
        let content = "
        namespace Global(8);
            pol fixed FIRST = [1] + [0]*;
            pol witness x;
            pol witness y;
            y * y = y;
            x' = (1 - FIRST') * (x + 1);
            public out0 = x(6);
            public out1 = x(7);
            public out2 = y(3);
            public out3 = y(5);
        ";
        run_test(content);
    }

    #[test]
    #[should_panic = "Witness generation failed."]
    fn public_reference() {
        let content = r#"
        namespace Global(8);
            col witness x;
            col witness y;
            public oldstate = x(0);
            x = 0;
            y = 1 + oldstate;
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
        let malicious_publics = Some(vec![0]);
        run_test_publics(content, &malicious_publics);
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
    fn next() {
        let content = r#"
        namespace Next(8);
            col witness x;
            col witness y;
            x' + y = 0;
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
    fn stage_1_public() {
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
        run_test(content);
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
