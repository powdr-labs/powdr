use std::{io, path::PathBuf, sync::Arc};

use itertools::Itertools;
use powdr_ast::analyzed::Analyzed;
use powdr_executor::{constant_evaluator::VariablySizedColumn, witgen::WitgenCallback};
use powdr_number::{BabyBearField, GoldilocksField, Mersenne31Field};
use powdr_plonky3::{Commitment, FieldElementMap, Plonky3Prover, ProverData};

use crate::{
    field_filter::generalize_factory, Backend, BackendFactory, BackendOptions, Error, Proof,
};

struct RestrictedFactory;

impl<T: FieldElementMap> BackendFactory<T> for RestrictedFactory
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    fn create(
        &self,
        pil: Arc<Analyzed<T>>,
        fixed: Arc<Vec<(String, VariablySizedColumn<T>)>>,
        _output_dir: Option<PathBuf>,
        setup: Option<&mut dyn io::Read>,
        verification_key: Option<&mut dyn io::Read>,
        verification_app_key: Option<&mut dyn io::Read>,
        _: BackendOptions,
    ) -> Result<Box<dyn crate::Backend<T>>, Error> {
        if setup.is_some() {
            return Err(Error::NoSetupAvailable);
        }
        if verification_app_key.is_some() {
            return Err(Error::NoAggregationAvailable);
        }
        if pil.degrees().len() > 1 {
            return Err(Error::NoVariableDegreeAvailable);
        }
        if pil.public_declarations_in_source_order().any(|(_, d)| {
            pil.definitions.iter().any(|(_, (symbol, _))| {
                symbol.absolute_name == d.name && symbol.stage.unwrap_or_default() > 0
            })
        }) {
            return Err(Error::NoLaterStagePublicAvailable);
        }

        let mut p3 = Box::new(Plonky3Prover::new(pil.clone(), fixed));

        if let Some(verification_key) = verification_key {
            p3.set_verifying_key(verification_key);
        } else {
            p3.setup();
        }

        Ok(p3)
    }
}

generalize_factory!(Factory <- RestrictedFactory, [BabyBearField, GoldilocksField, Mersenne31Field]);

impl<T: FieldElementMap> Backend<T> for Plonky3Prover<T>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    fn verify(&self, proof: &[u8], instances: &[Vec<T>]) -> Result<(), Error> {
        assert_eq!(instances.len(), 1);
        let instances = &instances[0];
        // the backend API currently uses a single array of public inputs
        // plonky3 supports public inputs for each table, for each stage
        let analyzed = self.analyzed();
        let stage_count = analyzed.stage_count();
        let publics = analyzed
            .get_publics()
            .iter()
            .zip_eq(instances.iter())
            .map(|((poly_name, _, _, stage), value)| {
                let namespace = poly_name.split("::").next().unwrap();
                (namespace, stage, value)
            })
            .into_group_map_by(|(namespace, _, _)| namespace.to_string())
            .into_iter()
            .map(|(namespace, inputs)| {
                (
                    namespace.to_string(),
                    inputs.into_iter().fold(
                        vec![vec![]; stage_count],
                        |mut acc, (_, stage, value)| {
                            acc[*stage as usize].push(*value);
                            acc
                        },
                    ),
                )
            })
            .collect();

        Ok(self.verify(proof, publics)?)
    }

    fn prove(
        &self,
        witness: &[(String, Vec<T>)],
        prev_proof: Option<Proof>,
        witgen_callback: WitgenCallback<T>,
    ) -> Result<Proof, Error> {
        if prev_proof.is_some() {
            return Err(Error::NoAggregationAvailable);
        }

        Ok(self.prove(witness, witgen_callback)?)
    }

    fn export_verification_key(&self, output: &mut dyn io::Write) -> Result<(), Error> {
        let vk = self
            .export_verifying_key()
            .map_err(|e| Error::BackendError(e.to_string()))?;
        output.write_all(&vk).unwrap();
        Ok(())
    }
}
