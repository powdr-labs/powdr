mod stark;

use std::{collections::BTreeMap, io, path::PathBuf, sync::Arc};

use powdr_ast::analyzed::Analyzed;
use powdr_executor::{constant_evaluator::VariablySizedColumn, witgen::WitgenCallback};
use powdr_number::{BabyBearField, GoldilocksField, KoalaBearField, Mersenne31Field};
use powdr_plonky3::{Commitment, FieldElementMap, ProverData};
use serde::{Deserialize, Serialize};
use stark::Plonky3Prover;

use crate::{
    field_filter::generalize_factory, Backend, BackendFactory, BackendOptions, Error, Proof,
};

struct RestrictedFactory;

impl<T: FieldElementMap> BackendFactory<T> for RestrictedFactory
where
    ProverData<T>: Send + Serialize + for<'a> Deserialize<'a>,
    Commitment<T>: Send,
{
    fn create(
        &self,
        pil: Arc<Analyzed<T>>,
        fixed: Arc<Vec<(String, VariablySizedColumn<T>)>>,
        _output_dir: Option<PathBuf>,
        setup: Option<&mut dyn io::Read>,
        proving_key: Option<&mut dyn io::Read>,
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

        let mut p3 = Box::new(Plonky3Prover::new(pil.clone(), fixed));

        match (proving_key, verification_key) {
            (Some(pk), Some(vk)) => {
                p3.set_proving_key(pk);
                p3.set_verifying_key(vk);
            }
            _ => {
                p3.setup();
            }
        }

        Ok(p3)
    }
}

generalize_factory!(Factory <- RestrictedFactory, [BabyBearField, KoalaBearField, GoldilocksField, Mersenne31Field]);

impl<T: FieldElementMap> Backend<T> for Plonky3Prover<T>
where
    ProverData<T>: Send + Serialize + for<'a> Deserialize<'a>,
    Commitment<T>: Send,
{
    fn verify(&self, proof: &[u8], instances: &[Vec<T>]) -> Result<(), Error> {
        assert_eq!(instances.len(), 1);
        let instances = &instances[0];

        Ok(self.verify(proof, instances)?)
    }

    fn prove(
        &self,
        witness: &[(String, Vec<T>)],
        public: &BTreeMap<String, T>,
        prev_proof: Option<Proof>,
        witgen_callback: WitgenCallback<T>,
    ) -> Result<Proof, Error> {
        if prev_proof.is_some() {
            return Err(Error::NoAggregationAvailable);
        }

        Ok(self.prove(witness, public, witgen_callback)?)
    }

    fn export_verification_key(&self, output: &mut dyn io::Write) -> Result<(), Error> {
        let vk = self
            .export_verifying_key()
            .map_err(|e| Error::BackendError(e.to_string()))?;
        output.write_all(&vk).unwrap();
        Ok(())
    }

    fn export_proving_key(&self, output: &mut dyn io::Write) -> Result<(), Error> {
        self.export_proving_key(output)
            .map_err(|e| Error::BackendError(e.to_string()))
    }
}
