use std::{io, path::PathBuf, sync::Arc};

use powdr_ast::analyzed::Analyzed;
use powdr_executor::{
    constant_evaluator::{get_uniquely_sized_cloned, VariablySizedColumn},
    witgen::WitgenCallback,
};
use powdr_number::{FieldElement, GoldilocksField, LargeInt};
use powdr_plonky3::{Commitment, FieldElementMap, Plonky3Prover, ProverData};

use crate::{Backend, BackendFactory, BackendOptions, Error, Proof};

pub(crate) struct Factory;

impl<T: FieldElementMap> BackendFactory<T> for Factory
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
        if T::modulus().to_arbitrary_integer() != GoldilocksField::modulus().to_arbitrary_integer()
        {
            unimplemented!("plonky3 is only implemented for the Goldilocks field");
        }
        if setup.is_some() {
            return Err(Error::NoSetupAvailable);
        }
        if verification_app_key.is_some() {
            return Err(Error::NoAggregationAvailable);
        }
        if pil.degrees().len() > 1 {
            return Err(Error::NoVariableDegreeAvailable);
        }

        let fixed = Arc::new(
            get_uniquely_sized_cloned(&fixed).map_err(|_| Error::NoVariableDegreeAvailable)?,
        );

        let mut p3 = Box::new(Plonky3Prover::new(pil, fixed));

        if let Some(verification_key) = verification_key {
            p3.set_verifying_key(verification_key);
        } else {
            p3.setup();
        }

        Ok(p3)
    }
}

impl<T: FieldElementMap> Backend<T> for Plonky3Prover<T>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    fn verify(&self, proof: &[u8], instances: &[Vec<T>]) -> Result<(), Error> {
        Ok(self.verify(proof, instances)?)
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
