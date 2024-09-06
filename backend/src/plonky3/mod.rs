use std::{
    any::{Any, TypeId},
    io,
    path::PathBuf,
    sync::Arc,
};

use powdr_ast::analyzed::Analyzed;
use powdr_executor::{
    constant_evaluator::{get_uniquely_sized_cloned, VariablySizedColumn},
    witgen::WitgenCallback,
};
use powdr_number::{BabyBearField, FieldElement, GoldilocksField};
use powdr_plonky3::{Commitment, FieldElementMap, Plonky3Prover, ProverData};

use crate::{Backend, BackendFactory, BackendOptions, Error, Proof};

pub(crate) struct Factory;

fn try_create<FInner: FieldElementMap, FOuter: FieldElement>(
    pil: &Arc<Analyzed<FOuter>>,
    fixed: &Arc<Vec<(String, Vec<FOuter>)>>,
    verification_key: &mut Option<&mut dyn io::Read>,
) -> Option<Box<dyn Backend<FOuter>>>
where
    ProverData<FInner>: Send,
    Commitment<FInner>: Send,
{
    // We ensure that FInner and FOuter are the same type, so we can even safely
    // transmute between them.
    if TypeId::of::<FInner>() != TypeId::of::<FOuter>() {
        return None;
    }

    let pil = (pil as &dyn Any)
        .downcast_ref::<Arc<Analyzed<FInner>>>()
        .unwrap();
    let fixed = (fixed as &dyn Any)
        .downcast_ref::<Arc<Vec<(String, Vec<FInner>)>>>()
        .unwrap();

    let mut p3 = Box::new(Plonky3Prover::new(pil.clone(), fixed.clone()));

    if let Some(verification_key) = verification_key {
        p3.set_verifying_key(*verification_key);
    } else {
        p3.setup();
    }

    let p3: Box<dyn Backend<FInner>> = p3;
    let p3 = Box::into_raw(p3);

    // This is safe because we know that FInner == FOuter.
    Some(unsafe { Box::from_raw(p3 as *mut dyn Backend<FOuter>) })
}

impl<T: FieldElement> BackendFactory<T> for Factory {
    fn create(
        &self,
        pil: Arc<Analyzed<T>>,
        fixed: Arc<Vec<(String, VariablySizedColumn<T>)>>,
        _output_dir: Option<PathBuf>,
        setup: Option<&mut dyn io::Read>,
        mut verification_key: Option<&mut dyn io::Read>,
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

        let fixed = Arc::new(
            get_uniquely_sized_cloned(&fixed).map_err(|_| Error::NoVariableDegreeAvailable)?,
        );

        Ok(
            if let Some(p3) = try_create::<GoldilocksField, T>(&pil, &fixed, &mut verification_key)
            {
                p3
            } else if let Some(p3) =
                try_create::<BabyBearField, T>(&pil, &fixed, &mut verification_key)
            {
                p3
            } else {
                unimplemented!("unsupported field type: {:?}", TypeId::of::<T>())
            },
        )
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
