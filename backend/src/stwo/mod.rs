use std::io;
use std::path::PathBuf;
use std::sync::Arc;

use crate::{Backend, BackendFactory, BackendOptions, Error, Proof};
use powdr_ast::analyzed::Analyzed;
use powdr_executor::constant_evaluator::{get_uniquely_sized_cloned, VariablySizedColumn};
use powdr_executor::witgen::WitgenCallback;
use powdr_number::FieldElement;
use prover::StwoProver;

mod prover;

pub(crate) struct StwoProverFactory;

impl<F: FieldElement> BackendFactory<F> for StwoProverFactory {
    fn create(
        &self,
        pil: Arc<Analyzed<F>>,
        fixed: Arc<Vec<(String, VariablySizedColumn<F>)>>,
        _output_dir: Option<PathBuf>,
        setup: Option<&mut dyn io::Read>,
        verification_key: Option<&mut dyn io::Read>,
        verification_app_key: Option<&mut dyn io::Read>,
        options: BackendOptions,
    ) -> Result<Box<dyn crate::Backend<F>>, Error> {
        if pil.degrees().len() > 1 {
            return Err(Error::NoVariableDegreeAvailable);
        }
        let fixed = Arc::new(
            get_uniquely_sized_cloned(&fixed).map_err(|_| Error::NoVariableDegreeAvailable)?,
        );
        let stwo = Box::new(StwoProver::new(pil, fixed, setup)?);
        println!("StwoProverFactory create not implemented yet");
        Ok(stwo)
    }
}

impl<T: FieldElement> Backend<T> for StwoProver<T> {
    fn verify(&self, proof: &[u8], instances: &[Vec<T>]) -> Result<(), Error> {
        assert!(instances.len() == 1);
        Ok(())
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

        panic!("prove function is not implement yet");
        let mut proof = vec![0u8; 10];
        Ok(proof)
    }

    fn export_verification_key(&self, output: &mut dyn io::Write) -> Result<(), Error> {
        Ok(())
    }
}
