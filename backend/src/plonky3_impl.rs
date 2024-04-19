use std::{io, path::Path};

use powdr_ast::analyzed::Analyzed;
use powdr_executor::witgen::WitgenCallback;
use powdr_number::FieldElement;
use powdr_plonky3::Plonky3Prover;

use crate::{Backend, BackendFactory, Error, Proof};

pub(crate) struct Plonky3ProverFactory;

impl<T: FieldElement> BackendFactory<T> for Plonky3ProverFactory {
    fn create<'a>(
        &self,
        pil: &'a Analyzed<T>,
        fixed: &'a [(String, Vec<T>)],
        _output_dir: Option<&'a Path>,
        setup: Option<&mut dyn io::Read>,
        verification_key: Option<&mut dyn io::Read>,
    ) -> Result<Box<dyn crate::Backend<'a, T> + 'a>, Error> {
        if setup.is_some() {
            return Err(Error::NoSetupAvailable);
        }
        let mut plonky3 = Box::new(Plonky3Prover::new(pil, fixed));
        if let Some(vk) = verification_key {
            plonky3.add_verification_key(vk);
        }
        Ok(plonky3)
    }
}

impl<'a, T: FieldElement> Backend<'a, T> for Plonky3Prover<'a, T> {
    fn verify(&self, proof: &[u8], instances: &[Vec<T>]) -> Result<(), Error> {
        self.verify(proof, instances)
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

        Ok(self.prove_ast(witness, witgen_callback)?)
    }

    fn export_verification_key(&self, _output: &mut dyn io::Write) -> Result<(), Error> {
        todo!()
    }
}
