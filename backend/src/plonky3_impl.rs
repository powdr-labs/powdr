use std::{io, path::Path};

use powdr_ast::analyzed::Analyzed;
use powdr_executor::witgen::WitgenCallback;
use powdr_number::{DegreeType, FieldElement};
use powdr_plonky3::{generate_setup, Plonky3Prover};

use crate::{Backend, BackendFactory, BackendOptions, Error, Proof};

pub(crate) struct Plonky3ProverFactory;

impl<T: FieldElement> BackendFactory<T> for Plonky3ProverFactory {
    fn create<'a>(
        &self,
        pil: &'a Analyzed<T>,
        fixed: &'a [(String, Vec<T>)],
        _: Option<&'a Path>,
        setup: Option<&mut dyn io::Read>,
        verification_key: Option<&mut dyn io::Read>,
        _: Option<&mut dyn io::Read>,
        _: BackendOptions,
    ) -> Result<Box<dyn crate::Backend<'a, T> + 'a>, Error> {
        let mut plonky3 = Box::new(Plonky3Prover::new(pil, fixed, setup)?);
        if let Some(vk) = verification_key {
            plonky3.add_verification_key(vk);
        }
        Ok(plonky3)
    }

    fn generate_setup(&self, _size: DegreeType, output: &mut dyn io::Write) -> Result<(), Error> {
        serde_json::to_writer(output, &generate_setup()).unwrap();

        Ok(())
    }
}

impl<'a, T: FieldElement> Backend<'a, T> for Plonky3Prover<'a, T> {
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

        Ok(self.prove_ast(witness, witgen_callback)?)
    }

    fn export_setup(&self, output: &mut dyn io::Write) -> Result<(), Error> {
        self.write_setup(output);

        Ok(())
    }

    fn export_verification_key(&self, output: &mut dyn io::Write) -> Result<(), Error> {
        self.write_vkey(output);

        Ok(())
    }
}
