use std::{io, path::Path};

use crate::{Backend, BackendFactory, Error, Proof};
use powdr_ast::analyzed::Analyzed;
use powdr_executor::witgen::WitgenCallback;
use powdr_halo2::{generate_setup, Halo2Prover, Params};
use powdr_number::{DegreeType, FieldElement};

pub(crate) struct Halo2ProverFactory;

impl<F: FieldElement> BackendFactory<F> for Halo2ProverFactory {
    fn create<'a>(
        &self,
        pil: &'a Analyzed<F>,
        fixed: &'a [(String, Vec<F>)],
        _output_dir: Option<&'a Path>,
        setup: Option<&mut dyn io::Read>,
        verification_key: Option<&mut dyn io::Read>,
    ) -> Result<Box<dyn crate::Backend<'a, F> + 'a>, Error> {
        let mut halo2 = Box::new(Halo2Prover::new(pil, fixed, setup)?);
        if let Some(vk) = verification_key {
            halo2.add_verification_key(vk);
        }
        Ok(halo2)
    }

    fn generate_setup(
        &self,
        size: DegreeType,
        mut output: &mut dyn io::Write,
    ) -> Result<(), Error> {
        let setup = generate_setup(size);
        Ok(setup.write(&mut output)?)
    }
}

impl<'a, T: FieldElement> Backend<'a, T> for Halo2Prover<'a, T> {
    fn verify(&self, proof: &[u8], instances: &[Vec<T>]) -> Result<(), Error> {
        Ok(self.verify(proof, instances)?)
    }

    fn prove(
        &self,
        witness: &[(String, Vec<T>)],
        prev_proof: Option<Proof>,
        witgen_callback: WitgenCallback<T>,
    ) -> Result<Proof, Error> {
        let proof = match prev_proof {
            Some(proof) => self.prove_aggr(witness, proof),
            None => self.prove_ast(witness, witgen_callback),
        };

        Ok(proof?)
    }

    fn export_setup(&self, mut output: &mut dyn io::Write) -> Result<(), Error> {
        Ok(self.write_setup(&mut output)?)
    }

    fn export_verification_key(&self, mut output: &mut dyn io::Write) -> Result<(), Error> {
        let vk = self.verification_key()?;
        vk.write(&mut output, powdr_halo2::SerdeFormat::Processed)?;

        Ok(())
    }
}

pub(crate) struct Halo2MockFactory;

impl<F: FieldElement> BackendFactory<F> for Halo2MockFactory {
    fn create<'a>(
        &self,
        pil: &'a Analyzed<F>,
        fixed: &'a [(String, Vec<F>)],
        _output_dir: Option<&'a Path>,
        setup: Option<&mut dyn io::Read>,
        verification_key: Option<&mut dyn io::Read>,
    ) -> Result<Box<dyn crate::Backend<'a, F> + 'a>, Error> {
        if setup.is_some() {
            return Err(Error::NoSetupAvailable);
        }
        if verification_key.is_some() {
            return Err(Error::NoVerificationAvailable);
        }
        Ok(Box::new(Halo2Mock { pil, fixed }))
    }
}

pub struct Halo2Mock<'a, F: FieldElement> {
    pil: &'a Analyzed<F>,
    fixed: &'a [(String, Vec<F>)],
}

impl<'a, T: FieldElement> Backend<'a, T> for Halo2Mock<'a, T> {
    fn prove(
        &self,
        witness: &[(String, Vec<T>)],
        prev_proof: Option<Proof>,
        witgen_callback: WitgenCallback<T>,
    ) -> Result<Proof, Error> {
        if prev_proof.is_some() {
            return Err(Error::NoAggregationAvailable);
        }

        powdr_halo2::mock_prove(self.pil, self.fixed, witness, witgen_callback)
            .map_err(Error::BackendError)?;

        Ok(vec![])
    }
}
