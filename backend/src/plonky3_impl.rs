use std::{io, path::Path};

use powdr_ast::analyzed::Analyzed;
use powdr_number::{DegreeType, FieldElement};
use powdr_plonky3::Plonky3Prover;

use crate::{Backend, BackendFactory, Error, Proof};

pub(crate) struct Plonky3ProverFactory;

impl<F: FieldElement> BackendFactory<F> for Plonky3ProverFactory {
    fn create<'a>(
        &self,
        pil: &'a Analyzed<F>,
        fixed: &'a [(String, Vec<F>)],
        _output_dir: Option<&'a Path>,
        setup: Option<&mut dyn io::Read>,
        verification_key: Option<&mut dyn io::Read>,
    ) -> Result<Box<dyn crate::Backend<'a, F> + 'a>, Error> {
        todo!()
    }

    fn generate_setup(
        &self,
        size: DegreeType,
        mut output: &mut dyn io::Write,
    ) -> Result<(), Error> {
        todo!()
    }
}

impl<'a, T: FieldElement> Backend<'a, T> for Plonky3Prover<'a, T> {
    fn verify(&self, proof: &[u8], instances: &[Vec<T>]) -> Result<(), Error> {
        todo!()
    }

    fn prove(
        &self,
        witness: &[(String, Vec<T>)],
        prev_proof: Option<Proof>,
    ) -> Result<Proof, Error> {
        todo!()
    }

    fn export_setup(&self, mut output: &mut dyn io::Write) -> Result<(), Error> {
        todo!()
    }

    fn export_verification_key(&self, mut output: &mut dyn io::Write) -> Result<(), Error> {
        todo!()
    }
}
