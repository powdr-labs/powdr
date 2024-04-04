use std::{io, path::Path};

use powdr_ast::analyzed::Analyzed;
use powdr_executor::witgen::WitgenCallback;
use powdr_number::{DegreeType, FieldElement};
use powdr_plonky3::Plonky3Prover;

use crate::{Backend, BackendFactory, Error, Proof};

pub(crate) struct Plonky3ProverFactory;

impl<T: FieldElement> BackendFactory<T> for Plonky3ProverFactory {
    fn create<'a>(
        &self,
        _pil: &'a Analyzed<T>,
        _fixed: &'a [(String, Vec<T>)],
        _output_dir: Option<&'a Path>,
        _setup: Option<&mut dyn io::Read>,
        _verification_key: Option<&mut dyn io::Read>,
    ) -> Result<Box<dyn crate::Backend<'a, T> + 'a>, Error> {
        todo!()
    }

    fn generate_setup(&self, _size: DegreeType, _output: &mut dyn io::Write) -> Result<(), Error> {
        todo!()
    }
}

impl<'a, T: FieldElement> Backend<'a, T> for Plonky3Prover<'a, T> {
    fn verify(&self, _proof: &[u8], _instances: &[Vec<T>]) -> Result<(), Error> {
        todo!()
    }

    fn prove(
        &self,
        _witness: &[(String, Vec<T>)],
        _prev_proof: Option<Proof>,
        _witgen_callback: WitgenCallback<T>,
    ) -> Result<Proof, Error> {
        todo!()
    }

    fn export_setup(&self, _output: &mut dyn io::Write) -> Result<(), Error> {
        todo!()
    }

    fn export_verification_key(&self, _output: &mut dyn io::Write) -> Result<(), Error> {
        todo!()
    }
}
