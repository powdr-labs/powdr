pub mod estark;
mod json_exporter;

use crate::{BackendImpl, Error, Proof};
use powdr_ast::analyzed::Analyzed;
use powdr_number::{DegreeType, FieldElement};

pub struct PilStarkCli;

impl<T: FieldElement> BackendImpl<T> for PilStarkCli {
    fn new(_degree: DegreeType) -> Self {
        Self
    }

    fn add_verification_key(
        &mut self,
        _pil: &Analyzed<T>,
        _fixed: &[(String, Vec<T>)],
        _vkey: Vec<u8>,
    ) {
        unimplemented!("pil-stark CLI backend does not yet support verification key");
    }

    fn verification_key(
        &self,
        _pil: &Analyzed<T>,
        _fixed: &[(String, Vec<T>)],
    ) -> Result<Vec<u8>, Error> {
        unimplemented!("pil-stark CLI backend does not yet support verification key");
    }

    fn verify(&self, _proof: &Proof, _instances: &[Vec<T>]) -> Result<(), Error> {
        unimplemented!("pil-stark CLI backend does not yet support separate proof verification");
    }

    fn prove(
        &self,
        pil: &Analyzed<T>,
        _fixed: &[(String, Vec<T>)],
        _witness: &[(String, Vec<T>)],
        prev_proof: Option<Proof>,
    ) -> Result<(Proof, Option<String>), Error> {
        if prev_proof.is_some() {
            unimplemented!("Aggregration is not implemented for pil-stark CLI backend");
        }

        Ok((
            vec![],
            Some(serde_json::to_string(&json_exporter::export(pil)).unwrap()),
        ))
    }
}
