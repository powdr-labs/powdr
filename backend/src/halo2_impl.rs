use std::io::{self};

use crate::{BackendImpl, BackendImplWithSetup, Error, Proof};
use powdr_ast::analyzed::Analyzed;
use powdr_halo2::Halo2Prover;
use powdr_number::{DegreeType, FieldElement};

impl<T: FieldElement> BackendImpl<T> for Halo2Prover {
    fn new(degree: DegreeType) -> Self {
        Halo2Prover::assert_field_is_compatible::<T>();
        Halo2Prover::new(degree)
    }

    fn add_verification_key(
        &mut self,
        pil: &Analyzed<T>,
        fixed: &[(String, Vec<T>)],
        vkey: Vec<u8>,
    ) {
        self.add_verification_key(pil, fixed, vkey)
    }

    fn verification_key(
        &self,
        pil: &Analyzed<T>,
        fixed: &[(String, Vec<T>)],
    ) -> Result<Vec<u8>, Error> {
        match self.verification_key(pil, fixed) {
            Ok(vkey) => Ok(vkey),
            Err(e) => Err(Error::VerificationKeyFailed(e)),
        }
    }

    fn verify(&self, proof: &Proof, instances: &[Vec<T>]) -> Result<(), Error> {
        match self.verify(proof, instances) {
            Ok(_) => Ok(()),
            Err(e) => Err(Error::VerificationFailed(e)),
        }
    }

    fn prove(
        &self,
        pil: &Analyzed<T>,
        fixed: &[(String, Vec<T>)],
        witness: &[(String, Vec<T>)],
        prev_proof: Option<Proof>,
    ) -> Result<(Proof, Option<String>), Error> {
        let proof = match prev_proof {
            Some(proof) => self.prove_aggr(pil, fixed, witness, proof),
            None => self.prove_ast(pil, fixed, witness),
        };

        match proof {
            Ok(proof) => Ok((proof, None)),
            Err(e) => Err(Error::ProofFailed(e.to_string())),
        }
    }
}

impl<T: FieldElement> BackendImplWithSetup<T> for powdr_halo2::Halo2Prover {
    fn new_from_setup(mut input: &mut dyn io::Read) -> Result<Self, io::Error> {
        Halo2Prover::assert_field_is_compatible::<T>();
        Halo2Prover::new_from_setup(&mut input)
    }

    fn write_setup(&self, mut output: &mut dyn io::Write) -> Result<(), io::Error> {
        self.write_setup(&mut output)
    }
}

pub struct Halo2Mock;
impl<T: FieldElement> BackendImpl<T> for Halo2Mock {
    fn new(_degree: DegreeType) -> Self {
        Self
    }

    fn add_verification_key(
        &mut self,
        _pil: &Analyzed<T>,
        _fixed: &[(String, Vec<T>)],
        _vkey: Vec<u8>,
    ) {
        unimplemented!("Halo2Mock backend does not require verification key");
    }

    fn verification_key(
        &self,
        _pil: &Analyzed<T>,
        _fixed: &[(String, Vec<T>)],
    ) -> Result<Vec<u8>, Error> {
        unimplemented!("Halo2Mock backend does not require verification key");
    }

    fn verify(&self, _proof: &Proof, _instances: &[Vec<T>]) -> Result<(), Error> {
        unimplemented!("Halo2Mock backend does not yet support separate proof verification");
    }

    fn prove(
        &self,
        pil: &Analyzed<T>,
        fixed: &[(String, Vec<T>)],
        witness: &[(String, Vec<T>)],
        prev_proof: Option<Proof>,
    ) -> Result<(Proof, Option<String>), Error> {
        if prev_proof.is_some() {
            unimplemented!("Halo2Mock backend does not support aggregation");
        }

        powdr_halo2::mock_prove(pil, fixed, witness);

        Ok((vec![], None))
    }
}
