use std::{io, path::Path};

use crate::{Backend, BackendFactory, BackendOptions, Error, Proof};
use powdr_ast::analyzed::Analyzed;
use powdr_executor::witgen::WitgenCallback;
use powdr_halo2::{
    generate_setup, read_verification_key, Halo2Mock, Halo2Prover, Params, ProofType,
};
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
        verification_app_key: Option<&mut dyn io::Read>,
        options: BackendOptions,
    ) -> Result<Box<dyn crate::Backend<'a, F> + 'a>, Error> {
        let proof_type = ProofType::from(options);
        let mut halo2 = Box::new(Halo2Prover::new(pil, fixed, setup, proof_type)?);
        if let Some(vk) = verification_key {
            halo2.add_verification_key(vk);
        }
        if let Some(vk_app) = verification_app_key {
            halo2.add_verification_app_key(vk_app);
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
        match self.proof_type() {
            ProofType::Poseidon => Ok(self.verify_poseidon(proof, instances)?),
            ProofType::SnarkSingle | ProofType::SnarkAggr => {
                Ok(self.verify_snark(proof, instances)?)
            }
        }
    }

    fn prove(
        &self,
        witness: &[(String, Vec<T>)],
        prev_proof: Option<Proof>,
        witgen_callback: WitgenCallback<T>,
    ) -> Result<Proof, Error> {
        let proof = match self.proof_type() {
            ProofType::Poseidon => self.prove_poseidon(witness, witgen_callback),
            ProofType::SnarkSingle => self.prove_snark_single(witness, witgen_callback),
            ProofType::SnarkAggr => match prev_proof {
                Some(proof) => self.prove_snark_aggr(witness, witgen_callback, proof),
                None => Err("Aggregated proof requires a previous proof".to_string()),
            },
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

    fn export_ethereum_verifier(&self, output: &mut dyn io::Write) -> Result<(), Error> {
        match self.proof_type() {
            ProofType::Poseidon => Err(Error::BackendError(
                "Ethereum verifier is only supported for snark_single and snark_aggr".to_string(),
            )),
            ProofType::SnarkSingle | ProofType::SnarkAggr => {
                match self.export_ethereum_verifier_snark(output) {
                    Ok(_) => Ok(()),
                    Err(e) => Err(Error::BackendError(e.to_string())),
                }
            }
        }
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
        _verification_key: Option<&mut dyn io::Read>,
        verification_app_key: Option<&mut dyn io::Read>,
        options: BackendOptions,
    ) -> Result<Box<dyn crate::Backend<'a, F> + 'a>, Error> {
        if setup.is_some() {
            return Err(Error::NoSetupAvailable);
        }
        let vkey = verification_app_key
            .map(|vkey_file| read_verification_key(&ProofType::from(options), pil, vkey_file));

        Ok(Box::new(Halo2Mock::new(pil, fixed, setup, vkey).unwrap()))
    }
}

impl<'a, T: FieldElement> Backend<'a, T> for Halo2Mock<'a, T> {
    fn prove(
        &self,
        witness: &[(String, Vec<T>)],
        prev_proof: Option<Proof>,
        witgen_callback: WitgenCallback<T>,
    ) -> Result<Proof, Error> {
        self.prove(witness, prev_proof, witgen_callback)
            .map_err(Error::BackendError)?;

        Ok(vec![])
    }
}
