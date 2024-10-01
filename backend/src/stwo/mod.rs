#![deny(clippy::print_stdout)]

use std::io;
use std::path::PathBuf;
use std::sync::Arc;

use crate::{Backend, BackendFactory, BackendOptions, Error, Proof};
use powdr_ast::analyzed::Analyzed;
use powdr_executor::constant_evaluator::{get_uniquely_sized_cloned, VariablySizedColumn};
use powdr_executor::witgen::WitgenCallback;
use powdr_number::{DegreeType, FieldElement};
use prover::{generate_setup, StwoProver};

use serde::de::{self, Deserializer};
use serde::ser::Serializer;
use serde::{Deserialize, Serialize};

mod circuit_builder;
mod prover;

use halo2_proofs::poly::commitment::Params;

pub(crate) struct StwoProverFactory;

#[derive(Clone)]
enum ProofType {
    /// Create a single proof for a given PIL using Poseidon transcripts.
    Poseidon,
    /// Create a single proof for a given PIL using Keccak transcripts,
    /// which can be verified directly on Ethereum.
    SnarkSingle,
    /// Create a recursive proof that compresses a Poseidon proof,
    /// which can be verified directly on Ethereum.
    SnarkAggr,
}

impl From<BackendOptions> for ProofType {
    fn from(options: BackendOptions) -> Self {
        match options.as_str() {
            "" | "poseidon" => Self::Poseidon,
            "snark_single" => Self::SnarkSingle,
            "snark_aggr" => Self::SnarkAggr,
            _ => panic!("Unsupported proof type: {options}"),
        }
    }
}

fn serialize_as_hex<S>(bytes: &Vec<u8>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let hex_string = hex::encode(bytes);
    serializer.serialize_str(&hex_string)
}

fn deserialize_from_hex<'de, D>(deserializer: D) -> Result<Vec<u8>, D::Error>
where
    D: Deserializer<'de>,
{
    let s = String::deserialize(deserializer)?;
    hex::decode(s).map_err(de::Error::custom)
}

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
        let proof_type = ProofType::from(options);
        let fixed = Arc::new(
            get_uniquely_sized_cloned(&fixed).map_err(|_| Error::NoVariableDegreeAvailable)?,
        );
        let mut stwo = Box::new(StwoProver::new(pil, fixed, setup)?);
        println!("StwoProverFactory create not implemented yet");
        Ok(stwo)
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

fn fe_slice_to_string<F: FieldElement>(fe: &[F]) -> Vec<String> {
    fe.iter().map(|x| x.to_string()).collect()
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

        println!("prove for backend trait of stwoprover is not implement yet");
        self.prove(witness, witgen_callback);
          
        
       
        panic!("function not implement yet");
        let mut proof = vec![0u8; 10];
        Ok(proof)
    }

    fn export_setup(&self, mut output: &mut dyn io::Write) -> Result<(), Error> {
        Ok(self.write_setup(&mut output)?)
    }

    fn export_verification_key(&self, output: &mut dyn io::Write) -> Result<(), Error> {
       
        Ok(())
    }

    
}


