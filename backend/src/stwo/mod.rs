use serde::de::DeserializeOwned;
use serde::Serialize;
use std::io;
use std::path::PathBuf;
use std::sync::Arc;

use crate::{
    field_filter::generalize_factory, Backend, BackendFactory, BackendOptions, Error, Proof,
};
use powdr_ast::analyzed::Analyzed;
use powdr_executor::constant_evaluator::{get_uniquely_sized_cloned, VariablySizedColumn};
use powdr_executor::witgen::WitgenCallback;
use powdr_number::{FieldElement, Mersenne31Field};
use prover::StwoProver;
use stwo_prover::core::backend::simd::SimdBackend;
use stwo_prover::core::backend::BackendForChannel;
use stwo_prover::core::channel::Blake2sChannel;
use stwo_prover::core::channel::Channel;
use stwo_prover::core::channel::MerkleChannel;
use stwo_prover::core::vcs::blake2_merkle::Blake2sMerkleChannel;

mod circuit_builder;
mod prover;
#[allow(dead_code)]

struct RestrictedFactory;

impl<F: FieldElement> BackendFactory<F> for RestrictedFactory {
    #[allow(unreachable_code)]
    #[allow(unused_variables)]
    fn create(
        &self,
        pil: Arc<Analyzed<F>>,
        fixed: Arc<Vec<(String, VariablySizedColumn<F>)>>,
        _output_dir: Option<PathBuf>,
        setup: Option<&mut dyn io::Read>,
        proving_key: Option<&mut dyn io::Read>,
        verification_key: Option<&mut dyn io::Read>,
        verification_app_key: Option<&mut dyn io::Read>,
        options: BackendOptions,
    ) -> Result<Box<dyn crate::Backend<F>>, Error> {
        if proving_key.is_some() {
            return Err(Error::BackendError("Proving key unused".to_string()));
        }
        if pil.degrees().len() > 1 {
            return Err(Error::NoVariableDegreeAvailable);
        }
        let fixed = Arc::new(
            get_uniquely_sized_cloned(&fixed).map_err(|_| Error::NoVariableDegreeAvailable)?,
        );
        let stwo: Box<StwoProver<F, SimdBackend, Blake2sMerkleChannel, Blake2sChannel>> =
            Box::new(StwoProver::new(pil, fixed)?);
        Ok(stwo)
    }
}

generalize_factory!(Factory <- RestrictedFactory, [Mersenne31Field]);

impl<T: FieldElement, MC: MerkleChannel + Send, C: Channel + Send> Backend<T>
    for StwoProver<T, SimdBackend, MC, C>
where
    SimdBackend: BackendForChannel<MC>, // Ensure B implements BackendForChannel<MC>
    MC: MerkleChannel,
    C: Channel,
    MC::H: DeserializeOwned + Serialize,
{
    #[allow(unused_variables)]
    fn verify(&self, proof: &[u8], instances: &[Vec<T>]) -> Result<(), Error> {
        assert_eq!(instances.len(), 1);
        let instances = &instances[0];

        Ok(self.verify(proof, instances)?)
    }
    #[allow(unreachable_code)]
    #[allow(unused_variables)]
    fn prove(
        &self,
        witness: &[(String, Vec<T>)],
        prev_proof: Option<Proof>,
        witgen_callback: WitgenCallback<T>,
    ) -> Result<Proof, Error> {
        if prev_proof.is_some() {
            return Err(Error::NoAggregationAvailable);
        }
        Ok(StwoProver::prove(self, witness)?)
    }
    #[allow(unused_variables)]
    fn export_verification_key(&self, output: &mut dyn io::Write) -> Result<(), Error> {
        unimplemented!()
    }
}
