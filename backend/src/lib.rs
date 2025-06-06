#[cfg(feature = "plonky3")]
mod plonky3;
#[cfg(feature = "stwo")]
mod stwo;

#[cfg(any(feature = "plonky3", feature = "stwo"))]
mod composite;
mod field_filter;
mod mock;

use powdr_ast::analyzed::Analyzed;
use powdr_executor::{constant_evaluator::VariablySizedColumn, witgen::WitgenCallback};
use powdr_number::{DegreeType, FieldElement};
use std::{collections::BTreeMap, io, path::PathBuf, sync::Arc};
use strum::{Display, EnumString, EnumVariantNames};

#[derive(Clone, Default, EnumString, EnumVariantNames, Display, Copy)]
pub enum BackendType {
    #[strum(serialize = "mock")]
    #[default]
    Mock,
    #[cfg(feature = "plonky3")]
    #[strum(serialize = "plonky3")]
    Plonky3,
    #[cfg(feature = "plonky3")]
    #[strum(serialize = "plonky3-composite")]
    Plonky3Composite,
    #[cfg(feature = "stwo")]
    #[strum(serialize = "stwo")]
    Stwo,
    #[cfg(feature = "stwo")]
    #[strum(serialize = "stwo-composite")]
    StwoComposite,
}

pub type BackendOptions = String;

impl BackendType {
    pub fn factory<T: FieldElement>(&self) -> Box<dyn BackendFactory<T>> {
        match self {
            BackendType::Mock => Box::new(mock::MockBackendFactory),
            #[cfg(feature = "plonky3")]
            BackendType::Plonky3 => Box::new(plonky3::Factory),
            #[cfg(feature = "plonky3")]
            BackendType::Plonky3Composite => {
                Box::new(composite::CompositeBackendFactory::new(plonky3::Factory))
            }
            #[cfg(feature = "stwo")]
            BackendType::Stwo => Box::new(stwo::Factory),
            #[cfg(feature = "stwo")]
            BackendType::StwoComposite => {
                Box::new(composite::CompositeBackendFactory::new(stwo::Factory))
            }
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("input/output error")]
    IO(#[from] std::io::Error),
    #[error("the witness is empty")]
    EmptyWitness,
    #[error("the backend has no setup operations")]
    NoSetupAvailable,
    #[error("the backend does not use a proving key setup")]
    NoProvingKeyAvailable,
    #[error("the backend does not implement proof verification")]
    NoVerificationAvailable,
    #[error("the backend does not support Ethereum onchain verification")]
    NoEthereumVerifierAvailable,
    #[error("the backend does not support proof aggregation")]
    NoAggregationAvailable,
    #[error("the backend does not support variable degrees")]
    NoVariableDegreeAvailable,
    #[error("internal backend error")]
    BackendError(String),
    #[error("the backend does not support public values which rely on later stage witnesses")]
    NoLaterStagePublicAvailable,
}

impl From<String> for Error {
    fn from(s: String) -> Self {
        Error::BackendError(s)
    }
}

pub type Proof = Vec<u8>;

/*
    Bellow are the public interface traits. They are implemented in this
    module, wrapping the traits implemented by each backend.
*/

/// Dynamic interface for a backend factory.
pub trait BackendFactory<F: FieldElement> {
    /// Create a new backend object.
    #[allow(clippy::too_many_arguments)]
    fn create(
        &self,
        pil: Arc<Analyzed<F>>,
        fixed: Arc<Vec<(String, VariablySizedColumn<F>)>>,
        output_dir: Option<PathBuf>,
        setup: Option<&mut dyn io::Read>,
        proving_key: Option<&mut dyn io::Read>,
        verification_key: Option<&mut dyn io::Read>,
        verification_app_key: Option<&mut dyn io::Read>,
        backend_options: BackendOptions,
    ) -> Result<Box<dyn Backend<F>>, Error>;

    /// Generate a new setup.
    fn generate_setup(&self, _size: DegreeType, _output: &mut dyn io::Write) -> Result<(), Error> {
        Err(Error::NoSetupAvailable)
    }

    fn specialize_pil(&self, pil: Analyzed<F>) -> Analyzed<F> {
        // TODO: currently defaults to the identity function
        // Move `bus_multi_linker` calls here in the future
        pil
    }
}

/// Dynamic interface for a backend.
pub trait Backend<F: FieldElement>: Send {
    /// Perform the proving.
    ///
    /// The backend uses the BackendOptions provided at creation time
    /// to potentially perform aggregation/compression.
    ///
    /// Returns the generated proof.
    fn prove(
        &self,
        witness: &[(String, Vec<F>)],
        publics: &BTreeMap<String, Option<F>>,
        prev_proof: Option<Proof>,
        witgen_callback: WitgenCallback<F>,
    ) -> Result<Proof, Error>;

    /// Verifies a proof.
    fn verify(&self, _proof: &[u8], _instances: &[Vec<F>]) -> Result<(), Error> {
        Err(Error::NoVerificationAvailable)
    }

    /// Exports the setup in a backend specific format. Can be used to create a
    /// new backend object of the same kind.
    fn export_setup(&self, _output: &mut dyn io::Write) -> Result<(), Error> {
        Err(Error::NoSetupAvailable)
    }

    /// Exports the verification key in a backend specific format. Can be used
    /// to create a new backend object of the same kind.
    fn export_verification_key(&self, output: &mut dyn io::Write) -> Result<(), Error> {
        let v = self.verification_key_bytes()?;
        log::info!("Verification key size: {} bytes", v.len());
        output
            .write_all(&v)
            .map_err(|_| Error::BackendError("Could not write verification key".to_string()))?;
        Ok(())
    }

    fn export_proving_key(&self, _output: &mut dyn io::Write) -> Result<(), Error> {
        Err(Error::NoProvingKeyAvailable)
    }

    fn verification_key_bytes(&self) -> Result<Vec<u8>, Error> {
        Err(Error::NoVerificationAvailable)
    }

    /// Exports an Ethereum verifier.
    fn export_ethereum_verifier(&self, _output: &mut dyn io::Write) -> Result<(), Error> {
        Err(Error::NoEthereumVerifierAvailable)
    }
}
