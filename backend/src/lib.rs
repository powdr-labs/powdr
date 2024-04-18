#![deny(clippy::print_stdout)]

mod estark;
#[cfg(feature = "halo2")]
mod halo2_impl;

use powdr_ast::analyzed::Analyzed;
use powdr_executor::witgen::WitgenCallback;
use powdr_number::{DegreeType, FieldElement};
use std::{io, path::Path};
use strum::{Display, EnumString, EnumVariantNames};

#[derive(Clone, EnumString, EnumVariantNames, Display, Copy)]
pub enum BackendType {
    #[cfg(feature = "halo2")]
    #[strum(serialize = "halo2")]
    Halo2,
    #[cfg(feature = "halo2")]
    #[strum(serialize = "halo2-mock")]
    Halo2Mock,
    #[cfg(feature = "estark-polygon")]
    #[strum(serialize = "estark-polygon")]
    EStarkPolygon,
    #[strum(serialize = "estark-starky")]
    EStarkStarky,
    #[strum(serialize = "estark-dump")]
    EStarkDump,
}

impl BackendType {
    pub fn factory<T: FieldElement>(&self) -> &'static dyn BackendFactory<T> {
        #[cfg(feature = "halo2")]
        const HALO2_FACTORY: halo2_impl::Halo2ProverFactory = halo2_impl::Halo2ProverFactory;
        #[cfg(feature = "halo2")]
        const HALO2_MOCK_FACTORY: halo2_impl::Halo2MockFactory = halo2_impl::Halo2MockFactory;
        #[cfg(feature = "estark-polygon")]
        const ESTARK_POLYGON_FACTORY: estark::polygon_wrapper::Factory =
            estark::polygon_wrapper::Factory;
        const ESTARK_STARKY_FACTORY: estark::starky_wrapper::Factory =
            estark::starky_wrapper::Factory;
        const ESTARK_DUMP_FACTORY: estark::DumpFactory = estark::DumpFactory;

        match self {
            #[cfg(feature = "halo2")]
            BackendType::Halo2 => &HALO2_FACTORY,
            #[cfg(feature = "halo2")]
            BackendType::Halo2Mock => &HALO2_MOCK_FACTORY,
            #[cfg(feature = "estark-polygon")]
            BackendType::EStarkPolygon => &ESTARK_POLYGON_FACTORY,
            BackendType::EStarkStarky => &ESTARK_STARKY_FACTORY,
            BackendType::EStarkDump => &ESTARK_DUMP_FACTORY,
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
    #[error("the backend does not implement proof verification")]
    NoVerificationAvailable,
    #[error("the backend does not support proof aggregation")]
    NoAggregationAvailable,
    #[error("internal backend error")]
    BackendError(String),
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
    fn create<'a>(
        &self,
        pil: &'a Analyzed<F>,
        fixed: &'a [(String, Vec<F>)],
        output_dir: Option<&'a Path>,
        setup: Option<&mut dyn io::Read>,
        verification_key: Option<&mut dyn io::Read>,
    ) -> Result<Box<dyn Backend<'a, F> + 'a>, Error>;

    /// Generate a new setup.
    fn generate_setup(&self, _size: DegreeType, _output: &mut dyn io::Write) -> Result<(), Error> {
        Err(Error::NoSetupAvailable)
    }
}

/// Dynamic interface for a backend.
pub trait Backend<'a, F: FieldElement> {
    /// Perform the proving.
    ///
    /// If prev_proof is provided, proof aggregation is performed.
    ///
    /// Returns the generated proof.
    fn prove(
        &self,
        witness: &[(String, Vec<F>)],
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
    fn export_verification_key(&self, _output: &mut dyn io::Write) -> Result<(), Error> {
        Err(Error::NoVerificationAvailable)
    }
}
