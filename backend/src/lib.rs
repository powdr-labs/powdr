#![deny(clippy::print_stdout)]

#[cfg(feature = "halo2")]
mod halo2_impl;
mod pilstark;

use powdr_ast::analyzed::Analyzed;
use powdr_number::{DegreeType, FieldElement};
use std::{io, marker::PhantomData};
use strum::{Display, EnumString, EnumVariantNames};

#[derive(Clone, EnumString, EnumVariantNames, Display, Copy)]
pub enum BackendType {
    #[cfg(feature = "halo2")]
    #[strum(serialize = "halo2")]
    Halo2,
    #[cfg(feature = "halo2")]
    #[strum(serialize = "halo2-mock")]
    Halo2Mock,
    #[strum(serialize = "estark")]
    EStark,
    #[strum(serialize = "pil-stark-cli")]
    PilStarkCli,
}

impl BackendType {
    pub fn factory<T: FieldElement>(&self) -> &'static dyn BackendFactory<T> {
        #[cfg(feature = "halo2")]
        const HALO2_FACTORY: WithSetupFactory<powdr_halo2::Halo2Prover> =
            WithSetupFactory(PhantomData);
        #[cfg(feature = "halo2")]
        const HALO2_MOCK_FACTORY: WithoutSetupFactory<halo2_impl::Halo2Mock> =
            WithoutSetupFactory(PhantomData);
        const ESTARK_FACTORY: WithoutSetupFactory<pilstark::estark::EStark> =
            WithoutSetupFactory(PhantomData);
        const PIL_STARK_CLI_FACTORY: WithoutSetupFactory<pilstark::PilStarkCli> =
            WithoutSetupFactory(PhantomData);

        match self {
            #[cfg(feature = "halo2")]
            BackendType::Halo2 => &HALO2_FACTORY,
            #[cfg(feature = "halo2")]
            BackendType::Halo2Mock => &HALO2_MOCK_FACTORY,
            BackendType::PilStarkCli => &PIL_STARK_CLI_FACTORY,
            BackendType::EStark => &ESTARK_FACTORY,
        }
    }
}

/// Factory for backends without setup.
struct WithoutSetupFactory<B>(PhantomData<B>);

/// Factory implementation for backends without setup.
impl<F: FieldElement, B: BackendImpl<F> + 'static> BackendFactory<F> for WithoutSetupFactory<B> {
    fn create(&self, degree: DegreeType) -> Box<dyn Backend<F>> {
        Box::new(ConcreteBackendWithoutSetup(B::new(degree)))
    }

    fn create_from_setup(&self, _input: &mut dyn io::Read) -> Result<Box<dyn Backend<F>>, Error> {
        Err(Error::NoSetupAvailable)
    }
}

/// Concrete dynamic dispatch Backend object, for backends without setup.
struct ConcreteBackendWithoutSetup<B>(B);

/// Concrete implementation for backends with setup.
impl<F: FieldElement, B: BackendImpl<F>> Backend<F> for ConcreteBackendWithoutSetup<B> {
    fn add_verification_key(
        &mut self,
        pil: &Analyzed<F>,
        fixed: &[(String, Vec<F>)],
        vkey: Vec<u8>,
    ) {
        self.0.add_verification_key(pil, fixed, vkey);
    }

    fn verification_key(
        &self,
        pil: &Analyzed<F>,
        fixed: &[(String, Vec<F>)],
    ) -> Result<Vec<u8>, Error> {
        self.0.verification_key(pil, fixed)
    }

    fn verify(&self, proof: &Proof, instances: &[Vec<F>]) -> Result<(), Error> {
        self.0.verify(proof, instances)
    }

    fn prove(
        &self,
        pil: &Analyzed<F>,
        fixed: &[(String, Vec<F>)],
        witness: &[(String, Vec<F>)],
        prev_proof: Option<Proof>,
    ) -> Result<(Proof, Option<String>), Error> {
        self.0.prove(pil, fixed, witness, prev_proof)
    }

    fn write_setup(&self, _output: &mut dyn io::Write) -> Result<(), Error> {
        Err(Error::NoSetupAvailable)
    }
}

/// Factory for backends with setup.
struct WithSetupFactory<B>(PhantomData<B>);

/// Factory implementation for backends with setup.
impl<F: FieldElement, B: BackendImplWithSetup<F> + 'static> BackendFactory<F>
    for WithSetupFactory<B>
{
    fn create(&self, degree: DegreeType) -> Box<dyn Backend<F>> {
        Box::new(ConcreteBackendWithSetup(B::new(degree)))
    }

    fn create_from_setup(&self, input: &mut dyn io::Read) -> Result<Box<dyn Backend<F>>, Error> {
        Ok(Box::new(ConcreteBackendWithSetup(B::new_from_setup(
            input,
        )?)))
    }
}

/// Concrete dynamic dispatch Backend object, for backends with setup.
struct ConcreteBackendWithSetup<B>(B);

/// Concrete implementation for backends with setup.
impl<F: FieldElement, B: BackendImplWithSetup<F>> Backend<F> for ConcreteBackendWithSetup<B> {
    fn add_verification_key(
        &mut self,
        pil: &Analyzed<F>,
        fixed: &[(String, Vec<F>)],
        vkey: Vec<u8>,
    ) {
        self.0.add_verification_key(pil, fixed, vkey);
    }

    fn verification_key(
        &self,
        pil: &Analyzed<F>,
        fixed: &[(String, Vec<F>)],
    ) -> Result<Vec<u8>, Error> {
        self.0.verification_key(pil, fixed)
    }

    fn verify(&self, proof: &Proof, instances: &[Vec<F>]) -> Result<(), Error> {
        self.0.verify(proof, instances)
    }

    fn prove(
        &self,
        pil: &Analyzed<F>,
        fixed: &[(String, Vec<F>)],
        witness: &[(String, Vec<F>)],
        prev_proof: Option<Proof>,
    ) -> Result<(Proof, Option<String>), Error> {
        self.0.prove(pil, fixed, witness, prev_proof)
    }

    fn write_setup(&self, output: &mut dyn io::Write) -> Result<(), Error> {
        Ok(self.0.write_setup(output)?)
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
    #[error("proof generation failed")]
    ProofFailed(String),
    #[error("proof verification failed")]
    VerificationFailed(String),
    #[error("verification key generation failed")]
    VerificationKeyFailed(String),
}

pub type Proof = Vec<u8>;

/*
    Bellow are the public interface traits. They are implemented in this
    module, wrapping the traits implemented by each backend.
*/

/// Dynamic interface for a backend.
pub trait Backend<F: FieldElement> {
    fn add_verification_key(
        &mut self,
        pil: &Analyzed<F>,
        fixed: &[(String, Vec<F>)],
        vkey: Vec<u8>,
    );

    fn verification_key(
        &self,
        pil: &Analyzed<F>,
        fixed: &[(String, Vec<F>)],
    ) -> Result<Vec<u8>, Error>;

    fn verify(&self, proof: &Proof, instances: &[Vec<F>]) -> Result<(), Error>;

    /// Perform the proving.
    ///
    /// If prev_proof is provided, proof aggregation is performed.
    ///
    /// Returns the generated proof, and the string serialization of the
    /// constraints.
    fn prove(
        &self,
        pil: &Analyzed<F>,
        fixed: &[(String, Vec<F>)],
        witness: &[(String, Vec<F>)],
        prev_proof: Option<Proof>,
    ) -> Result<(Proof, Option<String>), Error>;

    /// Write the prover setup to a file, so that it can be loaded later.
    fn write_setup(&self, output: &mut dyn io::Write) -> Result<(), Error>;
}

/// Dynamic interface for a backend factory.
pub trait BackendFactory<F: FieldElement> {
    /// Maybe perform the setup, and create a new backend object.
    fn create(&self, degree: DegreeType) -> Box<dyn Backend<F>>;

    /// Create a backend object from a prover setup loaded from a file.
    fn create_from_setup(&self, input: &mut dyn io::Read) -> Result<Box<dyn Backend<F>>, Error>;
}

/*
    Below are the traits implemented by the backends.
*/

/// Trait implemented by all backends.
trait BackendImpl<F: FieldElement> {
    fn new(degree: DegreeType) -> Self;

    fn add_verification_key(
        &mut self,
        pil: &Analyzed<F>,
        fixed: &[(String, Vec<F>)],
        vkey: Vec<u8>,
    );

    fn verification_key(
        &self,
        pil: &Analyzed<F>,
        fixed: &[(String, Vec<F>)],
    ) -> Result<Vec<u8>, Error>;

    fn verify(&self, proof: &Proof, instances: &[Vec<F>]) -> Result<(), Error>;

    fn prove(
        &self,
        pil: &Analyzed<F>,
        fixed: &[(String, Vec<F>)],
        witness: &[(String, Vec<F>)],
        prev_proof: Option<Proof>,
    ) -> Result<(Proof, Option<String>), Error>;
}

/// Trait implemented by backends that have a setup phase that must be saved to
/// a file.
trait BackendImplWithSetup<F: FieldElement>
where
    Self: Sized + BackendImpl<F>,
{
    /// Create a backend object from a setup loaded from a file.
    fn new_from_setup(input: &mut dyn io::Read) -> Result<Self, io::Error>;

    /// Write the setup to a file.
    fn write_setup(&self, output: &mut dyn io::Write) -> Result<(), io::Error>;
}
