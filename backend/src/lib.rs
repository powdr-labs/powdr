#![deny(clippy::print_stdout)]

// #[cfg(feature = "bberg")]
mod bberg_impl;
mod pilstark;

use ast::analyzed::Analyzed;
use number::{DegreeType, FieldElement};
use std::{io, marker::PhantomData};
use strum::{Display, EnumString, EnumVariantNames};

#[derive(Clone, EnumString, EnumVariantNames, Display)]
pub enum BackendType {
    // #[cfg(feature = "bberg")]
    #[strum(serialize = "bberg")]
    BBerg,
    #[strum(serialize = "estark")]
    EStark,
    #[strum(serialize = "pil-stark-cli")]
    PilStarkCli,
}

impl BackendType {
    pub fn factory<T: FieldElement>(&self) -> &'static dyn BackendFactory<T> {
        const ESTARK_FACTORY: WithoutSetupFactory<pilstark::estark::EStark> =
            WithoutSetupFactory(PhantomData);
        const PIL_STARK_CLI_FACTORY: WithoutSetupFactory<pilstark::PilStarkCli> =
            WithoutSetupFactory(PhantomData);
        const BBERG_FACTORY: WithoutSetupFactory<bberg::bberg_codegen::BBergCodegen> =
            WithoutSetupFactory(PhantomData);

        match self {
            BackendType::PilStarkCli => &PIL_STARK_CLI_FACTORY,
            BackendType::EStark => &ESTARK_FACTORY,
            BackendType::BBerg => &BBERG_FACTORY,
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
    fn prove(
        &self,
        pil: &Analyzed<F>,
        fixed: &[(String, Vec<F>)],
        witness: &[(String, Vec<F>)],
        prev_proof: Option<Proof>,
        bname: Option<String>,
    ) -> (Option<Proof>, Option<String>) {
        self.0.prove(pil, fixed, witness, prev_proof, bname)
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
    fn prove(
        &self,
        pil: &Analyzed<F>,
        fixed: &[(String, Vec<F>)],
        witness: &[(String, Vec<F>)],
        prev_proof: Option<Proof>,
        bname: Option<String>,
    ) -> (Option<Proof>, Option<String>) {
        self.0.prove(pil, fixed, witness, prev_proof, bname)
    }

    fn write_setup(&self, output: &mut dyn io::Write) -> Result<(), Error> {
        Ok(self.0.write_setup(output)?)
    }
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("input/output error")]
    IO(#[from] std::io::Error),
    #[error("the backend has not setup operations")]
    NoSetupAvailable,
}

pub type Proof = Vec<u8>;

/*
    Bellow are the public interface traits. They are implemented in this
    module, wrapping the traits implemented by each backend.
*/

/// Dynamic interface for a backend.
pub trait Backend<F: FieldElement> {
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
        bname: Option<String>,
    ) -> (Option<Proof>, Option<String>);

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

    fn prove(
        &self,
        pil: &Analyzed<F>,
        fixed: &[(String, Vec<F>)],
        witness: &[(String, Vec<F>)],
        prev_proof: Option<Proof>,
        bname: Option<String>,
    ) -> (Option<Proof>, Option<String>);
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
