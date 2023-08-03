#[cfg(feature = "halo2")]
mod halo2_structs;
mod pilcom_cli;

use ast::analyzed::Analyzed;
use number::{DegreeType, FieldElement};
use std::{io, marker::PhantomData, path::Path};
use strum::{Display, EnumString, EnumVariantNames};

#[derive(Clone, EnumString, EnumVariantNames, Display)]
pub enum BackendType {
    #[cfg(feature = "halo2")]
    #[strum(serialize = "halo2")]
    Halo2,
    #[cfg(feature = "halo2")]
    #[strum(serialize = "halo2-mock")]
    Halo2Mock,
    #[strum(serialize = "pilcom-cli")]
    PilcomCli,
}

impl BackendType {
    pub fn build<T: FieldElement>(&self) -> &'static dyn BackendFactory<T> {
        #[cfg(feature = "halo2")]
        const HALO2_FACTORY: WithSetupFactory<halo2::Halo2Prover> = WithSetupFactory(PhantomData);
        #[cfg(feature = "halo2")]
        const HALO2_MOCK_FACTORY: WithoutSetupFactory<halo2_structs::Halo2Mock> =
            WithoutSetupFactory(PhantomData);
        const PILCOM_CLI_FACTORY: WithoutSetupFactory<pilcom_cli::PilcomCli> =
            WithoutSetupFactory(PhantomData);

        match self {
            #[cfg(feature = "halo2")]
            BackendType::Halo2 => &HALO2_FACTORY,
            #[cfg(feature = "halo2")]
            BackendType::Halo2Mock => &HALO2_MOCK_FACTORY,
            BackendType::PilcomCli => &PILCOM_CLI_FACTORY,
        }
    }
}

/// Factory for backends without setup.
struct WithoutSetupFactory<B>(PhantomData<B>);

/// Factory implementation for backends without setup.
impl<F: FieldElement, B: BackendWithoutSetup<F> + 'static> BackendFactory<F>
    for WithoutSetupFactory<B>
{
    fn new_from_params(&self, degree: DegreeType) -> Box<dyn Backend<F>> {
        Box::new(ConcreteBackendWithoutSetup(B::new(degree)))
    }

    fn load_setup(&self, _input: &mut dyn io::Read) -> Result<Box<dyn Backend<F>>, Error> {
        Err(Error::NotApplicable)
    }
}

/// Concrete dynamic dispatch Backend object, for backends without setup.
struct ConcreteBackendWithoutSetup<B>(B);

/// Concrete implementation for backends with setup.
impl<F: FieldElement, B: BackendWithoutSetup<F>> Backend<F> for ConcreteBackendWithoutSetup<B> {
    fn prove(
        &self,
        pil: &Analyzed<F>,
        fixed: &[(&str, Vec<F>)],
        witness: Option<&[(&str, Vec<F>)]>,
        prev_proof: Option<Proof>,
        output_dir: &Path,
    ) -> io::Result<()> {
        self.0.prove(pil, fixed, witness, prev_proof, output_dir)
    }

    fn write_setup(&self, _output: &mut dyn io::Write) -> Result<(), Error> {
        Err(Error::NotApplicable)
    }
}

/// Factory for backends with setup.
struct WithSetupFactory<B>(PhantomData<B>);

/// Factory implementation for backends with setup.
impl<F: FieldElement, B: BackendWithSetup<F> + 'static> BackendFactory<F> for WithSetupFactory<B> {
    fn new_from_params(&self, degree: DegreeType) -> Box<dyn Backend<F>> {
        Box::new(ConcreteBackendWithSetup(B::new_setup_from_params(degree)))
    }

    fn load_setup(&self, input: &mut dyn io::Read) -> Result<Box<dyn Backend<F>>, Error> {
        Ok(Box::new(ConcreteBackendWithSetup(B::load_setup(input)?)))
    }
}

/// Concrete dynamic dispatch Backend object, for backends with setup.
struct ConcreteBackendWithSetup<B>(B);

/// Concrete implementation for backends with setup.
impl<F: FieldElement, B: BackendWithSetup<F>> Backend<F> for ConcreteBackendWithSetup<B> {
    fn prove(
        &self,
        pil: &Analyzed<F>,
        fixed: &[(&str, Vec<F>)],
        witness: Option<&[(&str, Vec<F>)]>,
        prev_proof: Option<Proof>,
        output_dir: &Path,
    ) -> io::Result<()> {
        self.0.prove(pil, fixed, witness, prev_proof, output_dir)
    }

    fn write_setup(&self, output: &mut dyn io::Write) -> Result<(), Error> {
        Ok(self.0.write_setup(output)?)
    }
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("input/output error")]
    IO(#[from] std::io::Error),
    #[error("operation is not applicable to backend type")]
    NotApplicable,
}

pub type Proof = Vec<u8>;

/// Dynamic interface for a backend.
pub trait Backend<F: FieldElement> {
    /// Perform the backend proving. If prev_proof is provided, proof
    /// aggregation is performed.
    fn prove(
        &self,
        pil: &Analyzed<F>,
        fixed: &[(&str, Vec<F>)],
        witness: Option<&[(&str, Vec<F>)]>,
        prev_proof: Option<Proof>,
        output_dir: &Path,
    ) -> io::Result<()>;

    /// Write the prover setup to a file, so that it can be loaded later.
    fn write_setup(&self, output: &mut dyn io::Write) -> Result<(), Error>;
}

/// Dynamic interface for a backend factory.
pub trait BackendFactory<F: FieldElement> {
    /// Maybe perform the setup, and create a new backend object.
    fn new_from_params(&self, degree: DegreeType) -> Box<dyn Backend<F>>;

    /// Create a backend object from a prover setup loaded from a file.
    fn load_setup(&self, input: &mut dyn io::Read) -> Result<Box<dyn Backend<F>>, Error>;
}

/// Trait implemented by all backends.
trait BackendImpl<F: FieldElement> {
    fn prove(
        &self,
        pil: &Analyzed<F>,
        fixed: &[(&str, Vec<F>)],
        witness: Option<&[(&str, Vec<F>)]>,
        prev_proof: Option<Proof>,
        output_dir: &Path,
    ) -> io::Result<()>;
}

/// Trait implemented by backends that have a setup phase that must be saved to
/// a file.
trait BackendWithSetup<F: FieldElement>
where
    Self: Sized + BackendImpl<F>,
{
    /// Perform the setup and create a new backend object.
    fn new_setup_from_params(degree: DegreeType) -> Self;

    /// Create a backend object from a setup loaded from a file.
    fn load_setup(input: &mut dyn io::Read) -> Result<Self, io::Error>;

    /// Write the setup to a file.
    fn write_setup(&self, output: &mut dyn io::Write) -> Result<(), io::Error>;
}

/// Trait implemented by backends with no setup phase, that can be constructed
/// directly.
trait BackendWithoutSetup<F: FieldElement>
where
    Self: BackendImpl<F>,
{
    /// Perform the setup and create a new backend object.
    fn new(degree: DegreeType) -> Self;
}
