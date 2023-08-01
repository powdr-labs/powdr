#[cfg(feature = "halo2")]
mod halo2_structs;
mod pilcom_cli;

use ast::analyzed::Analyzed;
use number::{DegreeType, FieldElement};
use std::{io, path::Path};
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
    pub fn build<T: FieldElement>(&self) -> &'static dyn BackendBuilder<T> {
        #[cfg(feature = "halo2")]
        const HALO2_BUILDER: halo2_structs::Halo2Builder = halo2_structs::Halo2Builder;
        #[cfg(feature = "halo2")]
        const HALO2_MOCK_BUILDER: halo2_structs::Halo2MockBuilder = halo2_structs::Halo2MockBuilder;
        const PILCOM_CLI_BUILDER: pilcom_cli::PilcomCliBuilder = pilcom_cli::PilcomCliBuilder;

        match self {
            #[cfg(feature = "halo2")]
            BackendType::Halo2 => &HALO2_BUILDER,
            #[cfg(feature = "halo2")]
            BackendType::Halo2Mock => &HALO2_MOCK_BUILDER,
            BackendType::PilcomCli => &PILCOM_CLI_BUILDER,
        }
    }
}

pub type Proof = Vec<u8>;

pub trait BackendBuilder<T: FieldElement> {
    fn setup_from_params(&self, size: DegreeType) -> Box<dyn Backend<T>>;
    fn read(&self, input: &mut dyn io::Read) -> Result<Box<dyn Backend<T>>, io::Error>;
}

pub trait Backend<T: FieldElement> {
    // Why this returns Option???
    fn prove(
        &self,
        pil: &Analyzed<T>,
        fixed: &[(&str, Vec<T>)],
        witness: Option<&[(&str, Vec<T>)]>,
        prev_proof: Option<Proof>,
        output_dir: &Path,
    ) -> io::Result<()>;

    fn write(&self, output: &mut dyn io::Write) -> Result<(), io::Error>;
}
