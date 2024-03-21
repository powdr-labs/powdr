pub mod estark;
mod json_exporter;

use std::{
    fs::File,
    io::{BufWriter, Write},
    path::Path,
};

use crate::{Backend, BackendFactory, Error, Proof};
use powdr_ast::analyzed::Analyzed;
use powdr_executor::witgen::WitgenCallback;
use powdr_number::FieldElement;

pub struct PilStarkCliFactory;

impl<F: FieldElement> BackendFactory<F> for PilStarkCliFactory {
    fn create<'a>(
        &self,
        analyzed: &'a Analyzed<F>,
        _fixed: &'a [(String, Vec<F>)],
        output_dir: Option<&'a Path>,
        setup: Option<&mut dyn std::io::Read>,
        verification_key: Option<&mut dyn std::io::Read>,
    ) -> Result<Box<dyn crate::Backend<'a, F> + 'a>, Error> {
        if setup.is_some() {
            return Err(Error::NoSetupAvailable);
        }
        if verification_key.is_some() {
            return Err(Error::NoVerificationAvailable);
        }
        Ok(Box::new(PilStarkCli {
            analyzed,
            output_dir,
        }))
    }
}

pub struct PilStarkCli<'a, F: FieldElement> {
    analyzed: &'a Analyzed<F>,
    output_dir: Option<&'a Path>,
}

impl<'a, F: FieldElement> Backend<'a, F> for PilStarkCli<'a, F> {
    fn prove(
        &self,
        _witness: &[(String, Vec<F>)],
        prev_proof: Option<Proof>,
        // TODO: Implement challenges
        _witgen_callback: WitgenCallback<F>,
    ) -> Result<Proof, Error> {
        if prev_proof.is_some() {
            return Err(Error::NoAggregationAvailable);
        }

        // Write the constraints in the format expected by the prover-cpp
        if let Some(output_dir) = self.output_dir {
            let path = output_dir.join("constraints.json");
            let mut writer = BufWriter::new(File::create(path)?);
            serde_json::to_writer(&mut writer, &json_exporter::export(self.analyzed))
                .map_err(|e| e.to_string())?;
            writer.flush()?;
        } else {
            // If we were going to call the prover-cpp, we could write the
            // constraints.json to a temporary directory in case no output_dir
            // is provided.
        }

        // TODO: actually use prover-cpp to generate the proof
        Ok(Vec::new())
    }
}
