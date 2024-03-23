pub mod estark;
mod json_exporter;

use std::{
    fs::File,
    io::{BufWriter, Write},
    path::Path,
};

use crate::{Backend, BackendFactory, Error, Proof};
use powdr_ast::analyzed::Analyzed;
use powdr_number::{DegreeType, FieldElement};
use serde::Serialize;
use starky::types::{StarkStruct, Step};

pub struct PilStarkCliFactory;

fn create_stark_struct(degree: DegreeType) -> StarkStruct {
    assert!(degree > 1);
    let n_bits = (DegreeType::BITS - (degree - 1).leading_zeros()) as usize;
    let n_bits_ext = n_bits + 1;

    let steps = (2..=n_bits_ext)
        .rev()
        .step_by(4)
        .map(|b| Step { nBits: b })
        .collect();

    StarkStruct {
        nBits: n_bits,
        nBitsExt: n_bits_ext,
        nQueries: 2,
        verificationHashType: "GL".to_owned(),
        steps,
    }
}

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

fn write_json_file<T: ?Sized + Serialize>(path: &Path, data: &T) -> Result<(), Error> {
    let mut writer = BufWriter::new(File::create(path)?);
    serde_json::to_writer(&mut writer, data).map_err(|e| e.to_string())?;
    writer.flush()?;
    Ok(())
}

impl<'a, F: FieldElement> Backend<'a, F> for PilStarkCli<'a, F> {
    fn prove(
        &self,
        _witness: &[(String, Vec<F>)],
        prev_proof: Option<Proof>,
    ) -> Result<Proof, Error> {
        if prev_proof.is_some() {
            return Err(Error::NoAggregationAvailable);
        }

        // Write the constraints in the format expected by the prover-cpp
        if let Some(output_dir) = self.output_dir {
            // Write the stark struct JSON.
            let stark_struct_path = output_dir.join("starkstruct.json");
            log::info!("Writing {}.", stark_struct_path.to_string_lossy());
            write_json_file(
                &stark_struct_path,
                &create_stark_struct(self.analyzed.degree()),
            )?;

            // Write the constraints in the json format expected by the zkvm-prover:
            let contraints_path = output_dir.join("constraints.json");
            log::info!("Writing {}.", contraints_path.to_string_lossy());
            write_json_file(&contraints_path, &json_exporter::export(self.analyzed))?;
        } else {
            // If we were going to call the prover-cpp, we could write the
            // constraints.json to a temporary directory in case no output_dir
            // is provided.
        }

        // TODO: actually use prover-cpp to generate the proof
        Ok(Vec::new())
    }
}
