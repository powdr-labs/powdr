use std::{fs, path::Path};

use powdr_ast::analyzed::Analyzed;
use powdr_executor::witgen::WitgenCallback;
use powdr_number::FieldElement;

use crate::{Backend, BackendFactory, Error, Proof};

use super::EStarkFilesCommon;

pub struct Factory;

impl<F: FieldElement> BackendFactory<F> for Factory {
    fn create<'a>(
        &self,
        analyzed: &'a Analyzed<F>,
        fixed: &'a [(String, Vec<F>)],
        output_dir: Option<&'a Path>,
        setup: Option<&mut dyn std::io::Read>,
        verification_key: Option<&mut dyn std::io::Read>,
    ) -> Result<Box<dyn crate::Backend<'a, F> + 'a>, Error> {
        Ok(Box::new(PolygonBackend(EStarkFilesCommon::create(
            analyzed,
            fixed,
            output_dir,
            setup,
            verification_key,
        )?)))
    }
}

struct PolygonBackend<'a, F: FieldElement>(EStarkFilesCommon<'a, F>);

// TODO: make both eStark backends interchangeable, from user perspective.
// TODO: implement the other Backend trait methods.
impl<'a, F: FieldElement> Backend<'a, F> for PolygonBackend<'a, F> {
    fn prove(
        &self,
        // Witness is taken from file written by the pipeline.
        _witness: &[(String, Vec<F>)],
        prev_proof: Option<Proof>,
        // TODO: Implement challenges
        _witgen_callback: WitgenCallback<F>,
    ) -> Result<Proof, Error> {
        if prev_proof.is_some() {
            return Err(Error::NoAggregationAvailable);
        }

        let tmp_dir;
        let output_dir = if let Some(output_dir) = self.0.output_dir {
            output_dir
        } else {
            tmp_dir = mktemp::Temp::new_dir()?;
            tmp_dir.as_path()
        };

        let input_paths = self.0.write_files(output_dir)?;

        let commits_path = output_dir.join("commits.bin");

        // Generate the proof.
        let proof_paths = pil_stark_prover::generate_proof(
            &input_paths.contraints,
            &input_paths.stark_struct,
            &input_paths.constants,
            &commits_path,
            output_dir,
        )
        .map_err(|e| Error::BackendError(e.to_string()))?;

        // Sanity check: verify the proof.
        let publics_path = output_dir.join("publics.json");
        // TODO: properly handle publics
        fs::write(&publics_path, "[]")?;
        pil_stark_prover::verify_proof(
            &proof_paths.verification_key_json,
            &proof_paths.starkinfo_json,
            &proof_paths.proof_json,
            &publics_path,
        )
        .map_err(|e| Error::BackendError(e.to_string()))?;

        // Read the proof.
        Ok(fs::read(&proof_paths.proof_json)?)
    }
}
