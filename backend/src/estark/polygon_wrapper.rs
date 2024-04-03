use std::{fs, path::Path};

use powdr_ast::analyzed::Analyzed;
use powdr_number::FieldElement;

use crate::{BackendFactory, Error};

use super::EStarkFileDumper;

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
        EStarkFileDumper::create(analyzed, fixed, output_dir, setup, verification_key, false)
    }
}

pub(crate) fn prove_and_verify(
    contraints_path: &Path,
    stark_struct_path: &Path,
    constants_path: &Path,
    commits_path: &Path,
    output_dir: &Path,
) -> Result<Vec<u8>, Error> {
    let proof = pil_stark_prover::generate_proof(
        contraints_path,
        stark_struct_path,
        constants_path,
        commits_path,
        output_dir,
    )
    .map_err(|e| Error::BackendError(e.to_string()))?;

    // Sanity check: verify the proof.
    // TODO: properly handle publics
    let publics_path = output_dir.join("publics.json");
    fs::write(&publics_path, "[]")?;
    pil_stark_prover::verify_proof(
        &proof.verification_key_json,
        &proof.starkinfo_json,
        &proof.proof_json,
        &publics_path,
    )
    .map_err(|e| Error::BackendError(e.to_string()))?;

    Ok(Vec::new())
}
