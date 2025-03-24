use std::{collections::BTreeMap, fs, path::PathBuf, sync::Arc};

use powdr_ast::analyzed::Analyzed;
use powdr_executor::{
    constant_evaluator::{get_uniquely_sized_cloned, VariablySizedColumn},
    witgen::WitgenCallback,
};
use powdr_number::FieldElement;

use crate::{Backend, BackendFactory, BackendOptions, Error, Proof};

use super::EStarkFilesCommon;

pub struct Factory;

impl<F: FieldElement> BackendFactory<F> for Factory {
    fn create(
        &self,
        analyzed: Arc<Analyzed<F>>,
        fixed: Arc<Vec<(String, VariablySizedColumn<F>)>>,
        output_dir: Option<PathBuf>,
        setup: Option<&mut dyn std::io::Read>,
        proving_key: Option<&mut dyn std::io::Read>,
        verification_key: Option<&mut dyn std::io::Read>,
        verification_app_key: Option<&mut dyn std::io::Read>,
        options: BackendOptions,
    ) -> Result<Box<dyn crate::Backend<F>>, Error> {
        let fixed = Arc::new(
            get_uniquely_sized_cloned(&fixed).map_err(|_| Error::NoVariableDegreeAvailable)?,
        );
        Ok(Box::new(PolygonBackend(EStarkFilesCommon::create(
            &analyzed,
            fixed,
            output_dir,
            setup,
            proving_key,
            verification_key,
            verification_app_key,
            options,
        )?)))
    }
}

struct PolygonBackend<F: FieldElement>(EStarkFilesCommon<F>);

// TODO: make both eStark backends interchangeable, from user perspective.
// TODO: implement the other Backend trait methods.
impl<F: FieldElement> Backend<F> for PolygonBackend<F> {
    fn prove(
        &self,
        witness: &[(String, Vec<F>)],
        _publics: &BTreeMap<String, Option<F>>,
        prev_proof: Option<Proof>,
        // TODO: Implement challenges
        _witgen_callback: WitgenCallback<F>,
    ) -> Result<Proof, Error> {
        if prev_proof.is_some() {
            return Err(Error::NoAggregationAvailable);
        }

        let tmp_dir;
        let output_dir = if let Some(output_dir) = self.0.output_dir.clone() {
            output_dir
        } else {
            tmp_dir = mktemp::Temp::new_dir()?;
            tmp_dir.to_path_buf()
        };

        let input_paths = self.0.write_files(witness, &output_dir)?;

        // Generate the proof.
        let proof_paths = pil_stark_prover::generate_proof(
            &input_paths.contraints,
            &input_paths.stark_struct,
            &input_paths.constants,
            &input_paths.commits,
            &output_dir,
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
