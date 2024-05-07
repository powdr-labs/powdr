mod json_exporter;
#[cfg(feature = "estark-polygon")]
pub mod polygon_wrapper;
pub mod starky_wrapper;

use std::{
    fs::hard_link,
    iter::{once, repeat},
    path::{Path, PathBuf},
};

use crate::{Backend, BackendFactory, BackendOptions, Error, Proof};
use powdr_ast::analyzed::Analyzed;

use powdr_executor::witgen::WitgenCallback;
use powdr_number::{buffered_write_file, write_polys_file, DegreeType, FieldElement};
use serde::Serialize;
use starky::types::{StarkStruct, Step, PIL};

enum ProofType {
    StarkGL,
    StarkBN,
    SnarkBN,
}

impl From<BackendOptions> for ProofType {
    fn from(options: BackendOptions) -> Self {
        match options.as_str() {
            "" | "stark_gl" => ProofType::StarkGL,
            "stark_bn" => ProofType::StarkBN,
            "snark_bn" => ProofType::SnarkBN,
            _ => panic!("Unsupported proof type: {options}"),
        }
    }
}

impl ProofType {
    pub fn hash_type(&self) -> &'static str {
        match self {
            ProofType::StarkGL => "GL",
            ProofType::StarkBN => "BN",
            ProofType::SnarkBN => "BN",
        }
    }
}

fn create_stark_struct(degree: DegreeType, hash_type: &str) -> StarkStruct {
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
        verificationHashType: hash_type.to_string(),
        steps,
    }
}

type PatchedConstants<F> = Vec<(String, Vec<F>)>;

/// eStark provers require a fixed column with the equivalent semantics to
/// Polygon zkEVM's `L1` column. Powdr generated PIL will always have
/// `main.first_step`, but directly given PIL may not have it. This is a fixup
/// to inject such column if it doesn't exist.
///
/// TODO Improve how this is done.
fn first_step_fixup<'a, F: FieldElement>(
    pil: &'a Analyzed<F>,
    fixed: &'a [(String, Vec<F>)],
) -> (PIL, Option<PatchedConstants<F>>) {
    let degree = pil.degree();

    let mut pil: PIL = json_exporter::export(pil);

    let patched_constants = if !fixed.iter().any(|(k, _)| k == "main.first_step") {
        use starky::types::Reference;
        pil.nConstants += 1;
        pil.references.insert(
            "main.first_step".to_string(),
            Reference {
                polType: None,
                type_: "constP".to_string(),
                id: fixed.len(),
                polDeg: degree as usize,
                isArray: false,
                elementType: None,
                len: None,
            },
        );

        Some(
            fixed
                .iter()
                .cloned()
                .chain(once((
                    "main.first_step".to_string(),
                    once(F::one())
                        .chain(repeat(F::zero()))
                        .take(degree as usize)
                        .collect(),
                )))
                .collect(),
        )
    } else {
        None
    };

    (pil, patched_constants)
}

struct EStarkFilesCommon<'a, F: FieldElement> {
    degree: DegreeType,
    pil: PIL,
    /// If this field is present, it means the constants were patched with
    /// "main.first_step" column and must be written again to a file.
    patched_constants: Option<Vec<(String, Vec<F>)>>,
    output_dir: Option<&'a Path>,
    proof_type: ProofType,
}

// TODO move to `backend` and sync with the similar function in halo2
fn write_json_file<T: ?Sized + Serialize>(path: &Path, data: &T) -> Result<(), Error> {
    buffered_write_file(path, |writer| {
        serde_json::to_writer(writer, data).map_err(|e| e.to_string())
    })??;

    Ok(())
}

impl<'a, F: FieldElement> EStarkFilesCommon<'a, F> {
    fn create(
        analyzed: &'a Analyzed<F>,
        fixed: &'a [(String, Vec<F>)],
        output_dir: Option<&'a Path>,
        setup: Option<&mut dyn std::io::Read>,
        verification_key: Option<&mut dyn std::io::Read>,
        verification_app_key: Option<&mut dyn std::io::Read>,
        options: BackendOptions,
    ) -> Result<Self, Error> {
        if setup.is_some() {
            return Err(Error::NoSetupAvailable);
        }
        if verification_key.is_some() {
            return Err(Error::NoVerificationAvailable);
        }
        if verification_app_key.is_some() {
            return Err(Error::NoAggregationAvailable);
        }

        // Pre-process the PIL and fixed columns.
        let (pil, patched_constants) = first_step_fixup(analyzed, fixed);

        let proof_type: ProofType = ProofType::from(options);

        Ok(EStarkFilesCommon {
            degree: analyzed.degree(),
            pil,
            patched_constants,
            output_dir,
            proof_type,
        })
    }
}

struct ProverInputFilePaths {
    constants: PathBuf,
    stark_struct: PathBuf,
    contraints: PathBuf,
}

impl<'a, F: FieldElement> EStarkFilesCommon<'a, F> {
    /// Write the files in the EStark Polygon format.
    fn write_files(&self, output_dir: &Path) -> Result<ProverInputFilePaths, Error> {
        let paths = ProverInputFilePaths {
            constants: output_dir.join("constants_estark.bin"),
            stark_struct: output_dir.join("starkstruct.json"),
            contraints: output_dir.join("constraints.json"),
        };

        // If they were patched, write them. Otherwise, just hardlink.
        if let Some(patched_constants) = &self.patched_constants {
            log::info!("Writing {}.", paths.constants.to_string_lossy());
            write_polys_file(&paths.constants, patched_constants)?;
        } else {
            log::info!("Hardlinking constants.bin to constants_estark.bin.");
            hard_link(output_dir.join("constants.bin"), &paths.constants)?;
        }

        // Write the stark struct JSON.
        write_json_file(
            &paths.stark_struct,
            &create_stark_struct(self.degree, self.proof_type.hash_type()),
        )?;

        // Write the constraints in JSON.
        log::info!("Writing {}.", paths.contraints.to_string_lossy());
        write_json_file(&paths.contraints, &self.pil)?;

        Ok(paths)
    }
}

pub struct DumpFactory;

impl<F: FieldElement> BackendFactory<F> for DumpFactory {
    fn create<'a>(
        &self,
        analyzed: &'a Analyzed<F>,
        fixed: &'a [(String, Vec<F>)],
        output_dir: Option<&'a Path>,
        setup: Option<&mut dyn std::io::Read>,
        verification_key: Option<&mut dyn std::io::Read>,
        verification_app_key: Option<&mut dyn std::io::Read>,
        options: BackendOptions,
    ) -> Result<Box<dyn crate::Backend<'a, F> + 'a>, Error> {
        Ok(Box::new(DumpBackend(EStarkFilesCommon::create(
            analyzed,
            fixed,
            output_dir,
            setup,
            verification_key,
            verification_app_key,
            options,
        )?)))
    }
}

/// A backend that just dumps the files to the output directory.
struct DumpBackend<'a, F: FieldElement>(EStarkFilesCommon<'a, F>);

impl<'a, F: FieldElement> Backend<'a, F> for DumpBackend<'a, F> {
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

        let output_dir = self
            .0
            .output_dir
            .ok_or(Error::BackendError("output_dir is None".to_owned()))?;

        self.0.write_files(output_dir)?;

        Ok(Vec::new())
    }
}
