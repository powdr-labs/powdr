mod bin_exporter;
mod json_exporter;
#[cfg(feature = "estark-polygon")]
pub mod polygon_wrapper;
pub mod starky_wrapper;

use std::{
    collections::BTreeMap,
    fs::File,
    io::{self, BufWriter, Write},
    iter::{once, repeat},
    path::{Path, PathBuf},
    sync::Arc,
};

use crate::{Backend, BackendFactory, BackendOptions, Error, Proof};
use powdr_ast::analyzed::Analyzed;

use powdr_executor::{
    constant_evaluator::{get_uniquely_sized_cloned, VariablySizedColumn},
    witgen::WitgenCallback,
};
use powdr_number::{DegreeType, FieldElement};
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

type Fixed<F> = Vec<(String, Vec<F>)>;

/// eStark provers require a fixed column with the equivalent semantics to
/// Polygon zkEVM's `L1` column. Powdr generated PIL will always have
/// `main::first_step`, but directly given PIL may not have it. This is a fixup
/// to inject such column if it doesn't exist.
///
/// TODO Improve how this is done.
fn first_step_fixup<F: FieldElement>(
    pil: &Analyzed<F>,
    fixed: Arc<Fixed<F>>,
) -> (PIL, Arc<Fixed<F>>) {
    let degree = pil.degree();

    let mut pil: PIL = json_exporter::export(pil);

    let patched_constants = if !fixed.iter().any(|(k, _)| k == "main::first_step") {
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
            .collect::<Vec<_>>()
            .into()
    } else {
        fixed
    };

    (pil, patched_constants)
}

struct EStarkFilesCommon<F: FieldElement> {
    degree: DegreeType,
    pil: PIL,
    /// If this field is present, it means the constants were patched with
    /// "main::first_step" column and must be written again to a file.
    constants: Arc<Fixed<F>>,
    output_dir: Option<PathBuf>,
    proof_type: ProofType,
}

// TODO move to `backend` and sync with the similar function in halo2
fn write_json_file<T: ?Sized + Serialize>(path: &Path, data: &T) -> Result<(), Error> {
    buffered_write_file(path, |writer| {
        serde_json::to_writer(writer, data).map_err(|e| e.to_string())
    })??;

    Ok(())
}

impl<F: FieldElement> EStarkFilesCommon<F> {
    #[allow(clippy::too_many_arguments)]
    fn create(
        analyzed: &Analyzed<F>,
        fixed: Arc<Fixed<F>>,
        output_dir: Option<PathBuf>,
        setup: Option<&mut dyn std::io::Read>,
        proving_key: Option<&mut dyn std::io::Read>,
        verification_key: Option<&mut dyn std::io::Read>,
        verification_app_key: Option<&mut dyn std::io::Read>,
        options: BackendOptions,
    ) -> Result<Self, Error> {
        if setup.is_some() {
            return Err(Error::NoSetupAvailable);
        }
        if proving_key.is_some() {
            return Err(Error::NoProvingKeyAvailable);
        }
        if verification_key.is_some() {
            return Err(Error::NoVerificationAvailable);
        }
        if verification_app_key.is_some() {
            return Err(Error::NoAggregationAvailable);
        }
        if analyzed.degrees().len() > 1 {
            return Err(Error::NoVariableDegreeAvailable);
        }

        // Pre-process the PIL and fixed columns.
        let (pil, fixed) = first_step_fixup(analyzed, fixed);

        let proof_type: ProofType = ProofType::from(options);

        Ok(EStarkFilesCommon {
            degree: analyzed.degree(),
            pil,
            constants: fixed,
            output_dir,
            proof_type,
        })
    }
}

struct ProverInputFilePaths {
    commits: PathBuf,
    constants: PathBuf,
    stark_struct: PathBuf,
    contraints: PathBuf,
}

impl<F: FieldElement> EStarkFilesCommon<F> {
    /// Write the files in the EStark Polygon format.
    fn write_files(
        &self,
        witness: &[(String, Vec<F>)],
        output_dir: &Path,
    ) -> Result<ProverInputFilePaths, Error> {
        let paths = ProverInputFilePaths {
            commits: output_dir.join("commits_estark.bin"),
            constants: output_dir.join("constants_estark.bin"),
            stark_struct: output_dir.join("starkstruct.json"),
            contraints: output_dir.join("constraints.json"),
        };

        log::info!("Writing {}.", paths.constants.to_string_lossy());
        bin_exporter::write_polys_file(&paths.constants, &self.constants)?;

        log::info!("Writing {}.", paths.commits.to_string_lossy());
        bin_exporter::write_polys_file(&paths.commits, witness)?;

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
        Ok(Box::new(DumpBackend(EStarkFilesCommon::create(
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

/// A backend that just dumps the files to the output directory.
struct DumpBackend<F: FieldElement>(EStarkFilesCommon<F>);

impl<F: FieldElement> Backend<F> for DumpBackend<F> {
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

        let output_dir = self
            .0
            .output_dir
            .as_ref()
            .ok_or(Error::BackendError("output_dir is None".to_owned()))?;

        self.0.write_files(witness, output_dir)?;

        Ok(Vec::new())
    }
}

fn buffered_write_file<R>(
    path: &Path,
    do_write: impl FnOnce(&mut BufWriter<File>) -> R,
) -> Result<R, io::Error> {
    let mut writer = BufWriter::new(File::create(path)?);
    let result = do_write(&mut writer);
    writer.flush()?;

    Ok(result)
}
