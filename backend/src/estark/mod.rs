mod json_exporter;
#[cfg(feature = "estark-polygon")]
pub mod polygon_wrapper;
pub mod starky_wrapper;

use std::{
    fs::File,
    io::{self, BufWriter, Write},
    iter::{once, repeat},
    path::{Path, PathBuf},
};

use crate::{Backend, BackendFactory, BackendOptions, Error, Proof};
use powdr_ast::analyzed::Analyzed;

use powdr_executor::witgen::WitgenCallback;
use powdr_number::{write_polys_file, DegreeType, FieldElement};
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

/// eStark provers require a fixed column with the equivalent semantics to
/// Polygon zkEVM's `L1` column. Powdr generated PIL will always have
/// `main.first_step`, but directly given PIL may not have it. This is a fixup
/// to inject such column if it doesn't exist.
///
/// TODO Improve how this is done.
fn first_step_fixup<'a, F: FieldElement>(
    pil: &'a Analyzed<F>,
    fixed: &'a [(String, Vec<F>)],
) -> (PIL, Vec<(String, Vec<F>)>) {
    let degree = pil.degree();

    let mut pil: PIL = json_exporter::export(pil);

    let mut fixed = fixed.to_vec();
    if !fixed.iter().any(|(k, _)| k == "main.first_step") {
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
        fixed.push((
            "main.first_step".to_string(),
            once(F::one())
                .chain(repeat(F::zero()))
                .take(degree as usize)
                .collect(),
        ));
    }

    (pil, fixed)
}

struct EStarkFilesCommon<'a, F: FieldElement> {
    degree: DegreeType,
    pil: PIL,
    fixed: Vec<(String, Vec<F>)>,
    output_dir: Option<&'a Path>,
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

fn write_json_file<T: ?Sized + Serialize>(path: &Path, data: &T) -> Result<(), Error> {
    buffered_write_file(path, |writer| {
        serde_json::to_writer(writer, data).map_err(|e| e.to_string())
    })??;

    Ok(())
}

fn write_polys_bin<F: FieldElement>(
    path: &Path,
    constants: &[(String, Vec<F>)],
) -> Result<(), Error> {
    buffered_write_file(path, |writer| write_polys_file(writer, constants))??;

    Ok(())
}

impl<'a, F: FieldElement> EStarkFilesCommon<'a, F> {
    fn create(
        analyzed: &'a Analyzed<F>,
        fixed: &'a [(String, Vec<F>)],
        output_dir: Option<&'a Path>,
        setup: Option<&mut dyn std::io::Read>,
        verification_key: Option<&mut dyn std::io::Read>,
    ) -> Result<Self, Error> {
        if setup.is_some() {
            return Err(Error::NoSetupAvailable);
        }
        if verification_key.is_some() {
            return Err(Error::NoVerificationAvailable);
        }

        // Pre-process the PIL and fixed columns.
        let (pil, fixed) = first_step_fixup(analyzed, fixed);

        Ok(EStarkFilesCommon {
            degree: analyzed.degree(),
            pil,
            fixed,
            output_dir,
        })
    }
}

struct ProverInputFilePaths {
    constants: PathBuf,
    commits: PathBuf,
    stark_struct: PathBuf,
    contraints: PathBuf,
}

impl<'a, F: FieldElement> EStarkFilesCommon<'a, F> {
    /// Write the files in the EStark Polygon format.
    fn write_files(
        &self,
        output_dir: &Path,
        witness: &[(String, Vec<F>)],
    ) -> Result<ProverInputFilePaths, Error> {
        let paths = ProverInputFilePaths {
            constants: output_dir.join("constants.bin"),
            commits: output_dir.join("commits.bin"),
            stark_struct: output_dir.join("starkstruct.json"),
            contraints: output_dir.join("constraints.json"),
        };

        // Write the constants.
        log::info!("Writing {}.", paths.constants.to_string_lossy());
        write_polys_bin(&paths.constants, &self.fixed)?;

        // Write the commits.
        log::info!("Writing {}.", paths.commits.to_string_lossy());
        write_polys_bin(&paths.commits, witness)?;

        // Write the stark struct JSON.
        log::info!("Writing {}.", paths.stark_struct.to_string_lossy());
        write_json_file(&paths.stark_struct, &create_stark_struct(self.degree))?;

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
    ) -> Result<Box<dyn crate::Backend<'a, F> + 'a>, Error> {
        Ok(Box::new(DumpBackend(EStarkFilesCommon::create(
            analyzed,
            fixed,
            output_dir,
            setup,
            verification_key,
        )?)))
    }
}

/// A backend that just dumps the files to the output directory.
struct DumpBackend<'a, F: FieldElement>(EStarkFilesCommon<'a, F>);

impl<'a, F: FieldElement> Backend<'a, F> for DumpBackend<'a, F> {
    fn prove(
        &self,
        witness: &[(String, Vec<F>)],
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

        self.0.write_files(output_dir, witness)?;

        Ok(Vec::new())
    }
}
