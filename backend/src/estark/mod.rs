mod json_exporter;
#[cfg(feature = "estark-polygon")]
pub mod polygon_wrapper;
pub mod starky_wrapper;

use std::{
    fs::File,
    io::{self, BufWriter, Write},
    iter::{once, repeat},
    path::Path,
};

use crate::{Backend, BackendFactory, Error, Proof};
use powdr_ast::analyzed::Analyzed;

use powdr_executor::witgen::WitgenCallback;
use powdr_number::{write_polys_file, DegreeType, FieldElement};
use serde::Serialize;
use starky::types::{StarkStruct, Step, PIL};

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

fn pil_hack_fix<'a, F: FieldElement>(
    pil: &'a Analyzed<F>,
    fixed: &'a [(String, Vec<F>)],
) -> (PIL, Vec<(String, Vec<F>)>) {
    let degree = pil.degree();

    let mut pil: PIL = json_exporter::export(pil);

    // TODO eStark provers requires a fixed column with the equivalent semantics
    // to Polygon zkEVM's `L1` column. Powdr generated PIL will always have
    // `main.first_step`, but directly given PIL may not have it. This is a hack
    // to inject such column if it doesn't exist. It should be eventually
    // improved.
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
        EStarkFileDumper::create(analyzed, fixed, output_dir, setup, verification_key, true)
    }
}

pub struct EStarkFileDumper<'a, F: FieldElement> {
    degree: DegreeType,
    pil: PIL,
    fixed: Vec<(String, Vec<F>)>,
    output_dir: Option<&'a Path>,
    just_dump: bool,
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

impl<'a, F: FieldElement> EStarkFileDumper<'a, F> {
    fn create(
        analyzed: &'a Analyzed<F>,
        fixed: &'a [(String, Vec<F>)],
        output_dir: Option<&'a Path>,
        setup: Option<&mut dyn std::io::Read>,
        verification_key: Option<&mut dyn std::io::Read>,
        just_dump: bool,
    ) -> Result<Box<dyn crate::Backend<'a, F> + 'a>, Error> {
        if setup.is_some() {
            return Err(Error::NoSetupAvailable);
        }
        if verification_key.is_some() {
            return Err(Error::NoVerificationAvailable);
        }

        // Pre-process the PIL and fixed columns.
        let (pil, fixed) = pil_hack_fix(analyzed, fixed);

        Ok(Box::new(EStarkFileDumper {
            degree: analyzed.degree(),
            pil,
            fixed,
            output_dir,
            just_dump,
        }))
    }
}

impl<'a, F: FieldElement> Backend<'a, F> for EStarkFileDumper<'a, F> {
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

        // Write the files in the format expected by the zkvm-prover.
        if let Some(output_dir) = self.output_dir {
            // Write the constants.
            let constants_path = output_dir.join("constants.bin");
            log::info!("Writing {}.", constants_path.to_string_lossy());
            write_polys_bin(&constants_path, &self.fixed)?;

            // Write the commits.
            let commits_path = output_dir.join("commits.bin");
            log::info!("Writing {}.", commits_path.to_string_lossy());
            write_polys_bin(&commits_path, witness)?;

            // Write the stark struct JSON.
            let stark_struct_path = output_dir.join("starkstruct.json");
            log::info!("Writing {}.", stark_struct_path.to_string_lossy());
            write_json_file(&stark_struct_path, &create_stark_struct(self.degree))?;

            // Write the constraints in JSON.
            let contraints_path = output_dir.join("constraints.json");
            log::info!("Writing {}.", contraints_path.to_string_lossy());
            write_json_file(&contraints_path, &self.pil)?;

            if self.just_dump {
                Ok(Vec::new())
            } else {
                // Generate the proof.
                #[cfg(feature = "estark-polygon")]
                return polygon_wrapper::prove_and_verify(
                    &contraints_path,
                    &stark_struct_path,
                    &constants_path,
                    &commits_path,
                    output_dir,
                );

                #[cfg(not(feature = "estark-polygon"))]
                Ok(Vec::new())
            }
        } else {
            // TODO: use a temporary directory to generate the proof.
            //
            // TODO: there is no point in using a temporary dir unless we find a
            // way to return a meaningful proof to the caller. Preferably a
            // unified one, compatible with starky.
            //
            // TODO: make both eStark backends interchangeable, from results perspective.
            Ok(Vec::new())
        }
    }
}
