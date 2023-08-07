use std::{
    fs,
    io::{self, Write},
    path::Path,
};

use crate::{BackendImpl, BackendImplWithSetup, Proof};
use ast::analyzed::Analyzed;
use halo2::Halo2Prover;
use number::{DegreeType, FieldElement};

impl<T: FieldElement> BackendImpl<T> for Halo2Prover {
    fn new(degree: DegreeType) -> Self {
        Halo2Prover::assert_field_is_compatible::<T>();
        Halo2Prover::new(degree)
    }

    fn prove(
        &self,
        pil: &Analyzed<T>,
        fixed: &[(&str, Vec<T>)],
        witness: &[(&str, Vec<T>)],
        prev_proof: Option<Proof>,
        output_dir: Option<&Path>,
    ) -> io::Result<Option<Proof>> {
        let (proof, fname) = match prev_proof {
            Some(proof) => (
                self.prove_aggr(pil, fixed, witness, proof),
                "proof_aggr.bin",
            ),
            None => (self.prove_ast(pil, fixed, witness), "proof.bin"),
        };

        if let Some(output_dir) = output_dir {
            // No need to bufferize the writing, because we write the whole
            // proof in one call.
            let mut proof_file = fs::File::create(output_dir.join(fname))?;
            proof_file.write_all(&proof)?;
            log::info!("Wrote {fname}.");
        }

        Ok(Some(proof))
    }
}

impl<T: FieldElement> BackendImplWithSetup<T> for halo2::Halo2Prover {
    fn new_from_setup(mut input: &mut dyn io::Read) -> Result<Self, io::Error> {
        Halo2Prover::assert_field_is_compatible::<T>();
        Halo2Prover::read(&mut input)
    }

    fn write_setup(&self, mut output: &mut dyn io::Write) -> Result<(), io::Error> {
        self.write(&mut output)
    }
}

pub struct Halo2Mock;
impl<T: FieldElement> BackendImpl<T> for Halo2Mock {
    fn new(_degree: DegreeType) -> Self {
        Self
    }

    fn prove(
        &self,
        pil: &Analyzed<T>,
        fixed: &[(&str, Vec<T>)],
        witness: &[(&str, Vec<T>)],
        prev_proof: Option<Proof>,
        _output_dir: Option<&Path>,
    ) -> io::Result<Option<Proof>> {
        if prev_proof.is_some() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "Halo2Mock backend does not support aggregation",
            ));
        }

        halo2::mock_prove(pil, fixed, witness);

        Ok(None)
    }
}
