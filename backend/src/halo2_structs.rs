use std::{
    fs,
    io::{self, Write},
    path::Path,
};

use crate::{Backend, BackendBuilder, Proof};
use ast::analyzed::Analyzed;
use number::{DegreeType, FieldElement};

pub struct Halo2Builder;
impl<T: FieldElement> BackendBuilder<T> for Halo2Builder {
    fn setup_from_params(&self, size: DegreeType) -> Box<dyn Backend<T>> {
        Box::new(halo2::Halo2::new(size))
    }

    fn read(&self, mut input: &mut dyn io::Read) -> Result<Box<dyn Backend<T>>, io::Error> {
        Ok(Box::new(halo2::Halo2::read(&mut input)?))
    }
}
#[cfg(feature = "halo2")]
impl<T: FieldElement> Backend<T> for halo2::Halo2<T> {
    fn prove(
        &self,
        pil: &Analyzed<T>,
        fixed: &[(&str, Vec<T>)],
        witness: Option<&[(&str, Vec<T>)]>,
        prev_proof: Option<Proof>,
        output_dir: &Path,
    ) -> io::Result<()> {
        let Some(witness) = witness else {
            return Err(io::Error::new(io::ErrorKind::InvalidInput, "witness is missing"));
        };

        let (proof, fname) = match prev_proof {
            Some(proof) => (self.prove_aggr(pil, fixed, witness, proof), "proof.bin"),
            None => (self.prove_ast(pil, fixed, witness), "proof_aggr.bin"),
        };

        {
            // No need to bufferize the writing, because we write the whole
            // proof in one call.
            let mut proof_file = fs::File::create(output_dir.join(fname))?;
            proof_file.write_all(&proof)?;
        }
        log::info!("Wrote {fname}.");

        Ok(())
    }

    fn write(&self, mut output: &mut dyn io::Write) -> Result<(), io::Error> {
        self.write(&mut output)
    }
}

pub struct Halo2MockBuilder;
impl<T: FieldElement> BackendBuilder<T> for Halo2MockBuilder {
    fn setup_from_params(&self, _size: DegreeType) -> Box<dyn Backend<T>> {
        Box::new(Halo2Mock)
    }

    fn read(&self, _input: &mut dyn io::Read) -> Result<Box<dyn Backend<T>>, io::Error> {
        panic!("Halo2Mock backend does not support serialization");
    }
}

pub struct Halo2Mock;
impl<T: FieldElement> Backend<T> for Halo2Mock {
    fn prove(
        &self,
        pil: &Analyzed<T>,
        fixed: &[(&str, Vec<T>)],
        witness: Option<&[(&str, Vec<T>)]>,
        prev_proof: Option<Proof>,
        _output_dir: &Path,
    ) -> io::Result<()> {
        if prev_proof.is_some() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "Halo2Mock backend does not support aggregation",
            ));
        }

        let Some(witness) = witness else {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "witness is missing",
            ));
        };

        halo2::mock_prove(pil, fixed, witness);

        Ok(())
    }

    fn write(&self, _output: &mut dyn io::Write) -> Result<(), io::Error> {
        panic!("Halo2Mock backend does not support serialization");
    }
}
