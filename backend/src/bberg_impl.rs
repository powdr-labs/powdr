use std::io::{self};

use crate::{BackendImpl, BackendImplWithSetup, Proof};
use ast::analyzed::Analyzed;

// We do not directly have a bberg prover at the moment
// however we can just perform codegen
use bberg::bberg_codegen::BBergCodegen;
use number::{DegreeType, FieldElement};

impl<T: FieldElement> BackendImpl<T> for BBergCodegen {
    fn new(degree: DegreeType) -> Self {
        BBergCodegen::assert_field_is_compatible::<T>();
        BBergCodegen::new(degree)
    }

    fn prove(
        &self,
        pil: &Analyzed<T>,
        fixed: &[(String, Vec<T>)],
        witness: &[(String, Vec<T>)],
        _prev_proof: Option<Proof>,
        bname: Option<String>,
    ) -> (Option<Proof>, Option<String>) {
        self.build_ast(pil, fixed, witness, bname);

        // Note(md): In the current bberg impl we do not produce proofs here
        // as we do cpp code generation, and then create proofs with bberg
        // This may change in the future when the library matures to be more pluggable
        (None, None)
    }
}

impl<T: FieldElement> BackendImplWithSetup<T> for BBergCodegen {
    fn new_from_setup(mut input: &mut dyn io::Read) -> Result<Self, io::Error> {
        BBergCodegen::assert_field_is_compatible::<T>();
        BBergCodegen::new_from_setup(&mut input)
    }

    // TODO: implement this
    fn write_setup(&self, _output: &mut dyn io::Write) -> Result<(), io::Error> {
        Ok(())
        // self.write_setup(&mut output)
    }
}

pub struct BBergMock;
impl<T: FieldElement> BackendImpl<T> for BBergMock {
    fn new(_degree: DegreeType) -> Self {
        Self
    }

    fn prove(
        &self,
        _pil: &Analyzed<T>,
        _fixed: &[(String, Vec<T>)],
        _witness: &[(String, Vec<T>)],
        prev_proof: Option<Proof>,
        _bname: Option<String>,
    ) -> (Option<Proof>, Option<String>) {
        if prev_proof.is_some() {
            unimplemented!("BBergMock backend does not support aggregation");
        }

        // TODO: mock prover
        unimplemented!("BBergMock backend is not implemented");
    }
}
