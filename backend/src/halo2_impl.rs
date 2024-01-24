use std::io::{self};

use crate::{BackendImpl, BackendImplWithSetup, Proof};
use powdr_ast::analyzed::Analyzed;
use powdr_halo2::Halo2Prover;
use powdr_number::{DegreeType, FieldElement};

impl<T: FieldElement> BackendImpl<T> for Halo2Prover {
    fn new(degree: DegreeType) -> Self {
        Halo2Prover::assert_field_is_compatible::<T>();
        Halo2Prover::new(degree)
    }

    fn prove(
        &self,
        pil: &Analyzed<T>,
        fixed: &[(String, Vec<T>)],
        witness: &[(String, Vec<T>)],
        prev_proof: Option<Vec<Proof>>,
    ) -> (Option<Vec<Proof>>, Option<String>) {
        let proof = match prev_proof {
            Some(proof) => self.prove_aggr(pil, fixed, witness, proof[0]),
            None => self.prove_ast(pil, fixed, witness),
        };

        (Some(vec![proof]), None)
    }
}

impl<T: FieldElement> BackendImplWithSetup<T> for powdr_halo2::Halo2Prover {
    fn new_from_setup(mut input: &mut dyn io::Read) -> Result<Self, io::Error> {
        Halo2Prover::assert_field_is_compatible::<T>();
        Halo2Prover::new_from_setup(&mut input)
    }

    fn write_setup(&self, mut output: &mut dyn io::Write) -> Result<(), io::Error> {
        self.write_setup(&mut output)
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
        fixed: &[(String, Vec<T>)],
        witness: &[(String, Vec<T>)],
        prev_proof: Option<Vec<Proof>>,
    ) -> (Option<Vec<Proof>>, Option<String>) {
        if prev_proof.is_some() {
            unimplemented!("Halo2Mock backend does not support aggregation");
        }

        powdr_halo2::mock_prove(pil, fixed, witness);

        (None, None)
    }
}
