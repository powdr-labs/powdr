use ast::analyzed::Analyzed;
use number::{DegreeType, FieldElement};

use crate::{BackendImpl, Proof};

pub struct PilStarkCli;

impl<T: FieldElement> BackendImpl<T> for PilStarkCli {
    fn new(_degree: DegreeType) -> Self {
        Self
    }

    fn prove(
        &self,
        pil: &Analyzed<T>,
        _fixed: &[(&str, Vec<T>)],
        _witness: &[(&str, Vec<T>)],
        prev_proof: Option<Proof>,
    ) -> (Option<Proof>, Option<String>) {
        if prev_proof.is_some() {
            unimplemented!("Aggregration is not implemented for pil-stark CLI backend");
        }

        (None, Some(estark::pil_to_json(pil)))
    }
}

pub struct EStark {
    degree: DegreeType,
}

impl<F: FieldElement> BackendImpl<F> for EStark {
    fn new(degree: DegreeType) -> Self {
        Self { degree }
    }

    fn prove(
        &self,
        pil: &Analyzed<F>,
        fixed: &[(&str, Vec<F>)],
        witness: &[(&str, Vec<F>)],
        prev_proof: Option<Proof>,
    ) -> (Option<Proof>, Option<String>) {
        if prev_proof.is_some() {
            unimplemented!("aggregration is not implemented");
        }

        let mut actual_prover = estark::EStark::new(pil, fixed, self.degree);
        let ret = actual_prover.prove_and_verify(witness);
        (Some(ret.0), Some(ret.1))
    }
}
