pub mod estark;
mod json_exporter;

use crate::{BackendImpl, Proof};
use ast::analyzed::Analyzed;
use number::{DegreeType, FieldElement};

pub struct PilStarkCli;

impl<T: FieldElement> BackendImpl<T> for PilStarkCli {
    fn new(_degree: DegreeType) -> Self {
        Self
    }

    fn prove(
        &self,
        pil: &Analyzed<T>,
        _fixed: &[(String, Vec<T>)],
        _witness: &[(String, Vec<T>)],
        prev_proof: Option<Proof>,
        _bname: Option<String>,
    ) -> (Option<Proof>, Option<String>) {
        if prev_proof.is_some() {
            unimplemented!("Aggregration is not implemented for pil-stark CLI backend");
        }

        (
            None,
            Some(serde_json::to_string(&json_exporter::export(pil)).unwrap()),
        )
    }
}
