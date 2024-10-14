use powdr_ast::analyzed::Analyzed;
use std::sync::Arc;
use std::{io, option};

use powdr_number::FieldElement;

pub struct StwoProver<F> {
    analyzed: Arc<Analyzed<F>>,
    fixed: Arc<Vec<(String, Vec<F>)>>,
    /// Proving key placeholder
    proving_key: Option<()>,
    /// Verifying key placeholder
    verifying_key: Option<()>,
}

impl<F: FieldElement> StwoProver<F> {
    pub fn new(
        analyzed: Arc<Analyzed<F>>,
        fixed: Arc<Vec<(String, Vec<F>)>>,
        setup: Option<&mut dyn io::Read>,
    ) -> Result<Self, io::Error> {
        Ok(Self {
            analyzed,
            fixed,
            proving_key: None,
            verifying_key: None,
        })
    }
}
