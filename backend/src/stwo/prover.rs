use powdr_ast::analyzed::Analyzed;
use std::sync::Arc;
use std::io;

use powdr_number::FieldElement;

#[allow(unused_variables)]
pub struct StwoProver<F> {
    _analyzed: Arc<Analyzed<F>>,
    _fixed: Arc<Vec<(String, Vec<F>)>>,
    /// Proving key placeholder
    _proving_key: Option<()>,
    /// Verifying key placeholder
    _verifying_key: Option<()>,
}

impl<F: FieldElement> StwoProver<F> {
   
    #[allow(unused_variables)]
    pub fn new(
        _analyzed: Arc<Analyzed<F>>,
        _fixed: Arc<Vec<(String, Vec<F>)>>,
        setup: Option<&mut dyn io::Read>,
    ) -> Result<Self, io::Error> {
        Ok(Self {
            _analyzed,
            _fixed,
            _proving_key: None,
            _verifying_key: None,
        })
    }
}
