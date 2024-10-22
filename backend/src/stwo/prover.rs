use powdr_ast::analyzed::Analyzed;
use std::io;
use std::sync::Arc;

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
    #[allow(dead_code)]
    #[allow(unused_variables)]
    pub fn new(
        _analyzed: Arc<Analyzed<F>>,
        _fixed: Arc<Vec<(String, Vec<F>)>>,
      //  setup: Option<&mut dyn io::Read>,
    ) -> Result<Self, io::Error> {
        Ok(Self {
            _analyzed,
            _fixed,
            _proving_key: None,
            _verifying_key: None,
        })
    }
}

#[cfg(feature = "stwo")]
#[cfg(test)]
mod tests {
    use super::*;
    use powdr_number::Mersenne31Field as F;
    use powdr_pipeline::Pipeline;
    fn run_test(pil: &str) {
        run_test_publics(pil, &None);
    }
    fn run_test_publics(pil: &str, malicious_publics: &Option<Vec<usize>>) {
        let mut pipeline = Pipeline::<F>::default().from_pil_string(pil.to_string());

        let pil = pipeline.compute_optimized_pil().unwrap();
        println!("{}", pil);

        let witness_callback = pipeline.witgen_callback().unwrap();
        let witness = &mut pipeline.compute_witness().unwrap();
        println!("{:?}", witness);

        let fixed = pipeline.compute_fixed_cols().unwrap();

        //let mut prover = StwoProver::new(pil, fixed);
    }

    #[test]
    fn shuang_keep_doing() {
        let content = r#"
        namespace Mul(4);
            col witness x;
            col witness y;
            col witness z;
            x * y = z;
        "#;
        run_test(content);
    }
}
