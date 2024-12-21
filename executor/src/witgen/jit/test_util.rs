use powdr_ast::analyzed::Analyzed;
use powdr_executor_utils::VariablySizedColumn;
use powdr_number::FieldElement;

use crate::constant_evaluator;

pub fn read_pil<T: FieldElement>(
    input_pil: &str,
) -> (Analyzed<T>, Vec<(String, VariablySizedColumn<T>)>) {
    let analyzed = powdr_pil_analyzer::analyze_string(input_pil).unwrap();
    let fixed_col_vals = constant_evaluator::generate(&analyzed);
    (analyzed, fixed_col_vals)
}
