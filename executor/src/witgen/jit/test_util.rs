use itertools::Itertools;
use powdr_ast::analyzed::Analyzed;
use powdr_ast::indent;
use powdr_executor_utils::VariablySizedColumn;
use powdr_number::{FieldElement, GoldilocksField};

use crate::{constant_evaluator, witgen::jit::effect::MachineCallArgument};

use super::{
    effect::{Assertion, BranchCondition, Effect},
    variable::Variable,
};

pub fn read_pil<T: FieldElement>(
    input_pil: &str,
) -> (Analyzed<T>, Vec<(String, VariablySizedColumn<T>)>) {
    let analyzed = powdr_pil_analyzer::analyze_string(input_pil).unwrap();
    let fixed_col_vals = constant_evaluator::generate(&analyzed);
    (analyzed, fixed_col_vals)
}
