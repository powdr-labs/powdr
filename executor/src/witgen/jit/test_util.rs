use itertools::Itertools;
use powdr_ast::analyzed::Analyzed;
use powdr_executor_utils::VariablySizedColumn;
use powdr_number::{FieldElement, GoldilocksField};

use crate::{constant_evaluator, witgen::jit::effect::MachineCallArgument};

use super::{
    effect::{Assertion, Effect},
    variable::Variable,
};

pub fn format_code(effects: &[Effect<GoldilocksField, Variable>]) -> String {
    effects
        .iter()
        .map(|effect| match effect {
            Effect::Assignment(v, expr) => format!("{v} = {expr};"),
            Effect::Assertion(Assertion {
                lhs,
                rhs,
                expected_equal,
            }) => {
                format!(
                    "assert {lhs} {} {rhs};",
                    if *expected_equal { "==" } else { "!=" }
                )
            }
            Effect::MachineCall(id, args) => {
                format!(
                    "machine_call({id}, [{}]);",
                    args.iter()
                        .map(|arg| match arg {
                            MachineCallArgument::Known(k) => format!("Known({k})"),
                            MachineCallArgument::Unknown(u) => format!("Unknown({u})"),
                        })
                        .join(", ")
                )
            }
            Effect::RangeConstraint(..) => {
                panic!("Range constraints should not be part of the code.")
            }
        })
        .join("\n")
}

pub fn read_pil<T: FieldElement>(
    input_pil: &str,
) -> (Analyzed<T>, Vec<(String, VariablySizedColumn<T>)>) {
    let analyzed = powdr_pil_analyzer::analyze_string(input_pil).unwrap();
    let fixed_col_vals = constant_evaluator::generate(&analyzed);
    (analyzed, fixed_col_vals)
}
