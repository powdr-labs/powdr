use itertools::Itertools;
use powdr_number::GoldilocksField;

use crate::witgen::jit::affine_symbolic_expression::MachineCallArgument;

use super::{
    affine_symbolic_expression::{Assertion, Effect},
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
