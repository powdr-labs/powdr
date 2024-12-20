use itertools::Itertools;
use powdr_ast::indent;
use powdr_number::GoldilocksField;

use crate::witgen::jit::effect::MachineCallArgument;

use super::{
    effect::{Assertion, BranchCondition, Effect},
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
            Effect::Branch(condition, first, second) => {
                let first = indent(format_code(first), 1);
                let second = indent(format_code(second), 1);
                let condition = format_condition(condition);

                format!("if ({condition}) {{\n{first}\n}} else {{\n{second}\n}}")
            }
        })
        .join("\n")
}

fn format_condition(condition: &BranchCondition<GoldilocksField, Variable>) -> String {
    let var = &condition.variable;
    let (min, max) = condition.first_branch.range();
    if min == max {
        format!("{var} == {min}")
    } else if min < max {
        format!("{min} <= {var} && {var} <= {max}")
    } else {
        format!("{var} <= {min} || {var} >= {max}")
    }
}
