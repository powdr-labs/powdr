use itertools::Itertools;
use powdr_ast::analyzed::{Analyzed, Identity};
use powdr_executor_utils::VariablySizedColumn;
use powdr_number::{FieldElement, GoldilocksField};

use crate::{
    constant_evaluator,
    witgen::{
        data_structures::mutable_state::MutableState,
        global_constraints,
        jit::effect::MachineCallArgument,
        machines::{machine_extractor::MachineExtractor, KnownMachine},
        FixedData, QueryCallback,
    },
};

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

// TODO: Doesn't compile
// pub fn prepare<'a, T: FieldElement, Q: QueryCallback<T>>(
//     analyzed: &'a Analyzed<T>,
//     fixed_col_vals: &'a [(String, VariablySizedColumn<T>)],
//     external_witness_values: &'a [(String, Vec<T>)],
//     query_callback: &'a Q,
// ) -> (
//     FixedData<'a, T>,
//     MutableState<'a, T, _>,
//     Vec<&'a Identity<T>>,
// ) {
//     let fixed_data = FixedData::new(
//         analyzed,
//         fixed_col_vals,
//         external_witness_values,
//         Default::default(),
//         0,
//     );
//     let (fixed_data, retained_identities) =
//         global_constraints::set_global_constraints(fixed_data, &analyzed.identities);

//     let machines =
//         MachineExtractor::new(&fixed_data).split_out_machines(retained_identities.clone());
//     let mutable_state = MutableState::new(machines.into_iter(), query_callback);
//     (fixed_data, mutable_state, retained_identities)
// }
