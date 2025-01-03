#![allow(dead_code)]
use std::collections::HashSet;

use itertools::Itertools;
use powdr_ast::analyzed::AlgebraicReference;
use powdr_number::FieldElement;

use crate::witgen::{machines::MachineParts, FixedData};

use super::{
    effect::Effect,
    variable::{Cell, Variable},
    witgen_inference::{CanProcessCall, FixedEvaluator, WitgenInference},
};

/// A processor for generating JIT code that computes the next row from the previous row.
pub struct SingleStepProcessor<'a, T: FieldElement> {
    fixed_data: &'a FixedData<'a, T>,
    machine_parts: MachineParts<'a, T>,
}

impl<'a, T: FieldElement> SingleStepProcessor<'a, T> {
    pub fn new(fixed_data: &'a FixedData<'a, T>, machine_parts: MachineParts<'a, T>) -> Self {
        SingleStepProcessor {
            fixed_data,
            machine_parts,
        }
    }

    pub fn generate_code<CanProcess: CanProcessCall<T> + Clone>(
        &self,
        can_process: CanProcess,
    ) -> Result<Vec<Effect<T, Variable>>, String> {
        // All witness columns in row 0 are known.
        let known_variables = self.machine_parts.witnesses.iter().map(|id| {
            Variable::Cell(Cell {
                column_name: self.fixed_data.column_name(id).to_string(),
                id: id.id,
                row_offset: 0,
            })
        });
        let mut witgen = WitgenInference::new(self.fixed_data, self, known_variables);

        let mut complete = HashSet::new();
        for iteration in 0.. {
            let mut progress = false;

            for id in &self.machine_parts.identities {
                if !complete.contains(&id.id()) {
                    let row_offset = if id.contains_next_ref() { 0 } else { 1 };
                    let result = witgen.process_identity(can_process.clone(), id, row_offset);
                    if result.complete {
                        complete.insert(id.id());
                    }
                    progress |= result.progress;
                }
            }
            if !progress {
                log::debug!("Finishing after {iteration} iterations");
                break;
            }
        }

        // Check that we could derive all witness values in the next row.
        let unknown_witnesses = self
            .machine_parts
            .witnesses
            .iter()
            .filter(|wit| {
                !witgen.is_known(&Variable::Cell(Cell {
                    column_name: self.fixed_data.column_name(wit).to_string(),
                    id: wit.id,
                    row_offset: 1,
                }))
            })
            .sorted()
            .collect_vec();

        let missing_identities = self.machine_parts.identities.len() - complete.len();
        if unknown_witnesses.is_empty() && missing_identities == 0 {
            Ok(witgen.code())
        } else {
            Err(format!(
                "Unable to derive algorithm to compute values for witness columns in the next row for the following columns:\n{}\nand {missing_identities} identities are missing.",
                unknown_witnesses.iter().map(|wit| self.fixed_data.column_name(wit)).format(", ")
            ))
        }
    }
}

impl<T: FieldElement> FixedEvaluator<T> for &SingleStepProcessor<'_, T> {
    fn evaluate(&self, _var: &AlgebraicReference, _row_offset: i32) -> Option<T> {
        // We can only return something here if the fixed column is constant
        // in the region we are considering.
        // This might be the case if we know we are not evaluating the first or the last
        // row, but this is not yet implemented.
        None
    }
}

#[cfg(test)]
mod test {

    use itertools::Itertools;

    use powdr_number::GoldilocksField;

    use crate::witgen::{
        data_structures::mutable_state::MutableState,
        global_constraints,
        jit::{
            effect::{format_code, Effect},
            test_util::read_pil,
        },
        machines::{machine_extractor::MachineExtractor, KnownMachine, Machine},
        FixedData,
    };

    use super::{SingleStepProcessor, Variable};

    fn generate_single_step(
        input_pil: &str,
        machine_name: &str,
    ) -> Result<Vec<Effect<GoldilocksField, Variable>>, String> {
        let (analyzed, fixed_col_vals) = read_pil(input_pil);

        let fixed_data = FixedData::new(&analyzed, &fixed_col_vals, &[], Default::default(), 0);
        let (fixed_data, retained_identities) =
            global_constraints::set_global_constraints(fixed_data, &analyzed.identities);
        let machines = MachineExtractor::new(&fixed_data).split_out_machines(retained_identities);
        let [KnownMachine::DynamicMachine(machine)] = machines
            .iter()
            .filter(|m| m.name().contains(machine_name))
            .collect_vec()
            .as_slice()
        else {
            panic!("Expected exactly one matching dynamic machine")
        };
        let machine_parts = machine.machine_parts().clone();
        let mutable_state = MutableState::new(machines.into_iter(), &|_| {
            Err("Query not implemented".to_string())
        });

        SingleStepProcessor {
            fixed_data: &fixed_data,
            machine_parts,
        }
        .generate_code(&mutable_state)
    }

    #[test]
    fn fib() {
        let input = "namespace M(256); let X; let Y; X' = Y; Y' = X + Y;";
        let code = generate_single_step(input, "M").unwrap();
        assert_eq!(
            format_code(&code),
            "M::X[1] = M::Y[0];\nM::Y[1] = (M::X[0] + M::Y[0]);"
        );
    }

    #[test]
    fn no_progress() {
        let input = "namespace M(256); let X; let Y; X' = X;";
        let err = generate_single_step(input, "M").err().unwrap();
        assert_eq!(
            err.to_string(),
            "Unable to derive algorithm to compute values for witness columns in the next row for the following columns:\n\
            M::Y\n\
            and 0 identities are missing."
        );
    }
}
