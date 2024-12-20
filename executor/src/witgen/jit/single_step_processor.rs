#![allow(dead_code)]
use std::collections::HashSet;

use itertools::Itertools;
use powdr_ast::analyzed::{AlgebraicReference, PolyID};
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
        self.generate_code_for_branch(can_process, self.initialize_witgen(), Default::default())
    }

    pub fn generate_code_for_branch<CanProcess: CanProcessCall<T> + Clone>(
        &self,
        can_process: CanProcess,
        mut witgen: WitgenInference<'a, T, NoEval>,
        mut complete: HashSet<u64>,
    ) -> Result<Vec<Effect<T, Variable>>, String> {
        self.process_until_no_progress(can_process.clone(), &mut witgen, &mut complete);

        // Check that we could derive all witness values in the next row.
        let unknown_witnesses = self
            .unknown_witness_cols_on_next_row(&witgen)
            .sorted()
            .collect_vec();

        let code = if unknown_witnesses.is_empty() {
            // TODO also check that we completed all machine calls?
            witgen.code()
        } else {
            let Some((most_constrained_var, _)) = witgen
                .known_variables()
                .filter_map(|var| witgen.range_constraint(var).map(|rc| (var, rc)))
                .filter(|(_, rc)| rc.try_to_single_value().is_none())
                .sorted()
                .min_by_key(|(_, rc)| rc.range_width())
            else {
                return Err(format!(
                    "Unable to derive algorithm to compute values for witness columns in the next row and\n\
                    unable to branch on a variable. The following columns are still missing:\n{}",
                    unknown_witnesses.iter().map(|wit| self.fixed_data.column_name(wit)).format(", ")
                ));
            };

            let (common_code, condition, other_branch) =
                witgen.branch_on(&most_constrained_var.clone());

            // TODO Tuning: If this fails (or also if it does not generate progress right away),
            // we could also choose a different variable to branch on.
            let left_branch_code =
                self.generate_code_for_branch(can_process.clone(), witgen, complete.clone())?;
            let right_branch_code =
                self.generate_code_for_branch(can_process, other_branch, complete)?;
            if left_branch_code == right_branch_code {
                common_code.into_iter().chain(left_branch_code).collect()
            } else {
                common_code
                    .into_iter()
                    .chain(std::iter::once(Effect::Branch(
                        condition,
                        left_branch_code,
                        right_branch_code,
                    )))
                    .collect()
            }
        };
        Ok(code)
    }

    fn initialize_witgen(&self) -> WitgenInference<'a, T, NoEval> {
        let known_variables = self.machine_parts.witnesses.iter().map(|id| {
            Variable::Cell(Cell {
                column_name: self.fixed_data.column_name(id).to_string(),
                id: id.id,
                row_offset: 0,
            })
        });
        WitgenInference::new(self.fixed_data, NoEval, known_variables)
    }

    fn process_until_no_progress<CanProcess: CanProcessCall<T> + Clone>(
        &self,
        can_process: CanProcess,
        witgen: &mut WitgenInference<'a, T, NoEval>,
        complete: &mut HashSet<u64>,
    ) {
        let mut progress = true;
        while progress {
            progress = false;

            // TODO propagate known.

            for id in &self.machine_parts.identities {
                if complete.contains(&id.id()) {
                    continue;
                }
                let row_offset = if id.contains_next_ref() { 0 } else { 1 };
                let result = witgen.process_identity(can_process.clone(), id, row_offset);
                progress |= result.progress;
                if result.complete {
                    complete.insert(id.id());
                }
            }
        }
    }

    fn code_if_successful(
        &self,
        witgen: WitgenInference<'_, T, NoEval>,
    ) -> Result<Vec<Effect<T, Variable>>, String> {
        // Check that we could derive all witness values in the next row.
        let unknown_witnesses = self
            .unknown_witness_cols_on_next_row(&witgen)
            .sorted()
            .collect_vec();

        // TODO also check that we completed all machine calls?
        if unknown_witnesses.is_empty() {
            Ok(witgen.code())
        } else {
            Err(format!(
                    "Unable to derive algorithm to compute values for witness columns in the next row for the following columns: {}",
                    unknown_witnesses.iter().map(|wit| self.fixed_data.column_name(wit)).format(", ")
                ))
        }
    }

    fn unknown_witness_cols_on_next_row<'b>(
        &'b self,
        witgen: &'b WitgenInference<'_, T, NoEval>,
    ) -> impl Iterator<Item = &'b PolyID> + 'b {
        self.machine_parts.witnesses.iter().filter(move |wit| {
            !witgen.is_known(&Variable::Cell(Cell {
                column_name: self.fixed_data.column_name(wit).to_string(),
                id: wit.id,
                row_offset: 1,
            }))
        })
    }
}

#[derive(Clone)]
struct NoEval;

impl<T: FieldElement> FixedEvaluator<T> for NoEval {
    fn evaluate(&self, _var: &AlgebraicReference, _row_offset: i32) -> Option<T> {
        // We can only return something here if the fixed column is constant
        // in the region wer are considering.
        // This might be the case if we know we are not evaluating the first or the last
        // row, but this is not yet implemented.
        None
    }
}

#[cfg(test)]
mod test {

    use pretty_assertions::assert_eq;
    use test_log::test;

    use powdr_ast::analyzed::Analyzed;
    use powdr_number::GoldilocksField;

    use crate::{
        constant_evaluator,
        witgen::{
            data_structures::mutable_state::MutableState,
            global_constraints,
            jit::{effect::Effect, test_util::format_code},
            machines::{Connection, FixedLookup, KnownMachine, MachineParts},
            FixedData,
        },
    };

    use super::{SingleStepProcessor, Variable};

    fn generate_single_step(
        input_pil: &str,
    ) -> Result<Vec<Effect<GoldilocksField, Variable>>, String> {
        let analyzed: Analyzed<GoldilocksField> =
            powdr_pil_analyzer::analyze_string(input_pil).unwrap();
        let fixed_col_vals = constant_evaluator::generate(&analyzed);
        let fixed_data = FixedData::new(&analyzed, &fixed_col_vals, &[], Default::default(), 0);
        let (fixed_data, retained_identities) =
            global_constraints::set_global_constraints(fixed_data, &analyzed.identities);

        let fixed_lookup_connections = retained_identities
            .iter()
            .filter_map(|i| Connection::try_from(*i).ok())
            .filter(|c| FixedLookup::is_responsible(c))
            .map(|c| (c.id, c))
            .collect();

        let global_constr = fixed_data.global_range_constraints.clone();
        let fixed_machine = FixedLookup::new(global_constr, &fixed_data, fixed_lookup_connections);
        let known_fixed = KnownMachine::FixedLookup(fixed_machine);
        let mutable_state = MutableState::new([known_fixed].into_iter(), &|_| {
            Err("Query not implemented".to_string())
        });

        let witness_columns = analyzed
            .committed_polys_in_source_order()
            .flat_map(|(symbol, _)| symbol.array_elements().map(|(_, id)| id))
            .collect();

        let machine_parts = MachineParts::new(
            &fixed_data,
            // no connections
            Default::default(),
            retained_identities,
            witness_columns,
            // No prover functions
            Vec::new(),
        );

        SingleStepProcessor::new(&fixed_data, machine_parts).generate_code(&mutable_state)
    }

    #[test]
    fn fib() {
        let input = "let X; let Y; X' = Y; Y' = X + Y;";
        let code = generate_single_step(input).unwrap();
        assert_eq!(format_code(&code), "X[1] = Y[0];\nY[1] = (X[0] + Y[0]);");
    }

    #[test]
    fn no_progress() {
        let input = "let X; let Y; X' = X;";
        let err = generate_single_step(input).err().unwrap();
        assert_eq!(
            err.to_string(),
            "Unable to derive algorithm to compute values for witness columns in the next row for the following columns: Y"
        );
    }

    #[test]
    fn branching() {
        let input = "
    namespace VM(256);
        let A: col;
        let B: col;
        let instr_add: col;
        let instr_mul: col;
        let pc: col;

        col fixed LINE = [0, 1] + [2]*;
        col fixed INSTR_ADD = [0, 1] +  [0]*;
        col fixed INSTR_MUL = [1, 0] + [1]*;

        pc' = pc + 1;
        [ pc, instr_add, instr_mul ] in [ LINE, INSTR_ADD, INSTR_MUL ];

        instr_add * (A' - (A + B)) + instr_mul * (A' - A * B)  + (1 - instr_add - instr_mul) * (A' - A) = 0;
        B' = B;
        ";
        let code = generate_single_step(input).unwrap();
        assert_eq!(
            format_code(&code),
            "\
VM::pc[1] = (VM::pc[0] + 1);
machine_call(1, [Known(VM::pc[1]), Unknown(ret(1, 1, 1)), Unknown(ret(1, 1, 2))]);
VM::instr_add[1] = ret(1, 1, 1);
VM::instr_mul[1] = ret(1, 1, 2);
VM::B[1] = VM::B[0];
if (VM::instr_add[0] == 1) {
    if (VM::instr_mul[0] == 1) {
        VM::A[1] = -((-(VM::A[0] + VM::B[0]) + -(VM::A[0] * VM::B[0])) + VM::A[0]);
    } else {
        VM::A[1] = (VM::A[0] + VM::B[0]);
    }
} else {
    if (VM::instr_mul[0] == 1) {
        VM::A[1] = (VM::A[0] * VM::B[0]);
    } else {
        VM::A[1] = VM::A[0];
    }
}"
        );
    }
}
