#![allow(dead_code)]
use std::collections::HashSet;

use itertools::Itertools;
use powdr_ast::analyzed::AlgebraicReference;
use powdr_number::FieldElement;

use crate::witgen::{machines::MachineParts, FixedData};

use super::{
    effect::Effect,
    variable::{Cell, Variable},
    witgen_inference::{FixedEvaluator, WitgenInference},
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

    pub fn generate_code(&self) -> Result<Vec<Effect<T, Variable>>, String> {
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
                let row_offset = if id.contains_next_ref() { 0 } else { 1 };
                if !complete.contains(&(id.id(), row_offset)) {
                    let result = witgen.process_identity(id, row_offset);
                    if result.complete {
                        complete.insert((id.id(), row_offset));
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

        if unknown_witnesses.is_empty() {
            assert_eq!(complete, self.machine_parts.identities.len());
            Ok(witgen.code())
        } else {
            Err(format!(
                "Unable to derive algorithm to compute values for witness columns in the next row for the following columns: {}",
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

    use powdr_ast::analyzed::Analyzed;
    use powdr_number::GoldilocksField;

    use crate::{
        constant_evaluator,
        witgen::{
            global_constraints,
            jit::{effect::Effect, test_util::format_code},
            machines::MachineParts,
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

        SingleStepProcessor {
            fixed_data: &fixed_data,
            machine_parts,
        }
        .generate_code()
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
}
