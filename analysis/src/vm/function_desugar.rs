//! Desugar asm functions which operate on values to functions which operate on witness columns
//! TODO: Using the same term "function" for both is a bit awkward

use ast::{
    asm_analysis::{
        utils::{previsit_expression_in_statement_mut, previsit_expression_mut},
        AnalysisASMFile, FunctionStatement, Machine,
    },
    parsed::{
        asm::{Param, ParamList},
        Expression,
    },
};
use number::FieldElement;
use std::{collections::HashMap, ops::ControlFlow};

use crate::utils::{parse_register_declaration};

/// Desugar VM functions
pub fn desugar<T: FieldElement>(file: AnalysisASMFile<T>) -> AnalysisASMFile<T> {
    AnalysisASMFile {
        machines: file
            .machines
            .into_iter()
            .map(|(name, m)| (name, desugar_machine(m)))
            .collect(),
    }
}

fn input_at(i: usize) -> String {
    format!("_input_{}", i)
}
fn output_at(i: usize) -> String {
    format!("_output_{}", i)
}

/// Substitute all visited columns inside expressions of `s`
/// This *only* applies to expressions, so for example identifiers the left hand side of statements is not substituted
/// This is fine in this case since inputs are only present in expressions
fn substitute_statement<T>(
    s: &mut FunctionStatement<T>,
    substitution: &HashMap<String, String>,
) -> ControlFlow<()> {
    fn substitute<T>(
        e: &mut Expression<T>,
        substitution: &HashMap<String, String>,
    ) -> ControlFlow<()> {
        previsit_expression_mut(e, &mut |e| {
            if let Expression::PolynomialReference(r) = e {
                if let Some(v) = substitution.get(&r.name).cloned() {
                    r.name = v;
                }
            };
            ControlFlow::Continue::<()>(())
        });
        ControlFlow::Continue(())
    }

    previsit_expression_in_statement_mut(s, &mut |e| substitute(e, substitution))
}

fn desugar_machine<T: FieldElement>(mut machine: Machine<T>) -> Machine<T> {
    if machine.has_pc() {
        // the number of inputs is the max of the number of inputs needed in each operation
        let input_count = machine
            .functions
            .iter()
            .map(|o| o.params.inputs.params.len())
            .max()
            .unwrap_or(0);
        let output_count = machine
            .functions
            .iter()
            .map(|o| {
                o.params
                    .outputs
                    .as_ref()
                    .map(|o| o.params.len())
                    .unwrap_or(0)
            })
            .max()
            .unwrap_or(0);

        // create one read-only register for each input
        let input_assignment_registers_declarations = (0..input_count)
            .map(|i| parse_register_declaration::<T>(&format!("reg {}[@r];", input_at(i))));

        // create one assignment register for each output
        let output_assignment_registers_declarations = (0..output_count)
            .map(|i| parse_register_declaration::<T>(&format!("reg {}[<=];", output_at(i))));

        machine.registers.extend(
            input_assignment_registers_declarations.chain(output_assignment_registers_declarations),
        );

        // replace the parameters in all functions
        machine.functions = machine
            .functions
            .into_iter()
            .map(|mut f| {
                // create substitution map from declared input to the hidden witness columns
                let input_substitution = f
                    .params
                    .inputs
                    .params
                    .iter()
                    .enumerate()
                    .map(|(index, param)| (param.name.clone(), input_at(index)))
                    .collect();

                f.params.inputs.params = f
                    .params
                    .inputs
                    .params
                    .iter()
                    .enumerate()
                    .map(|(i, _)| Param {
                        name: input_at(i),
                        ty: None,
                    })
                    .collect();
                f.params.outputs = f.params.outputs.map(|outputs| ParamList {
                    params: outputs
                        .params
                        .into_iter()
                        .enumerate()
                        .map(|(i, _)| Param {
                            name: output_at(i),
                            ty: None,
                        })
                        .collect(),
                });

                // substitute the names in the operation body
                for s in f.body.statements.iter_mut() {
                    substitute_statement(s, &input_substitution);
                }
                f
            })
            .collect();
    } else {
        // do nothing for static machines
    };

    machine
}

#[cfg(test)]
mod tests {
    use number::Bn254Field;

    use crate::vm::test_utils::function_desugar_str;

    use super::*;

    #[test]
    fn book() {
        let before =
            std::fs::read_to_string("../test_data/asm/book/asm_function_before.asm").unwrap();
        let desugared: AnalysisASMFile<Bn254Field> = function_desugar_str(&before);
        // TODO: this file does not quite compile yet because the latch `instr_return` conflicts with the existence of a pc.
        // this should be fixed so that this file can be compiled
        let expected =
            std::fs::read_to_string("../test_data/asm/book/asm_function_after.asm.ignore").unwrap();
        assert_eq!(
            desugared.to_string().split_whitespace().collect::<Vec<_>>(),
            expected.to_string().split_whitespace().collect::<Vec<_>>()
        );
    }
}
