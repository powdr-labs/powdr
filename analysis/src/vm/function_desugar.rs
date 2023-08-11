//! Desugar asm functions which operate on values to functions which operate on witness columns
//! TODO: Using the same term "function" for both is a bit awkward

use ast::{
    asm_analysis::{
        utils::previsit_expression_mut, AnalysisASMFile, AssignmentStatement, CallableSymbol,
        CallableSymbolDeclarations, FunctionStatement, FunctionStatements, InstructionStatement,
        Machine,
    },
    parsed::{
        asm::{Param, ParamList},
        Expression,
    },
};
use number::FieldElement;
use std::{collections::HashMap, marker::PhantomData, ops::ControlFlow};

use crate::utils::{parse_instruction_definition, parse_register_declaration};

/// Desugar VM functions
pub fn desugar<T: FieldElement>(file: AnalysisASMFile<T>) -> AnalysisASMFile<T> {
    FunctionDesugar::default().generate(file)
}

#[derive(Default)]
struct FunctionDesugar<T> {
    marker: PhantomData<T>,
}

fn input_at(i: usize) -> String {
    format!("_input_{}", i)
}
fn output_at(i: usize) -> String {
    format!("_output_{}", i)
}

fn substitute<T>(mut e: Expression<T>, inputs: &HashMap<String, usize>) -> Expression<T> {
    previsit_expression_mut(&mut e, &mut |e| {
        if let Expression::PolynomialReference(r) = e {
            if let Some(v) = inputs.get(&r.name).map(|i| input_at(*i)) {
                r.name = v;
            }
        };
        ControlFlow::Continue::<()>(())
    });
    e
}

impl<T: FieldElement> FunctionDesugar<T> {
    fn generate(&self, file: AnalysisASMFile<T>) -> AnalysisASMFile<T> {
        AnalysisASMFile {
            machines: file
                .machines
                .into_iter()
                .map(|(name, m)| (name, self.generate_machine_rom(m)))
                .collect(),
        }
    }

    fn generate_machine_rom(&self, mut machine: Machine<T>) -> Machine<T> {
        if machine.has_pc() {
            let latch = "instr_return";

            // the number of inputs is the max of the number of inputs needed in each operation
            let input_count = machine
                .function_definitions()
                .map(|f| f.function.params.inputs.params.len())
                .max()
                .unwrap_or(0);
            let output_count = machine
                .function_definitions()
                .map(|f| {
                    f.function
                        .params
                        .outputs
                        .as_ref()
                        .map(|o| o.params.len())
                        .unwrap_or(0)
                })
                .max()
                .unwrap_or(0);

            // add the return instruction. its exact implementation is responsibility of romgen
            machine
                .instructions
                .push(parse_instruction_definition(&format!(
                    "instr return {} {{ }}",
                    (0..output_count)
                        .map(output_at)
                        .collect::<Vec<_>>()
                        .join(", "),
                )));

            // create one register for each input. They are created after as their reset is different (unconstrained, not 0)
            let input_assignment_registers_declarations = (0..input_count)
                .map(|i| parse_register_declaration::<T>(&format!("reg {}[@r];", input_at(i))));

            // create one assignment register for each output
            let output_assignment_registers_declarations = (0..output_count)
                .map(|i| parse_register_declaration::<T>(&format!("reg {}[<=];", output_at(i))));

            machine.registers.extend(
                input_assignment_registers_declarations
                    .chain(output_assignment_registers_declarations),
            );

            // replace the parameters in all functions
            machine.callable = CallableSymbolDeclarations(
                machine
                    .callable
                    .0
                    .into_iter()
                    .map(|(name, symbol)| {
                        let symbol = match symbol {
                            CallableSymbol::Function(mut f) => {
                                // create substitution map from declared input to the hidden witness columns
                                let inputs = f
                                    .params
                                    .inputs
                                    .params
                                    .iter()
                                    .enumerate()
                                    .map(|(index, param)| (param.name.clone(), index))
                                    .collect();

                                f.params.inputs.params = (0..f.params.inputs.params.len())
                                    .map(|i| Param {
                                        name: input_at(i),
                                        ty: None,
                                    })
                                    .collect();
                                f.params.outputs = f.params.outputs.map(|outputs| {
                                    assert_eq!(
                                        outputs.params.len(),
                                        1,
                                        "number of function outputs should be 0 or 1"
                                    );
                                    ParamList {
                                        params: vec![Param {
                                            name: output_at(0),
                                            ty: None,
                                        }],
                                    }
                                });

                                // substitute the names in the operation body
                                f.body.statements = FunctionStatements::new(
                                    f.body
                                        .statements
                                        .into_inner()
                                        .into_iter()
                                        .map(move |s| match s {
                                            FunctionStatement::Assignment(assignment) => {
                                                FunctionStatement::Assignment(AssignmentStatement {
                                                    rhs: Box::new(substitute(
                                                        *assignment.rhs,
                                                        &inputs,
                                                    )),
                                                    ..assignment
                                                })
                                            }
                                            FunctionStatement::Instruction(instruction) => {
                                                FunctionStatement::Instruction(
                                                    InstructionStatement {
                                                        inputs: instruction
                                                            .inputs
                                                            .into_iter()
                                                            .map(|i| substitute(i, &inputs))
                                                            .collect(),
                                                        ..instruction
                                                    },
                                                )
                                            }
                                            FunctionStatement::Label(l) => {
                                                FunctionStatement::Label(l)
                                            }
                                            FunctionStatement::DebugDirective(d) => {
                                                FunctionStatement::DebugDirective(d)
                                            }
                                        })
                                        .collect(),
                                );
                                f.into()
                            }
                            s => s.into(),
                        };

                        (name, symbol)
                    })
                    .collect(),
            );

            machine.latch = Some(latch.into());
        } else {
            // do nothing for static machines
        };

        machine
    }
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
