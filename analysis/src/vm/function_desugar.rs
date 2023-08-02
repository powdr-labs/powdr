/// Generate the rom
/// If this was written in the language, it would be something like
// fn romgen(self) -> Self {
//     self.instructions.extend([
//         Instruction::with_name("_return"), // etc, could also be a special keyword instead of a function
//         Instruction::with_name("_reset"), // etc, to reset the registers
//         Instruction::with_name("_jump_to_operation"), // etc, to jump to the correct operation
//     ]);
//
//     let main = Function::with_name("_main").statements(
//         [
//             Statement::Label("_start"),
//             Statement::Instruction("_reset"),
//             Statement::Instruction("_jump_to_operation")
//         ].chain(self.functions.iter().flat_map(|f|
//                 once(Statement::Label(format!("_{}", f.name))).chain(f.statements),
//         ))
//     );
//
//     self.functions.push(main); // this could be a special field in the machine also, instead of a function
//
//     self
// }

// Alternatively, this could be modelled as a Machine wrapper. It would be inlined, as we need access to the submachine pc, etc

// machine DynLib<Machine> {
//     // we could indicate that we want this machine to be inlined here, allowing us to add constraints about it
//     inlined Machine m;
//
//     // declare assignment registers for return values
//     <%
//     for o in m.outputs {
//         reg format!("o")[<=];
//     }
//     %>

//     constraints {
//         pol witness function_id;
//         pol commit function_id;
//         ((1 - instr_return) * (function_id' - function_id)) = 0;
//     }

//     // a return instruction which does nothing, we use its flag in constraints
//     instr return {}

//     // reset registers to 0 and leave inputs free
//     instr _reset {
//         <%
//         for reg in m.write_regs {
//             reg = 0;
//         }
//         for i in m.inputs {
//             format!("{}_read_free", i) = 1;
//         }
//         %>
//     }

//     instr _jump_to_instruction {
//         m.pc' = function_id;
//     }

//     instr _loop {
//         m.pc' = m.pc;
//     }

//     function main(<%m.inputs%>) -> <%m.outputs%> {
//         _start::
//         _reset::
//         _jump_to_instruction;
//         <%
//         for f in m.functions {
//             format!("_{}::", f.name);
//             f.body
//         }
//         %>
//         _sink::
//         _loop;
//     }
// }
use ast::{
    asm_analysis::{
        utils::previsit_expression_mut, AnalysisASMFile, AssignmentStatement, FunctionStatement,
        InstructionStatement, Machine,
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
            // todo: make them read-only
            let input_assignment_registers_declarations = (0..input_count)
                .map(|i| parse_register_declaration::<T>(&format!("reg {};", input_at(i))));

            // create one assignment register for each output
            let output_assignment_registers_declarations = (0..output_count)
                .map(|i| parse_register_declaration::<T>(&format!("reg {}[<=];", output_at(i))));

            machine.registers.extend(
                input_assignment_registers_declarations
                    .chain(output_assignment_registers_declarations),
            );

            // replace the parameters in all functions
            machine.functions = machine
                .functions
                .into_iter()
                .map(|mut f| {
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
                    f.body.statements = f
                        .body
                        .statements
                        .into_iter()
                        .map(move |s| match s {
                            FunctionStatement::Assignment(assignment) => {
                                FunctionStatement::Assignment(AssignmentStatement {
                                    rhs: Box::new(substitute(*assignment.rhs, &inputs)),
                                    ..assignment
                                })
                            }
                            FunctionStatement::Instruction(instruction) => {
                                FunctionStatement::Instruction(InstructionStatement {
                                    inputs: instruction
                                        .inputs
                                        .into_iter()
                                        .map(|i| substitute(i, &inputs))
                                        .collect(),
                                    ..instruction
                                })
                            }
                            FunctionStatement::Label(l) => FunctionStatement::Label(l),
                            FunctionStatement::DebugDirective(d) => {
                                FunctionStatement::DebugDirective(d)
                            }
                        })
                        .collect();
                    f
                })
                .collect();

            machine.latch = Some(latch.into());
        } else {
            // do nothing for static machines
        };

        machine
    }
}

#[cfg(test)]
mod tests {}
