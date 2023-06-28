use ast::{
    asm_analysis::{
        utils::previsit_expression_mut, AnalysisASMFile, AssignmentStatement, InstructionStatement,
        Machine, OperationStatement, PilBlock, Program,
    },
    parsed::{asm::Params, Expression},
};
use number::FieldElement;
use std::{collections::HashMap, iter::once, marker::PhantomData, ops::ControlFlow};

use crate::utils::{
    parse_instruction_definition, parse_operation_statement, parse_pil_statement,
    parse_register_declaration,
};

/// Generate the ROM for each machine based on its operations

pub fn generate_rom<T: FieldElement>(file: AnalysisASMFile<T>) -> AnalysisASMFile<T> {
    RomGenerator::default().generate(file)
}

#[derive(Default)]
struct RomGenerator<T> {
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

impl<T: FieldElement> RomGenerator<T> {
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
        let latch = if machine.has_pc() {
            "instr_return"
        } else {
            "latch"
        };

        // add the necessary embedded constraints which apply to both static and dynamic machines
        let embedded_constraints = [
            // we do not inject the latch, as we don't know if it's committed or constant
            // inject the operation_id
            parse_pil_statement("pol witness _operation_id"),
            // the operation id must be constant in a block
            parse_pil_statement(&format!(
                "(1 - {latch}) * (_operation_id' - _operation_id) = 0"
            )),
        ];

        machine.constraints.push(PilBlock {
            start: 0,
            statements: embedded_constraints.into_iter().collect(),
        });

        // the number of inputs is the max of the number of inputs needed in each operation
        let input_count = machine
            .operations
            .iter()
            .map(|o| o.params.inputs.params.len())
            .max()
            .unwrap_or(0);
        let output_count = machine
            .operations
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

        if !machine.has_pc() {
            // create the witness columns for the operations
            let params = machine
                .operations
                .iter()
                .map(|op| op.params.clone())
                .reduce(|left, right| {
                    assert_eq!(
                        left, right,
                        "all operations in static machine should have the same param names"
                    );
                    left
                })
                .unwrap_or(Params::default());

            let input_witnesses = params.inputs.params.iter().flat_map(|param| {
                [
                    parse_pil_statement(&format!("col witness {}", param.name)),
                    parse_pil_statement(&format!(
                        "(1 - {}) * ({}' - {}) = 0",
                        latch, param.name, param.name
                    )),
                ]
            });

            let output_witnesses = params
                .outputs
                .iter()
                .flat_map(|outputs| outputs.params.iter())
                .flat_map(|param| [parse_pil_statement(&format!("col witness {}", param.name))]);

            machine.constraints.push(PilBlock {
                start: 0,
                statements: input_witnesses.chain(output_witnesses).collect(),
            });

            // empty program for pc-less machine
            machine.program = Some(Program {
                statements: vec![],
                batches: None,
            });
            return machine;
        }

        // create one witness for each input
        let input_witnesses = (0..output_count)
            .flat_map(|i| {
                [
                    //parse_pil_statement(&format!("col witness {}", input_at(i))),
                    parse_pil_statement(&format!(
                        "(1 - {}) * ({}' - {}) = 0",
                        latch,
                        input_at(i),
                        input_at(i)
                    )),
                ]
            })
            .collect();
        machine.constraints.push(PilBlock {
            start: 0,
            statements: input_witnesses,
        });

        // create one register for each input. // todo: make them read-only
        let input_assignment_registers_declarations = (0..input_count)
            .map(|i| parse_register_declaration::<T>(&format!("reg {};", input_at(i))));

        // create one assignment register for each output
        let output_assignment_registers_declarations = (0..output_count)
            .map(|i| parse_register_declaration::<T>(&format!("reg {}[<=];", output_at(i))));

        let output_witnesses_constraints = (0..output_count)
            .flat_map(|i| {
                [
                    //parse_pil_statement(&format!("col witness {}", input_at(i))),
                    parse_pil_statement(&format!(
                        "(1 - {}) * ({}' - {}) = 0",
                        latch,
                        output_at(i),
                        output_at(i)
                    )),
                ]
            })
            .collect();
        machine.constraints.push(PilBlock {
            start: 0,
            statements: output_witnesses_constraints,
        });

        machine.registers.extend(
            input_assignment_registers_declarations.chain(output_assignment_registers_declarations),
        );

        let pc = machine.pc().unwrap();

        // add the necessary embedded instructions
        let embedded_instructions = [parse_instruction_definition(&format!(
            "instr return {} {{ {}' = _operation_id' }}",
            (0..output_count)
                .map(output_at)
                .collect::<Vec<_>>()
                .join(", "),
            pc
        ))];

        machine.instructions.extend(embedded_instructions);

        // define one label for each operation
        let rom =
        // insert an infinite loop at pc 0 which returns zeroes so that all zeroes is a valid block
        [
            parse_operation_statement("__noop::"),
            parse_operation_statement(&format!("return {};", (0..output_count).map(|_| "0".to_string()).collect::<Vec<_>>().join(", "))),
        ].into_iter().chain(machine
            .operations
            .iter()
            .flat_map(|o| {
                // create substitution map from declared input to the hidden witness columns
                let inputs = o
                    .params
                    .inputs
                    .params
                    .iter()
                    .enumerate()
                    .map(|(index, param)| (param.name.clone(), index))
                    .collect();

                // substitute the names in the operation body
                let body = o.body.clone().into_iter().map(move |s| match s {
                    OperationStatement::Assignment(assignment) => {
                        OperationStatement::Assignment(AssignmentStatement {
                            rhs: Box::new(substitute(*assignment.rhs, &inputs)),
                            ..assignment
                        })
                    }
                    OperationStatement::Instruction(instruction) => {
                        OperationStatement::Instruction(InstructionStatement {
                            inputs: instruction
                                .inputs
                                .into_iter()
                                .map(|i| substitute(i, &inputs))
                                .collect(),
                            ..instruction
                        })
                    }
                    OperationStatement::Label(l) => OperationStatement::Label(l),
                    OperationStatement::DebugDirective(d) => {
                        OperationStatement::DebugDirective(d)
                    }
                });

                once(parse_operation_statement(&format!("_{}::", o.name)))
                    // execute the operation after substituting the
                    .chain(body)
            }))
            .collect();

        machine.program = Some(Program {
            statements: rom,
            batches: None,
        });

        machine
    }
}
