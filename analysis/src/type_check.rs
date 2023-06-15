use std::{collections::HashMap, marker::PhantomData};

use ast::{
    asm_analysis::{
        AnalysisASMFile, AssignmentStatement, DegreeStatement, InstructionDefinitionStatement,
        InstructionStatement, LabelStatement, Machine, PilBlock, Program,
        RegisterDeclarationStatement,
    },
    parsed::asm::{ASMFile, MachineStatement, ProgramStatement},
};
use number::FieldElement;

#[derive(Default)]
struct TypeChecker<T> {
    machines_types: HashMap<String, Machine<T>>,
    marker: PhantomData<T>,
}

impl<T: FieldElement> TypeChecker<T> {
    fn check_machine_type(&mut self, machine: ast::parsed::asm::Machine<T>) -> Result<(), String> {
        let mut degree = None;
        let mut registers = vec![];
        let mut constraints = vec![];
        let mut instructions = vec![];
        let mut program = vec![];
        let mut submachines = vec![];

        for s in machine.statements {
            match s {
                MachineStatement::Degree(_, degree_value) => {
                    degree = Some(DegreeStatement {
                        degree: degree_value,
                    });
                }
                MachineStatement::RegisterDeclaration(start, name, flag) => {
                    registers.push(RegisterDeclarationStatement { start, name, flag });
                }
                MachineStatement::InstructionDeclaration(start, name, params, body) => {
                    instructions.push(InstructionDefinitionStatement {
                        start,
                        name,
                        params,
                        body,
                    });
                }
                MachineStatement::InlinePil(start, statements) => {
                    constraints.push(PilBlock { start, statements });
                }
                MachineStatement::Submachine(_, ty, name) => {
                    submachines.push((name, ty));
                }
                MachineStatement::Program(_, statements) => {
                    for s in statements {
                        match s {
                            ProgramStatement::Assignment(start, lhs, using_reg, rhs) => {
                                program.push(
                                    AssignmentStatement {
                                        start,
                                        lhs,
                                        using_reg,
                                        rhs,
                                    }
                                    .into(),
                                );
                            }
                            ProgramStatement::Instruction(start, instruction, inputs) => {
                                program.push(
                                    InstructionStatement {
                                        start,
                                        instruction,
                                        inputs,
                                    }
                                    .into(),
                                );
                            }
                            ProgramStatement::Label(start, name) => {
                                program.push(LabelStatement { start, name }.into());
                            }
                            ProgramStatement::DebugDirective(_start, _d) => {
                                todo!();
                            }
                        }
                    }
                }
            }
        }

        self.machines_types.insert(
            machine.name,
            Machine {
                degree,
                registers,
                instructions,
                constraints,
                program: Program {
                    statements: program,
                    ..Default::default()
                },
                submachines,
            },
        );

        Ok(())
    }

    fn check_file(&mut self, file: ASMFile<T>) -> Result<AnalysisASMFile<T>, String> {
        for m in file.machines {
            self.check_machine_type(m)?;
        }

        Ok(AnalysisASMFile {
            machines: std::mem::take(&mut self.machines_types),
        })
    }
}

/// A very stupid type checker. TODO: make it smart
pub fn check<T: FieldElement>(file: ASMFile<T>) -> Result<AnalysisASMFile<T>, String> {
    TypeChecker::default().check_file(file)
}
