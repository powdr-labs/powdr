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
    machines_types: HashMap<String, Option<Machine<T>>>,
    marker: PhantomData<T>,
}

impl<T: FieldElement> TypeChecker<T> {
    fn check_machine_type(
        &mut self,
        machine: ast::parsed::asm::Machine<T>,
    ) -> Result<(), Vec<String>> {
        if self
            .machines_types
            .get(&machine.name)
            .as_ref()
            .unwrap()
            .is_some()
        {
            // we already checked this machine type
            return Ok(());
        }

        let mut errors = vec![];

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
                MachineStatement::InstructionDeclaration(start, latch, name, params, body) => {
                    instructions.push(InstructionDefinitionStatement {
                        start,
                        latch,
                        name,
                        params,
                        body,
                    });
                }
                MachineStatement::InlinePil(start, statements) => {
                    constraints.push(PilBlock { start, statements });
                }
                MachineStatement::Submachine(_, ty, name) => {
                    if self.machines_types.contains_key(&ty) {
                        submachines.push((name, ty));
                    } else {
                        errors.push(format!("Undeclared machine type {}", ty))
                    }
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

        *self.machines_types.get_mut(&machine.name).unwrap() = Some(Machine {
            degree,
            registers,
            instructions,
            constraints,
            program: Program {
                statements: program,
                ..Default::default()
            },
            submachines,
        });

        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(())
        }
    }

    fn check_file(&mut self, file: ASMFile<T>) -> Result<AnalysisASMFile<T>, Vec<String>> {
        let mut errors = vec![];

        // first pass to get all the declared machine types
        for m in &file.machines {
            let already_declared = self.machines_types.contains_key(&m.name);
            if already_declared {
                errors.push(format!("Machine with name {} is already declared", m.name));
            } else {
                self.machines_types.insert(m.name.clone(), None);
            }
        }

        for m in file.machines {
            self.check_machine_type(m).unwrap_or_else(|e| {
                errors.extend(e);
            })
        }

        if !errors.is_empty() {
            Err(errors)
        } else {
            let machines = self
                .machines_types
                .drain()
                .map(|(name, machine)| (name, machine.unwrap()))
                .collect();
            Ok(AnalysisASMFile { machines })
        }
    }
}

/// A very stupid type checker. TODO: make it smart
pub fn check<T: FieldElement>(file: ASMFile<T>) -> Result<AnalysisASMFile<T>, Vec<String>> {
    TypeChecker::default().check_file(file)
}
