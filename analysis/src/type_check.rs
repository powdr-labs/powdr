use std::{collections::HashMap, marker::PhantomData};

use ast::{
    asm_analysis::{
        AnalysisASMFile, AssignmentStatement, DegreeStatement, InstructionDefinitionStatement,
        InstructionStatement, LabelStatement, Machine, OperationDefinitionStatement, PilBlock,
        RegisterDeclarationStatement,
    },
    parsed::asm::{ASMFile, MachineStatement, OperationStatement, RegisterFlag},
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
        let mut operations = vec![];
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
                    if self.machines_types.contains_key(&ty) {
                        submachines.push((name, ty));
                    } else {
                        errors.push(format!("Undeclared machine type {}", ty))
                    }
                }
                MachineStatement::OperationDeclaration(start, name, params, statements) => {
                    let mut body = vec![];
                    for s in statements {
                        match s {
                            OperationStatement::Assignment(start, lhs, using_reg, rhs) => {
                                body.push(
                                    AssignmentStatement {
                                        start,
                                        lhs,
                                        using_reg,
                                        rhs,
                                    }
                                    .into(),
                                );
                            }
                            OperationStatement::Instruction(start, instruction, inputs) => {
                                body.push(
                                    InstructionStatement {
                                        start,
                                        instruction,
                                        inputs,
                                    }
                                    .into(),
                                );
                            }
                            OperationStatement::Label(start, name) => {
                                body.push(LabelStatement { start, name }.into());
                            }
                            OperationStatement::DebugDirective(_start, _d) => {
                                todo!();
                            }
                        }
                    }
                    operations.push(OperationDefinitionStatement {
                        start,
                        name,
                        params,
                        body,
                    })
                }
            }
        }

        if !registers.iter().any(|r| r.flag == Some(RegisterFlag::IsPC)) {
            for o in &operations {
                if !o.body.is_empty() {
                    errors.push(format!("Operation {} in machine {} should have an empty body because this machine does not have a pc", o.name, machine.name));
                }
            }
        }

        if registers
            .iter()
            .filter(|r| r.flag == Some(RegisterFlag::IsPC))
            .count()
            > 1
        {
            errors.push(format!(
                "Machine {} cannot have more than one pc",
                machine.name
            ));
        }

        *self.machines_types.get_mut(&machine.name).unwrap() = Some(Machine {
            degree,
            pc: registers
                .iter()
                .enumerate()
                .filter_map(|(i, r)| (r.flag == Some(RegisterFlag::IsPC)).then_some(i))
                .next(),
            registers,
            instructions,
            constraints,
            operations,
            submachines,
            program: None,
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
