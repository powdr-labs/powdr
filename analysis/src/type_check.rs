use std::collections::BTreeMap;

use ast::{
    asm_analysis::{
        AnalysisASMFile, AssignmentStatement, DebugDirective, DegreeStatement, FunctionBody,
        FunctionDefinitionStatement, FunctionStatements, InstructionDefinitionStatement,
        InstructionStatement, LabelStatement, Machine, PilBlock, RegisterDeclarationStatement,
        RegisterTy, SubmachineDeclaration,
    },
    parsed::asm::{ASMFile, FunctionStatement, MachineStatement, RegisterFlag},
};
use number::FieldElement;

pub fn check<T: FieldElement>(file: ASMFile<T>) -> Result<AnalysisASMFile<T>, Vec<String>> {
    TypeChecker::default().check_file(file)
}

#[derive(Default)]
struct TypeChecker<T> {
    /// The types of the machines. The value is an option, as we do a first pass populating with `None`, so that a machine type can be
    /// declared later in the file than its usage in a submachine
    machines_types: BTreeMap<String, Option<Machine<T>>>,
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
        let mut functions = vec![];
        let mut submachines = vec![];

        for s in machine.statements {
            match s {
                MachineStatement::Degree(_, degree_value) => {
                    degree = Some(DegreeStatement {
                        degree: degree_value,
                    });
                }
                MachineStatement::RegisterDeclaration(start, name, flag) => {
                    let ty = match flag {
                        Some(RegisterFlag::IsAssignment) => RegisterTy::Assignment,
                        Some(RegisterFlag::IsPC) => RegisterTy::Pc,
                        Some(RegisterFlag::IsReadOnly) => RegisterTy::ReadOnly,
                        None => RegisterTy::Write,
                    };
                    registers.push(RegisterDeclarationStatement { start, name, ty });
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
                        submachines.push(SubmachineDeclaration { name, ty });
                    } else {
                        errors.push(format!("Undeclared machine type {}", ty))
                    }
                }
                MachineStatement::FunctionDeclaration(
                    start,
                    name,
                    function_id,
                    params,
                    statements,
                ) => {
                    let mut function_statements = vec![];
                    let id = function_id.map(|id| id.id);
                    for s in statements {
                        match s {
                            FunctionStatement::Assignment(start, lhs, using_reg, rhs) => {
                                function_statements.push(
                                    AssignmentStatement {
                                        start,
                                        lhs,
                                        using_reg,
                                        rhs,
                                    }
                                    .into(),
                                );
                            }
                            FunctionStatement::Instruction(start, instruction, inputs) => {
                                function_statements.push(
                                    InstructionStatement {
                                        start,
                                        instruction,
                                        inputs,
                                    }
                                    .into(),
                                );
                            }
                            FunctionStatement::Label(start, name) => {
                                function_statements.push(LabelStatement { start, name }.into());
                            }
                            FunctionStatement::DebugDirective(start, directive) => {
                                function_statements
                                    .push(DebugDirective { start, directive }.into());
                            }
                        }
                    }
                    functions.push(FunctionDefinitionStatement {
                        start,
                        name,
                        id,
                        params,
                        body: FunctionBody {
                            statements: FunctionStatements::new(function_statements),
                        },
                    })
                }
            }
        }

        let latch = machine.arguments.latch;
        let function_id = machine.arguments.function_id;

        if !registers.iter().any(|r| r.ty.is_pc()) {
            if latch.is_none() {
                errors.push(format!(
                    "Machine {} should have a latch column because it does not have a pc",
                    machine.name
                ));
            }
            if function_id.is_none() {
                errors.push(format!(
                    "Machine {} should have a function id column because it does not have a pc",
                    machine.name
                ));
            }
            for o in &functions {
                if o.id.is_none() {
                    errors.push(format!("Function {} in machine {} should have an id because this machine does not have a pc", o.name, machine.name));
                }
                if !o.body.statements.is_empty() {
                    errors.push(format!("Function {} in machine {} should have an empty body because this machine does not have a pc", o.name, machine.name));
                }
            }
        } else {
            if latch.is_some() {
                errors.push(format!(
                    "Machine {} should not have a latch column because it has a pc",
                    machine.name
                ));
            }
            if function_id.is_some() {
                errors.push(format!(
                    "Machine {} should not have a function id column because it has a pc",
                    machine.name
                ));
            }
            for o in &functions {
                if o.id.is_some() {
                    errors.push(format!("Function {} in machine {} should not have an id because this machine has a pc", o.name, machine.name));
                }
            }
        }

        if registers.iter().filter(|r| r.ty.is_pc()).count() > 1 {
            errors.push(format!(
                "Machine {} cannot have more than one pc",
                machine.name
            ));
        }

        *self.machines_types.get_mut(&machine.name).unwrap() = Some(Machine {
            degree,
            latch,
            function_id,
            pc: registers
                .iter()
                .enumerate()
                .filter_map(|(i, r)| (r.ty.is_pc()).then_some(i))
                .next(),
            registers,
            instructions,
            constraints,
            functions,
            submachines,
            rom: None,
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
            let machines = std::mem::take(&mut self.machines_types)
                .into_iter()
                .map(|(name, machine)| (name, machine.unwrap()))
                .collect();
            Ok(AnalysisASMFile { machines })
        }
    }
}
