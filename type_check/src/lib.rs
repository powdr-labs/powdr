use std::collections::BTreeMap;

use ast::{
    asm_analysis::{
        AnalysisASMFile, AssignmentStatement, CallableSymbolDefinitions, DebugDirective,
        DegreeStatement, FunctionBody, FunctionStatements, FunctionSymbol, Instruction,
        InstructionDefinitionStatement, InstructionStatement, LabelStatement,
        LinkDefinitionStatement, Machine, OperationSymbol, PilBlock, RegisterDeclarationStatement,
        RegisterTy, Return, SubmachineDeclaration,
    },
    parsed::asm::{ASMFile, FunctionStatement, LinkDeclaration, MachineStatement, RegisterFlag},
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
        let mut links = vec![];
        let mut callable = CallableSymbolDefinitions::default();
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
                MachineStatement::InstructionDeclaration(start, name, instruction) => {
                    if name == "return" {
                        errors.push("Instruction cannot use reserved name `return`".into());
                    }
                    instructions.push(InstructionDefinitionStatement {
                        start,
                        name,
                        instruction: Instruction {
                            params: instruction.params,
                            body: instruction.body,
                        },
                    });
                }
                MachineStatement::LinkDeclaration(LinkDeclaration {
                    start,
                    flag,
                    params,
                    to,
                }) => {
                    links.push(LinkDefinitionStatement {
                        start,
                        flag,
                        params,
                        to,
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
                MachineStatement::FunctionDeclaration(start, name, params, statements) => {
                    let mut function_statements = vec![];
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
                            FunctionStatement::Return(start, values) => {
                                function_statements.push(Return { start, values }.into());
                            }
                        }
                    }
                    assert!(callable
                        .insert(
                            name,
                            FunctionSymbol {
                                start,
                                params,
                                body: FunctionBody {
                                    statements: FunctionStatements::new(function_statements),
                                },
                            },
                        )
                        .is_none());
                }
                MachineStatement::OperationDeclaration(start, name, id, params) => {
                    assert!(callable
                        .insert(name, OperationSymbol { start, id, params })
                        .is_none());
                }
            }
        }

        let latch = machine.arguments.latch;
        let operation_id = machine.arguments.operation_id;

        if !registers.iter().any(|r| r.ty.is_pc()) {
            if latch.is_none() {
                errors.push(format!(
                    "Machine {} should have a latch column as it does not have a pc",
                    machine.name
                ));
            }
            if operation_id.is_none() {
                errors.push(format!(
                    "Machine {} should have an operation id column as it does not have a pc",
                    machine.name
                ));
            }
            for f in callable.function_definitions() {
                errors.push(format!(
                    "Machine {} should not have functions as it does not have a pc, found `{}`",
                    machine.name, f.name
                ))
            }
            for i in &instructions {
                errors.push(format!(
                    "Machine {} should not have instructions as it does not have a pc, found `{}`",
                    machine.name, i.name
                ))
            }
        } else {
            if latch.is_some() {
                errors.push(format!(
                    "Machine {} should not have a latch column as it has a pc",
                    machine.name
                ));
            }
            if operation_id.is_some() {
                errors.push(format!(
                    "Machine {} should not have an operation id column as it has a pc",
                    machine.name
                ));
            }
            for f in callable.operation_definitions() {
                errors.push(format!(
                    "Machine {} should not have operations as it has a pc, found `{}`",
                    machine.name, f.name
                ))
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
            operation_id,
            pc: registers
                .iter()
                .enumerate()
                .filter_map(|(i, r)| (r.ty.is_pc()).then_some(i))
                .next(),
            registers,
            links,
            instructions,
            constraints,
            callable,
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
            let machines = std::mem::take(&mut self.machines_types)
                .into_iter()
                .map(|(name, machine)| (name, machine.unwrap()))
                .collect();
            Ok(AnalysisASMFile { machines })
        }
    }
}
