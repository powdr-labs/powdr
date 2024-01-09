#![deny(clippy::print_stdout)]

use std::{collections::BTreeMap, marker::PhantomData};

use ast::{
    asm_analysis::{
        AnalysisASMFile, AssignmentStatement, CallableSymbolDefinitions, DebugDirective,
        DegreeStatement, FunctionBody, FunctionStatements, FunctionSymbol, Instruction,
        InstructionDefinitionStatement, InstructionStatement, LabelStatement,
        LinkDefinitionStatement, Machine, OperationSymbol, RegisterDeclarationStatement,
        RegisterTy, Return, SubmachineDeclaration,
    },
    parsed::{
        self,
        asm::{
            self, ASMModule, ASMProgram, AbsoluteSymbolPath, AssignmentRegister, FunctionStatement,
            InstructionBody, LinkDeclaration, MachineStatement, ModuleStatement, RegisterFlag,
            SymbolDefinition,
        },
    },
};
use number::FieldElement;

pub fn check<T: FieldElement>(file: ASMProgram<T>) -> Result<AnalysisASMFile<T>, Vec<String>> {
    let ctx = AbsoluteSymbolPath::default();
    let machines = TypeChecker::default().check_module(file.main, &ctx)?;
    Ok(AnalysisASMFile {
        machines: machines.into_iter().collect(),
    })
}

#[derive(Default)]
struct TypeChecker<T> {
    marker: PhantomData<T>,
}

impl<T: FieldElement> TypeChecker<T> {
    fn check_machine_type(
        &mut self,
        machine: asm::Machine<T>,
        ctx: &AbsoluteSymbolPath,
    ) -> Result<Machine<T>, Vec<String>> {
        let mut errors = vec![];

        let mut degree = None;
        let mut registers = vec![];
        let mut pil = vec![];
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
                    match self.check_instruction(&name, instruction) {
                        Ok(instruction) => instructions.push(InstructionDefinitionStatement {
                            start,
                            name,
                            instruction,
                        }),
                        Err(e) => errors.extend(e),
                    }
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
                MachineStatement::Pil(_start, statement) => {
                    pil.push(statement);
                }
                MachineStatement::Submachine(_, ty, name) => {
                    submachines.push(SubmachineDeclaration {
                        name,
                        ty: AbsoluteSymbolPath::default().join(ty),
                    });
                }
                MachineStatement::FunctionDeclaration(start, name, params, statements) => {
                    let mut function_statements = vec![];
                    for s in statements {
                        let statement_string = s.to_string();
                        match s {
                            FunctionStatement::Assignment(start, lhs, using_reg, rhs) => {
                                if let Some(using_reg) = &using_reg {
                                    if using_reg.len() != lhs.len() {
                                        errors.push(format!(
                                            "Mismatched number of registers for assignment {}",
                                            statement_string
                                        ));
                                    }
                                }
                                let using_reg = using_reg.unwrap_or_else(|| {
                                    vec![AssignmentRegister::Wildcard; lhs.len()]
                                });
                                let lhs_with_reg = lhs
                                    .into_iter()
                                    .zip(using_reg.into_iter())
                                    .collect::<Vec<_>>();
                                function_statements.push(
                                    AssignmentStatement {
                                        start,
                                        lhs_with_reg,
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
            let operation_count = callable.operation_definitions().count();
            if operation_count > 0 && latch.is_none() {
                errors.push(format!(
                    "Machine {} should have a latch column as it does not have a pc and has operations",
                    ctx
                ));
            }
            if operation_count > 1 && operation_id.is_none() {
                errors.push(format!(
                    "Machine {} should have an operation id column as it does not have a pc and has more than one operation",
                    ctx
                ));
            }
            for f in callable.function_definitions() {
                errors.push(format!(
                    "Machine {} should not have functions as it does not have a pc, found `{}`",
                    ctx, f.name
                ))
            }
            for i in &instructions {
                errors.push(format!(
                    "Machine {} should not have instructions as it does not have a pc, found `{}`",
                    ctx, i.name
                ))
            }
        } else {
            if latch.is_some() {
                errors.push(format!(
                    "Machine {} should not have a latch column as it has a pc",
                    ctx
                ));
            }
            if operation_id.is_some() {
                errors.push(format!(
                    "Machine {} should not have an operation id column as it has a pc",
                    ctx
                ));
            }
            for o in callable.operation_definitions() {
                errors.push(format!(
                    "Machine {} should not have operations as it has a pc, found `{}`",
                    ctx, o.name
                ))
            }
        }

        if registers.iter().filter(|r| r.ty.is_pc()).count() > 1 {
            errors.push(format!("Machine {} cannot have more than one pc", ctx));
        }

        let machine = Machine {
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
            pil,
            callable,
            submachines,
        };

        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(machine)
        }
    }

    fn check_module(
        &mut self,
        module: ASMModule<T>,
        ctx: &AbsoluteSymbolPath,
    ) -> Result<BTreeMap<AbsoluteSymbolPath, Machine<T>>, Vec<String>> {
        let mut errors = vec![];

        let mut res: BTreeMap<AbsoluteSymbolPath, Machine<T>> = BTreeMap::default();

        for m in module.statements {
            match m {
                ModuleStatement::SymbolDefinition(SymbolDefinition { name, value }) => {
                    match value {
                        asm::SymbolValue::Machine(m) => {
                            match self.check_machine_type(m, ctx) {
                                Err(e) => {
                                    errors.extend(e);
                                }
                                Ok(machine) => {
                                    res.insert(ctx.clone().join(name), machine);
                                }
                            };
                        }
                        asm::SymbolValue::Import(_) => {
                            unreachable!("Imports should have been removed")
                        }
                        asm::SymbolValue::Module(m) => {
                            // add the name of this module to the context
                            let ctx = ctx.clone().join(name.clone());

                            let m = match m {
                                asm::Module::External(_) => unreachable!(),
                                asm::Module::Local(m) => m,
                            };

                            match self.check_module(m, &ctx) {
                                Err(err) => {
                                    errors.extend(err);
                                }
                                Ok(m) => {
                                    res.extend(m);
                                }
                            };
                        }
                    }
                }
            }
        }

        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(res)
        }
    }

    fn check_instruction(
        &mut self,
        name: &str,
        instruction: parsed::asm::Instruction<T>,
    ) -> Result<Instruction<T>, Vec<String>> {
        if name == "return" {
            return Err(vec!["Instruction cannot use reserved name `return`".into()]);
        }

        if let InstructionBody::Local(statements) = &instruction.body {
            let errors: Vec<_> = statements
                .iter()
                .filter_map(|s| match s {
                    ast::parsed::PilStatement::PolynomialIdentity(_, _, _) => None,
                    ast::parsed::PilStatement::PermutationIdentity(
                        _, //
                        _, //
                        l,
                        _,
                    )
                    | ast::parsed::PilStatement::PlookupIdentity(_, _, l, _) => l
                        .selector
                        .is_some()
                        .then_some(format!("LHS selector not yet supported in {s}.")),
                    _ => Some(format!("Statement not allowed in instruction body: {s}")),
                })
                .collect();
            if !errors.is_empty() {
                return Err(errors);
            }
        }
        Ok(Instruction {
            params: instruction.params,
            body: instruction.body,
        })
    }
}

#[cfg(test)]
mod tests {
    use importer::resolve_str;
    use number::Bn254Field;

    use crate::check;

    // A utility to test behavior of the type checker on source inputs
    // TODO: test returned values, not just success
    fn expect_check_str(src: &str, expected: Result<(), Vec<&str>>) {
        let resolved = resolve_str::<Bn254Field>(src);
        let checked = check(resolved);
        assert_eq!(
            checked.map(|_| ()),
            expected.map_err(|e| e.into_iter().map(|s| s.to_string()).collect())
        )
    }

    // TODO: remove `should_panic` below by detecting that `A` is not a `Machine`
    #[test]
    #[should_panic = "Ok"]
    fn submachine_is_not_a_machine() {
        let src = r#"
        mod A {
        }
        machine M(l, i) {
            A a;
        }"#;
        expect_check_str(
            src,
            Err(vec![
                "Expected A to be of type Machine, but found type module",
            ]),
        );
    }
}
