#![deny(clippy::print_stdout)]

use std::{
    cell::RefCell,
    collections::{BTreeMap, HashSet},
};

use powdr_ast::{
    asm_analysis::{
        AnalysisASMFile, AssignmentStatement, CallableSymbolDefinitions, DebugDirective,
        FunctionBody, FunctionStatements, FunctionSymbol, InstructionDefinitionStatement,
        InstructionStatement, Item, LabelStatement, LinkDefinition, Machine, OperationSymbol,
        RegisterDeclarationStatement, RegisterTy, Return, SubmachineDeclaration,
    },
    parsed::{
        self,
        asm::{
            self, ASMModule, ASMProgram, AbsoluteSymbolPath, AssignmentRegister, FunctionStatement,
            Instruction, LinkDeclaration, MachineProperties, MachineStatement, Module,
            ModuleStatement, RegisterFlag, SymbolDefinition, SymbolValue,
        },
        EnumDeclaration, TraitDeclaration,
    },
};

/// Verifies certain properties of each machine and constructs the Machine objects.
/// Also transfers generic PIL definitions but does not verify anything about them.
pub fn check(file: ASMProgram) -> Result<AnalysisASMFile, Vec<String>> {
    let ctx = AbsoluteSymbolPath::default();
    let checker = TypeChecker::new(file.main);
    checker.check_path(&ctx)?;
    Ok(AnalysisASMFile {
        items: checker.analyzed.into_inner(),
    })
}

struct TypeChecker {
    /// The parsed AST as a single [SymbolValue::Module]
    ast: SymbolValue,
    /// The already checked paths
    checked_paths: RefCell<HashSet<AbsoluteSymbolPath>>,
    /// The checked items
    /// We use [RefCell] because we want to mutate this while borrowing from `parsed`
    analyzed: RefCell<BTreeMap<AbsoluteSymbolPath, Item>>,
}

impl TypeChecker {
    fn new(root: ASMModule) -> Self {
        Self {
            ast: SymbolValue::Module(Module::Local(root)),
            checked_paths: Default::default(),
            analyzed: Default::default(),
        }
    }

    fn check_path(&self, ctx: &AbsoluteSymbolPath) -> Result<(), Vec<String>> {
        // check if the path is already checked
        if self.checked_paths.borrow().contains(ctx) {
            return Ok(());
        }

        let symbol = ctx.parts().fold(&self.ast, |acc, part| match acc {
            SymbolValue::Module(Module::Local(m)) => m
                .statements
                .iter()
                .find_map(|s| match s {
                    ModuleStatement::SymbolDefinition(s) if s.name == part => Some(&s.value),
                    _ => None,
                })
                .unwrap(),
            s => s,
        });

        let res = match symbol {
            SymbolValue::Module(Module::Local(module)) => self.check_module(module, ctx),
            SymbolValue::Machine(machine) => self.check_machine_type(machine.clone(), ctx),
            SymbolValue::Expression(e) => self.check_expression(e.clone(), ctx),
            SymbolValue::TypeDeclaration(d) => self.check_type_declaration(d.clone(), ctx),
            SymbolValue::TraitDeclaration(d) => self.check_trait_declaration(d.clone(), ctx),
            _ => unreachable!(),
        };

        self.checked_paths.borrow_mut().insert(ctx.clone());

        res
    }

    fn check_machine_type(
        &self,
        machine: asm::Machine,
        ctx: &AbsoluteSymbolPath,
    ) -> Result<(), Vec<String>> {
        let mut errors = vec![];

        let mut registers = vec![];
        let mut pil = vec![];
        let mut instructions = vec![];
        let mut links = vec![];
        let mut callable = CallableSymbolDefinitions::default();
        let mut submachines = vec![];

        for s in machine.statements {
            match s {
                MachineStatement::RegisterDeclaration(source, name, flag) => {
                    let ty = match flag {
                        Some(RegisterFlag::IsAssignment) => RegisterTy::Assignment,
                        Some(RegisterFlag::IsPC) => RegisterTy::Pc,
                        Some(RegisterFlag::IsReadOnly) => RegisterTy::ReadOnly,
                        None => RegisterTy::Write,
                    };
                    registers.push(RegisterDeclarationStatement { source, name, ty });
                }
                MachineStatement::InstructionDeclaration(source, name, instruction) => {
                    match self.check_instruction(&name, instruction) {
                        Ok(instruction) => instructions.push(InstructionDefinitionStatement {
                            source,
                            name,
                            instruction,
                        }),
                        Err(e) => errors.extend(e),
                    }
                }
                MachineStatement::LinkDeclaration(
                    source,
                    LinkDeclaration {
                        flag,
                        link,
                        is_permutation,
                    },
                ) => links.push(LinkDefinition {
                    source,
                    instr_flag: None,
                    link_flag: flag,
                    to: link,
                    is_permutation,
                }),
                MachineStatement::Pil(_source, statement) => {
                    pil.push(statement);
                }
                MachineStatement::Submachine(_, ty, name, args) => {
                    let ty = AbsoluteSymbolPath::default().join(ty);

                    if let Err(e) = self.assert_type(&ty, Type::Machine) {
                        errors.extend(e);
                    }

                    args.iter().for_each(|arg| {
                        if arg.try_to_identifier().is_none() {
                            errors
                                .push(format!("submachine argument not a machine instance: {arg}"))
                        }
                    });
                    submachines.push(SubmachineDeclaration { name, ty, args });
                }
                MachineStatement::FunctionDeclaration(source, name, params, statements) => {
                    let mut function_statements = vec![];
                    for s in statements {
                        let statement_string = s.to_string();
                        match s {
                            FunctionStatement::Assignment(source, lhs, using_reg, rhs) => {
                                if let Some(using_reg) = &using_reg {
                                    if using_reg.len() != lhs.len() {
                                        errors.push(format!(
                                            "Mismatched number of registers for assignment {statement_string}"
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
                                        source,
                                        lhs_with_reg,
                                        rhs,
                                    }
                                    .into(),
                                );
                            }
                            FunctionStatement::Instruction(source, instruction, inputs) => {
                                function_statements.push(
                                    InstructionStatement {
                                        source,
                                        instruction,
                                        inputs,
                                    }
                                    .into(),
                                );
                            }
                            FunctionStatement::Label(source, name) => {
                                function_statements.push(LabelStatement { source, name }.into());
                            }
                            FunctionStatement::DebugDirective(source, directive) => {
                                function_statements
                                    .push(DebugDirective { source, directive }.into());
                            }
                            FunctionStatement::Return(source, values) => {
                                function_statements.push(Return { source, values }.into());
                            }
                        }
                    }
                    assert!(callable
                        .insert(
                            name,
                            FunctionSymbol {
                                source,
                                params,
                                body: FunctionBody {
                                    statements: FunctionStatements::new(function_statements),
                                },
                            },
                        )
                        .is_none());
                }
                MachineStatement::OperationDeclaration(source, name, id, params) => {
                    assert!(callable
                        .insert(name, OperationSymbol { source, id, params })
                        .is_none());
                }
            }
        }

        let MachineProperties {
            degree,
            latch,
            operation_id,
            call_selectors,
        } = machine.properties;

        if !registers.iter().any(|r| r.ty.is_pc()) {
            let operation_count = callable.operation_definitions().count();
            if operation_count > 0 && latch.is_none() {
                errors.push(format!(
                    "Machine {ctx} should have a latch column as it does not have a pc and has operations"
                ));
            }

            if operation_id.is_some() {
                for o in callable.operation_definitions() {
                    if o.operation.id.id.is_none() {
                        errors.push(format!(
                            "Operation `{}` in machine {ctx} needs an operation id because the machine has an operation id column",
                            o.name
                        ))
                    }
                }
            } else {
                // no operation id column
                if operation_count > 1 {
                    errors.push(format!(
                        "Machine {ctx} should have an operation id column as it does not have a pc and has more than one operation"
                    ));
                }
                if let Some(o) = callable.operation_definitions().next() {
                    if o.operation.id.id.is_some() {
                        errors.push(format!(
                            "Operation `{}` in machine {ctx} can't have an operation id because the machine does not have an operation id column",
                            o.name
                        ))
                    }
                }
            }

            for r in &registers {
                errors.push(format!(
                    "Machine {ctx} should not have registers as it does not have a pc, found `{}`",
                    r.name
                ));
            }

            for f in callable.function_definitions() {
                errors.push(format!(
                    "Machine {ctx} should not have functions as it does not have a pc, found `{}`",
                    f.name
                ))
            }

            for i in &instructions {
                errors.push(format!(
                    "Machine {ctx} should not have instructions as it does not have a pc, found `{}`",
                    i.name
                ))
            }
        } else {
            if latch.is_some() {
                errors.push(format!(
                    "Machine {ctx} should not have a latch column as it has a pc"
                ));
            }
            if operation_id.is_some() {
                errors.push(format!(
                    "Machine {ctx} should not have an operation id column as it has a pc"
                ));
            }
            if call_selectors.is_some() {
                errors.push(format!(
                    "Machine {ctx} should not have call_selectors as it has a pc"
                ));
            }
            for o in callable.operation_definitions() {
                errors.push(format!(
                    "Machine {ctx} should not have operations as it has a pc, found `{}`",
                    o.name
                ))
            }
        }

        if registers.iter().filter(|r| r.ty.is_pc()).count() > 1 {
            errors.push(format!("Machine {ctx} cannot have more than one pc"));
        }

        let machine = Machine {
            degree,
            latch,
            operation_id,
            call_selectors,
            params: machine.params,
            pc: registers
                .iter()
                .enumerate()
                .find_map(|(i, r)| (r.ty.is_pc()).then_some(i)),
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
            self.insert_item(ctx, Item::Machine(machine));
            Ok(())
        }
    }

    fn check_module(
        &self,
        module: &ASMModule,
        ctx: &AbsoluteSymbolPath,
    ) -> Result<(), Vec<String>> {
        let mut errors = vec![];

        for m in &module.statements {
            match m {
                ModuleStatement::SymbolDefinition(SymbolDefinition { name, .. }) => {
                    if let Err(e) = self.check_path(&ctx.with_part(name)) {
                        errors.extend(e);
                    }
                }
                ModuleStatement::TraitImplementation(trait_impl) => {
                    // treat trait implementations as items whose path is the module they are defined in
                    // see [https://github.com/powdr-labs/powdr/issues/1697]
                    self.insert_item(ctx, Item::TraitImplementation(trait_impl.clone()));
                }
            }
        }

        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(())
        }
    }

    fn check_expression(
        &self,
        e: parsed::TypedExpression,
        ctx: &AbsoluteSymbolPath,
    ) -> Result<(), Vec<String>> {
        // TODO: actually check the expression?
        self.insert_item(ctx, Item::Expression(e));
        Ok(())
    }

    fn check_type_declaration(
        &self,
        enum_decl: EnumDeclaration<parsed::Expression>,
        ctx: &AbsoluteSymbolPath,
    ) -> Result<(), Vec<String>> {
        // TODO: actually check the type declaration?
        self.insert_item(ctx, Item::TypeDeclaration(enum_decl));
        Ok(())
    }

    fn check_trait_declaration(
        &self,
        trait_decl: TraitDeclaration<parsed::Expression>,
        ctx: &AbsoluteSymbolPath,
    ) -> Result<(), Vec<String>> {
        // TODO: actually check the trait declaration?
        self.insert_item(ctx, Item::TraitDeclaration(trait_decl.clone()));
        Ok(())
    }

    fn check_instruction(
        &self,
        name: &str,
        instruction: parsed::asm::Instruction,
    ) -> Result<Instruction, Vec<String>> {
        if name == "return" {
            return Err(vec!["Instruction cannot use reserved name `return`".into()]);
        }

        let errors: Vec<_> = instruction
            .body
            .0
            .iter()
            .filter_map(|s| match s {
                // TODO this could be a function call that returns an identity including a selector in the future.
                powdr_ast::parsed::PilStatement::Expression(_, _) => None,
                powdr_ast::parsed::PilStatement::PermutationIdentity(_, l, _)
                | powdr_ast::parsed::PilStatement::PlookupIdentity(_, l, _) => l
                    .selector
                    .is_some()
                    .then_some(format!("LHS selector not yet supported in {s}.")),
                _ => Some(format!("Statement not allowed in instruction body: {s}")),
            })
            .collect();
        if !errors.is_empty() {
            return Err(errors);
        }
        Ok(Instruction {
            params: instruction.params,
            body: instruction.body,
            links: instruction.links,
        })
    }

    /// Get the type of a path. If required, check the path first.
    fn type_of(&self, path: &AbsoluteSymbolPath) -> Result<Type, Vec<String>> {
        self.check_path(path)?;

        Ok(self
            .analyzed
            .borrow()
            .get(path)
            .map(|item| match item {
                Item::Machine(_) => Type::Machine,
                Item::Expression(_) => Type::Expression,
                Item::TypeDeclaration(_) => Type::Ty,
                Item::TraitImplementation(_) => unreachable!(),
                Item::TraitDeclaration(_) => Type::Trait,
            })
            // if a path isn't found in the output, it must have been a module
            // this could be cleaner
            .unwrap_or(Type::Module))
    }

    fn assert_type(&self, path: &AbsoluteSymbolPath, expected: Type) -> Result<(), Vec<String>> {
        let found = self.type_of(path)?;
        if found != expected {
            Err(vec![format!(
                "Expected {path} to be of type {expected:?}, found type {found:?}"
            )])
        } else {
            Ok(())
        }
    }

    fn insert_item(&self, path: &AbsoluteSymbolPath, item: Item) {
        assert!(self
            .analyzed
            .borrow_mut()
            .insert(path.clone(), item)
            .is_none());
    }
}

#[derive(PartialEq, Eq, Debug)]
enum Type {
    Machine,
    Module,
    Expression,
    Ty,
    Trait,
}

#[cfg(test)]
mod tests {
    use powdr_importer::load_dependencies_and_resolve_str;

    use super::check;

    // A utility to test behavior of the type checker on source inputs
    // TODO: test returned values, not just success
    fn expect_check_str(src: &str, expected: Result<(), Vec<&str>>) {
        let resolved = load_dependencies_and_resolve_str(src);
        let checked = check(resolved);
        assert_eq!(
            checked.map(|_| ()),
            expected.map_err(|e| e.into_iter().map(|s| s.to_string()).collect())
        )
    }

    #[test]
    fn submachine_is_not_a_machine() {
        let src = r#"
        mod A {
        }
        machine M with latch: l, operation_id: i {
            A a;
        }"#;
        expect_check_str(
            src,
            Err(vec![
                "Expected ::A to be of type Machine, found type Module",
            ]),
        );
    }

    #[test]
    fn constrained_machine_has_no_registers() {
        let src = r#"
machine Main with latch: latch, operation_id: id {
   reg A;
}
"#;
        expect_check_str(
            src,
            Err(vec![
                "Machine ::Main should not have registers as it does not have a pc, found `A`",
            ]),
        );
    }

    #[test]
    fn virtual_machine_with_links() {
        let src = r#"
machine Main {
   reg pc[@pc];
   reg A;
   reg B;

   link => B = submachine.foo(A);
}
"#;
        expect_check_str(src, Ok(()));
    }

    #[test]
    fn multiple_ops_need_op_id() {
        let src = r#"
machine Arith with latch: latch {
   operation add a, b -> c;
   operation sub a, b -> c;
}
"#;
        expect_check_str(src, Err(vec!["Machine ::Arith should have an operation id column as it does not have a pc and has more than one operation"]));
    }

    #[test]
    fn id_column_requires_op_id() {
        let src = r#"
machine Arith with latch: latch, operation_id: id  {
   operation add a, b -> c;
   operation sub a, b -> c;
}
"#;
        expect_check_str(src, Err(vec!["Operation `add` in machine ::Arith needs an operation id because the machine has an operation id column",
                                       "Operation `sub` in machine ::Arith needs an operation id because the machine has an operation id column"]));
    }

    #[test]
    fn id_op_id_requires_id_column() {
        let src = r#"
machine Arith with latch: latch {
   operation add<0> a, b -> c;
}
"#;
        expect_check_str(src, Err(vec!["Operation `add` in machine ::Arith can't have an operation id because the machine does not have an operation id column"]));
    }

    #[test]
    fn virtual_machine_has_no_call_selectors() {
        let src = r#"
machine Main with call_selectors: sel {
   reg pc[@pc];
}
"#;
        expect_check_str(
            src,
            Err(vec![
                "Machine ::Main should not have call_selectors as it has a pc",
            ]),
        );
    }
}
