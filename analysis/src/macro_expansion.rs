use std::{
    collections::{HashMap, HashSet},
    ops::ControlFlow,
};

use ast::parsed::{
    asm::{
        ASMModule, ASMProgram, Instruction, InstructionBody, MachineStatement, ModuleStatement,
        SymbolDefinition,
    },
    postvisit_expression_in_statement_mut, postvisit_expression_mut, Expression,
    FunctionDefinition, PilStatement,
};
use number::FieldElement;

pub fn expand<T: FieldElement>(program: ASMProgram<T>) -> ASMProgram<T> {
    let mut expander = MacroExpander::default();
    ASMProgram {
        main: expander.expand_asm(program.main),
    }
}

#[derive(Debug, Default)]
pub struct MacroExpander<T> {
    macros: HashMap<String, MacroDefinition<T>>,
    arguments: Vec<Expression<T>>,
    parameter_names: HashMap<String, usize>,
    shadowing_locals: HashSet<String>,
    statements: Vec<PilStatement<T>>,
}

#[derive(Debug)]
struct MacroDefinition<T> {
    pub parameters: Vec<String>,
    pub identities: Vec<PilStatement<T>>,
    pub expression: Option<Expression<T>>,
}

impl<T> MacroExpander<T>
where
    T: FieldElement,
{
    fn expand_asm(&mut self, module: ASMModule<T>) -> ASMModule<T> {
        let statements = module
            .statements
            .into_iter()
            .map(|mut s| {
                match &mut s {
                    ModuleStatement::SymbolDefinition(SymbolDefinition {
                        ref mut value, ..
                    }) => {
                        match value {
                            ast::parsed::asm::SymbolValue::Machine(m) => {
                                m.statements.iter_mut().for_each(|s| match s {
                                    MachineStatement::InstructionDeclaration(
                                        _,
                                        _,
                                        Instruction { body, .. },
                                    ) => {
                                        match body {
                                            InstructionBody::Local(body) => {
                                                *body = self.expand_macros(std::mem::take(body))
                                            }
                                            InstructionBody::CallableRef(..) => {
                                                // there is nothing to expand in a callable ref
                                            }
                                        }
                                    }
                                    MachineStatement::InlinePil(_, statements) => {
                                        *statements =
                                            self.expand_macros(std::mem::take(statements));
                                    }
                                    _ => {}
                                });
                            }
                            ast::parsed::asm::SymbolValue::Import(_) => {
                                // there is nothing to expand inside an import statement
                            }
                            ast::parsed::asm::SymbolValue::Module(ref mut m) => {
                                let m = match m {
                                    ast::parsed::asm::Module::External(_) => unreachable!(),
                                    ast::parsed::asm::Module::Local(m) => m,
                                };

                                *m = self.expand_asm(std::mem::take(m));
                            }
                        }
                    }
                };
                s
            })
            .collect();
        ASMModule { statements }
    }

    /// Expands all macro references inside the statements and also adds
    /// any macros defined therein to the list of macros.
    ///
    /// Note that macros are not namespaced!
    pub fn expand_macros(&mut self, statements: Vec<PilStatement<T>>) -> Vec<PilStatement<T>> {
        assert!(self.statements.is_empty());
        for statement in statements {
            self.handle_statement(statement);
        }
        std::mem::take(&mut self.statements)
    }

    fn handle_statement(&mut self, mut statement: PilStatement<T>) {
        let mut added_locals = false;
        if let PilStatement::PolynomialConstantDefinition(_, _, f)
        | PilStatement::PolynomialCommitDeclaration(_, _, Some(f)) = &statement
        {
            if let FunctionDefinition::Mapping(params, _) | FunctionDefinition::Query(params, _) = f
            {
                assert!(self.shadowing_locals.is_empty());
                self.shadowing_locals.extend(params.iter().cloned());
                added_locals = true;
            }
        }

        postvisit_expression_in_statement_mut(&mut statement, &mut |e| self.process_expression(e));

        match &mut statement {
            PilStatement::FunctionCall(_start, name, arguments) => {
                if !self.macros.contains_key(name) {
                    panic!(
                        "Macro {name} not found - only macros allowed at this point, no fixed columns."
                    );
                }
                if self.expand_macro(name, std::mem::take(arguments)).is_some() {
                    panic!("Invoked a macro in statement context with non-empty expression.");
                }
            }
            PilStatement::MacroDefinition(_start, name, parameters, statements, expression) => {
                // We expand lazily. Is that a mistake?
                let is_new = self
                    .macros
                    .insert(
                        std::mem::take(name),
                        MacroDefinition {
                            parameters: std::mem::take(parameters),
                            identities: std::mem::take(statements),
                            expression: std::mem::take(expression),
                        },
                    )
                    .is_none();
                assert!(is_new);
            }
            _ => self.statements.push(statement),
        };

        if added_locals {
            self.shadowing_locals.clear();
        }
    }

    fn expand_macro(&mut self, name: &str, arguments: Vec<Expression<T>>) -> Option<Expression<T>> {
        let old_arguments = std::mem::replace(&mut self.arguments, arguments);

        let mac = &self
            .macros
            .get(name)
            .unwrap_or_else(|| panic!("Macro {name} not found."));
        let parameters = mac
            .parameters
            .iter()
            .enumerate()
            .map(|(i, n)| (n.clone(), i))
            .collect();
        let old_parameters = std::mem::replace(&mut self.parameter_names, parameters);

        let mut expression = mac.expression.clone();
        let identities = mac.identities.clone();
        for identity in identities {
            self.handle_statement(identity)
        }
        if let Some(e) = &mut expression {
            postvisit_expression_mut(e, &mut |e| self.process_expression(e));
        };

        self.arguments = old_arguments;
        self.parameter_names = old_parameters;
        expression
    }

    fn process_expression(&mut self, e: &mut Expression<T>) -> ControlFlow<()> {
        if let Expression::PolynomialReference(poly) = e {
            if poly.namespace().is_none() && self.parameter_names.contains_key(poly.name()) {
                // TODO to make this work inside macros, "next" and "index" need to be
                // their own ast nodes / operators.
                assert!(!poly.shift());
                assert!(poly.index().is_none());
                *e = self.arguments[self.parameter_names[poly.name()]].clone()
            }
        } else if let Expression::FunctionCall(call) = e {
            if self.macros.contains_key(call.id.as_str()) {
                *e = self
                    .expand_macro(call.id.as_str(), std::mem::take(&mut call.arguments))
                    .expect("Invoked a macro in expression context with empty expression.")
            }
        }

        ControlFlow::<()>::Continue(())
    }
}
