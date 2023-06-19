use std::{
    collections::{HashMap, HashSet},
    ops::ControlFlow,
};

use ast::parsed::{
    asm::{ASMFile, ASMStatement, InstructionBodyElement},
    postvisit_expression_in_statement_mut, postvisit_expression_mut, Expression,
    FunctionDefinition, SelectedExpressions, Statement,
};
use number::FieldElement;

pub fn expand<T: FieldElement>(file: ASMFile<T>) -> ASMFile<T> {
    let mut expander = MacroExpander::default();
    expander.expand_asm(file)
}

#[derive(Debug, Default)]
pub struct MacroExpander<T> {
    macros: HashMap<String, MacroDefinition<T>>,
    arguments: Vec<Expression<T>>,
    parameter_names: HashMap<String, usize>,
    shadowing_locals: HashSet<String>,
    statements: Vec<Statement<T>>,
}

#[derive(Debug)]
struct MacroDefinition<T> {
    pub parameters: Vec<String>,
    pub identities: Vec<Statement<T>>,
    pub expression: Option<Expression<T>>,
}

impl<T> MacroExpander<T>
where
    T: FieldElement,
{
    fn expand_asm(&mut self, mut file: ASMFile<T>) -> ASMFile<T> {
        let mut expander = MacroExpander::default();
        file.0.iter_mut().for_each(|s| match s {
            ASMStatement::InstructionDeclaration(_, _, _, body) => {
                body.iter_mut().for_each(|e| match e {
                    InstructionBodyElement::Expression(e) => {
                        self.process_expression(e);
                    }
                    InstructionBodyElement::PlookupIdentity(left, _, right) => {
                        self.process_selected_expressions(left);
                        self.process_selected_expressions(right);
                    }
                    InstructionBodyElement::FunctionCall(_, inputs) => {
                        self.process_expressions(inputs);
                    }
                });
            }
            ASMStatement::InlinePil(_, statements) => {
                *statements = expander.expand_macros(std::mem::take(statements));
            }
            _ => {}
        });
        file
    }

    /// Expands all macro references inside the statements and also adds
    /// any macros defined therein to the list of macros.
    ///
    /// Note that macros are not namespaced!
    pub fn expand_macros(&mut self, statements: Vec<Statement<T>>) -> Vec<Statement<T>> {
        assert!(self.statements.is_empty());
        for statement in statements {
            self.handle_statement(statement);
        }
        std::mem::take(&mut self.statements)
    }

    fn handle_statement(&mut self, mut statement: Statement<T>) {
        let mut added_locals = false;
        if let Statement::PolynomialConstantDefinition(_, _, f)
        | Statement::PolynomialCommitDeclaration(_, _, Some(f)) = &statement
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
            Statement::FunctionCall(_start, name, arguments) => {
                if !self.macros.contains_key(name) {
                    panic!(
                        "Macro {name} not found - only macros allowed at this point, no fixed columns."
                    );
                }
                if self.expand_macro(name, std::mem::take(arguments)).is_some() {
                    panic!("Invoked a macro in statement context with non-empty expression.");
                }
            }
            Statement::MacroDefinition(_start, name, parameters, statements, expression) => {
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
            if poly.namespace.is_none() && self.parameter_names.contains_key(&poly.name) {
                // TODO to make this work inside macros, "next" and "index" need to be
                // their own ast nodes / operators.
                assert!(!poly.next);
                assert!(poly.index.is_none());
                *e = self.arguments[self.parameter_names[&poly.name]].clone()
            }
        } else if let Expression::FunctionCall(name, arguments) = e {
            if self.macros.contains_key(name.as_str()) {
                *e = self
                    .expand_macro(name, std::mem::take(arguments))
                    .expect("Invoked a macro in expression context with empty expression.")
            }
        }

        ControlFlow::<()>::Continue(())
    }

    fn process_expressions(&mut self, exprs: &mut [Expression<T>]) -> ControlFlow<()> {
        for e in exprs.iter_mut() {
            self.process_expression(e)?;
        }
        ControlFlow::Continue(())
    }

    fn process_selected_expressions(
        &mut self,
        exprs: &mut SelectedExpressions<T>,
    ) -> ControlFlow<()> {
        if let Some(e) = &mut exprs.selector {
            self.process_expression(e)?;
        };
        self.process_expressions(&mut exprs.expressions)
    }
}
