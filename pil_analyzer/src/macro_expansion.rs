use std::{collections::HashMap, ops::ControlFlow};

use number::FieldElement;
use parser::ast::*;

use crate::pil_analyzer::MacroDefinition;

//TODO this could also go into the parser.

pub fn expand_macros<T>(
    macros: &HashMap<String, MacroDefinition<T>>,
    statement: Statement<T>,
) -> Vec<Statement<T>>
where
    T: FieldElement,
{
    let mut expander = MacroExpander {
        macros,
        arguments: vec![],
        parameter_names: Default::default(),
        statements: vec![],
    };
    expander.handle_statement(statement);
    std::mem::take(&mut expander.statements)
}

struct MacroExpander<'a, T> {
    macros: &'a HashMap<String, MacroDefinition<T>>,
    arguments: Vec<Expression<T>>,
    parameter_names: HashMap<String, usize>,
    statements: Vec<Statement<T>>,
}

impl<'a, T> MacroExpander<'a, T>
where
    T: FieldElement,
{
    pub fn handle_statement(&mut self, mut statement: Statement<T>) {
        match &mut statement {
            Statement::FunctionCall(_start, name, arguments) => {
                if !self.macros.contains_key(name) {
                    panic!(
                        "Macro {name} not found - only macros allowed at this point, no fixed columns."
                    );
                }
                // TODO check that it does not contain local variable references.
                // But we also need to do some other well-formedness checks.
                let mut arguments = std::mem::take(arguments);
                self.process_expressions(&mut arguments);
                if self.expand_macro(name, arguments).is_some() {
                    panic!("Invoked a macro in statement context with non-empty expression.");
                }
                return;
            }

            Statement::PlookupIdentity(_start, key, haystack) => {
                self.process_selected_expressions(key);
                self.process_selected_expressions(haystack);
            }
            Statement::PermutationIdentity(_start, left, right) => {
                self.process_selected_expressions(left);
                self.process_selected_expressions(right);
            }
            Statement::ConnectIdentity(_start, left, right) => {
                self.process_expressions(left);
                self.process_expressions(right);
            }
            Statement::Include(_, _)
            | Statement::PolynomialConstantDeclaration(_, _)
            | Statement::PolynomialCommitDeclaration(_, _, None) => {}

            Statement::Namespace(_, _, e)
            | Statement::PolynomialDefinition(_, _, e)
            | Statement::PolynomialIdentity(_, e)
            | Statement::PublicDeclaration(_, _, _, e)
            | Statement::ConstantDefinition(_, _, e) => self.process_expression(e),

            Statement::PolynomialConstantDefinition(_, _, f)
            | Statement::PolynomialCommitDeclaration(_, _, Some(f)) => {
                self.process_function_definition(f)
            }
            Statement::MacroDefinition(_, _, _, _, _) => {} // We expand lazily. Is that a mistake?
            Statement::ASMBlock(_, _) => {}
        };
        self.statements.push(statement);
    }

    fn expand_macro(&mut self, name: &str, arguments: Vec<Expression<T>>) -> Option<Expression<T>> {
        println!("Expanding {name}");
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
        expression
            .iter_mut()
            .for_each(|expr| self.process_expression(expr));
        self.arguments = old_arguments;
        self.parameter_names = old_parameters;
        expression
    }

    fn process_function_definition(&mut self, fun: &mut FunctionDefinition<T>) {
        // TODO the local parameters here should shield / shadow the macros.
        match fun {
            FunctionDefinition::Mapping(_, e) | FunctionDefinition::Query(_, e) => {
                self.process_expression(e)
            }
            FunctionDefinition::Array(ae) => self.process_array_expression(ae),
        }
    }

    fn process_array_expression(&mut self, ae: &mut ArrayExpression<T>) {
        match ae {
            ArrayExpression::Value(expressions) | ArrayExpression::RepeatedValue(expressions) => {
                self.process_expressions(expressions)
            }
            ArrayExpression::Concat(ae1, ae2) => {
                self.process_array_expression(ae1);
                self.process_array_expression(ae2)
            }
        }
    }

    fn process_expression(&mut self, expression: &mut Expression<T>) {
        postvisit_expression_mut(expression, &mut |e| {
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
        });
    }

    fn process_expressions(&mut self, exprs: &mut [Expression<T>]) {
        exprs.iter_mut().for_each(|e| self.process_expression(e))
    }

    fn process_selected_expressions(&mut self, exprs: &mut SelectedExpressions<T>) {
        if let Some(e) = &mut exprs.selector {
            self.process_expression(e)
        };
        self.process_expressions(&mut exprs.expressions);
    }
}
